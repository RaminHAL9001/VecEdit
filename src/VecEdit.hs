module VecEdit
  ( main,
    Hack, HackEnv, newHackEnv, runHack,
    HackEnvState, bufferTable, workerTable, processTable,
    Buffer, TextTags, newBuffer, listBuffers, bufferHandle, bufferFile, showBuffer,
    Worker, WorkerStatus, listWorkers, getWorkerStatus, startWork,
    Process, listProcesses, runInBuffer, sendToProc, rerunProcess,
    newReadPipeControl, pipeToBuffer,
    GlobalTableSearchIndex(..),
    withBuffer, withBufferPath,
    redisplay, redisplayIOBuffer, debugViewBuffer,
    ------------------------
    testEditor, testByteStreamToLines, testGapBuffer, testTextEditor,
    ------------------------
    module Control.Monad,
    module VecEdit.Text.Editor,
    module VecEdit.Text.String,
    module VecEdit.Text.DisplayInfo,
  ) where

import VecEdit.Types
  ( VectorSize, RelativeDirection(..), Range(..), TextRange(..), LineIndex(..),
    GapBufferError(..), EditTextError(..),
    ppGapBufferErrorInfo, 
  )

import qualified VecEdit.Vector.Editor as Vector
import VecEdit.Vector.Editor
  ( EditorMVectorType, EditorMVectorElem,
    liftEditor, sliceRange, currentBuffer, ralign6,
  )
import VecEdit.Vector.Editor.GapBuffer
  ( newGapBufferState, evalGapBuffer,
    shiftCursor, pushItem, pullItem, popItem,
    gapBuffer3SliceInRange, rethrowErrorInfo,
  )
import VecEdit.Text.DisplayInfo (DisplayInfo(..), showAsText)
import VecEdit.Text.String
  ( TextLine, ReadLines, HaltReadLines, Word8GapBuffer, StringData,
    fromStringData, hReadLines, hReadLineBufferSize,
    streamByteString, byteStreamToLines, cutUTF8TextLine,
    isAsciiOnly, readLinesLiftGapBuffer, convertString, theTextLineData,
    textLineIsUndefined,
  )
import VecEdit.Text.Editor
  ( EditText, EditTextState,
    newEditTextState, runEditText, evalEditText,
    lineNumber, pushLine, flushLine, loadHandleEditText, cursorToEnd, insertString,
    foldLinesFromCursor, foldLinesInRange, loadHandleEditText,
    editTextLiftGapBuffer, debugViewTextEditor,
  )
import qualified VecEdit.Table as Table
import VecEdit.Table (Table, EditTable)

import VecEdit.Text.TokenizerTable (tokTableFold)
import VecEdit.Text.LineBreak (allLineBreaks, allLineBreaks, lineBreak_LF)

import Control.Concurrent (ThreadId, forkIO, myThreadId)
import Control.Concurrent.MVar
       (MVar, newEmptyMVar, newMVar, readMVar, modifyMVar, modifyMVar_, takeMVar, putMVar, withMVar)
import Control.Exception (SomeException, IOException, try, catch)
import Control.Lens (Lens', lens, use, (^.), (%=), (.=), (+=))
import Control.Monad -- re-exporting
import Control.Monad.Except (MonadError(..))
import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Monad.Reader (MonadReader, ask, ReaderT(..))
import Control.Monad.State (MonadState(..), StateT(..), runStateT, modify)
import Control.Monad.Trans (lift)

import qualified Data.ByteString.UTF8 as UTF8
import Data.Char (isSpace)
import Data.Function (on)
import qualified Data.Vector as Vec
import Data.Vector.Mutable (MVector, IOVector)
import qualified Data.Vector.Mutable as MVec
import qualified Data.Text as Strict
import qualified Data.Text.IO as Strict
import qualified Data.Text.Lazy as Lazy
import Data.Time.Clock (UTCTime, getCurrentTime)

import System.IO (Handle, IOMode(ReadMode), openFile, hClose)
import System.Process
       ( ProcessHandle, CreateProcess, CmdSpec(..), Pid, StdStream(Inherit),
         getPid, createProcess, cmdspec, std_in, std_out, waitForProcess
       )
import System.Exit (ExitCode(..))

----------------------------------------------------------------------------------------------------

textShow :: Show a => a -> Strict.Text
textShow = Strict.pack . show

----------------------------------------------------------------------------------------------------

newtype Buffer = Buffer (MVar BufferState)
  deriving (Eq)

type TextTags = ()

-- | This is an element of a 'HackEnv' which contains metadata for manipulating the buffer in
-- GHCI. This is also the content accessible by a 'Buffer' handle.
data BufferState
  = BufferState
    { theBufStateFilePath :: !Strict.Text
    , theBufStateLockedBy :: !(Maybe (Table.Row Worker))
    , theBufStateBuffer   :: !(EditTextState TextTags)
    }

instance DisplayInfo BufferState where
  displayInfo putStr
    (BufferState
     {theBufStateFilePath=path
     ,theBufStateLockedBy=locked
     }) = do
      putStr path
      maybe (pure ()) (\ a -> putStr " " >> displayInfo putStr a) locked

instance DisplayInfo Buffer where
  displayInfo putStr (Buffer mvar) =
    readMVar mvar >>=
    displayInfo putStr

bufStateBuffer :: Lens' BufferState (EditTextState TextTags)
bufStateBuffer = lens theBufStateBuffer $ \ a b -> a{ theBufStateBuffer = b }

bufStateFilePath :: Lens' BufferState Strict.Text
bufStateFilePath = lens theBufStateFilePath $ \ a b -> a{ theBufStateFilePath = b }

----------------------------------------------------------------------------------------------------

-- | This is a thread that can perform work without freezing the controlling thread which runs the
-- REPL.
data Worker
  = Worker
    { theWorkerId :: !ThreadId
    , theWorkerStatus :: !(MVar WorkerStatus)
    }

-- | The "status" of a worker is the state of operation the 'Worker' is in at any given time.
data WorkerStatus
  = Ready
  | Working
  | Success
  | WorkerError !EditTextError
  | WorkerFailed !SomeException
  deriving Show

instance Eq  Worker where { (==) = (==) `on` theWorkerId; }
instance Ord Worker where { compare = compare `on` theWorkerId; }

instance DisplayInfo Worker where
  displayInfo putStr (Worker{theWorkerId=thid,theWorkerStatus=mvar}) = do
    stat <- readMVar mvar
    putStr $ Lazy.toStrict $ showAsText thid
    putStr " "
    putStr $ Lazy.toStrict $ showAsText stat

-- | Get the state of operation of a 'Worker'.
getWorkerStatus :: MonadIO io => Worker -> io WorkerStatus
getWorkerStatus = liftIO . readMVar . theWorkerStatus

----------------------------------------------------------------------------------------------------

-- | An entry in the global process table, it can be used to launch and relaunch processes, retrieve
-- the process exit code, and retrieve any output from the process that was captured in a buffer.
newtype Process = Process (MVar ProcessConfig)

data ProcessConfig
  = ProcessConfig
    { theProcSpec :: !CreateProcess
    , theProcState :: !ProcessState
    , theProcInPipe :: !(Maybe WritePipeControl)
    , theProcOutPipe :: !(Maybe (ReadPipeControl Int))
    , theProcErrPipe :: !(Maybe (ReadPipeControl Int))
    , theProcWaitThread :: !ThreadId
    , theProcStartTime :: !(Maybe UTCTime)
    , theProcEndTime :: !(Maybe UTCTime)
    , theProcCaptureBuffer :: !(Maybe Buffer)
    }

-- | Describes what state a 'Process' is in.
data ProcessState
  = ProcPending -- ^ Process is configured but hasn't been run yet.
  | ProcRunning
    { theProcStateProcHandle :: !ProcessHandle
    , theProcStatePid :: !Pid
    } -- ^ Process is running with this given handle.
  | ProcHalted
    { theProcStateExitCode :: !ExitCode
    } -- ^ Process halted with this exit code.

instance Show ProcessState where
  show = \ case
    ProcPending -> "ProcPending"
    ProcRunning{theProcStatePid=pid} -> "ProcRunning " <> show pid
    ProcHalted{theProcStateExitCode=exid} -> "ProcHalted " <> show exid

instance DisplayInfo ProcessConfig where
  displayInfo putStr cfg = do
    putStr $ Strict.pack $ show $ theProcState cfg
    putStr " "
    putStr $ Strict.pack $ show $ theProcSpec cfg

instance DisplayInfo Process where
  displayInfo putStr (Process mvar) = readMVar mvar >>= displayInfo putStr

-- | An element of the 'ProcessState', if this is not 'Nothing' it indicates that 'theProcSpec' is
-- defined to use the 'CreatePipe' flag to either 'std_out' or 'std_err'. If a pipe is created, the
-- associated 'Handle' is passed to a function that runs in it's own thread to buffer all of the
-- characters read-from/written-to the 'Handle'. If the buffered characters are also stored into a
-- 'Buffer' 'thePipeBuffer' will be non-'Nothing' so you can retrieve the 'Buffer' to which this
-- process buffered lines.
data ReadPipeControl fold
  = ReadPipeControl
    { thePipeHandle :: !Handle
    , thePipeThread :: !ThreadId
    , thePipeBuffer :: !(Maybe Buffer)
    , thePipeWaitClose :: !(MVar (Either EditTextError (), fold))
    }

-- | Objects of this data type allow input to be written to the standard input of a child process.
newtype WritePipeControl = WritePipeControl Handle

writePipeControl :: Maybe Handle -> Maybe WritePipeControl
writePipeControl = fmap WritePipeControl

closeWritePipe :: MonadIO io => Maybe WritePipeControl -> io ()
closeWritePipe =
  liftIO .
  ( maybe (pure ()) $ \ (WritePipeControl handle) ->
    try (hClose handle) >>= \ case
      Left err -> seq (err :: IOException) $ pure ()
      Right () -> pure ()
  )

closeReadPipe :: MonadIO io => Maybe (ReadPipeControl fold) -> io ()
closeReadPipe = liftIO . maybe (pure ()) (hClose . thePipeHandle)

-- | Construct a 'PipeControl' for a particular 'Handle' that receives the output or error stream of
-- a child process. Pass a 'ReadLines' function to handle each line of input. If this 'ReadLines'
-- function throws an exception, the parent process handle is closed, which may or may not end the
-- child process. This function returns a new 'PipeControl' and an 'MVar' which will be filled when
-- the 'ReadLines' function halts. If you read this 'MVar' your thread will block until the child
-- process closes the pipe's file handle, which may not happen until the child process halts.
newReadPipeControl
  :: MonadIO io
  => Maybe Handle
  -> (HaltReadLines fold void -> TextLine tags -> ReadLines fold ())
  -> fold
  -> io (Maybe (ReadPipeControl fold))
newReadPipeControl stream useLine fold = liftIO $ case stream of
  Nothing -> pure Nothing
  Just stream -> do
    foldMVar <- newEmptyMVar
    lineBuffer <- newGapBufferState Nothing hReadLineBufferSize  
    thid <- forkIO $ do
      result <- hReadLines lineBuffer stream useLine fold
      hClose stream
      putMVar foldMVar result
    pure $ Just $
      ReadPipeControl
      { thePipeHandle = stream
      , thePipeThread = thid
      , thePipeBuffer = Nothing
      , thePipeWaitClose = foldMVar
      }

-- | Like 'newReadPipeControl' but simply writes all lines into the given 'Buffer'. The number of
-- lines read are sent to the 'MVar'.
pipeToBuffer
  :: MonadIO io
  => Maybe Handle
  -> RelativeDirection -- ^ incoming lines can be buffered in reverse order
  -> Buffer
  -> io (Maybe (ReadPipeControl Int))
pipeToBuffer stream rel buffer =
  newReadPipeControl stream
  ( const $
    liftIO . withBuffer0 buffer . pushLine rel >=> \ case
      Left err -> readLinesLiftGapBuffer $ errorEditTextToReadLine err
      Right () -> modify (+ 1)
  ) 0

errorEditTextToReadLine :: EditTextError -> Word8GapBuffer void
errorEditTextToReadLine = \ case
  EditTextError  err -> rethrowErrorInfo err
  EditLineError  err -> rethrowErrorInfo err
  TextEditUndefined  -> throwError $ GapBufferFail "(evaluated to 'empty')"
  EditTextFailed msg -> throwError $ GapBufferFail msg
  EditLineIndexError i lo hi ->
    throwError $
    GapBufferFail $
    "(index " <> textShow i <>
    " out of bounds, limits are " <> textShow lo <> ".." <> textShow hi <> ")"
  EditCharIndexError line i lo hi ->
    throwError $
    GapBufferFail $
    "(on line " <> textShow line <>
    ": char index " <> textShow i <>
    " out of bounds, limits are " <> textShow lo <> ".." <> textShow hi <> ")"

labelCreateProcess :: CreateProcess -> Strict.Text
labelCreateProcess mkproc =
  case cmdspec mkproc of
    ShellCommand path -> Strict.pack $ takeWhile (not . isSpace) $ dropWhile isSpace path
    RawCommand path args -> Strict.unwords $ Strict.pack <$> (path : args)

rerunProcess0 :: MonadIO io => Process -> io ()
rerunProcess0 (Process mvar) =
  liftIO $
  modifyMVar_ mvar $ \ cfg -> do
    (pin, pout, perr, phandle) <- createProcess $ theProcSpec cfg
    pid <- getPid phandle
    case pid of
      Nothing -> do
        this <- myThreadId
        t0 <- getCurrentTime
        exitCode <- waitForProcess phandle
        t1 <- getCurrentTime
        pure cfg
          { theProcState = ProcHalted exitCode
          , theProcInPipe = Nothing
          , theProcOutPipe = Nothing
          , theProcErrPipe = Nothing
          , theProcWaitThread = this
          , theProcStartTime = Just t0
          , theProcEndTime = Just t1
          }
      Just pid -> do
        let inPipe = writePipeControl pin
        let capture pipe =
              maybe
              (pure Nothing)
              (pipeToBuffer pipe Before)
              (theProcCaptureBuffer cfg)
        errPipe <- capture perr
        outPipe <- capture pout
        waitThread <- forkIO $ do
          exitCode <- waitForProcess phandle
          t1 <- getCurrentTime
          closeWritePipe inPipe
          closeReadPipe outPipe
          closeReadPipe errPipe
          modifyMVar_ mvar $ \ cfg -> pure $ cfg
            { theProcState = ProcHalted exitCode
            , theProcEndTime = Just t1
            }
        t0 <- getCurrentTime
        pure cfg
          { theProcState =
            ProcRunning
            { theProcStateProcHandle = phandle
            , theProcStatePid = pid
            }
          , theProcInPipe = inPipe
          , theProcOutPipe = outPipe
          , theProcErrPipe = errPipe
          , theProcWaitThread = waitThread
          , theProcStartTime = Just t0
          , theProcEndTime = Nothing
          }

-- | Re-run a particular 'Process'. The 'Table.Row' entry is reused, so it will have the same row ID
-- and label as before, and will remain in the process table in the same position as before.
rerunProcess :: MonadIO io => Table.Row Process -> io ()
rerunProcess = void . rerunProcess0 . Table.theRowObject

-- | not for export
--
-- Create the meta-information around a 'CreateProcess' specification that will be used by the
-- mechanisms in this module for controlling child processes.
newProcessConfig :: MonadIO io => CreateProcess -> io ProcessConfig
newProcessConfig exe = liftIO $ do
  thid <- myThreadId
  pure ProcessConfig
    { theProcSpec = exe
    , theProcState = ProcPending
    , theProcInPipe = Nothing
    , theProcOutPipe = Nothing
    , theProcErrPipe = Nothing
    , theProcWaitThread = thid
    , theProcStartTime = Nothing
    , theProcEndTime = Nothing
    , theProcCaptureBuffer = Nothing
    }

-- | Run an external system process from a 'CreateProcess' spec, store it's output in the process
-- table. If you configure an input pipe, you can dump strings to the process's standard input
-- stream using the 'sendToProc' function. All output from the process created if buffered in the
-- given 'Buffer'. Note that the 'CreateProcess' 'std_out' and 'std_in' fields are forced to
-- 'Inherit' regardless of it's value when passed to this function, this is to force the capture of
-- the process output into the given 'Buffer', and to allow input to be sent to the process, since
-- it will run asynchronously.
--
-- This function returns a 'Table.Row' because, like with 'Buffer's, any new process created has a
-- handle for it stored in a table in the 'Hack' monad execution environment so it can be retrieved
-- at any time.
runInBuffer :: Table.Row Buffer -> CreateProcess -> Hack (Table.Row Process)
runInBuffer buffer exe =
  editHackEnvTable processTable $ do
    mvar <- liftIO $ do
      cfg <- newProcessConfig (exe{ std_in = Inherit, std_out = Inherit })
      newMVar (cfg{ theProcCaptureBuffer = Just $ Table.theRowObject buffer })
    Table.insert (labelCreateProcess exe) $ Process mvar

-- | If the 'Process' was created with the 'std_in' field of the 'CreateProcess' spec set to
-- 'Inherit', a 'Handle' is available for sending input to the Process (unless the 'Process' has
-- recently closed this handle). This function allows you to send a string of input to the
-- 'Process'.
sendToProc :: MonadIO io => Table.Row Process -> StringData -> io ()
sendToProc row str = liftIO $ do
  let (Process mvar) = Table.theRowObject row
  withMVar mvar $ \ cfg -> case theProcInPipe cfg of
    Nothing -> fail $ show (Table.theRowLabel row) <> " process created without input pipe"
    Just (WritePipeControl handle) -> case fromStringData str of
      Nothing -> pure ()
      Just txt -> Strict.hPutStr handle txt

----------------------------------------------------------------------------------------------------

-- | This is the monad that contains the global state, including a list of all buffers, and a list
-- of all processes and threads created by the APIs in this module. It is the global runtime
-- environment. Sorry, but I couldn't think of a better name for it, 'Hack' somehow seemed better
-- than more descriptive but possibly confusing names like 'Global' or 'Runtime' or 'Environment'.
newtype Hack a = Hack (ReaderT HackEnv (StateT HackEnvState IO) a)
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader HackEnv)

-- | Provided a 'HackEnv' execution environment to keep all mutable state, evaluate a 'Hack'
-- function within the @IO@ context. Any changes made to the 'HackEnv' by evaluation of the 'Hack'
-- function will be remembered and visible to any later call to a 'Hack' function with that same
-- 'HackEnv'.
--
-- __DANGER:__ never run this function from within 'liftIO' in another 'Hack' monad, it will almost
-- certainly result in deadlocks, since this function itself obtains a lock and does not release it
-- until evaluation of the 'Hack' function completes. The only exception is if you evaluate this
-- function using @'liftIO' . 'forkIO'@, or if you are evaluating an entirely different 'HackEnv'
-- from the 'HackEnv' context of the current 'Hack' monad.
runHack :: Hack a -> HackEnv -> IO a
runHack (Hack (ReaderT f)) env@(HackEnv mvar) =
  modifyMVar mvar $ fmap (\ (a,b) -> (b,a) ) . runStateT (f env)

-- | Values of this type maintain mutable data in an execution environment for evaluating functions
-- of type 'Hack', it contains tables of elements that should not be lost even if their handles go
-- out of scope. This data type locks all data shared between threads within an 'MVar' to ensure
-- thread safety.
newtype HackEnv = HackEnv (MVar HackEnvState)

-- | This is the mutable state data held within the 'HackEnv'. It is possible to perform read-only
-- operations on this data indirectly with functions like 'listHackEnv', which can perform 'Table'
-- opereations on the data within. There are 3 tables stored within: the 'bufferTable' holds
-- in-memory data buffers, the 'workerTable' stores managed threads, and the 'processTable' stores
-- child processes.
data HackEnvState
  = HackEnvState
    { theBufferTable :: !(Table Buffer)
    , theWorkerTable :: !(Table Worker)
    , theProcessTable :: !(Table Process)
    }

-- | Create an execution environment shared by all calls to functions in the 'Hack' monad, and
-- across all threads. (It is thread-safe). Any changes made to this execution environment by
-- evaluation of a 'Hack' function will be remembered, and visible to later calls of other 'Hack'
-- functions.
newHackEnv :: IO HackEnv
newHackEnv = do
  buffers <- Table.new 128
  workers <- Table.new 32
  processes <- Table.new 32
  fmap HackEnv $ newMVar HackEnvState
    { theBufferTable = buffers
    , theWorkerTable = workers
    , theProcessTable = processes
    }

-- | The table of 'Worker's stored within the 'HackEnv'.
workerTable :: Lens' HackEnvState (Table Worker)
workerTable = lens theWorkerTable $ \ a b -> a{ theWorkerTable = b }

-- | The table of data 'Buffer's stored within the 'HackEnv'.
bufferTable :: Lens' HackEnvState (Table Buffer)
bufferTable = lens theBufferTable $ \ a b -> a{ theBufferTable = b }

-- | The table of child 'Process's stored within the 'HackEnv'.
processTable :: Lens' HackEnvState (Table Process)
processTable = lens theProcessTable $ \ a b -> a{ theProcessTable = b }

-- | not for export
--
-- Performs an arbitray update on any of the 'Table's in the 'HackEnv'.
editHackEnvTable :: Lens' HackEnvState (Table thing) -> EditTable thing a -> Hack a
editHackEnvTable table = Hack . lift . Table.withinGroup table

-- | List one of the 'Table's in the 'HackEnv': this could be 'bufferTable', 'threadTable', or
-- 'processTable'.
listHackEnv :: DisplayInfo elem => Lens' HackEnvState (Table elem) -> Hack ()
listHackEnv inTable =
  editHackEnvTable inTable $
  Table.list Strict.putStr

-- | Print a list of all data 'Buffer's in the 'HackEnv' that were created by 'newBuffer'.
listBuffers :: Hack ()
listBuffers = listHackEnv bufferTable

-- | Print a list of all managed 'Worker' threads that are known to the 'HackEnv', meaning they were
-- created by the 'startWork' function. The 'HackEnv' execution context cannot track threads created
-- by 'forkIO' or 'forkOS'.
listWorkers :: Hack ()
listWorkers = listHackEnv workerTable

-- | Prints a list of all child 'Process's that are known to the 'HackEnv' created by
-- 'runInBuffer'. The 'HackEnv' execution context cannot track processes created by 'createProcess'
-- or any of its convenient functional wrappers.
listProcesses :: Hack ()
listProcesses = listHackEnv processTable

hackEnvTableSelect1
  :: Lens' HackEnvState (Table elem)
  -> (Table.Row elem -> Bool)
  -> Hack (Maybe (Table.Row elem))
hackEnvTableSelect1 inTable testElem =
  editHackEnvTable inTable $
  Table.select1 testElem

-- | Create a new 'Worker', passing a 'Label' to identify the worker in the 'listWorkers' table, and
-- a task of type @IO ()@ to perform.
--
-- This function returns a 'Table.Row' because, like with 'Buffer's, any new 'Worker' thread created
-- has a handle for it stored in a table in the 'Hack' monad execution environment so it can be
-- retrieved at any time.
startWork
  :: Table.Label
  -> (Table.Row Worker -> IO (Either EditTextError ()))
  -> Hack (Table.Row Worker)
startWork label task =
  ask >>= \ env ->
  editHackEnvTable workerTable $ do
    selfMVar <- liftIO $ newEmptyMVar
    statMVar <- liftIO $ newMVar Ready
    thid <-
      liftIO $
      forkIO $
      (do modifyMVar_ statMVar (pure . const Working)
          self   <- takeMVar selfMVar
          result <- task self
          runHack (clearWorker self) env
          modifyMVar_ statMVar $
            const $ pure $
            case result of
              Left err -> WorkerError err
              Right () -> Success
      )
      `catch` \ err ->
        modifyMVar_ statMVar (pure . const (WorkerFailed err))
    let worker = Worker
          { theWorkerId = thid
          , theWorkerStatus = statMVar
          }
    row <- Table.insert label worker
    liftIO $ putMVar selfMVar row
    pure row

-- | not for export
--
-- Remove a worker from the 'globalBufferTable'
clearWorker :: Table.Row Worker -> Hack Bool
clearWorker worker =
  editHackEnvTable workerTable $
  Table.remove1 (Table.byRowValue (== (worker ^. Table.rowValue)))

-- | Create a new data 'Buffer'. Thhis function also registers the buffer into the 'bufferTable' of
-- the 'HackEnv' execution environment, which is why it returns a 'Table.Row'. This is so if you
-- lose the handle to it while working in GHCI, you can get it back using 'getBuffer'.
newBuffer :: Table.Label -> VectorSize -> Hack (Table.Row Buffer)
newBuffer label initSize =
  editHackEnvTable bufferTable $
  ( liftIO $
    newEditTextState initSize >>= \ ed ->
    fmap Buffer $
    newMVar $
    BufferState
    { theBufStateFilePath = ""
    , theBufStateLockedBy = Nothing
    , theBufStateBuffer   = ed
    }
  ) >>=
  Table.insert label

withBufferState :: MonadIO io => StateT BufferState IO a -> Buffer -> io a
withBufferState f (Buffer mvar) =
  liftIO $
  modifyMVar mvar $
  fmap (\ (a, b) -> (b, a)) . runStateT f

withBuffer0 :: MonadIO io => Buffer -> EditText TextTags a -> io (Either EditTextError a)
withBuffer0 buffer f =
  flip withBufferState buffer $ do
    ed <- use bufStateBuffer
    (a, ed) <- liftIO $ runEditText f ed
    bufStateBuffer .= ed
    pure a

-- | Manipulate a buffer if you have it's handle already.
withBuffer :: MonadIO io => Table.Row Buffer -> EditText TextTags a -> io (Either EditTextError a)
withBuffer = withBuffer0 . Table.theRowObject

-- | Pretty-print each line of data in a 'Buffer'.
showBuffer :: MonadIO io => Table.Row Buffer -> TextRange LineIndex -> io (Either EditTextError ())
showBuffer row range =
  withBuffer row $
  foldLinesInRange range
  (\ _halt lineNum line -> liftIO $ do
     putStr (ralign6 lineNum)
     Strict.putStr ": "
     putStrLn (convertString (theTextLineData line))
     pure Nothing
  )
  ()

-- | Change the file path to which a buffer will be written when saved.
withBufferPath :: MonadIO io => Table.Row Buffer -> (Strict.Text -> Strict.Text) -> io ()
withBufferPath buf f = flip withBufferState (Table.theRowObject buf) $ bufStateFilePath %= f

-- | Read entire content of a 'Handle' into a 'Buffer'. The 'Label' here is for what the 'Worker'
-- should be called.
bufferHandle :: Table.Label -> Table.Row Buffer -> Handle -> Hack (Table.Row Worker)
bufferHandle label row handle =
  startWork label $ \ _worker ->
  withBuffer row $
  loadHandleEditText Nothing handle

-- | Load a file from the given path, return a reference to the 'Buffer' and the 'Worker' that was
-- used to load the buffer.
bufferFile :: Strict.Text -> Hack (Table.Row Worker, Table.Row Buffer)
bufferFile path = do
  handle <- liftIO $ openFile (Strict.unpack path) ReadMode
  buffer <- newBuffer path 128
  worker <- bufferHandle ("(bufferFile " <> Strict.pack (show path) <> ")") buffer handle
  return (worker, buffer)

----------------------------------------------------------------------------------------------------

-- | When searching throught the global table, you can select items by one of 2 types of index
-- values: 1. their unique id, or 2. by their nickname. The 'getBuffer' and 'getWorker' functions
-- are polymorphic over the search index so you can use either with the same function.
class GlobalTableSearchIndex i where
  -- | Search for a 'BufferState' created by 'newBuffer'.
  getBuffer :: i -> Hack (Table.Row Buffer)
  -- | Search for a 'ThreadTableElem' created by 'newThread'.
  getWorker :: i -> Hack (Table.Row Worker)

instance GlobalTableSearchIndex Int where
  getBuffer index =
    hackEnvTableSelect1 bufferTable (Table.byRowId index) >>=
    maybe (error $ "no buffer identified by index " <> show index) pure
  getWorker index =
    hackEnvTableSelect1 workerTable (Table.byRowId index) >>=
    maybe (error $ "no worker identified by index " <> show index) pure

instance GlobalTableSearchIndex (Strict.Text -> Bool) where
  getBuffer label =
    hackEnvTableSelect1 bufferTable (Table.byRowLabel label) >>=
    maybe (error $ "no buffer matching label predicate") pure
  getWorker label =
    hackEnvTableSelect1 workerTable (Table.byRowLabel label) >>=
    maybe (error $ "no worker matching label predicate") pure

instance GlobalTableSearchIndex Strict.Text where
  getBuffer label =
    hackEnvTableSelect1 bufferTable (Table.byRowLabel (== label)) >>=
    maybe (error $ "no buffer matching label " <> show label) pure
  getWorker label =
    hackEnvTableSelect1 workerTable (Table.byRowLabel (== label)) >>=
    maybe (error $ "no worker matching label " <> show label) pure

----------------------------------------------------------------------------------------------------

redisplayFunction
  :: MonadIO editor
  => ((Strict.Text -> editor Bool)
      -> (Strict.Text -> Strict.Text -> editor ())
      -> editor ()
      -> editor ()
     )
  -> editor ()
redisplayFunction f = f
  (pure . not . Strict.null)
  (\ prefix elem -> liftIO $ Strict.putStrLn $ prefix <> Strict.pack (show elem))
  (liftIO $ putStrLn "....")

-- | Used for testing 'Buffer's, shows the low-level mutable vector conent of an editable vector of
-- 'Strict.Text'.
redisplay
  :: ( MonadIO editor, MonadState st editor, Vector.HasEditorState st
     , EditorMVectorType st ~ MVector
     , EditorMVectorElem st ~ Strict.Text
     )
  => editor ()
redisplay = redisplayFunction Vector.printBuffer

-- | Used for testing 'Buffer's, pretty-prints the content of a mutable 'IOVector' of 'Strict.Text'
-- elements.
redisplayIOBuffer :: MonadIO editor => IOVector Strict.Text -> editor ()
redisplayIOBuffer mvec = redisplayFunction (\ a b c -> Vector.printOverIOBuffer a b c mvec)

-- | Run tests on the 'Buffer' data structure.
testEditor :: IO ()
testEditor = do
  -- test "sweepIOBuffer"
  edst <- Vector.newEditorState :: IO (Vector.EditorState MVector Strict.Text)
  flip Vector.evalEditor edst $ do
    let clean = do
          Vector.filterBuffer (pure . not . Strict.null) >>=
            Vector.withSubRange (Vector.fillWith "")
          liftIO $ Strict.putStrLn "buffer cleaned"
    Vector.newCurrentBuffer 1
    Vector.putCurrentElem "zero"
    liftIO $ Strict.putStrLn "created buffer of 1 element"
    clean
    redisplay
    Vector.newCurrentBuffer 2
    Vector.putCurrentElem ""
    Vector.currentCursor += 1
    Vector.putCurrentElem "zero"
    liftIO $ Strict.putStrLn "created buffer of 2 elements"
    clean
    redisplay
    Vector.newCurrentBuffer 20
    Vector.currentCursor .= 0
    mapM_
      (\ e -> do
        Vector.putCurrentElem e
        Vector.currentCursor += 1
      )
      [ "zero", "one" , "two", "three"
      , "four", "five", "six", "seven"
      , ""
      , "eight", "nine", "ten", "eleven"
      , ""
      , "twelve", "thirteen"
      , ""
      , "fourteen", "fifteen", "sixteen"
      ]
    use Vector.currentCursor >>= \ i ->
      liftIO $ putStrLn $ "added " <> show i <> " elements"
    found <- Vector.searchBuffer (pure . Strict.null)
    liftIO $ Strict.putStrLn $ "Search for null element -> " <> Strict.pack (show found)
    redisplay
    liftIO $ Strict.putStrLn "--------------------"
    clean
    redisplay

-- | Run tests on the 'GapBuffer' data structure.
testGapBuffer :: IO ()
testGapBuffer = do
  buf <- newGapBufferState (Just "") 16
  let echo = liftIO . Strict.putStrLn
  let lbrk = liftIO $ Strict.putStrLn ""
  let redisplayln = redisplay >> lbrk
  let inline msg mvec = liftIO $ do
        let len = MVec.length mvec
        Strict.putStr msg
        if len == 0 then putStrLn ": [] (0)" else do
          Strict.putStr ": ["
          forM_ [0 .. len - 1] $ \ i ->
            MVec.read mvec i >>=
            Strict.putStr . Strict.pack . show >>
            Strict.putStr
            (if i < len - 1 then ", " else "] (" <> Strict.pack (show len) <> ")\n")
  let testRange a b = do
        let range = Range a b
        echo $ "select " <> Strict.pack (show range) <>
          " " <> Strict.pack (show (a, a+b-1))
        (lo, gap, hi) <- gapBuffer3SliceInRange range
        (imlo, imgap, imhi) <- liftIO $ (,,) <$> Vec.freeze lo <*> Vec.freeze gap <*> Vec.freeze hi
        slice <- liftEditor (sliceRange range <$> use currentBuffer) >>= liftIO . Vec.freeze
        if slice == imlo <> imgap <> imhi then echo "OK\n" else do
          echo "unexpected slices:"
          inline " lo" lo
          inline "gap" gap
          inline " hi" hi
          liftIO $ Strict.putStrLn $
            "expected: " <> Strict.pack (show slice) <>
            " (" <> Strict.pack (show $ Vec.length slice) <> ")"
          echo ""
  result <- flip evalGapBuffer buf $ do
    let test msg run = do
          e <- echo msg >> run
          redisplay
          echo $ "got item: " <> Strict.pack (show e <> "\n")
    mapM_ (pushItem Before) ["zero", "one", "two", "three", "four"]
    mapM_ (pushItem After) ["nine", "eight", "seven", "six", "five"]
    redisplayln
    test "pullItem Before" (pullItem Before)
    test "pullItem After"  (pullItem After)
    test "popItem Before"  (popItem Before)
    test "popItem After"   (popItem After)
    echo "shiftCursor -1" >> shiftCursor (-1) >> redisplayln
    echo "shiftCursor 1"  >> shiftCursor 1    >> redisplayln
    echo "shiftCursor -2" >> shiftCursor (-2) >> redisplayln
    echo "shiftCursor 2"  >> shiftCursor 2    >> redisplayln
    testRange  0 16
    testRange  1 15
    testRange  1 14
    testRange  2 11
    testRange  3 10
    testRange  3  9
    testRange  4  9
    testRange  4 10
    testRange  6  5
    testRange  0  4
    testRange  0  3
    testRange  1  3
    testRange  1  4
    testRange  1  6
    testRange 12  4
    testRange 11  5
    testRange 11  4
    testRange 10  3
  case result of
    Right{}  -> return ()
    Left err -> error $ Strict.unpack $ ppGapBufferErrorInfo err

-- | Used for testing and debugging 'Buffer's.
debugViewBuffer :: Table.Row Buffer -> IO (Either EditTextError ())
debugViewBuffer = flip withBuffer $ debugViewTextEditor

-- | Run tests on 'Buffer's.
testTextEditor :: Hack (Table.Row Buffer)
testTextEditor = do
  buf <- newBuffer "test editor" 32
  result <-
    withBuffer buf $ do
      insertString $
        "<p>Emacs is a Lisp programming environment and app platform that is one\n" <>
        "of the most generally useful programming tools anyone could possibly ever\n" <>
        "use. The original Emacs software was first written at MIT back in 1976.\n" <>
        "It is well known among people in computer-related professions, and has a\n" <>
        "thriving community of users who contribute their own apps (which are\n" <>
        "called \"Major Modes\" and \"Minor Modes\"). This active community of users,\n" <>
        "along with a small team of and highly competent maintainers, keeps Emacs\n" <>
        "useful even in modern times.</p>\n\n" <>
        "<p>But due to how old the Emacs source code base is, the Lisp interpreter\n"
      let ins2 a b = insertString a >> insertString b
      ins2 "built-in to Emacs has a bit too much " "historical baggage. That is not to\n"
      ins2 "say that Emacs is a dinosaur. The code " "base (written in C) is actively\n"
      ins2 "maintained and has new feautres added " "regularly. Functionality for\n"
      ins2 "international font rendering, SVG graphics, " "and JSON parsing and data\n"
      ins2 "structure manipulation, are all built-" "in. Pretty soon, JIT compilation of\n"
      ins2 "Emacs Lisp code will be a standard feature, " "which will make Emacs Lisp\n"
      ins2 "code comparable in speed to lanugages " "like JavaScript.</p>"
      flushLine
      --debugViewTextEditor
      lineNumber
  case result of
    Right line ->
      showBuffer buf (TextRange 1 line) >>= \ case
        Right () -> return buf
        Left err -> error (show err)
    Left  err  -> error (show err)

----------------------------------------------------------------------------------------------------

-- | Run tests on 'TextString' functions.
testByteStreamToLines :: IO ()
testByteStreamToLines = do
  -- Show the content of the 'allLineBreaks' table.
  tokTableFold
    (\ before (c, tok, st) -> before >> print (c, show tok, st))
    (pure ())
    allLineBreaks
  putStrLn ""
  --
  -- Test 'byteStreamToLines' on an ByteString
  let mkStream = streamByteString $ UTF8.fromString $
        "this is a simple test\nto see what I can accomplish.\nIt is getting late\r\n" <>
        "I am having trouble focusing.\n\rI am not insomniac, just have lots to do.\v" <>
        "Here is hoping my tests all pass.\f" <>
        "zero\0one\0two\0three\0four\0five\0six\0seven\0eight\0nine\0" <>
        "The numbers are done for today.\n"
  let foldline _halt lbrk = do
        i <- get
        line <-
          isAsciiOnly >>=
          readLinesLiftGapBuffer .
          cutUTF8TextLine lbrk
        liftIO $ putStrLn $ show i <> ": " <> show (line :: TextLine ())
        put $! (i::Int) + 1
  let runLineBreaker table = do
        h <- mkStream
        buf <- newGapBufferState (Just 0) 128
        byteStreamToLines table buf h foldline (1::Int)
  putStrLn "test 'allLineBreaks'"
  runLineBreaker allLineBreaks
  putStrLn ""
  --
  putStrLn "test 'lineBreak_LF'"
  runLineBreaker lineBreak_LF
  putStrLn ""
  --
  -- Test 'byteStreamToLines' on a file via the 'hReadLines' function.
  buf <- newGapBufferState (Just 0) 128
  h <- openFile "./emacs-hacker.cabal" ReadMode
  flip (hReadLines buf h) (1::Int) $ \ _halt line -> do
    i <- get
    liftIO $ putStrLn $ show i <> ": " <> show (line :: TextLine ())
    put $! i + 1
  hClose h

----------------------------------------------------------------------------------------------------

-- | Run all tests.
main :: IO ()
main =
  newEditTextState 128 >>=
  ( evalEditText $
    liftIO (openFile "./emacs-hacker.cabal" ReadMode) >>=
    loadHandleEditText Nothing >>
    (do cursorToEnd Before
        flip (foldLinesFromCursor After) (1::Int) $ \ _halt i line -> do
          liftIO $ putStrLn $ show i <> ": " <> show (fmap (const ()) line)
          pure Nothing
    )
  ) >>= \ case
    Left err -> error $ show err
    Right _i -> pure ()
