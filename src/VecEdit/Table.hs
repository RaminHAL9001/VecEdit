-- | __NOTE:__ this module is intended to be used as a @qualified@ import.
--
-- This module defines a 'Table' of objects. Each table row has a unique ID, a non-unique human
-- readable 'Label', and an 'MVar' containing the object itself. The API might seem SQL-like, but
-- this is not a persistent database, it exists only in memory and only for as long as the process
-- is alive, so things the 'RowId' type is
module VecEdit.Table
  ( Table, Row, theRowObject, theRowUniqueId, theRowLabel, rowLabel, rowValue,
    Label, RowId(..),
    EditTable(..), withinGroup,
    exec, new, insert, select1, update1, remove1,
    byRowId, byRowLabel, byRowValue, list,
  ) where

import VecEdit.Types (VectorIndex, VectorSize)

import VecEdit.Print.DisplayInfo (DisplayInfo(..), LinePrinter)
import VecEdit.Vector.Editor
  ( EditorState, Editor,
    runEditor, currentBuffer, currentCursor, getElemAt, putElemAt,
    newEditorState, newCurrentBuffer, withSubRange, fillWith,
    filterBuffer, searchBuffer, putCurrentElem,
    printBuffer, ralign6,
    growBufferWithCursor,
  )

--import Control.Concurrent (ThreadId)
--import Control.Concurrent.MVar (MVar, newMVar, modifyMVar)
import Control.Lens (Lens', lens, use, (^.), (.~), (.=), (+=))
import Control.Monad -- re-exporting
import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Monad.State (MonadState(..), StateT, runStateT)

import Data.Maybe (isJust)
import qualified Data.Vector.Mutable as MVec
import Data.Vector.Mutable (MVector)
import qualified Data.Text as Strict
import qualified Data.Text.Lazy as Lazy

----------------------------------------------------------------------------------------------------

-- | A string to identify objects in the current working environment. 'Label's do not need to
-- uniquely identify data structures, they are simply brief descriptors of the content of data
-- structures which can be used to retrieve a handle to that structure by fuzzy string matching.
type Label = Strict.Text

-- | An auto-incrementing integer unique to each 'Table' which identifies each 'Row'.
newtype RowId obj = RowId{ unwrapRowId :: Int }
  deriving (Eq, Ord)

----------------------------------------------------------------------------------------------------

-- | A 'Table' associates arbitrary objects (of type @obj@) with a unique ID and a non-unique human
-- readable 'Label'. It typically resides in it's own 'MVar', but may also be made part of or a
-- larger table.
data Table obj
  = Table
    { theTableUniqueIDGen :: !Int
    , theTableBuffer :: !(EditorState MVector (Maybe (Row obj)))
    }

instance DisplayInfo obj => DisplayInfo (Table obj) where
  displayInfo = fmap void . exec . list
   -- The 'void' here discards changes to the 'Table' state, but that is OK since 'list' only
   -- reads the buffer, it never modifies it, so the 'Table' and the 'EditorState' will not end up
   -- in an inconsistent state.

tableUniqueIDGen :: Lens' (Table obj) Int
tableUniqueIDGen = lens theTableUniqueIDGen $ \ a b -> a{ theTableUniqueIDGen = b }

tableBuffer :: Lens' (Table obj) (EditorState MVector (Maybe (Row obj)))
tableBuffer = lens theTableBuffer $ \ a b -> a{ theTableBuffer = b }

----------------------------------------------------------------------------------------------------

data Row obj
  = Row
    { theRowUniqueId :: !(RowId obj)
    , theRowLabel    :: !Label
    , theRowObject   :: !obj
    }

instance DisplayInfo obj => DisplayInfo (Row obj) where
  displayInfo putStr (Row{theRowUniqueId=(RowId i),theRowLabel=lbl,theRowObject=obj}) = do
    putStr $
      Lazy.toStrict $
      Lazy.pack $
      ralign6 i <> ": " <>
      let str = Strict.take 16 lbl in
      replicate (16 - Strict.length str) ' ' <>
      show str
    displayInfo putStr obj

rowLabel :: Lens' (Row obj) Label
rowLabel = lens theRowLabel $ \ a b -> a{ theRowLabel = b }

rowValue :: Lens' (Row obj) obj
rowValue = lens theRowObject $ \ a b -> a{ theRowObject = b }

----------------------------------------------------------------------------------------------------

newtype EditTable obj a
  = EditTable{ unwrapEditTable :: StateT (Table obj) IO a }
  deriving (Functor, Applicative, Monad, MonadIO)

exec :: EditTable obj a -> Table obj -> IO (a, Table obj)
exec (EditTable f) = runStateT f

liftEditor :: Editor MVector (Maybe (Row obj)) a -> EditTable obj a
liftEditor ed =
  EditTable $ do
    buf <- use tableBuffer
    (a, buf) <- liftIO $ runEditor ed buf
    tableBuffer .= buf
    pure a

-- | Most of the functions in this module are designed to operate on a 'Table' that is part of a
-- larger data structure using a 'Lens' to retrieve and update the 'Table' from the data
-- structure. This function evaluates a vector 'Editor' on the a 'Lens' accessible 'Table' in some
-- data structure @group@.
withinGroup
  :: (MonadIO m, MonadState group m)
  => Lens' group (Table obj) -> EditTable obj a -> m a
withinGroup subTable f = do
  (a, table) <-
    use subTable >>=
    liftIO .
    exec f
  subTable .= table
  return a

new :: VectorSize -> IO (Table obj)
new initSize = do
  ((), buf) <-
    newEditorState >>=
    runEditor
    ( newCurrentBuffer initSize >>
      fillWith Nothing
    )
  pure
    Table
    { theTableUniqueIDGen = 0
    , theTableBuffer = buf
    }

-- | Create a new 'Row' with the given 'Label' and @obj@ values, and insert it into the 'Table'
-- of the current 'EditTable' context.
insert :: Label -> obj -> EditTable obj (Row obj)
insert label obj = do
  newID <-
    EditTable $
    use tableUniqueIDGen <*
    (tableUniqueIDGen += 1)
  let newRow =
        Row
        { theRowUniqueId = RowId newID
        , theRowLabel    = label
        , theRowObject   = obj
        }
  liftEditor $ do
    cur  <- use currentCursor
    tsiz <- MVec.length <$> use currentBuffer
    -- Check if we are out of space. If so, fist try doing 'bufferCleanup'.
    when (cur >= tsiz) $
      filterBuffer (pure . isJust) >>=
      withSubRange (fillWith Nothing)
    -- If we are still out of space, resize the buffer.
    growBufferWithCursor (\ tsiz _ -> 2 * tsiz)
    putCurrentElem (Just newRow)
    currentCursor += 1
    pure newRow

find1
  :: (Row obj -> Bool)
  -> (VectorIndex -> Editor MVector (Maybe (Row obj)) (Maybe a))
  -> EditTable obj (Maybe a)
find1 testRow onIndex = 
  liftEditor $
  searchBuffer (pure . maybe False testRow) >>=
  maybe (pure Nothing) onIndex

-- | Search through the 'Table' in the current 'EditTable' function context, return the first 'Row'
-- that satifies the given predicate function.
select1 :: (Row obj -> Bool) -> EditTable obj (Maybe (Row obj))
select1 = fmap join . flip find1 (fmap Just . getElemAt)

-- | Search through the 'Table' in the current 'EditTable' function context, delete the first 'Row'
-- that satisfies the given predicate function, return 'True' if an element was found and deleted.
remove1 :: (Row obj -> Bool) -> EditTable obj Bool
remove1 =
  fmap (maybe False $ const True) .
  flip find1 (fmap Just . flip putElemAt Nothing)

update1 :: (Row obj -> Bool) -> (obj -> IO obj) -> EditTable obj (Maybe (Row obj))
update1 testRow f =
  find1 testRow $ \ i ->
  getElemAt i >>=
  maybe
  (return Nothing)
  (\ row0 ->
    liftIO (f (row0 ^. rowValue)) >>= \ obj ->
    let row = Just $ rowValue .~ obj $ row0 in
    putElemAt i row >>
    return row
  )

-- | A predicate to be used with 'select1'
byRowId :: Int -> (Row obj -> Bool)
byRowId a row = let (RowId b) = theRowUniqueId row in a == b

-- | Given a 'Strict.Text' predicate, construct a 'Row' predicate that can be used with the
-- 'select1' function.
byRowLabel :: (Label -> Bool) -> (Row obj -> Bool)
byRowLabel = (. theRowLabel)

-- | Given a predicate on the @obj@ value, construct a predicate to be used with 'select1'.
byRowValue :: (obj -> Bool) -> (Row obj -> Bool)
byRowValue = (. theRowObject)

-- | Evaluate a 'LinePrinter' against the 'Table' in the current 'EditTable' context. This function
-- is used to instantiate 'DisplayInfo' for the 'Table' datatype.
list :: DisplayInfo obj => LinePrinter -> EditTable obj ()
list putStr =
  liftEditor $
  printBuffer
  (pure . isJust)
  (\ prefix obj ->
     liftIO $ do
       putStr prefix
       maybe (putStr "-") (displayInfo putStr) obj
       putStr "\n"
  )
  (liftIO $ putStr "....\n")
