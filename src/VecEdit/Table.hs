-- | __NOTE:__ this module is intended to be used as a @qualified@ import.
--
-- This module defines a 'Table' of objects. Each table row has a unique ID, a non-unique human
-- readable 'Label', and an 'MVar' containing the object itself. The API might seem SQL-like, but
-- this is not a persistent database, it exists only in memory and only for as long as the process
-- is alive, so things the 'RowId' type is
module VecEdit.Table
  ( Table, Row, theRowObject, theRowUniqueId, theRowLabel, rowLabel, rowValue,
    Label, RowId(..),
    Edit(..), withinGroup,
    exec, new, insert, select1, update1, remove1, fold, list,
    byRowId, byRowSelf, byRowLabel, byRowValue, printRows,
    -- *** Class for lifting 'Edit'
    EditMonad(..),
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

import Control.Lens (Lens', lens, use, (^.), (.~), (.=), (+=))
import Control.Monad -- re-exporting
import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Monad.State (MonadState(..), StateT, runStateT)

import Data.Maybe (isJust)
import Data.Function (on)
import qualified Data.Vector.Mutable as MVec
import Data.Vector (Vector)
import qualified Data.Vector as Vec
import Data.Vector.Mutable (MVector)
import qualified Data.Text as Strict

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
  displayInfo = fmap void . exec . printRows
   -- The 'void' here discards changes to the 'Table' state, but that is OK since 'list' only
   -- reads the buffer, it never modifies it, so the 'Table' and the 'EditorState' will not end up
   -- in an inconsistent state.

tableUniqueIDGen :: Lens' (Table obj) Int
tableUniqueIDGen = lens theTableUniqueIDGen $ \ a b -> a{ theTableUniqueIDGen = b }

tableBuffer :: Lens' (Table obj) (EditorState MVector (Maybe (Row obj)))
tableBuffer = lens theTableBuffer $ \ a b -> a{ theTableBuffer = b }

----------------------------------------------------------------------------------------------------

-- | This data type represents a 'Row' of data in a 'Table'. It contains an auto-incrementing unique
-- id called 'theRowUniqueId', and so a 'Row' value can only be created by the effectful 'insert'
-- function. There is an 'Eq' instance for 'Row' but it only compares on 'theRowUniqueId'.
data Row obj
  = Row
    { theRowUniqueId :: !(RowId obj)
    , theRowLabel    :: !Label
    , theRowObject   :: !obj
    }

instance DisplayInfo obj => DisplayInfo (Row obj) where
  displayInfo putStr (Row{theRowUniqueId=(RowId i),theRowLabel=lbl,theRowObject=obj}) =
    liftIO $ do
      putStr $ Strict.pack $ ralign6 i <> ": " <> show lbl <> " "
      displayInfo putStr obj

rowLabel :: Lens' (Row obj) Label
rowLabel = lens theRowLabel $ \ a b -> a{ theRowLabel = b }

rowValue :: Lens' (Row obj) obj
rowValue = lens theRowObject $ \ a b -> a{ theRowObject = b }

----------------------------------------------------------------------------------------------------

newtype Edit obj a
  = Edit{ unwrapEdit :: StateT (Table obj) IO a }
  deriving (Functor, Applicative, Monad, MonadIO)

exec :: Edit obj a -> Table obj -> IO (a, Table obj)
exec (Edit f) = runStateT f

-- | Not for export. This function performs updates on the underlying vector of the table.
liftVecEditor :: Editor MVector (Maybe (Row obj)) a -> Edit obj a
liftVecEditor ed =
  Edit $ do
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
  => Lens' group (Table obj) -> Edit obj a -> m a
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
-- of the current 'Edit' context.
insert :: Label -> obj -> Edit obj (Row obj)
insert label obj = do
  newID <-
    Edit $
    use tableUniqueIDGen <*
    (tableUniqueIDGen += 1)
  let newRow =
        Row
        { theRowUniqueId = RowId newID
        , theRowLabel    = label
        , theRowObject   = obj
        }
  liftVecEditor $ do
    cur  <- use currentCursor
    tsiz <- MVec.length <$> use currentBuffer
    -- Check if we are out of space. If so, fist try doing 'bufferCleanup'.
    when (cur >= tsiz) $
      filterBuffer (pure . isJust) (\ () _ _ _ -> pure ()) ()  >>=
      withSubRange (fillWith Nothing) . fst
    -- If we are still out of space, resize the buffer.
    growBufferWithCursor (\ tsiz _ -> 2 * tsiz)
    putCurrentElem (Just newRow)
    currentCursor += 1
    pure newRow

find1
  :: (Row obj -> Bool)
  -> (VectorIndex -> Editor MVector (Maybe (Row obj)) (Maybe a))
  -> Edit obj (Maybe a)
find1 testRow onIndex = 
  liftVecEditor $
  searchBuffer (pure . maybe False testRow) (\ () _ _ _ -> pure ()) () >>=
  maybe (pure Nothing) onIndex . fst

-- | Fold over all elements in a 'Table'.
fold :: (fold -> Row obj -> IO fold) -> fold -> Edit obj fold
fold f fold = liftVecEditor $ do
  (dups, fold) <- filterBuffer
    (pure . isJust)
    (\ fold _ _ -> liftIO . maybe (pure fold) (f fold))
    fold
  withSubRange (fillWith Nothing) dups
  pure fold

-- | Runs 'fold' with a folding function that builds a list, returns the list.
list :: (Row obj -> IO Bool) -> Edit obj (Vector (Row obj))
list p =
  uncurry Vec.fromListN .
  fmap ($ []) <$>
  flip fold (0, id)
  (\ (n0, stack) row ->
    p row >>= \ doesMatch ->
    pure $
    if doesMatch then
      let n = n0 + 1 in
      seq n $!
      (n, stack . (row :))
    else (n0, stack)
  )

-- | Search through the 'Table' in the current 'Edit' function context, return the first 'Row'
-- that satifies the given predicate function.
select1 :: (Row obj -> Bool) -> Edit obj (Maybe (Row obj))
select1 = fmap join . flip find1 (fmap Just . getElemAt)

-- | Search through the 'Table' in the current 'Edit' function context, delete the first 'Row'
-- that satisfies the given predicate function, return 'True' if an element was found and deleted.
remove1 :: (Row obj -> Bool) -> Edit obj Bool
remove1 =
  fmap (maybe False $ const True) .
  flip find1 (fmap Just . flip putElemAt Nothing)

-- | Update the first 'Row' that matches the given predicate. 
update1 :: (Row obj -> Bool) -> (obj -> IO obj) -> Edit obj (Maybe (Row obj))
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

-- | A predicate to be used with 'select1'. This is the only way to select a row by 'Int' values of
-- 'theRowUniqueId', so it is mostly useful in an REPL environment where you have listed out the
-- entire 'Table' and can see the 'Int' value of each listed 'Row'.
byRowId :: Int -> (Row obj -> Bool)
byRowId a row = let (RowId b) = theRowUniqueId row in a == b

-- | There is no 'Eq' instance for 'Row', but you can compare 'Row's to 'Row's by comparing
-- 'theRowUniqueId' of each, which is what this function does.
byRowSelf :: Row obj -> (Row obj -> Bool)
byRowSelf = (==) `on` theRowUniqueId

-- | Given a 'Strict.Text' predicate, construct a 'Row' predicate that can be used with the
-- 'select1' function.
byRowLabel :: (Label -> Bool) -> (Row obj -> Bool)
byRowLabel = (. theRowLabel)

-- | Given a predicate on the @obj@ value, construct a predicate to be used with 'select1'.
byRowValue :: (obj -> Bool) -> (Row obj -> Bool)
byRowValue = (. theRowObject)

-- | Evaluate a 'LinePrinter' against the 'Table' in the current 'Edit' context. This function
-- is used to instantiate 'DisplayInfo' for the 'Table' datatype.
printRows :: DisplayInfo obj => LinePrinter -> Edit obj ()
printRows putStr =
  liftVecEditor $
  printBuffer
  (pure . isJust)
  (\ prefix obj ->
     liftIO $ do
       putStr prefix
       maybe (putStr "-") (displayInfo putStr) obj
       putStr "\n"
  )
  (liftIO $ putStr "....\n")

----------------------------------------------------------------------------------------------------

-- | This class allows you to define your own function for lifting an 'Edit' monad into some
-- other monad.
class Monad m => EditMonad obj m | m -> obj where
  liftEditor :: Edit obj a -> m a
