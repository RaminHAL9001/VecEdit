-- | __NOTE:__ this module is intended to be used as a @qualified@ import.
--
-- This module defines a 'Table' of objects. Each table row has a unique ID, a non-unique human
-- readable 'Label', and an 'MVar' containing the object itself. The API might seem SQL-like, but
-- this is not a persistent database, it exists only in memory and only for as long as the process
-- is alive, so things the 'RowId' type is
module VecEdit.Table
  ( Table, Row, theRowUniqueId, theRowLabel, theRowValue, rowLabel, rowValue, showRow,
    Label, RowId(..),
    Edit(..), withinGroup, exec,
    -- ** Create and Read
    new, insert, select1, fold, list,
    -- *** Row selectors
    byRowId, byRowSelf, byRowLabel, byRowValue, printRows,
    -- ** Updating and Delete
    Update(..),
    Update1Result(..), update1, remove1,
    UpdateResult(..), update, remove,
    showUpdate1Result,
    -- ** Class for lifting 'Edit'
    EditMonad(..),
  ) where

import VecEdit.Types (VectorIndex, VectorSize, Range(..), )

import VecEdit.Print.DisplayInfo (DisplayInfo(..), LinePrinter, showAsText, ralign6)
import VecEdit.Vector.Editor
  ( EditorState, Editor, HasEditorState(..), Update(..),
    runEditor, currentBuffer, currentCursor, getElemAt, putElemAt,
    newEditorState, newCurrentBuffer, withSubRange, fillWith,
    updateBuffer, updateBufferRange, searchBuffer, putCurrentElem,
    growBufferWithCursor,
    printBuffer,
  )

import Control.Lens (Lens', lens, use, (.=), (+=))
import Control.Monad -- re-exporting
import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Monad.State (MonadState(..), StateT(..), runStateT)

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

instance HasEditorState (Table obj) where
  type EditorMVectorType (Table obj) = MVector 
  type EditorMVectorElem (Table obj) = Maybe (Row obj)
  vectorEditorState = tableBuffer

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
    , theRowValue   :: !obj
    }

instance DisplayInfo obj => DisplayInfo (Row obj) where
  displayInfo putStr (Row{theRowUniqueId=(RowId i),theRowLabel=lbl,theRowValue=obj}) =
    liftIO $ do
      putStr $ Strict.pack $ ralign6 i <> ": " <> show lbl <> " "
      displayInfo putStr obj

rowLabel :: Lens' (Row obj) Label
rowLabel = lens theRowLabel $ \ a b -> a{ theRowLabel = b }

rowValue :: Lens' (Row obj) obj
rowValue = lens theRowValue $ \ a b -> a{ theRowValue = b }

-- | Construct a 'Strict.Text' string that describes the 'Row', showing 'theRowId' and 'theRowLabel'
-- before appending the 'Strict.Text' value returned by the given function that converts the @obj@
-- value to a 'Strict.Text' String. It is common to use 'showAsText' for the @(obj ->
-- 'Strict.Text')@ function type: @('showRow' 'showAsText' ("a String and Int tuple", 42))@
showRow :: (obj -> Strict.Text) -> Row obj -> Strict.Text
showRow show Row{theRowUniqueId=RowId{unwrapRowId=rowid},theRowLabel=lbl,theRowValue=obj} =
  showAsText rowid <> " " <> lbl <> " " <> show obj

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
        , theRowValue    = obj
        }
  (cur, tsiz) <- liftVecEditor $
    (,) <$>
    use currentCursor <*>
    (MVec.length <$> use currentBuffer)
  -- Check if we are out of space. If so, fist try doing 'bufferCleanup'.
  when (cur >= tsiz) $
    void $ optimize
  -- If we are still out of space, resize the buffer.
  liftVecEditor $ do
    cur <- use currentCursor
    when (cur >= tsiz) $ 
      growBufferWithCursor (\ tsiz _ -> 2 * tsiz)
    putCurrentElem (Just newRow)
    currentCursor += 1
  pure newRow

find1
  :: (Row obj -> Bool)
  -> (VectorIndex -> Editor MVector (Maybe (Row obj)) a)
  -> Edit obj (Maybe a)
find1 testRow onIndex = 
  liftVecEditor $
  searchBuffer (pure . maybe False testRow) (\ () _ _ _ -> pure ()) () >>=
  maybe (pure Nothing) (fmap Just . onIndex) . fst

-- | Fold over all elements in a 'Table'.
fold :: (fold -> Row obj -> IO fold) -> fold -> Edit obj fold
fold f fold = liftVecEditor $ do
  cur <- use currentCursor
  (dups, fold) <- updateBufferRange
    (\ _i elem fold ->
      liftIO $
      maybe
      (pure (ItemRemove, fold))
      (fmap ((,) ItemKeep) . f fold)
      elem
    )
    fold
    ( Range
      { theRangeStart  = 0
      , theRangeLength = cur
      }
    )
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
select1 = fmap join . flip find1 getElemAt

----------------------------------------------------------------------------------------------------

data UpdateResult = UpdateResult{ manyKept :: !Int, manyRemoved :: !Int, manyUpdated :: !Int }
  deriving (Eq, Ord, Show)

instance Semigroup UpdateResult where
  a <> b =
    UpdateResult
    { manyKept = manyKept a + manyKept b
    , manyRemoved = manyRemoved a + manyRemoved b
    , manyUpdated = manyUpdated a + manyUpdated b
    }

instance Monoid UpdateResult where
  mempty = UpdateResult{ manyKept = 0, manyRemoved = 0, manyUpdated = 0 }
  mappend = (<>)

-- | Update every element in the buffer, possibly deleting the element (by returning 'Nothing'). The
-- number of elements that were removed is returned as the 'fst' of a tuple, the number of items
-- changed (as an 'Update' and not merely a 'Keep') is returned as the 'snd' of the tuple. This
-- function does 'optimize' as it evaluates, so no need to evaluate 'optimize' after evaluating this
-- function.
update :: (Row obj -> IO (Update (Row obj))) -> Edit obj UpdateResult
update f =
  Edit $
  use currentCursor >>= \ cur ->
  updateBufferRange
  (\ _ elem result ->
    case elem of
      Nothing   -> pure (ItemRemove, result) -- result not changed, nothing existed here.
      Just elem ->
        liftIO $
        (\ case
          ItemRemove     ->
            ( ItemRemove
            , result{ manyRemoved = manyRemoved result + 1 }
            )
          ItemKeep       ->
            ( ItemKeep
            , result{ manyKept = manyKept result + 1 }
            )
          ItemUpdate row ->
            ( ItemUpdate (Just row)
            , result{ manyUpdated = manyUpdated result + 1 }
            )
        ) <$>
        f elem
  )
  mempty
  ( Range
    { theRangeStart  = 0
    , theRangeLength = cur
    }
  ) >>= \ (dups, result) ->
  withSubRange (fillWith Nothing) dups >>
  pure result

-- | This function is called 'remove' rather than 'delete' because the item is removed from the
-- 'Table', and not necessarily removed from memory. This function searches through the 'Table' in
-- the current 'Edit' function context and removes all 'Row's that satisfy the given predicate
-- function ('True' means to remove, 'False' does NOT remove). Returns the number of elements
-- removed. This function automatically performs an 'optimze' as it evaluates, so there is no need
-- to call 'optimize' after evaluating this function.
remove :: (Row obj -> IO Bool) -> Edit obj Int
remove p =
  fmap manyRemoved $
  update $
  fmap (\ remove -> if remove then ItemRemove else ItemKeep) . p

----------------------------------------------------------------------------------------------------

data Update1Result obj
  = NoUpdates
  | Removed1{ updatedIndex :: Int, removedRow :: Row obj }
  | Replaced1{ updatedIndex :: Int, removedRow :: Row obj, insertedRow :: Row obj }

showUpdate1Result :: (obj -> Strict.Text) -> Update1Result obj -> Strict.Text
showUpdate1Result showObj = \ case
  NoUpdates -> "(no updates)"
  Removed1{updatedIndex=i,removedRow=row} ->
    "(updated :index " <> showAsText i <>
    " :removed " <> showRow showObj row <> ")"
  Replaced1{updatedIndex=i,removedRow=remRow,insertedRow=insRow} ->
    "(replaced :index " <> showAsText i <>
    " :removed " <> showRow showObj remRow <>
    " :inserted " <> showRow showObj insRow <> ")"

-- | This function takes the given predicate and calls 'find1'. If any items matching the predicate
-- are found, the given updating continuation function (which returns an @'Update' ('Row' obj)@) is
-- called on only that single found item, updating it in place.
update1
  :: (Row obj -> Bool)
  -> (Row obj -> IO (Update (Row obj)))
  -> Edit obj (Update1Result obj)
update1 testRow f =
  fmap (maybe NoUpdates id) $
  find1 testRow $ \ i ->
  getElemAt i >>=
  maybe
  (return NoUpdates)
  (\ row0 ->
    liftIO (f row0) >>= \ case
      ItemKeep       -> pure NoUpdates
      ItemRemove     -> do
        putElemAt i Nothing
        pure Removed1
          { updatedIndex = i
          , removedRow = row0
          }
      ItemUpdate row -> do
        putElemAt i $ Just row
        pure Replaced1
          { updatedIndex = i
          , removedRow = row0
          , insertedRow = row
          }
  )

-- | Search through the 'Table' in the current 'Edit' function context, remove the first 'Row' that
-- satisfies the given predicate function ('True' means to remove, 'False' does NOT remove). This
-- function is a special kase of 'update1' in which the updating function always returns 'Nothing',
-- and the only removed element is returned, rather than a tuple containing both the new and removed
-- element (since there is no new element). This function does not 'optmize', so you may want to
-- call 'optimize' at some point after evaluating.
remove1 :: (Row obj -> Bool) -> Edit obj (Update1Result obj)
remove1 = flip update1 (const $ pure ItemRemove)

-- | Remove any holes in the 'Table' caused by deleting elements, usually through the 'update' or
-- 'update1' functions. This function is O(n) where n is the size of the table, so be careful about
-- using it too often. Also, functions like 'remove' perform optimization, so if you 'update' and
-- then 'remove' it is not necessary to evaluate this function. Returns the number of holes found
-- and removed. If the number is low, calling this function was probably a waste of computing time,
-- so ironically, calling this function too often is very sub-optimal.
optimize :: Edit obj Int
optimize =
  Edit $
  updateBuffer
  (\ _i e count ->
    pure $ case e of
      Nothing -> (ItemRemove, count + 1)
      Just{}  -> (ItemKeep, count)
  )
  0 >>= \ (dups, count) ->
  (currentCursor += negate count) >>
  withSubRange (fillWith Nothing) dups >>
  pure count

----------------------------------------------------------------------------------------------------

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
byRowValue = (. theRowValue)

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
