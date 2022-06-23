-- | This module extends the APIs of the "VecEdit.Vector.Editor" module with APIs for editing a
-- mutable vector with a cursor that splits the elements of the vector into two halves: the elements
-- before the cursor and the elements after the cursor. The 'GapBufferState' data structure is
-- designed to be able to insert elements at the cursor position in O(1) time up until all space is
-- used up and the buffer needs to be reallocated, at which point insertsion are no longer O(1).
--
-- 'GapBuffer' operations for the most part make use of cursors rather than taking indicies as
-- arguments, and operations tend to fail if there are no elements in the 'GapBufferState' due to
-- vector index out-of-bounds execptions. This is not a bug but a feature, the onus is on you, the
-- programmer using this library, to make sure your cursors are valid. As long as you stick to using
-- APIs such as 'putBeforeGap', 'putAfterGap', 'stepCursor' and 'shiftCursor', and you never use
-- 'getBeforeGap' unless you are sure you have at least 1 element before the cursor, then your
-- cursor indicies will remain valid and your program will not crash.
--
-- You could also develop some mathematically rigorous method of ensuring indicies are never out of
-- bounds using Haskell's elaborate types system, but this module makes no attempt to be so
-- rigorous, it is a somewhat low-level API.
module VecEdit.Vector.Editor.GapBuffer
  ( GapBuffer(..), GapBufferState(..),
    newGapBufferState, editorToGapBufferState, gapBufferEditorState,
    runGapBuffer, evalGapBuffer, execGapBuffer, withNewGapBuffer,
    afterCursor, beforeCursor, relativeCursor, cursorElemCount, emptyValue, clearGap,
    gapBufferGrowFunction, defaultGrowFunction, ensureFreeSpace,
    atCursor, stepCursor, pushItem, pullItem, popItem, fuseBuffer,
    countDefined, getGapSize, shiftCursor,
    fromGaplessIndex, getGaplessIndex, afterCursorIndex,
    gapBuffer3Slice, gapBuffer3SliceInRange,
    -- *** Exception Handling
    rethrowErrorInfo,
    -- *** Re-Export all of "VecEdit.Vector.Editor"
  )
  where

import VecEdit.Types
  ( Range(..), VectorIndex, VectorSize, canonicalRange,
    GaplessIndex(..), RelativeIndex, RelativeDirection(..), numToRelativeDirection,
    GapBufferErrorInfo(..), GapBufferError(..)
  )

import VecEdit.Vector.Editor
  ( EditorState(..), newEditorState, newBuffer, newCurrentBuffer, runEditor, liftEditor,
    HasEditorState(..), currentCursor, currentBuffer, currentRange, resetCurrentRange,
    getElemAt, putElemAt, sliceFromEnd, getAllocSize,
  )

import Control.Lens (Lens', lens, use, assign, view, (^.), (+=), (-=))
import Control.Arrow ((>>>))
import Control.Monad ((>=>))
import Control.Monad.Except (ExceptT, runExceptT, MonadError(throwError, catchError))
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Primitive (PrimState)
import Control.Monad.Random (MonadRandom(..))
import Control.Monad.State (MonadState(..), StateT(..), gets)

import qualified Data.Text as Strict
import Data.Vector.Generic.Mutable (MVector)
import qualified Data.Vector.Generic as GVec
import qualified Data.Vector.Generic.Mutable as GMVec

----------------------------------------------------------------------------------------------------

-- | This data structure contains 'EditorState' and adds one more value to it:
-- 'theGapBufferCountAfter'. Therefore this editor state actually has 2 cursors:
--
-- 1. 'beforeCursor', which indicates elements before the cursor, and
-- 2. 'afterCursor', which indicates elements after the cursor.
--
-- Remember, only *you* can keep these cursor indicies valid. Most of the API's in this module do
-- not bother checking if these cursor indicies are valid before referencing the vector, and invalid
-- indicies can cause the program to crash. If you stick to using APIs such as 'putBeforeGap',
-- 'putAfterGap', 'getBeforeGap', 'getAfterGap', 'stepCursor' and 'shiftCursor', and make sure you
-- always have a few elements element in the buffer, you should be OK.
data GapBufferState (mvec :: * -> * -> *) n
  = GapBufferState
    { theGapBufferEditorState :: !(EditorState mvec n)
    , theGapBufferAfterCursor :: !VectorSize
      -- ^ Indicates how many elements have been inserted after the cursor. By inserting elements
      -- into the vector index @(length - 1 - countAfter)@ you can insert elements into the gap in
      -- the buffer after the cursor.
    , theGapBufferEmptyValue  :: !(Maybe n)
      -- ^ The value to use to fill the gap between the upper and lower parts of the buffer.
    , theGapBufferGrowFunction :: !(VectorSize -> VectorSize -> GapBuffer mvec n VectorSize)
      -- ^ The callback function used to increase the size of the buffer when space runs out. The
      -- default value for this callback is 'defaultGrowFunction', which simply doubles the current
      -- vector size. A function of this type must take as arguments 1. the current buffer size, and
      -- 2. the new minimum required size.
    }

instance HasEditorState (GapBufferState mvec elem) where
  type EditorMVectorType (GapBufferState mvec elem) = mvec
  type EditorMVectorElem (GapBufferState mvec elem) = elem
  vectorEditorState = lens theGapBufferEditorState $ \ a b -> a{ theGapBufferEditorState = b }

-- | The value to use to fill the gap between the upper and lower parts of the buffer.
emptyValue :: Lens' (GapBufferState mvec n) (Maybe n)
emptyValue = lens theGapBufferEmptyValue $ \ a b -> a{ theGapBufferEmptyValue = b }

-- | The 'GapBufferState' wraps a vector 'EditorState', this lens allows access to this editor state
-- so that you might evaluate a vector 'Editor' function. Be warned that this may result in behavior
-- that is difficult to predict. In particular, the 'EditorState' matains a cursor that is shared
-- with the 'beforeCursor' of the 'GapBuffer' (i.e., they are actually lenses to the same value), so
-- modifications to this value in the vector 'Editor' function might be reflected in the
-- 'GapBufferState' if (after evaluating a vector 'Editor') you place the modified vector 'Editor'
-- back into the 'GapBufferState', which you should always do since the vector 'Editor' might have
-- resized the buffer.
gapBufferEditorState :: Lens' (GapBufferState mvec n) (EditorState mvec n)
gapBufferEditorState = lens theGapBufferEditorState $ \ a b -> a{ theGapBufferEditorState = b }

-- | The callback function used to increase the size of the buffer when space runs out. The default
-- value for this callback is 'defaultGrowFunction', which simply doubles the current vector size. A
-- function of this type must take as arguments 1. the current buffer size, and 2. the new minimum
-- required size.
gapBufferGrowFunction
  :: Lens' (GapBufferState mvec n) (VectorSize -> VectorSize -> GapBuffer mvec n VectorSize)
gapBufferGrowFunction = lens theGapBufferGrowFunction $ \ a b -> a{ theGapBufferGrowFunction = b }

-- | The default callback function used to increase the size of the buffer when space runs out,
-- which simply doubles the current vector size. You can change this value by using the
-- 'gapBufferGrowFunction' lens.
defaultGrowFunction :: VectorSize -> VectorSize -> GapBuffer mvec n VectorSize
defaultGrowFunction oldsize reqsize = pure $ head $ dropWhile (< reqsize) $ iterate (* 2) oldsize

-- | Indicates how many elements have been inserted after the cursor, rather than indexing an
-- element directly under the cursor. By inserting elements into the vector index @(length - 1 -
-- afterCursor)@ you can insert elements after the cursor in O(1) time. Note that this is not an
-- actual index into the vector, in order to translate this value to the index to which it actually
-- refers, use 'afterCursorIndex', which computes the equation @'GMVec.length' buf - 'afterCursor' -
-- 1)@
--
-- Note that the functions in this module __DO NOT__ guarantee that this cursor will always be a
-- valid vector index. It is the responsibility of the programmer using the module to ensure that
-- the value of 'afterCursor' is always greater or equal to zero, and always strictly less than
-- the length of the 'currentBuffer'.
afterCursor :: Lens' (GapBufferState mvec n) VectorSize
afterCursor = lens theGapBufferAfterCursor $ \ a b -> a{ theGapBufferAfterCursor = b }

-- | This function is identical to 'currentCursor', but specific to the 'GapBufferState' data
-- type. It exists mostly to avoid confusion as to whether 'currentCursor' is the counterpoint to
-- 'afterCursor', it is assured that 'beforeCursor' and 'afterCursor' are both lenses to valid index
-- counters in the 'GapBufferState'.
beforeCursor :: Lens' (GapBufferState mvec n) VectorSize
beforeCursor = currentCursor

relativeCursor :: RelativeDirection -> Lens' (GapBufferState mvec n) VectorIndex
relativeCursor = \ case
  Before -> beforeCursor
  After  -> afterCursor

-- | Translate the value of 'afterCursor' to the buffer index value of the element to which the
-- 'afterCursor' cursor is actually referring.
afterCursorIndex :: MVector mvec n => GapBuffer mvec n VectorIndex
afterCursorIndex =
  subtract <$>
  use afterCursor <*>
  ( subtract 1 . GMVec.length <$>
    liftEditor (use currentBuffer)
  )

-- | Return the sum of the number of elements marked by 'beforeCursor' and by 'afterCursor'. As long
-- as you have been careful to only use functions like 'pushItem' and 'popItem', these cursor values
-- will have an accurate accounting of the number of elements stored into the buffer.
cursorElemCount :: GapBuffer mvec n VectorSize
cursorElemCount = (+) <$> use beforeCursor <*> use afterCursor

----------------------------------------------------------------------------------------------------

-- | A function type for performing stateful computations on gap buffers. The 'GapBufferState',
-- which is the stateful context of this monad (as it instantiates the 'MonadState' typeclass)
-- contains an entire 'EditorState', and therefore any function in the "VecEdit.Vector.Editor"
-- module with a type context of:
--
-- @
-- ( MonadIO m
-- , MonadState st m
-- , HasEditorState st
-- )
-- @
--
-- can also be used within a function of type 'GapBuffer'. Functions that could be used with
-- 'Editor' or 'GapBuffer' include
newtype GapBuffer (mvec :: * -> * -> *) n a
  = GapBuffer
    { unwrapGapBuffer
        :: ExceptT GapBufferErrorInfo (StateT (GapBufferState mvec n) IO) a
    }
  deriving
  ( Functor, Applicative, Monad, MonadIO,
    MonadState (GapBufferState mvec n)
  )

-- | The 'GapBuffer' function type allows you to use 'throwError' to throw an error of type
-- 'GapBufferError', and the 'GapBufferErrorInfo' is gathered and filled-in automatically, so when
-- evaluation completes you get a more complete picture of the state of the 'GapBuffer' when the
-- error occurred.
--
-- However there might be times when you have received a 'GapBufferErrorInfo' value and you want to
-- re-throw it. You can't use 'throwError' because it takes a different type ('GapBufferError'), so
-- you must use this function to rethrow the 'GapBufferErrorInfo'.
rethrowErrorInfo :: GapBufferErrorInfo -> GapBuffer mvec n void
rethrowErrorInfo = GapBuffer . throwError

instance MVector mvec n => MonadFail (GapBuffer (mvec :: * -> * -> *) n) where
  fail = throwError . GapBufferFail . Strict.pack

instance MonadRandom (GapBuffer (mvec :: * -> * -> *) n) where
  getRandomR  = liftEditor . getRandomR
  getRandom   = liftEditor getRandom
  getRandomRs = liftEditor . getRandomRs
  getRandoms  = liftEditor getRandoms

instance MVector mvec n => MonadError GapBufferError (GapBuffer (mvec :: * -> * -> *) n) where
  throwError err =
    get >>= \ st ->
    GapBuffer $
    throwError $
    GapBufferErrorInfo
    { theErrorCursorBefore = st ^. beforeCursor
    , theErrorCursorAfter  = st ^. afterCursor
    , theErrorBufferAlloc  = GMVec.length (st ^. currentBuffer)
    , theErrorBufferRange  = st ^. currentRange
    , theErrorOperation    = err
    }
  catchError (GapBuffer try) catch =
    GapBuffer $
    catchError try $ \ err ->
    unwrapGapBuffer $
    catch $
    theErrorOperation err

-- | Define a new 'GapBufferState' with the specified 'VectorSize', and a @nil@ element @n@ that is
-- used to erase elements after 'shiftCursor' is evaluated -- this is especially important for
-- arrays of boxed elements, because if you shift elements around the buffer without allowing
-- 'shiftCursor' to overwrite the old boxes with @nil@, then this could result in a space leak when
-- due to the references to which the boxes point not being garbage collected until they are
-- overwritten by something else, which may never happen until the entire 'GapBuffer' is freed from
-- memory.
--
-- You can change the @nil@ value at any time using the 'emptyValue' lens (e.g. @'emptyValue' '.='
-- nil@), and you can force the gap to be cleared out (all elements set to @nil@) by calling the
-- 'clearGap' function, which is otherwise never evaluated.
newGapBufferState
  :: MVector mvec n
  => Maybe n -> VectorSize -> IO (GapBufferState (mvec :: * -> * -> *) n)
newGapBufferState nil size = do
  buf <- newEditorState
  ((), buf) <- flip runEditor buf $
    newBuffer size >>=
    assign currentBuffer >>
    resetCurrentRange
  maybe (pure ()) (GMVec.set (buf ^. currentBuffer)) nil
  return GapBufferState
    { theGapBufferEditorState = buf
    , theGapBufferAfterCursor = 0
    , theGapBufferEmptyValue  = nil
    , theGapBufferGrowFunction = defaultGrowFunction
    }

editorToGapBufferState :: EditorState mvec n -> GapBufferState mvec n
editorToGapBufferState buf = GapBufferState
  { theGapBufferEditorState  = buf
  , theGapBufferAfterCursor  = 0
  , theGapBufferEmptyValue   = Nothing
  , theGapBufferGrowFunction = defaultGrowFunction
  }

-- | This function uses the @nil@ set in the 'emptyValue' field of the 'GapBufferState' to overwrite
-- all elements in the gap of the gap buffer. If the 'emptyValue' is set to 'Nothing', this function
-- does nothing at all. The number of elements that were set to @nil@ is returned.
clearGap :: MVector mvec n => GapBuffer mvec n VectorSize
clearGap =
  use emptyValue >>=
  maybe
  (pure 0)
  (\ nil -> do
    (_, gap, _) <- gapBuffer3Slice
    liftIO $ GMVec.set gap nil
    return $ GMVec.length gap
  )

-- | Evalaute a 'GapBuffer' function on a 'GapBufferState'.
runGapBuffer
  :: GapBuffer mvec n a
  -> GapBufferState mvec n
  -> IO (Either GapBufferErrorInfo a, GapBufferState mvec n)
runGapBuffer (GapBuffer f) = runStateT (runExceptT f)

-- | Similar to 'runGapBuffer' but only returns the updated 'GapBufferState', and also ignores
-- errors thrown by 'throwError'.
execGapBuffer :: GapBuffer mvec n a -> GapBufferState mvec n -> IO (GapBufferState mvec n)
execGapBuffer f = fmap snd . runGapBuffer f

-- | Similar to 'runGapBuffer' but only returns the monadic result
evalGapBuffer :: GapBuffer mvec n a -> GapBufferState mvec n -> IO (Either GapBufferErrorInfo a)
evalGapBuffer f = fmap fst . runGapBuffer f

-- | Pass a function which takes the current 'VectorSize' of the 'currentBuffer' and returns a new
-- 'VectorSize'. If the new 'VectorSize' is larger than the current 'VectorSize', the
-- 'currentBuffer' will be re-allocated. This will invalidate any slices of the 'currentBuffer' that
-- have not been copied yet, so be sure never to call this after calling 'gapBuffer3Slice' or
-- similar slicing functions.
ensureFreeSpace :: MVector mvec n => (VectorSize -> VectorSize) -> GapBuffer mvec n VectorSize
ensureFreeSpace makeNewSize = do
  count <- cursorElemCount
  oldsize <- GMVec.length <$> use currentBuffer
  let reqsize = makeNewSize count
  if oldsize >= reqsize then return oldsize else do
    resize  <- use gapBufferGrowFunction
    newsize <- resize oldsize reqsize
    (oldLower, _, oldUpper) <- gapBuffer3Slice
    newCurrentBuffer newsize
    (newLower, gap, newUpper) <- gapBuffer3Slice
    nil <- use emptyValue
    liftIO $ do
      GMVec.copy newLower oldLower
      maybe (pure ()) (GMVec.set gap) nil
      GMVec.copy newUpper oldUpper
    return newsize

-- | Similar to 'VecEdit.Vector.Editor.withNewBuffer', but evaluates a 'GapBuffer' function
-- instead. The inner 'GapBuffer' function evaluated need not have the same vector data type as the
-- outer 'GapBuffer' function. This function calls 'newGapBufferState' and so takes values to be
-- handed on to this function, namely @e@ which determines the undefined element, and 'VectorSize'
-- which decides how big the initial buffer size is.
withNewGapBuffer
  :: (MVector outer n, MVector inner e)
  => Maybe e -> VectorSize
  -> (GapBufferState outer n -> GapBuffer inner e a)
  -> GapBuffer outer n (Either GapBufferErrorInfo a)
withNewGapBuffer nil size f =
  get >>= \ st ->
  liftIO $
  newGapBufferState nil size >>=
  evalGapBuffer (f st)

----------------------------------------------------------------------------------------------------

-- Not for export.
--
-- Evaluate a continuation on the element under the cursor computed from a 'RelativeDirection',
-- accepts an exception value as an argument so that other APIs in this module that use this
-- function can have errors thrown on their behalf.
atCursor_ 
  :: MVector mvec n
  => (RelativeDirection -> GapBufferError)
  -> RelativeDirection
  -> (VectorIndex -> GapBuffer mvec n a)
  -> GapBuffer mvec n a
atCursor_ err dir run = do
  len <- GMVec.length <$> use currentBuffer
  case dir of
    Before ->
      use beforeCursor >>= \ bef ->
      let i = bef - 1 in
      if bef <= 0 then throwError $ err dir else
      run i
    After ->
      use afterCursor >>= \ aft ->
      let i = len - aft in
      if i <= 0 then throwError $ err dir else
      run i

-- | This function translates a 'RelativeDirection' to a cursor index, computing a 'VectorIndex'
-- from either the 'beforeCursor' or 'afterCursor' value, and then passes this 'VectorIndex' to the
-- given continuation. If the 'VectorIndex' is out of bounds, 'throwError' is called instead of the
-- continuation.
atCursor
  :: MVector mvec n
  => RelativeDirection
  -> (VectorIndex -> GapBuffer mvec n a)
  -> GapBuffer mvec n a
atCursor = atCursor_ AtCursor

-- | Move the cursor by a single element in the direction given by 'RelativeDirection', which shifts
-- a single element in the opposite direction. Also, overwrite the pervious position of that single
-- element with a vill value of type @n@ given as the first argument. Returns the number of elements
-- that were shifted: -1 if shifted 'Before', 1 if shifted 'After', 0 if the cursor was at either
-- end of the buffer and couldn't be shifted.
stepCursor :: MVector mvec n => RelativeDirection -> GapBuffer mvec n VectorIndex
stepCursor dir = do
  nil <- use emptyValue
  bef <- use beforeCursor
  aft <- use afterCursor
  len <- GMVec.length <$> use currentBuffer
  case dir of
    Before ->
      let i = bef - 1 in
      if bef <= 0 then throwError $ StepCursor Before else do
        getElemAt i >>= putElemAt (len - aft - 1)
        maybe (pure ()) (putElemAt i) nil
        beforeCursor -= 1
        afterCursor  += 1
        return (-1)
    After  ->
      let i = len - aft in
      if aft <= 0 then throwError $ StepCursor After else do
        getElemAt i >>= putElemAt bef
        maybe (pure ()) (putElemAt i) nil
        beforeCursor += 1
        afterCursor  -= 1
        return 1

-- | Write an element to the index at 'beforeCursor' or 'afterCursor', then advance the cursor. For
-- example, if the 'RelativeDirection' is 'Before', then the given element is written to the index
-- given by 'beforeCursor', then the 'beforeCursor' value is incremented.
pushItem :: MVector mvec n => RelativeDirection -> n -> GapBuffer mvec n ()
pushItem dir elem = do
  _size <- ensureFreeSpace succ
  case dir of
    Before -> do
      bef <- use beforeCursor
      putElemAt bef elem
      beforeCursor += 1
    After  -> do
      aft <- use afterCursor
      len <- GMVec.length <$> use currentBuffer
      putElemAt (len - aft - 1) elem
      afterCursor += 1

-- | Read the element most recently pushed by 'pushElem', but __does not__ move the cursor. Returns
-- 'Nothing' if 'pushElem' has not been called for the given 'RelativeDirection' yet (i.e. if the
-- cursor is at the beginning of the buffer).
pullItem :: MVector mvec n => RelativeDirection -> GapBuffer mvec n n
pullItem dir = atCursor_ PullItem dir getElemAt

-- | Read the element most recently pushed by 'pushBuffer', and then overwrite that element with
-- 'emptyValue'.
popItem :: MVector mvec n => RelativeDirection -> GapBuffer mvec n n
popItem dir = atCursor_ PopItem dir $ \ i -> do
  elem <- getElemAt i
  use emptyValue >>= maybe (pure ()) (putElemAt i)
  relativeCursor dir -= 1
  return elem

----------------------------------------------------------------------------------------------------

-- | Convert a 'GaplessIndex' to a 'VectorIndex'.
fromGaplessIndex :: MVector mvec n => GaplessIndex -> GapBuffer mvec n VectorIndex
fromGaplessIndex gi@(GaplessIndex i) =
  let nope = throwError $ AtIndex gi in
  use beforeCursor >>= \ lo ->
  use afterCursor >>= \ hi ->
  getAllocSize >>= \ size ->
  if i < 0 then nope
  else if i < lo then pure i
  else if i < lo + hi then pure $ i - (size - hi)
  else nope

-- | Get an element at the 'GaplessIndex' value.
getGaplessIndex :: MVector mvec n => GaplessIndex -> GapBuffer mvec n n
getGaplessIndex = fromGaplessIndex >=> liftEditor . getElemAt

-- | This counts the "defined" elements, i.e. the number of elements before the 'currentCursor' and
--  the number of elements above the 'afterCursor'. It is assumed that elements have not been
--  inserted into the gap.
countDefined :: GapBuffer mvec n VectorSize
countDefined = (+) <$> use currentCursor <*> use afterCursor

-- | This counts the "undefined" elements, i.e. the number of elements between the 'currentCursor'
-- and below the 'afterCursor'. It is assumed that elements have not been inserted into the gap.
getGapSize :: MVector mvec n => GapBuffer mvec n VectorSize
getGapSize =
  subtract <$>
  countDefined <*>
  gets (GMVec.length . (view currentBuffer))

-- | Constructs a lazy 3-tuple of slices of type @mvec@ (where @mvec@ is any 'MVector'):
--
-- 1. from the start of the buffer up to but not including the 'currentCursor'
-- 2. between and including the 'currentCursor' index and the 'afterCursor' index,
-- 3. from but not including the 'afterCursor' to the end of the buffer.
--
-- Any or all of these slices may be empty. The returned slices __are not copies__ of the
-- 'currentBuffer', so stateful updates to any of these 3 slices will cause updates to the
-- 'currentBuffer' as well.
--
-- With these slices, you can shift elements around, or copy them to new buffers. __NOTE__ that the
-- cursors in this function are treated as counting the number of valid elements, and not as an
-- actual 'VectorIndex' value.
gapBuffer3Slice
  :: MVector mvec e
  => GapBuffer mvec e
      ( mvec (PrimState IO) e
      , mvec (PrimState IO) e
      , mvec (PrimState IO) e
      )
gapBuffer3Slice = do
  buf <- liftEditor $ use currentBuffer
  lo  <- use beforeCursor
  hi  <- use afterCursor
  let len = GMVec.length buf
  return
    ( GMVec.slice 0 lo buf
    , GMVec.slice lo (len - lo - hi) buf
    , GMVec.slice (len - hi) hi buf
    )

-- | Like 'gapBuffer3Slice' but restricted to the given 'Range'. If the 'Range' does not cover any
-- one of the slices that would be returned by 'gapBuffer3Slice', an empty slice is returned.
gapBuffer3SliceInRange
  :: MVector mvec e
  => Range
  -> GapBuffer mvec e
      ( mvec (PrimState IO) e
      , mvec (PrimState IO) e
      , mvec (PrimState IO) e
      )
gapBuffer3SliceInRange =
  canonicalRange >>> \ (Range{theRangeStart=start,theRangeLength=rlen}) ->
  use beforeCursor >>= \ before ->
  use afterCursor >>= \ after ->
  liftEditor (use currentBuffer) >>= \ buf ->
  pure $
  let end = start + rlen - 1 in
  let len = GMVec.length buf in
  let nil = GMVec.slice 0 0 buf in
  let upper = len - after in
  if rlen == 0 then (nil, nil, nil)
  else if start < before && end < before then
    (GMVec.slice start rlen buf, nil, nil)
  else if start > upper && end > upper then
    (nil, nil, GMVec.slice start rlen buf)
  else if start >= before && end <= upper then
    (nil, GMVec.slice start rlen buf, nil)
  else if start < before && end <= upper then
    let cut = before - start in
    (GMVec.slice start cut buf, GMVec.slice before (rlen - cut) buf, nil)
  else if start >= before && end > upper then
    let cut = upper - start in
    (nil, GMVec.slice start cut buf, GMVec.slice upper (rlen - cut) buf)
  else
    ( GMVec.slice start (before - start) buf
    , GMVec.slice before (upper - before) buf
    , GMVec.slice upper (rlen - upper + start) buf
    )

-- | Create an immutable copy of the buffer with no gap, so the elements in the buffer beffore and
-- after the gap are copied into a new buffer of exactly the correct size with no gaps, and then the
-- buffer is frozen.
fuseBuffer
  :: (MVector mvec n, GVec.Vector vec n, GVec.Mutable vec ~ mvec)
  => GapBuffer mvec n (vec n)
fuseBuffer = do
  (srclo, _, srchi) <- gapBuffer3Slice
  let lolen = GMVec.length srclo
  let hilen = GMVec.length srchi
  liftIO $ do
    targ <- GMVec.new (lolen + hilen)
    let targlo = GMVec.slice 0 lolen targ
    let targhi = GMVec.slice lolen hilen targ
    GMVec.copy targlo srclo
    GMVec.copy targhi srchi
    GVec.unsafeFreeze targ

-- | Shift the position of the 'currentCursor' such that elements in the 'afterCursor' region are
-- placed before the 'currentCursor', also overwriting the elements that are now in the gap with a
-- fill value of type @n@, which is typically an undefined value as the elements that were moved
-- should not be duplicated in the buffer. This assumes the accounting of 'currentCursor' and
-- 'afterCursor' have been done correctly, i.e. the number of elements before the 'currentCursor'
-- and the number of elements 'afterCursor' are correct, no checks are made to ensure this is the
-- case.
--
-- The number of elements shifted is clamped to the maximum number of elements available, so if for
-- example there are only 5 elements after the cursor and you request a 'RelativeIndex' of 10, only
-- 5 elements are moved. The actual number of shifted elements is returned. __However__ if the
-- number of available elements that can be shifted is zero, an exception is thrown. So if there are
-- zero elements after the cursor and any positive 'RelativeIndex' value is requested, and exception
-- is thrown. This makes it safe to call 'shiftCursor' in a loop and have an exception break the
-- loop when the last element is shifted.
shiftCursor :: MVector mvec n => RelativeIndex -> GapBuffer mvec n RelativeIndex
shiftCursor count = case numToRelativeDirection count of
  Nothing       -> return 0
  Just whichWay ->
    if abs count == 1 then stepCursor whichWay else do
      (~lo, _gap, ~hi) <- gapBuffer3Slice
      veclen <- GMVec.length <$> use currentBuffer
      nil <- use emptyValue
      let success maxSize = do
            beforeCursor += count
            afterCursor -= count
            return (maxSize * signum count)
      case whichWay of
        Before -> -- shift cursor downward, elements upward
          use beforeCursor >>= \ maxSize ->
          if maxSize <= 0 then throwError (ShiftCursor count) else
          let size = min maxSize (abs count) in
          if size <= 0 || size > veclen then return 0 else do
            buf <- use currentBuffer
            aft <- use afterCursor
            liftIO $ do
              let src  = sliceFromEnd size lo
              let targ = GMVec.slice (veclen - aft - size) size buf
              GMVec.move targ src
              maybe (pure ()) (GMVec.set src) nil
            success size
        After  ->
          use afterCursor >>= \ maxSize ->
          if maxSize <= 0 then throwError (ShiftCursor count) else
          let size = min maxSize (abs count) in
          if size <= 0 || size > veclen then return 0 else do
            buf <- use currentBuffer
            bef <- use beforeCursor
            liftIO $ do
              let src  = GMVec.slice 0 size hi
              let targ = GMVec.slice bef size buf
              GMVec.move targ src
              maybe (pure ()) (GMVec.set src) nil
            success size
