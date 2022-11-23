-- | An editor for mutable vectors. The module name suggests that this might be some sort of
-- interface to the Emacs text editor, which is unfortunate, because this particular module has
-- nothing to do with Emacs at all, and provides the functionality of an editor that much more
-- general than mere text editing.
--
-- The API in this module provides a foundation for other kinds of mutable vector data structures,
-- such as or a rotating log buffer, or a gap buffer that would be useful in defining a text editor.
module VecEdit.Vector.Editor
  ( -- ** The 'Editor' monad
    Editor(..), newEditorState, runEditor, evalEditor,
    HasEditorState(..), liftEditor, editWithTFGen,

    -- *** The internal state of the 'Editor' monad.
    EditorState(..), currentBuffer, currentCursor, currentRange, editorTFGen,
    getAllocSize,

    -- *** The 'Range' data type
    bufferRange, ioBufferRange, withSubRange, withFullRange, sliceRange, sliceFromEnd,

    -- *** Using the 'currentRange'
    resetCurrentRange, foldOverRange, foldOverRange_,

    -- *** Print Buffer Content
    printBuffer,
    printOverIOBuffer, printOverBuffer,
    printOverIOBufferRange, printOverBufferRange,

    -- *** Updating the current cursor
    --
    -- There is a cursor function maintained by the 'Editor' context. This is a separate value from
    -- the @'currentRange' . 'rangeStart'@ value.
    cursor, stepWrap, backWrap, withSubCursor,
    getCurrentElem, putCurrentElem, getElemAt, putElemAt,

    -- *** Changing the current buffer
    newBuffer, newCurrentBuffer, changeCurrentBuffer, withBuffer, withNewBuffer,
    growBufferWithCursor,

    -- *** Copying and freezing the current buffer
    --
    -- A "freeze" is when all or a portion of a mutable vector is copied into an immutable vector.
    sliceCurrentBuffer, sliceCurrentRange,
    copyCurrentRange, thawBuffer, freezeCurrentRange, freezeCurrentBuffer,

    -- ** Folding and Mapping
    --
    -- You can fold and map over some other 'IOBuffer', or over the 'currentBuffer'. All of these
    -- functions are specialized calls to 'foldOverRange' with additional stateful updates to the
    -- buffers passed as arguments.

    -- *** Folding over elements in a buffer
    foldOverIOBuffer, foldOverBuffer, mapOverIOBuffer, foldBuffer, mapBuffer,

    -- *** Searching for elements in a buffer
    searchOverIOBufferRange, searchOverIOBuffer, searchBufferRange, searchBuffer,

    -- *** Updating elements in a buffer
    Update(..), updateOverIOBufferRange, updateOverIOBuffer, updateBufferRange, updateBuffer,

    -- *** Filling (overwriting) elements to the buffer
    --
    -- These functions for filling the 'currentRange' of the 'currentBuffer' are similar to
    -- 'mapBuffer', but importantly, do not read any elements from the array. It is better to use
    -- @fill*@ family of functions rather than mapping with a constant function.
    fillOverIOBuffer, fillBuffer,
    fillWith, fillRandom, fillRandomR,

    -- *** Filling the current range with a fold
    --
    -- The term "generator" here is used to describe a "fill" which also folds over a value after
    -- each element is written.
    foldFillOverIOBuffer, foldFillOverIOBufferT, foldFillBuffer, foldFillBufferT,

    -- *** Mapping the current range with a fold
    --
    -- This is similar to the @foldFill*@ functions above, but additionally reads each element from
    -- the array and passes it's value to the folding function before overwriting it with the new
    -- value returned by the folding function.
    foldMapOverIOBuffer, foldMapOverIOBufferT, foldMapBuffer, foldMapBufferT,

    -- *** Blitting
    --
    -- 1-dimensional buffer blitting to the 'currentBuffer'.
    blitBuffer, blitIOBuffer, zipBlitBuffer, zipBlitIOBuffer,

    -- ** Other useful algorithms
    bufferElemBounds, ioBufferElemBounds, currentRangeElemBounds,
    convolution, normalizeIOBuffer, normalizeCurrentRange,
  ) where

import VecEdit.Types
  ( VectorIndex, VectorSize, VectorRange,
    Range(..), rangeStart, rangeLength, rangeEnd, canonicalizeRange
  )

import VecEdit.Print.DisplayInfo (ralign6)

import Control.Arrow ((***), (>>>))
import Control.Lens (Lens', lens, use, assign, modifying, (&), (^.), (.~), (.=))
import Control.Monad (when, (>=>))
import Control.Monad.Cont (runContT, callCC)
import Control.Monad.State (MonadState(..), StateT(..), evalStateT)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Primitive (PrimState)
import Control.Monad.Random.Class (MonadRandom(..))

import Data.Functor (void)
import qualified Data.Text as Strict
import Data.Vector.Generic (Vector)
import qualified Data.Vector.Generic as GVec
import Data.Vector.Generic.Mutable (MVector)
import qualified Data.Vector.Generic.Mutable as GMVec

import Numeric.IEEE (IEEE(..), infinity)

import System.Random (Random(..))
import System.Random.TF.Init (newTFGen)
import System.Random.TF.Gen (TFGen)
import qualified System.Random.TF.Gen as TFGen

----------------------------------------------------------------------------------------------------

-- | A function type for operating on vectors. Instantiates 'MonadRandom' so you can generate
-- random values as well.
--
-- This function provides the stateful context used by numerous APIs throughout this module. Any
-- function where the type context contains any or all of the following constraints:
-- @
--  :: ( 'MonadIO' editor, 'MonadState' st editor, 'HasMonadState' st
--     , 'EditorMVectorType' st ~ mvec
--     , 'EditorMVectorElem' st ~ elem
--     , ...
--     )
--  => ...
-- @
--
-- know that this 'Editor' function type satisfies all of those constraints. To make things slightly
-- easier to understand, the type variable @editor@ is used wherever the 'Editor' function type
-- would usually be the type inferred by the type checker as the concrete type of @editor@. Of
-- course, 'Editor' is also member of the 'Monad' typeclass, so it can also be used with @do@
-- notation, or any function of type @Monad m => m a@
newtype Editor mvec n a = Editor (StateT (EditorState mvec n) IO a)
  deriving (Functor, Applicative, Monad, MonadIO, MonadState (EditorState mvec n))

-- | Like the 'MonadIO' typeclass, but lifts an 'Editor' function instead of an @IO@
-- function. Unlike the 'MonadIO' typeclass, this typeclass requires a 'Lens'' be instantiated which
-- allows access to a 'EditorState'. The function 'liftEditor' then uses this lens to evaluate an
-- 'Editor' function within a parent function context.
class HasEditorState st where
  type EditorMVectorType st :: * -> * -> *
  type EditorMVectorElem st :: *
  vectorEditorState :: Lens' st (EditorState (EditorMVectorType st) (EditorMVectorElem st))

instance HasEditorState (EditorState mvec elem) where
  type EditorMVectorType (EditorState mvec elem) = mvec
  type EditorMVectorElem (EditorState mvec elem) = elem
  vectorEditorState = lens id (flip const)

instance MonadRandom (Editor mvec n) where
  getRandomR = editWithTFGen . randomR
  getRandom = editWithTFGen random
  getRandomRs =  (<$> (editWithTFGen TFGen.split)) . randomRs
  getRandoms = randoms <$> editWithTFGen TFGen.split

-- | Create a new default 'EditorState'. The 'currentBuffer' is empty so you can use
-- 'newCurrentBuffer'.
newEditorState :: (MVector mvec n) => IO (EditorState mvec n)
newEditorState = do
  gen <- newTFGen
  targ <- GMVec.new 0
  return EditorState
    { theCurrentBuffer = targ
    , theCurrentRange = Range 0 0
    , theCurrentCursor = 0
    , theEditorTF = gen
    }

-- | Reduce the 'Editor' function to an @IO@ function.
runEditor :: Editor mvec n a -> EditorState mvec n -> IO (a, EditorState mvec n)
runEditor (Editor f) = runStateT f

-- | Like 'runEditor' but discards the 'EditorState' when evaluation is completed.
evalEditor :: Editor mvec n a -> EditorState mvec n -> IO a
evalEditor (Editor f) = evalStateT f

-- | Evaluate an 'Editor' function type within another function of type @editor@ provided that
-- @editor@ insatntiates 'Monad' and 'MonadState', and the stateful value of @editor@ instantiates
-- 'HasEditorState' such that the 'EditorState' is accessible using the 'vectorEditorState'
-- lens. The 'vectorEditorState' lens is used to evaluate the 'Editor' function and any changes made
-- to the 'EditorState' are used to update the @st@ type.
liftEditor
  :: ( MonadIO editor, MonadState st editor, HasEditorState st
     , EditorMVectorType st ~ mvec
     , EditorMVectorElem st ~ elem
     )
  => Editor mvec elem a -> editor a
liftEditor f = do
  (a, st) <- use vectorEditorState >>= liftIO . runEditor f
  vectorEditorState .= st
  return a

-- | This function is used as the entry point for all of the 'MonadRandom' APIs, but it is provided
-- here in case you think of a reason you need to use this instead of any of the 'MonadRandom' APIs.
editWithTFGen
  :: (MonadState st editor, HasEditorState st)
  => (TFGen -> (a, TFGen)) -> editor a
editWithTFGen f = do
  gen0 <- use editorTFGen
  let (a, gen) = f gen0
  editorTFGen .= gen
  return a

-- | Return the size of the entire buffer.
getAllocSize
  :: ( MonadState st editor, HasEditorState st
     , EditorMVectorType st ~ mvec
     , EditorMVectorElem st ~ n
     , MVector mvec n
     )
  => editor VectorSize
getAllocSize = GMVec.length <$> use currentBuffer

----------------------------------------------------------------------------------------------------

-- | The state of the 'Editor' monad.
data EditorState (mvec :: * -> * -> *) n
  = EditorState
    { theCurrentBuffer :: !(mvec (PrimState IO) n)
      -- ^ A place to store a buffer that you want to work on.
    , theCurrentCursor :: !VectorIndex
      -- ^ A cursor to remember where you want to read or write elements of the current buffer.
    , theCurrentRange  :: !VectorRange
    , theEditorTF      :: !TFGen
      -- ^ A random number generator.
    }

-- | A 'Lens'' that can 'view' or 'set' the @mvec@ value of the 'EditorState'.
currentBuffer
  :: (HasEditorState st)
  => Lens' st ((EditorMVectorType st) (PrimState IO) (EditorMVectorElem st))
currentBuffer = vectorEditorState . lens theCurrentBuffer (\ a b -> a{ theCurrentBuffer = b })

currentCursor :: (HasEditorState st) => Lens' st VectorIndex
currentCursor = vectorEditorState . lens theCurrentCursor (\ a b -> a{ theCurrentCursor = b })

currentRange :: (HasEditorState st) => Lens' st VectorRange
currentRange = vectorEditorState . lens theCurrentRange (\ a b -> a{ theCurrentRange = b })

editorTFGen :: (HasEditorState st) => Lens' st TFGen
editorTFGen = vectorEditorState . lens theEditorTF (\ a b -> a{ theEditorTF = b })

----------------------------------------------------------------------------------------------------

-- | Get a 'Range' that encompases an entire 'Buffer'.
bufferRange :: Vector vec n => vec n -> VectorRange
bufferRange = Range 0 . GVec.length

-- | Get a 'Range' that encompases an entire 'IOBuffer'.
ioBufferRange :: (MVector mvec n) => mvec (PrimState IO) n -> VectorRange
ioBufferRange = Range 0 . GMVec.length

-- | Set the 'currentRange' to encompass the entier 'currentBuffer'.
resetCurrentRange
  :: ( MonadState st m, HasEditorState st
     , EditorMVectorType st ~ mvec
     , EditorMVectorElem st ~ n
     , MVector mvec n
     )
  => m ()
resetCurrentRange = ioBufferRange <$> use currentBuffer >>= assign currentRange

-- | Create a new pointer to the 'currentRange' of the 'currentBuffer', producing an 'IOBuffer' that
-- refers to the original 'currentBuffer' but with a different 'Range' of values. Modifying the
-- elements in the resultant 'IOBuffer' will also modify the original 'currentBuffer'.
sliceRange :: MVector mvec n => VectorRange -> mvec st n -> mvec st n
sliceRange = canonicalizeRange >>> \ (Range lo len) -> GMVec.slice lo len

-- | Similar to 'sliceRange', but slices a number of elements from the end of the given buffer. The
-- 'VectorSize' value given will be the number of elements contained in the new slice.
sliceFromEnd :: MVector mvec n => VectorSize -> mvec st n -> mvec st n
sliceFromEnd len vec = GMVec.slice (GMVec.length vec - len) len vec

-- | This function simply iterates over the 'VectorIndex' values within in a 'Range' using a monadic
-- continuation function, iteratively updating the fold value. Each iteration the 'VectorIndex' is
-- incremented from the 'rangeStart' until 'rangeLength' number of iterations have occurred. If
-- 'rangeLength' is negative, the iteration begins at @'rangeStart'@ and decreases until
-- @'rangeStart' + 'rangeLength'@ is reached. See also 'reverseRange'.
foldOverRange :: Monad m => (VectorIndex -> fold -> m fold) -> VectorRange -> fold -> m fold
foldOverRange f (Range lo len) =
  case compare len 0 of
    EQ -> return
    GT -> loop (<) succ lo
    LT -> loop (>) pred lo
  where
    lim = lo + len
    loop test step i = if test i lim then f i >=> (loop test step $! step i) else return

-- | Like 'foldOverRange' but purely for side effects, so no @fold@ value is required.
foldOverRange_ :: Monad m => (VectorIndex -> m ()) -> VectorRange -> m ()
foldOverRange_ f range = foldOverRange (\ i () -> f i) range ()

-- | Evaluate a given 'Editor' continuation function with a different 'currentRange' value, then
-- restore the previous range.
withSubRange
  :: (MonadState st editor, HasEditorState st)
  => editor a -> VectorRange -> editor a
withSubRange f newRange = do
  oldRange <- use currentRange
  currentRange .= newRange
  f <* (currentRange .= oldRange)

-- | Similar to 'withSubRange' but the sub-'Range' will encompass the whole buffer. When the
-- continuation returns, the previous 'currentRange' will be restored.
withFullRange
  :: ( MonadState st editor, HasEditorState st
     , EditorMVectorType st ~ mvec
     , EditorMVectorElem st ~ n
     , MVector mvec n
     )
  => editor a -> editor a
withFullRange f = use currentBuffer >>= withSubRange f . ioBufferRange

----------------------------------------------------------------------------------------------------

-- | Update the cursor position. This is a shorthand for @'modifying' 'currentCursor'@, but is
-- provided as a convenience because this operation is used often. For example: @('cursor' 'succ')@
-- increments the cursor, @('cursor' 'pred')@ decrements the cursor.
cursor
  :: (MonadState st editor, HasEditorState st)
  => (VectorIndex -> VectorIndex) -> editor ()
cursor = modifying currentCursor

-- | Increment the 'cursor' but if the cursor goes out of bounds, wrap back to the
-- currentRange. This function, along with 'getCurrentElem' or 'putCurrentElem', can be used to
-- implement a rotating log buffer.
stepWrap :: (MonadState st editor, HasEditorState st) => editor ()
stepWrap = do
  (Range lo len) <- use currentRange
  cursor $ \ i -> if i < lo + len then i + 1 else lo

-- | Like 'stepWrap' but decrements the cursor
backWrap :: (MonadState st editor, HasEditorState st) => editor ()
backWrap = do
  (Range lo len) <- use currentRange
  cursor $ \ i -> if i <= lo then lo + len - 1 else i - 1

-- | Evaluate a given 'Editor' continuation function with a different 'cursor' value, then restore
-- the previous range. This can be useful when using a blitting function like 'blitBuffer' which
-- modifies the 'cursor' position. If you want to keep the current 'cursor' without modifying it,
-- you could (for example) simply evaluate @'withSubCursor' 'id' $ 'blitBuffer' buf range@.
withSubCursor
  :: (MonadState st editor, HasEditorState st)
  => (VectorIndex -> VectorIndex) -> editor a -> editor a
withSubCursor move f = do
  old <- use currentCursor
  cursor move
  f <* (currentCursor .= old)

-- | Get the element in the 'currentBuffer' at the 'currentCursor' index. This function may throw an
-- exception if the 'currentCursor' is out of range.
getCurrentElem
  :: ( MonadIO editor, MonadState st editor, HasEditorState st
     , EditorMVectorType st ~ mvec
     , EditorMVectorElem st ~ n
     , MVector mvec n
     )
  => editor n
getCurrentElem = GMVec.read <$> use currentBuffer <*> use currentCursor >>= liftIO

-- | Overwrite an element into the 'currentBuffer' at the 'currentCursor' index, overwriting the
-- element that is currently there. This function may throw an exception if the 'currentCursor' is
-- out of range.
putCurrentElem
  :: ( MonadIO editor, MonadState st editor, HasEditorState st
     , EditorMVectorType st ~ mvec
     , EditorMVectorElem st ~ n
     , MVector mvec n
     )
  => n -> editor ()
putCurrentElem n = GMVec.write <$> use currentBuffer <*> use currentCursor <*> pure n >>= liftIO

-- | Read an element frome the 'currentBuffer' at the given index.
getElemAt
  :: ( MonadIO editor, MonadState st editor, HasEditorState st
     , EditorMVectorType st ~ mvec
     , EditorMVectorElem st ~ n
     , MVector mvec n
     )
  => VectorIndex -> editor n
getElemAt i = GMVec.read <$> use currentBuffer <*> pure i >>= liftIO

-- | Overrite an element in the 'currentBuffer' at the given index.
putElemAt
  :: ( MonadIO editor, MonadState st editor, HasEditorState st
     , EditorMVectorType st ~ mvec
     , EditorMVectorElem st ~ n
     , MVector mvec n
     )
  => VectorIndex -> n -> editor ()
putElemAt i e = GMVec.write <$> use currentBuffer <*> pure i <*> pure e >>= liftIO

----------------------------------------------------------------------------------------------------

-- | Create a new mutable buffer, but does not set it as the 'currentBuffer'. All elements are
-- uninitialized.
newBuffer
  :: ( MonadIO editor, MonadState st editor, HasEditorState st
     , EditorMVectorType st ~ mvec
     , EditorMVectorElem st ~ n
     , MVector mvec n
     )
  => VectorSize -> editor (mvec (PrimState IO) n)
newBuffer = liftIO . GMVec.new

-- | Create a new mutable buffer and set it as the 'currentBuffer'. All elements are
-- uninitialized. Also resets the 'currentRange'.
newCurrentBuffer
  :: ( MonadIO editor, MonadState st editor, HasEditorState st
     , EditorMVectorType st ~ mvec
     , EditorMVectorElem st ~ n
     , MVector mvec n
     )
  => VectorSize -> editor ()
newCurrentBuffer =
  newBuffer >=> \ buf ->
  currentBuffer .= buf >>
  resetCurrentRange

-- | Change the current 'IOBuffer' for the current 'Editor' monad evaluation context. This also
-- resets the 'currentRange' to a range fully encompassing the given 'IOBuffer'
changeCurrentBuffer
  :: ( MonadIO editor, MonadState st editor, HasEditorState st
     , EditorMVectorType st ~ mvec
     , EditorMVectorElem st ~ n
     , MVector mvec n
     )
  => mvec (PrimState IO) n -> editor ()
changeCurrentBuffer buf = currentBuffer .= buf >> currentRange .= ioBufferRange buf

-- | Save the current 'EditorState' and begin evaluating a 'Editor' continuation with the given
-- 'IOBuffer'. The previous 'EditorState' will be restored when the given continuation completes
-- evaluation. The continuation is provided a copy of the previous 'EditorState' but modifications
-- to this state are not preserved after evaluation completes.
--
-- This function also has the advantage of being able to use an 'IOBuffer' of a different element
-- type than what the enclosing 'Editor' context is using.
withBuffer
  :: ( MonadIO editor, MonadState st editor, HasEditorState st
     , EditorMVectorType st ~ v
     , EditorMVectorElem st ~ n
     , MVector v n, MVector vsrc e
     )
  => vsrc (PrimState IO) e -> (EditorState v n -> Editor vsrc e a) -> editor a
withBuffer new f = do
  st <- use vectorEditorState
  fmap fst $ liftIO $ runEditor (f st) EditorState
    { theCurrentBuffer = new
    , theCurrentCursor = theCurrentCursor st
    , theCurrentRange  = ioBufferRange new
    , theEditorTF      = theEditorTF st
    }

-- | Convenience function to evaluate both 'newBuffer' and 'withBuffer' at the same time. Be sure to
-- evaluate 'freezeCurrentBuffer' or @'use' 'currentBuffer'@ as the final step of the continuation
-- function, unless you really don't need the buffer that was created.
withNewBuffer 
  :: ( MonadIO editor, MonadState st editor, HasEditorState st
     , EditorMVectorType st ~ v
     , EditorMVectorElem st ~ n
     , MVector v n, MVector vsrc e
     )
  => VectorSize -> (EditorState v n -> Editor vsrc e a) -> editor a
withNewBuffer size f = liftIO (GMVec.new size) >>= flip withBuffer f

-- | Check the 'currentCursor' value, if it has increased to an index beyond the size of the size of
-- the 'currentBuffer', resize the 'currentBuffer' by allocating a new one given by the size
-- function, then copy all of the elements of the current buffer into the new buffer, then set the
-- 'currentBuffer' to the new buffer. No checks are performed to ensure that the size given by the
-- function that comptues the new size is actually bigger than the old size, but this function
-- assumes it is bigger and an exception will be thrown if the new buffer size is smaller.
growBufferWithCursor
  :: ( MonadIO editor, MonadState st editor, HasEditorState st
     , EditorMVectorType st ~ mvec
     , EditorMVectorElem st ~ n
     , MVector mvec n
     )
  => (VectorSize -> VectorIndex -> VectorSize)
      -- ^ Computes the new size of the buffer from the current buffer size and cursor index.
  -> editor ()
growBufferWithCursor calcNewSize =
  use currentCursor >>= \ cur ->
  use currentBuffer >>= \ old ->
  let len = GMVec.length old in
  when (cur >= len) $
  withFullRange $
  newCurrentBuffer (calcNewSize len cur) >>
  use currentBuffer >>= \ new ->
  liftIO (GMVec.copy (GMVec.slice 0 len new) old)

----------------------------------------------------------------------------------------------------

-- | Lice 'sliceRange' but slices the 'currentBuffer' with the given 'Range'.
sliceCurrentBuffer
  :: ( MonadState st editor, HasEditorState st
     , EditorMVectorType st ~ mvec
     , EditorMVectorElem st ~ n
     , MVector mvec n
     )
  => VectorRange -> editor (mvec (PrimState IO) n)
sliceCurrentBuffer = (<$> (use currentBuffer)) . sliceRange

-- | Like 'sliceCurrentBuffer' uses the 'currentRange' as the 'Range' of elements to slice.
sliceCurrentRange
  :: ( MonadState st editor, HasEditorState st
     , EditorMVectorType st ~ mvec
     , EditorMVectorElem st ~ n
     , MVector mvec n
     )
  => editor (mvec (PrimState IO) n)
sliceCurrentRange = sliceCurrentBuffer <$> use currentRange >>= id

-- | Create a mutable copy of the 'currentRange' of the 'currentBuffer'.
copyCurrentRange
  :: ( MonadIO editor, MonadState st editor, HasEditorState st
     , EditorMVectorType st ~ mvec
     , EditorMVectorElem st ~ n
     , MVector mvec n
     )
  => editor (mvec (PrimState IO) n)
copyCurrentRange = do
  new <- GMVec.new . abs <$> use (currentRange . rangeLength) >>= liftIO
  GMVec.copy new <$> sliceCurrentRange >>= liftIO
  return new

freezeBuffer
  :: ( MonadIO editor, MonadState st editor, HasEditorState st
     , EditorMVectorType st ~ mvec, mvec ~ GVec.Mutable vec
     , EditorMVectorElem st ~ n
     , MVector mvec n, Vector vec n
     )
  => mvec (PrimState IO) n -> editor (vec n)
freezeBuffer = liftIO . GVec.freeze

-- | Create a new mutable 'IOBuffer' by copying a given immutable 'Buffer'.
thawBuffer
  :: ( MonadIO editor, MonadState st editor, HasEditorState st
     , EditorMVectorType st ~ mvec, mvec ~ GVec.Mutable vec
     , EditorMVectorElem st ~ n
     , MVector mvec n, Vector vec n
     )
  => vec n -> editor (mvec (PrimState IO) n)
thawBuffer = liftIO . GVec.thaw

-- | Create an immutable copy of the 'currentRange' of the 'currentBuffer'.
freezeCurrentRange
  :: ( MonadIO editor, MonadState st editor, HasEditorState st
     , EditorMVectorType st ~ mvec, mvec ~ GVec.Mutable vec
     , EditorMVectorElem st ~ n
     , MVector mvec n, Vector vec n
     )
  => editor (vec n)
freezeCurrentRange = sliceCurrentRange >>= freezeBuffer

-- | Create an immutable copy of an 'IOBuffer'.
freezeCurrentBuffer
  :: ( MonadIO editor, MonadState st editor, HasEditorState st
     , EditorMVectorType st ~ mvec, mvec ~ GVec.Mutable vec
     , EditorMVectorElem st ~ n
     , MVector mvec n, Vector vec n
     )
  => mvec (PrimState IO) n -> editor (vec n)
freezeCurrentBuffer = liftIO . GVec.freeze

----------------------------------------------------------------------------------------------------

-- | Perform a fold over the elements in some 'IOBuffer' (not the 'currentBuffer'). The fold
-- continuation that you provide may still safely make use of the 'getCurrentElem' or
-- 'putCurrentElem' APIs which operate on the 'currentBuffer'.
foldOverIOBuffer
  :: (MVector mvec e, MonadIO m)
  => (fold -> VectorIndex -> e -> m fold)
  -> fold -> mvec (PrimState IO) e -> VectorRange -> m fold
foldOverIOBuffer f fold v range =
  foldOverRange (\ i fold -> liftIO (GMVec.read v i) >>= f fold i) range fold

foldOverBuffer
  :: (Vector vec e, Monad m)
  => (fold -> VectorIndex -> e -> m fold)
  -> fold -> vec e -> VectorRange -> m fold
foldOverBuffer f fold v range = foldOverRange (\ i fold -> f fold i $ v GVec.! i) range fold

-- | Perform a map over the elements in some 'IOBuffer' (not the 'currentBuffer'). The map
-- continuation that you provide may still safely make use of the 'getCurrentElem' or
-- 'putCurrentElem' APIs which operate on the 'currentBuffer'.
mapOverIOBuffer
  :: (MVector mvec e, MonadIO m)
  => (VectorIndex -> e -> m e)
  -> mvec (PrimState IO) e -> VectorRange -> m ()
mapOverIOBuffer f v =
  foldOverRange_ (\ i -> liftIO (GMVec.read v i) >>= f i >>= liftIO . GMVec.write v i)

-- | Like 'foldWithBuffer' but operates on the 'currentBuffer' and on the 'currentRange'.
foldBuffer
  :: ( MonadIO editor, MonadState st editor, HasEditorState st
     , EditorMVectorType st ~ mvec
     , EditorMVectorElem st ~ n
     , MVector mvec n
     )
  => (fold -> VectorIndex -> n -> editor fold)
  -> fold -> editor fold
foldBuffer f fold = foldOverIOBuffer f fold <$> use currentBuffer <*> use currentRange >>= id

-- | Like 'mapWithIOBuffer' but operates on the 'currentBuffer' and on the 'currentRange'.
mapBuffer
  :: ( MonadIO editor, MonadState st editor, HasEditorState st
     , EditorMVectorType st ~ mvec
     , EditorMVectorElem st ~ n
     , MVector mvec n
     )
  => (VectorIndex -> n -> editor n) -> editor ()
mapBuffer f = mapOverIOBuffer f <$> use currentBuffer <*> use currentRange >>= id

----------------------------------------------------------------------------------------------------

-- | Similar to 'mapWithBuffer', but does not perform a read operation. So it is more efficient to
-- use this function rather than use 'mapWithBuffer' using a constant function.
fillOverIOBuffer
  :: (MVector mvec e, MonadIO m)
  => (VectorIndex -> m e)
  -> mvec (PrimState IO) e -> VectorRange -> m ()
fillOverIOBuffer f v = foldOverRange_ (\ i -> f i >>= liftIO . GMVec.write v i)

-- | Like 'fillOverBuffer' but operates on the 'currentBuffer' and within the 'currentRange'.
fillBuffer
  :: ( MonadIO editor, MonadState st editor, HasEditorState st
     , EditorMVectorType st ~ mvec
     , EditorMVectorElem st ~ n
     , MVector mvec n
     )
  => (VectorIndex -> editor n) -> editor ()
fillBuffer f = fillOverIOBuffer f <$> use currentBuffer <*> use currentRange >>= id

-- | Fill the 'currentRange' of the 'currentBuffer' with the given value.
fillWith
  :: ( MonadIO editor, MonadState st editor, HasEditorState st
     , EditorMVectorType st ~ mvec
     , EditorMVectorElem st ~ n
     , MVector mvec n
     )
  => n -> editor ()
fillWith n = fillBuffer (const $ return n)

-- | Fill the 'currentRange' of the 'currentBuffer' with random values in the given range.
fillRandomR
  :: ( Random n, MonadRandom editor
     , MonadIO editor, MonadState st editor, HasEditorState st
     , EditorMVectorType st ~ mvec
     , EditorMVectorElem st ~ n
     , MVector mvec n
     )
  => (n, n) -> editor ()
fillRandomR r = fillBuffer (const $ getRandomR r)

-- | Like 'fillRandomR' but uses the 'minBound' and 'maxBound' as the range.
fillRandom
  :: ( Bounded n, Random n, MonadRandom editor
     , MonadIO editor, MonadState st editor, HasEditorState st
     , EditorMVectorType st ~ mvec
     , EditorMVectorElem st ~ n
     , MVector mvec n
     )
  => editor ()
fillRandom = fillRandomR (minBound, maxBound)

----------------------------------------------------------------------------------------------------

-- | Similar to 'fillOverBuffer', but folds over a value as well using the given continuation.
foldFillOverIOBuffer
  :: ( MonadIO editor, MonadState st editor, HasEditorState st
     , EditorMVectorType st ~ v
     , EditorMVectorElem st ~ e
     , MVector mvec e
     )
  => (VectorIndex -> fold -> editor (e, fold)) -- ^ the "generator"
  -> mvec (PrimState IO) e -> VectorRange -> fold -> editor fold
foldFillOverIOBuffer f v =
  foldOverRange $ \ i ->
  f i >=> uncurry (>>) . (liftIO . GMVec.write v i *** return)

-- | Same as the 'foldFillBuffer' function, but uses the 'StateT' monad transformer function type
-- for the continuation. This continuation is essentially the same exact type but wrapped in the
-- 'StateT' constructor, which allows you to use the 'get', 'put', and 'modify', as well as lenses
-- like @'%='@ and @'.='@, to define your continuation.
foldFillOverIOBufferT
  :: ( MonadIO editor, MonadState st editor, HasEditorState st
     , EditorMVectorType st ~ v
     , EditorMVectorElem st ~ e
     , MVector mvec e, MVector v e
     )
  => (VectorIndex -> StateT fold editor e)
  -> mvec (PrimState IO) e -> VectorRange -> fold -> editor fold
foldFillOverIOBufferT = foldFillOverIOBuffer . fmap runStateT

-- | Like 'foldFillBuffer' but operates on the 'currentRange' of the 'currentBuffer'.
foldFillBuffer
  :: ( MonadIO editor, MonadState st editor, HasEditorState st
     , EditorMVectorType st ~ mvec
     , EditorMVectorElem st ~ n
     , MVector mvec n
     )
  => (VectorIndex -> fold -> editor (n, fold)) -- ^ the "generator"
  -> fold -> editor fold
foldFillBuffer f fold =
  foldFillOverIOBuffer f <$>
  use currentBuffer <*>
  use currentRange <*>
  pure fold >>=
  id

-- | Like 'foldFillCurrentRange' but uses the 'StateT' monad transformer function type for the
-- continuation. This continuation is essentially the same exact type but wrapped in the 'StateT'
-- constructor, which allows you to use the 'get', 'put', and 'modify', as well as lenses like
-- @'%='@ and @'.='@, to define your continuation.
foldFillBufferT
  :: ( MonadIO editor, MonadState st editor, HasEditorState st
     , EditorMVectorType st ~ mvec
     , EditorMVectorElem st ~ n
     , MVector mvec n
     )
  => (VectorIndex -> StateT fold editor n) -- ^ the "generator"
  -> fold -> editor fold
foldFillBufferT f fold =
  foldFillOverIOBufferT f <$>
  use currentBuffer <*>
  use currentRange <*>
  pure fold >>=
  id

----------------------------------------------------------------------------------------------------

-- | Similar to 'mapBuffer', but folds over a value as well using the given continuation.
foldMapOverIOBuffer
  :: (MVector mvec e, MonadIO editor)
  => (VectorIndex -> e -> fold -> editor (e, fold)) -- ^ the "generator"
  -> mvec (PrimState IO) e -> VectorRange -> fold -> editor fold
foldMapOverIOBuffer f v =
  foldOverRange $ \ i fold ->
  liftIO (GMVec.read v i) >>= \ e ->
  f i e fold >>=
  uncurry (>>) . (liftIO . GMVec.write v i *** return)

-- | Same as the 'foldMapBuffer' function, but uses the 'StateT' monad transformer function type
-- for the continuation. This continuation is essentially the same exact type but wrapped in the
-- 'StateT' constructor, which allows you to use the 'get', 'put', and 'modify', as well as lenses
-- like @'%='@ and @'.='@, to define your continuation.
foldMapOverIOBufferT
  :: (MVector mvec e, MonadIO editor)
  => (VectorIndex -> e -> StateT fold editor e)
  -> mvec (PrimState IO) e -> VectorRange -> fold -> editor fold
foldMapOverIOBufferT = foldMapOverIOBuffer . fmap (fmap runStateT)

-- | Like 'foldMapBuffer' but operates on the 'currentRange' of the 'currentBuffer'.
foldMapBuffer
  :: ( MonadIO editor, MonadState st editor, HasEditorState st
     , EditorMVectorType st ~ mvec
     , EditorMVectorElem st ~ n
     , MVector mvec n
     )
  => (VectorIndex -> n -> fold -> editor (n, fold)) -- ^ the "generator"
  -> fold -> editor fold
foldMapBuffer f fold =
  foldMapOverIOBuffer f <$>
  use currentBuffer <*>
  use currentRange <*>
  pure fold >>=
  id

-- | Like 'foldMapCurrentRange' but uses the 'StateT' monad transformer function type for the
-- continuation. This continuation is essentially the same exact type but wrapped in the 'StateT'
-- constructor, which allows you to use the 'get', 'put', and 'modify', as well as lenses like
-- @'%='@ and @'.='@, to define your continuation.
foldMapBufferT
  :: MVector mvec n
  => (VectorIndex -> n -> StateT fold (Editor mvec n) n) -- ^ the "generator"
  -> fold -> Editor mvec n fold
foldMapBufferT f fold =
  foldMapOverIOBufferT f <$>
  use currentBuffer <*>
  use currentRange <*>
  pure fold >>=
  id

----------------------------------------------------------------------------------------------------

-- | Like 'printBufferRange' but operates on a range of an immutable vector within an arbitrary
-- Monadic function evaluation context.
bufferPrinter
  :: Monad m
  => ((Int -> VectorIndex -> e -> m Int) -> Int -> vec e -> VectorRange -> m Int)
  -> (e -> m Bool) -- ^ test if an element is null
  -> (Strict.Text -> e -> m ()) -- ^ print an element
  -> m () -- print an elipsis
  -> vec e
  -> VectorRange
  -> m ()
bufferPrinter foldOver testElem printElem elipsis vec range =
  void $
  foldOver
  (\ nullCount i elem ->
    let display = printElem (Strict.pack $ ralign6 i <> ": ") elem in
    testElem elem >>= \ nonNull ->
    if nonNull || i == rangeEnd range then display >> return 0 else
    ( if nullCount < 2 then display
      else if nullCount == 2 then elipsis
      else return ()
    ) >>
    return (nullCount + 1)
  )
  0
  vec
  range

-- | Like 'printOverBuffer' but lets you limit the range of elements printed with an additional
-- 'Range' parameter passed as the final argument.
printOverBufferRange
  :: (Vector vec e, Monad editor)
  => (e -> editor Bool) -- ^ test if an element is null
  -> (Strict.Text -> e -> editor ()) -- ^ print an element
  -> editor () -- print an elipsis
  -> vec e
  -> VectorRange
  -> editor ()
printOverBufferRange = bufferPrinter foldOverBuffer

-- | Like 'printOverIOBuffer', but operates on an immutable 'Vector'.
printOverBuffer
  :: (Vector vec e, Monad editor)
  => (e -> editor Bool) -- ^ test if an element is null
  -> (Strict.Text -> e -> editor ()) -- ^ print an element
  -> editor () -- print an elipsis
  -> vec e
  -> editor ()
printOverBuffer a b c vec =
  printOverBufferRange a b c vec $
  bufferRange vec

-- | Like 'printOverIOBuffer' but lets you limit the range of elements printed with an additional
-- 'Range' parameter passed as the final argument.
printOverIOBufferRange
  :: (MVector mvec e, MonadIO editor)
  => (e -> editor Bool) -- ^ test if an element is null
  -> (Strict.Text -> e -> editor ()) -- ^ print an element
  -> editor () -- print an elipsis
  -> mvec (PrimState IO) e
  -> VectorRange
  -> editor ()
printOverIOBufferRange = bufferPrinter foldOverIOBuffer

-- | This function is similar to 'printBuffer', but operates on an arbitrary 'MVector' (usually a
-- mutable vector of type 'Data.Vector.Mutable.IOVector') and in an arbitrary monadic function
-- context @editor@ (but constrained by 'MonadIO'). Refer to the docuemntation on 'printBuffer' for an
-- explanation of the first three arguments
printOverIOBuffer
  :: (MVector mvec e, MonadIO editor)
  => (e -> editor Bool) -- ^ test if an element is null
  -> (Strict.Text -> e -> editor ()) -- ^ print an element
  -> editor () -- print an elipsis
  -> mvec (PrimState IO) e
  -> editor ()
printOverIOBuffer a b c vec =
  printOverIOBufferRange a b c vec $
  ioBufferRange vec

-- | This is a simple way to visualize sparse buffers to a log in the IO function context. Pass two
-- arguments:
--
--   1. a predicate function that decides whether an element is null,
--   2. a continuation (callback) function for printing an element, and
--   3. a continuation for printing an elipsis.
--
-- Long runs of null elements will not be printed, every non-null element will be printed to a line
-- prefixed with it's index value. After 2 null elements, a single elipsis "...." is printed and all
-- output is blocked until a non-null element is found and outputting of each element resumes. The
-- 'currentRange' can be set to limit the range of elements printed.
printBuffer
  :: ( MonadIO editor, MonadState st editor, HasEditorState st
     , EditorMVectorType st ~ mvec
     , EditorMVectorElem st ~ elem
     , MVector mvec elem
     )
  => (elem -> editor Bool) -- ^ Return 'True' an element is non-null, 'False' if null
  -> (Strict.Text -> elem -> editor ()) -- ^ display an element with a string prefix
  -> editor () -- ^ display an elipsis
  -> editor ()
printBuffer a b c = do
  buf <- use currentBuffer
  range <- use currentRange
  printOverIOBufferRange a b c buf range

----------------------------------------------------------------------------------------------------

-- | Search for the first element within the given range of the given buffer that matches the given
-- predicate. This function can also perform a fold, which takes the result of the predicate, the
-- index, and the element as values.
searchOverIOBufferRange
  :: (MVector mvec e, MonadIO editor)
  => (e -> editor Bool)
  -> (fold -> Bool -> VectorIndex -> e -> editor fold)
  -> fold
  -> mvec (PrimState IO) e
  -> VectorRange
  -> editor (Maybe VectorIndex, fold)
searchOverIOBufferRange testElem f fold buf range =
  runContT
  ( callCC $ \ halt ->
    foldOverIOBuffer
    (\ (_, fold) i elem -> do
      wantThis <- lift $ testElem elem
      fold <- lift $ f fold wantThis i elem
      if wantThis then halt (Just i, fold) else return (Nothing, fold)
    )
    (Nothing, fold)
    buf
    range
  )
  return

-- | Like 'searchOverIOBufferRange' but over the whole buffer, not just a 'Range'.
searchOverIOBuffer
  :: (MVector mvec e, MonadIO editor)
  => (e -> editor Bool)
  -> (fold -> Bool -> VectorIndex -> e -> editor fold)
  -> fold
  -> mvec (PrimState IO) e
  -> editor (Maybe VectorIndex, fold)
searchOverIOBuffer wantThis f fold buf =
  searchOverIOBufferRange wantThis f fold buf (ioBufferRange buf)

-- | Like 'searchOverIOBufferRange' but over the 'currentBuffer'.
searchBufferRange
  :: ( MonadIO editor, MonadState st editor, HasEditorState st
     , EditorMVectorType st ~ mvec
     , EditorMVectorElem st ~ e
     , MVector mvec e
     )
  => (e -> editor Bool)
  -> (fold -> Bool -> VectorIndex -> e -> editor fold)
  -> fold
  -> VectorRange
  -> editor (Maybe VectorIndex, fold)
searchBufferRange testElem f fold range = do
  buf <- use currentBuffer
  searchOverIOBufferRange testElem f fold buf range

-- | Like 'searchBufferRange', but over the entire 'currentBuffer', not just a 'Range'.
searchBuffer
  :: ( MonadIO editor, MonadState st editor, HasEditorState st
     , EditorMVectorType st ~ mvec
     , EditorMVectorElem st ~ e
     , MVector mvec e
     )
  => (e -> editor Bool)
  -> (fold -> Bool -> VectorIndex -> e -> editor fold)
  -> fold
  -> editor (Maybe VectorIndex, fold)
searchBuffer testElem f fold =
  use currentBuffer >>=
  searchOverIOBuffer testElem f fold

----------------------------------------------------------------------------------------------------

-- | Used by the 'updateOverIOBufferRange' and related functions.
data Update e = ItemKeep | ItemRemove | ItemUpdate e
  deriving (Eq, Show)

-- | Updates items in place in the buffer, possibly deleting them. Cells in buffer that are deleted
-- are overwriten with items above them in the buffer, bubbling down upper elements to overwrite
-- lower elements not selected the predicate. Because this function touches every element in the
-- range, a fold is also performed. The fold receives the current index, the 'Bool' result of the
-- predicate, and the element.
--
-- Along with the fold result, this function returns a 'Range' of elements that were copied to a
-- lower position in the vector but not overwritten and so are duplicate elements. You can ignore
-- these elements, or you can choose to use a function such as 'fillOverIOBuffer' or 'fillWith' to
-- write this range with some null value of @e@.
updateOverIOBufferRange
  :: (MVector mvec e, MonadIO editor)
  => (Int -> e -> fold -> editor (Update e, fold))
  -> fold
  -> mvec (PrimState IO) e
  -> VectorRange
  -> editor (VectorRange, fold)
updateOverIOBufferRange testElem fold buf range =
  let len = (range ^. rangeLength) in
  let end = (range ^. rangeStart) + len in
  foldOverIOBuffer
  (\ (floor, fold) i elem -> do
     (shouldKeep, fold) <- testElem i elem fold
     case shouldKeep of
       ItemRemove      -> return (floor, fold)
       ItemKeep        -> do
         when (floor < i) $
           liftIO $ GMVec.write buf floor elem
         pure (floor + 1, fold)
       ItemUpdate elem -> do
         liftIO (GMVec.write buf floor elem)
         pure (floor + 1, fold)
  )
  (range ^. rangeStart, fold)
  buf
  range >>= \ (last, fold) ->
  return
  ( range & -- TODO: handle negative range lengths
    (rangeStart .~ last) &
    (rangeLength .~ max 0 (end - last))
  , fold
  )

-- | Similar to the 'updateIOBufferRange' function, but sweeps the entire buffer.
updateOverIOBuffer
  :: (MVector mvec e, MonadIO editor)
  => (Int -> e -> fold -> editor (Update e, fold))
  -> fold
  -> mvec (PrimState IO) e
  -> editor (VectorRange, fold)
updateOverIOBuffer testElem fold buf =
  updateOverIOBufferRange testElem fold buf (ioBufferRange buf)

-- | Like 'updateOverIOBufferInRange', but operates on the 'currentBuffer' in an 'Editor' function
-- context.
updateBufferRange
  :: ( MonadIO editor, MonadState st editor, HasEditorState st
     , EditorMVectorType st ~ mvec
     , EditorMVectorElem st ~ e
     , MVector mvec e
     )
  => (Int -> e -> fold -> editor (Update e, fold))
  -> fold
  -> VectorRange
  -> editor (VectorRange, fold)
updateBufferRange testElem fold range = do
  buf <- use currentBuffer
  updateOverIOBufferRange testElem fold buf range

-- | Like 'updateBufferRange', but operates on the entire 'currentBuffer', not just a 'Range'.
updateBuffer
  :: ( MonadIO editor, MonadState st editor, HasEditorState st
     , EditorMVectorType st ~ mvec
     , EditorMVectorElem st ~ e
     , MVector mvec e
     )
  => (Int -> e -> fold -> editor (Update e, fold))
  -> fold
  -> editor (VectorRange, fold)
updateBuffer testElem fold =
  use currentBuffer >>=
  updateOverIOBuffer testElem fold

----------------------------------------------------------------------------------------------------

-- | Blit the given 'Buffer' (called the "source buffer") limited to the given 'Range' to the
-- 'currentBuffer' at the current 'cursor' position, applying a mapping function to each element
-- before it is blitted which overwrites the existing current element without reading it. As the
-- iteration procedes, 'stepWrap' is used so the blit will wrap around to the start of the
-- 'currentBuffer' if the given 'Buffer' at the given 'cursor' position will blit out of bounds. The
-- 'cursor' position will not be restored after this operation is completed, so it might be a good
-- idea to use @'withSubCursor' 'id'@.
blitBuffer
  :: ( MonadIO editor, MonadState st editor, HasEditorState st
     , EditorMVectorType st ~ mvec
     , EditorMVectorElem st ~ n
     , MVector mvec n, Vector vec e
     )
  => (e -> editor n) -> vec e -> VectorRange -> editor ()
blitBuffer map buf =
  foldOverRange_ $
  map . (buf GVec.!) >=> putCurrentElem >=> const stepWrap

-- | Like 'blitBuffer' but uses an 'IOBuffer' as the source buffer.
blitIOBuffer
  :: ( MonadIO editor, MonadState st editor, HasEditorState st
     , EditorMVectorType st ~ vtarg
     , EditorMVectorElem st ~ n
     , MVector vsrc e, MVector vtarg n
     )
  => (e -> editor n) -> vsrc (PrimState IO) e -> VectorRange -> editor ()
blitIOBuffer map buf =
  foldOverRange_ $
  liftIO . GMVec.read buf >=> map >=> putCurrentElem >=> const stepWrap

-- | Like 'blitBuffer' but allows you to perform a binary operator on the source and target value
-- and the result of the binary operator computation is the value that is blitted.
zipBlitBuffer
  :: ( MonadIO editor, MonadState st editor, HasEditorState st
     , EditorMVectorType st ~ mvec
     , EditorMVectorElem st ~ n
     , MVector mvec n, Vector vec e
     )
  => (e -> n -> editor n) -> vec e -> VectorRange -> editor ()
zipBlitBuffer op buf =
  foldOverRange_ $ \ i ->
  op <$> pure (buf GVec.! i) <*> getCurrentElem >>= id >>=
  putCurrentElem >>
  stepWrap

-- | Like 'zipBlitBuffer' but uses an 'IOBuffer' as the source buffer.
zipBlitIOBuffer
  :: ( MonadIO editor, MonadState st editor, HasEditorState st
     , EditorMVectorType st ~ vtarg
     , EditorMVectorElem st ~ n
     , MVector vsrc e, MVector vtarg n
     )
  => (e -> n -> editor n) -> vsrc (PrimState IO) e -> VectorRange -> editor ()
zipBlitIOBuffer op buf =
  foldOverRange_ $ \ i ->
  op <$> liftIO (GMVec.read buf i) <*> getCurrentElem >>= id >>=
  putCurrentElem >>
  stepWrap

----------------------------------------------------------------------------------------------------

-- | Compute the minimum and maximum bounds for all elements in a given 'Buffer'.
bufferElemBounds
  :: (Ord e, Bounded e, Vector vec e, Monad editor)
  => vec e -> VectorRange -> editor (e, e)
bufferElemBounds =
  foldOverBuffer
  (\ (lo, hi) _ e -> return (min lo e, max hi e))
  (minBound, maxBound)

-- | Compute the minimum and maximum bounds for all elements in a given 'IOBuffer'.
ioBufferElemBounds
  :: (Ord e, MVector mvec e, MonadIO editor)
  => (e, e) -> mvec (PrimState IO) e -> VectorRange -> editor (e, e)
ioBufferElemBounds = foldOverIOBuffer (\ (lo, hi) _ e -> return (min lo e, max hi e))

-- | Compute the minimum and maximum bounds for all elements in the 'currentRange' of the
-- 'currentBuffer'.
currentRangeElemBounds
  :: ( Ord n, Bounded n
     , MonadIO editor, MonadState st editor, HasEditorState st
     , EditorMVectorType st ~ mvec
     , EditorMVectorElem st ~ n
     , MVector mvec n
     )
  => (n, n) -> editor (n, n)
currentRangeElemBounds init =
  ioBufferElemBounds init <$> use currentBuffer <*> use currentRange >>= id

-- | Find the minimum and maximum bounds of the elements in the 'currentBuffer', then scale all
-- elements to these bounds such that all elements are between 0.0 and 1.1.
normalizeIOBuffer
  :: ( Ord e, IEEE e
     , MonadIO editor, MonadState st editor, HasEditorState st
     , EditorMVectorType st ~ v
     , EditorMVectorElem st ~ n
     , MVector mvec e, MVector v n
     )
  => mvec (PrimState IO) e -> VectorRange -> editor ()
normalizeIOBuffer buf range = do
  (lo, hi) <- ioBufferElemBounds (infinity, negate infinity) buf range
  let scale = recip (hi - lo)
  mapOverIOBuffer (\ _ -> return . (* scale) . subtract lo) buf range

normalizeCurrentRange
  :: ( Ord n, IEEE n
     , MonadIO editor, MonadState st editor, HasEditorState st
     , EditorMVectorType st ~ mvec
     , EditorMVectorElem st ~ n
     , MVector mvec n
     )
  => editor ()
normalizeCurrentRange = normalizeIOBuffer <$> use currentBuffer <*> use currentRange >>= id

-- | Perform a convolution on the current buffer with a given kernel 'Buffer'. The 'cursor' is moved
-- to the beginning of the 'currentRange'. Then a copy of 'currentRange' is created using
-- 'freezeIORange' (called the "source" buffer). Then a sliding window the size of the kernel
-- 'Buffer' is moved along each element of the then source buffer and the dot product of the kernel
-- and the elements the window of the source buffer is taken, and the result of this dot product is
-- written to the 'currentBuffer' using 'putCurrentElem'. The as kernel moves past the end of the
-- source buffer, elements from the beginning of the source buffer are used in the dot product
-- computation.
convolution
  :: ( Num n, IEEE n, Vector vec n, Show n
     , MonadIO editor, MonadState st editor, HasEditorState st
     , EditorMVectorType st ~ mvec, mvec ~ GVec.Mutable vec
     , EditorMVectorElem st ~ n
     , MVector mvec n, Vector vec n
     )
  => vec n -> editor ()
convolution kernel = do
  source <- freezeCurrentRange
  let klen = GVec.length kernel
  let slen = GVec.length source
  let dot k s sum =
        if k < klen then
          ((dot $! k + 1) $! s + 1) $!
          sum + (kernel GVec.! k) * (source GVec.! mod s slen)
        else
          sum
  (lo, hi) <- foldFillBuffer
    (\ i (lo, hi) -> let e = dot 0 i 0 in pure (e, (min e lo, max e hi)))
    (infinity, negate infinity)
  let span = hi - lo
  mapBuffer (const $ pure . (/ span) . subtract lo)
