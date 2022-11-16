-- | Simple data types that can be shared across all modules, as well as error data types.
module VecEdit.Types
  ( -- ** For Vectors
    VectorIndex, VectorSize,
    Range(..), VectorRange, rangeStart, rangeLength, rangeEnd, reverseRange, canonicalizeRange,
    Boundary(..), LineBounds, CharBounds, TextBounds,
    boundaryStart, boundaryEnd, isCanonicalBoundary,
    boundaryToRange, rangeToBoundary,
    -- ** For Gap Buffers
    RelativeIndex, RelativeDirection(..), HasOpposite(..), relativeIndex,
    GaplessIndex(..), TextPrimOpError(..), GapBufferErrorInfo(..), ppGapBufferErrorInfo,
    -- ** For Text and Strings
    IndexValue(..),
    CharBufferSize, LineBufferSize, LineIndex(..), CharIndex(..), ToIndex(..), FromIndex(..),
    TextPoint(..), textPointRow, textPointColumn,
    Selection(..), SelectLines, SelectText,
    EditTextError(..),
  ) where

import VecEdit.Print.DisplayInfo (DisplayInfo(..), displayInfoShow)

import Control.Lens (Lens', lens, (^.))
import qualified Data.Text as Strict

----------------------------------------------------------------------------------------------------

-- | This type indicates an index position in a vector. One of the properties of a 'VectorIndex'
-- that cannot be enforced by the types of the APIs in the module is that the integer value must be
-- greater or equal to zero, and must be strictly less than the length of the buffer's vector.
type VectorIndex = Int

-- | This type indicates a vector length or a number of elements in a vector.
type VectorSize = Int

-- | The 'Range' data type defines a limited window over elements in an 'IOBuffer'. There is also a
-- 'currentBuffer' in the 'Editor' monad which can be used to limit the range of 'foldBuffer' or
-- 'mapBuffer' operations. All APIs in this module treat a negative 'rangeLength' value as a valid
-- 'Range' that will cause folds and map operations to iterate in the reverse direction. A
-- 'rangeLength' of zero will perform no iteration at all.
data Range index = Range { theRangeStart :: !index, theRangeLength :: !RelativeIndex }
  deriving (Eq, Ord, Show)

type VectorRange = Range VectorIndex

-- | The first element of an iteration. The iteration alwas begins here regardless of whether the
-- 'rangeLength' is negative or positive.
rangeStart :: Lens' (Range index) index
rangeStart = lens theRangeStart $ \ a b -> a{ theRangeStart = b }

-- | Compute the top-most index of the 'Range', or bottom-most if 'rangeLength' is negative.
rangeEnd :: (Num index, IndexValue index) => Range index -> index
rangeEnd Range{theRangeStart=i,theRangeLength=len} =
  wrapIndex $
  if len < 0 then unwrapIndex i - len + 1 else unwrapIndex i + len - 1

-- | This may be negative, all APIs in this module should be able to handle a negative length.
rangeLength :: Lens' (Range index) RelativeIndex
rangeLength = lens theRangeLength $ \ a b -> a{ theRangeLength = b }

-- | Reverses the 'Range' by setting 'rangeStart' equal to @'rangeStart' + 'rangeLength'@ and
-- 'negate'-ing 'rangeLength'.
reverseRange :: (Num index, IndexValue index) => Range index -> Range index
reverseRange r@(Range lo len) = case compare (theRangeLength r) 0 of
  GT -> Range (wrapIndex $ unwrapIndex lo + len - 1) (negate len)
  LT -> Range (wrapIndex $ unwrapIndex lo + len + 1) (negate len)
  EQ -> r

-- | If the 'rangeLength' is negative, evaluate 'rangeReverse'
canonicalizeRange :: (Num index, IndexValue index) => Range index -> Range index
canonicalizeRange r@(Range _ len) = if len < 0 then reverseRange r else r

----------------------------------------------------------------------------------------------------

-- | This data type is used to indicate an index into a 'GapBuffer' that selects an element as if
-- there were no gap.
--
-- For example, suppose there is a GapBufferState has space for 20 elements and there are 10
-- elements in the gap buffer, with 5 elements before the cursor and 5 elements after. If you index
-- this buffer with an 'Int' value of 8, you will select an undefined element, because 8 falls
-- within the gap between the lower and upper cursor. But if index this buffer with a 'GaplessIndex'
-- value of 8, you will select the 3rd element after the upper cursor. If you iterate 'GaplessIndex'
-- values from 0 to 9 and index an element with each value, you will retrieve all define array
-- elements.
newtype GaplessIndex = GaplessIndex{ unwrapGaplessIndex :: Int }
  deriving newtype (Eq, Ord, Enum, Num, Real, Integral, Bounded, Show, Read)

-- | This type indicates an index relative to the 'currentCursor'.
type RelativeIndex = Int

-- | A value that indicates a general direction relative to the 'currentCursor' value.
data RelativeDirection = Before | After
  deriving (Eq, Ord, Show, Enum, Bounded)

class HasOpposite a where { opposite :: a -> a; }
instance HasOpposite Bool where { opposite = not; }
instance HasOpposite RelativeDirection where
  opposite = \ case { Before -> After; After -> Before; }

-- | Construct a 'RelativeIndex' from a 'RelativeDirection' and a 'VectorIndex'.
relativeIndex :: RelativeDirection -> VectorIndex -> RelativeIndex
relativeIndex = \ case
  Before -> negate . abs
  After -> abs

----------------------------------------------------------------------------------------------------

-- | The kind of soft exception that can be thrown by a 'GapBuffer' function.
data TextPrimOpError
  = StepCursor    !RelativeDirection
  | AtCursor      !RelativeDirection
  | PullItem      !RelativeDirection
  | PopItem       !RelativeDirection
  | ShiftCursor   !RelativeIndex !RelativeIndex
    -- ^ If the shift is out of bounds, shows the requested shift size and the maximum allowable
    -- shift size.
  | AtIndex       !GaplessIndex
  | InsertVector  !RelativeDirection !VectorSize
    -- ^ The 'VectorSize' is the size of the vector that was requested but failed to be
    -- inserted. This is thrown when the gap size is not big enough to fit the requsted vector size.
  | BadCursor     !RelativeDirection !RelativeIndex
    -- ^ Thrown when one of the 'GapBuffer' 'relativeCursor' values is out of bounds.
  | GapBufferFail !Strict.Text
  deriving (Eq, Show)

data GapBufferErrorInfo
  = GapBufferErrorInfo
    { theErrorCursorBefore :: !VectorIndex
    , theErrorCursorAfter  :: !VectorIndex
    , theErrorBufferAlloc  :: !VectorSize
    , theErrorBufferRange  :: !VectorRange
    , theErrorOperation    :: !TextPrimOpError
    }
  deriving (Eq, Show)

instance DisplayInfo TextPrimOpError where
  displayInfo = displayInfoShow

ppGapBufferErrorInfo :: GapBufferErrorInfo -> Strict.Text
ppGapBufferErrorInfo info = Strict.pack $ unlines $
  let showInt f = show (f info) in
  [ show (theErrorOperation info)
  , "  beforeCursor: " <> showInt theErrorCursorBefore
  , "  afterCursor: " <> showInt theErrorCursorAfter
  , "  length currentBuffer: " <> showInt theErrorBufferAlloc
  , "  currentRange: " <> show (theErrorBufferRange info)
  ]

-- | This is the type of information that can be used to raise soft exceptions using 'throwError'
-- when within an 'EditText' function context.
data EditTextError
  = TextEditUndefined
    -- ^ Constructed by the 'empty' instance of the 'Alternative' typeclass.
  | EditTextFailed Strict.Text
    -- ^ Constructed by the 'fail' instance of the 'MonadFail' typeclass
  | EditorPrimOpError TextPrimOpError
  | EditLineError GapBufferErrorInfo
    -- ^ This is the error re-thrown when a 'GapBuffer' soft exception occurs within an 'EditLine'
    -- function evaluation.
  | EditTextError GapBufferErrorInfo
    -- ^ This is the error re-thrown when a 'GapBuffer' soft exception occurs within any 'EditText'
    -- function that manipulates the 'GapBuffer' that contains the lines of text.
  | EditLineIndexError
    { editLineInvalidIndex :: !LineIndex
    , editLineMinBound :: !LineIndex
    , editLineMaxBound :: !LineIndex
    } -- ^ Error occurs when an 'EditText' function selects a 'LineIndex' that is out of bounds.
  | EditCharIndexError
    { editCharOnLine :: !LineIndex
    , editCharInvalidIndex :: !CharIndex
    , editCharMinBound :: !CharIndex
    , editCharMaxBound :: !CharIndex
    } -- ^ Error occurs when an 'EditText' function selects a 'CharIndex' that is out of bounds.
  deriving (Eq, Show)

instance DisplayInfo EditTextError where
  displayInfo putStr = \ case
    TextEditUndefined -> putStr "text editor, undefined error"
    EditTextFailed msg -> putStr msg
    EditorPrimOpError err -> displayInfo putStr err
    EditLineError info -> do
      putStr "edit line error, "
      putStr $ ppGapBufferErrorInfo info
    EditTextError info -> do
      putStr "edit text error, "
      putStr $ ppGapBufferErrorInfo info
    EditLineIndexError
      { editLineInvalidIndex=i
      , editLineMinBound=minb
      , editLineMaxBound=maxb
      } -> do
        putStr "edit error, requested line " >> displayInfoShow putStr i
        putStr " out of bounds {min=" >> displayInfoShow putStr minb
        putStr ", max=" >> displayInfoShow putStr maxb >> putStr "}"
    EditCharIndexError
      { editCharOnLine=line
      , editCharInvalidIndex=i
      , editCharMinBound=minb
      , editCharMaxBound=maxb
      } -> do
        putStr "edit error, on line " >> displayInfoShow putStr line
        putStr "column " >> displayInfoShow putStr i
        putStr " out of bounds {min=" >> displayInfoShow putStr minb
        putStr ", max=" >> displayInfoShow putStr maxb >> putStr "}"

----------------------------------------------------------------------------------------------------

-- | Pre-allocate a character buffer when loading byte streams into an 'EditTextState'. For example,
-- if you happen to know the file you are loading never has lines longer than 100 characters in
-- width, pass @'Just' 128@ as the 'CharBufferSize' argument.
type CharBufferSize = Maybe VectorSize

-- | Pre-allocate a line buffer when loading byte streams into an 'EditTextState'. For example if
-- you happen to know that the file you are loading has exactly 1000 lines of text, pass @'Just'
-- 1000@ as the 'LineBufferSize' argument.
type LineBufferSize = Maybe VectorSize

-- | This is a 1-based index for lines in a text editor 'EditTextState'.
newtype LineIndex = LineIndex GaplessIndex
  deriving newtype (Eq, Ord, Bounded, Enum, Num, Real, Integral, Show, Read)

-- | This is a 1-based index for indexing characters in a line editor
newtype CharIndex = CharIndex GaplessIndex
  deriving newtype (Eq, Ord, Bounded, Enum, Num, Real, Integral, Show, Read)

-- | Convert 'LineIndex' or 'CharIndex' valiues to 'Int' values. This involves subtracting 1 from
-- the 'LineIndex' or 'CharIndex' value in order to ensure that @'LineIndex' 1@ actually points to
-- index 0 of a 'Vector'.
class ToIndex   i where { toIndex   :: GaplessIndex -> i; }

-- | The inverse operation of 'ToIndex', which involves adding 1 to an the given integer value to
-- ensure zero-indexed 'Vector's produce 1-indexed 'LineIndex' or 'CharIndex' values.
class FromIndex i where { fromIndex :: i -> GaplessIndex; }

instance ToIndex   LineIndex where { toIndex   = LineIndex . (+ 1); }
instance ToIndex   CharIndex where { toIndex   = CharIndex . (+ 1); }

instance FromIndex LineIndex where { fromIndex (LineIndex i) = i - 1; }
instance FromIndex CharIndex where { fromIndex (CharIndex i) = i - 1; }

-- | Unwrap or wrap a 'LineIndex', 'CharIndex', or 'GaplessIndex', to the actual 'Int' value stored
-- __without any conversion__. This should only be used for serialization, deserialization, or error
-- messages.
class IndexValue i where { unwrapIndex :: i -> Int; wrapIndex :: Int -> i; } 
instance IndexValue Int where
  unwrapIndex = id
  wrapIndex = id
instance IndexValue GaplessIndex where
  unwrapIndex (GaplessIndex i) = i
  wrapIndex = GaplessIndex
instance IndexValue LineIndex where
  unwrapIndex (LineIndex i) = unwrapIndex i
  wrapIndex = LineIndex . wrapIndex
instance IndexValue CharIndex where
  unwrapIndex (CharIndex i) = unwrapIndex i
  wrapIndex = CharIndex . wrapIndex

----------------------------------------------------------------------------------------------------

-- | A 'LineIndex' and 'CharIndex' to indicate the location of a character in a 'TextBufferState'.
data TextPoint
  = TextPoint
  { theTextPointRow :: !LineIndex
  , theTextPointColumn :: !CharIndex
  }
  deriving (Eq, Ord, Show)

instance Bounded TextPoint where
  minBound = TextPoint minBound minBound
  maxBound = TextPoint maxBound maxBound

textPointRow :: Lens' TextPoint LineIndex
textPointRow = lens theTextPointRow $ \ a b -> a{ theTextPointRow = b }

textPointColumn :: Lens' TextPoint CharIndex
textPointColumn = lens theTextPointColumn $ \ a b -> a{ theTextPointColumn = b }

----------------------------------------------------------------------------------------------------

-- | Like 'Range', but delimited by the __closed set__ (so including the boundary points) of
-- elements between two specified boundary points.
data Boundary point
  = Boundary
    { theBoundaryStart :: !point
    , theBoundaryEnd :: !point
    }
  deriving (Eq, Ord, Show)

-- | A range of lines in a text buffer
type LineBounds = Boundary LineIndex

-- | A range of characters on a single line of text in a text buffer.
type CharBounds = Boundary CharIndex

-- | A range of characters delimited by a a pair of 'LineRange' and 'CharRange' tuples.
type TextBounds = Boundary TextPoint

boundaryStart :: Lens' (Boundary point) point
boundaryStart = lens theBoundaryStart $ \ a b -> a{ theBoundaryStart = b }

boundaryEnd :: Lens' (Boundary point) point
boundaryEnd = lens theBoundaryEnd $ \ a b -> a{ theBoundaryEnd = b }

-- | Indicates whether 'Range' is greater than or equal to 'theTextRangeEnd', which
-- means the range selects lines or characters in an increasing direction. If this function
-- evaluates to 'False' it indicates that the 'Boundary' selects lines or characters in a
-- decreasing direction.
isCanonicalBoundary :: (Ord index, Num index) => Boundary index -> Bool
isCanonicalBoundary r = (r ^. boundaryStart) <= (r ^. boundaryEnd)

boundaryToRange :: IndexValue index => Boundary index -> Range index
boundaryToRange b =
  Range
  { theRangeStart  = theBoundaryStart b
  , theRangeLength =
      unwrapIndex (theBoundaryEnd b) -
      unwrapIndex (theBoundaryStart b) + 1
  }

-- | Might return 'Nothing' if 'theRangeLength' is zero.
rangeToBoundary :: IndexValue index => Range index -> Maybe (Boundary index)
rangeToBoundary r =
  let len = theRangeLength r in
  if len == 0 then Nothing else Just $
  Boundary
  { theBoundaryStart = theRangeStart r
  , theBoundaryEnd =
      wrapIndex $
      unwrapIndex (theRangeStart r) + len
  }

----------------------------------------------------------------------------------------------------

-- | A data type used to select lines in the buffer, used by 'foldLines' and 'mapLines'.
data Selection index
  = All
    -- ^ Selects all lines in the buffer.
  | ToFirst
    -- ^ Selects all lines from the cursor to the first line of the buffer.
  | ToLast
    -- ^ Selects all lines from the cursor to the final line of the buffer.
  | Between !index !index
    -- ^ Selects all lines between (and including) the given indicies.
  | BoundedBy (Boundary index)
  | InRange !(Range index)
    -- ^ Provide a range value selecting the lines.
  | FromHere !RelativeIndex
    -- ^ Select the given number of lines from the cursor. The current line counts as one line.
  deriving (Eq, Ord, Show)

type SelectLines = Selection LineIndex
type SelectText = Selection TextPoint
