-- | Simple data types that can be shared across all modules, as well as error data types.
module VecEdit.Types
  ( -- ** For Vectors
    VectorIndex, VectorSize,
    Range(..), rangeStart, rangeLength, rangeEnd, reverseRange, canonicalRange,
    -- ** For Gap Buffers
    RelativeIndex, RelativeDirection(..), HasOpposite(..), numToRelativeDirection,
    GaplessIndex(..), GapBufferError(..), GapBufferErrorInfo(..), ppGapBufferErrorInfo,
    -- ** For Text and Strings
    CharBufferSize, LineBufferSize, LineIndex(..), CharIndex(..), ToIndex(..), FromIndex(..),
    TextPoint(..), textPointRow, textPointColumn,
    TextRange(..), textRangeStart, textRangeEnd, textRangeIsForward,
    EditTextError(..),
  ) where

import Control.Arrow ((>>>))
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
data Range = Range { theRangeStart :: !VectorIndex, theRangeLength :: !VectorSize }
  deriving (Eq, Ord, Show)

-- | The first element of an iteration. The iteration alwas begins here regardless of whether the
-- 'rangeLength' is negative or positive.
rangeStart :: Lens' Range VectorIndex
rangeStart = lens theRangeStart $ \ a b -> a{ theRangeStart = b }

-- | Compute the top-most index of the 'Range', or bottom-most if 'rangeLength' is negative.
rangeEnd :: Range -> VectorIndex
rangeEnd Range{theRangeStart=i,theRangeLength=len} =
  if len < 0 then i - len + 1 else i + len - 1

-- | This may be negative, all APIs in this module should be able to handle a negative length.
rangeLength :: Lens' Range VectorSize
rangeLength = lens theRangeLength $ \ a b -> a{ theRangeLength = b }

-- | Reverses the 'Range' by setting 'rangeStart' equal to @'rangeStart' + 'rangeLength'@ and
-- 'negate'-ing 'rangeLength'.
reverseRange :: Range -> Range
reverseRange r@(Range lo len) = case compare (theRangeLength r) 0 of
  GT -> Range (lo + len - 1) (negate len)
  LT -> Range (lo + len + 1) (negate len)
  EQ -> r

-- | If the 'rangeLength' is negative, evaluate 'rangeReverse'
canonicalRange :: Range -> Range
canonicalRange r@(Range _ len) = if len < 0 then reverseRange r else r

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

numToRelativeDirection :: (Num n, Ord n) => n -> Maybe RelativeDirection
numToRelativeDirection = compare 0 >>> \ case
  EQ -> Nothing
  LT -> Just After
  GT -> Just Before

----------------------------------------------------------------------------------------------------

-- | The kind of soft exception that can be thrown by a 'GapBuffer' function.
data GapBufferError
  = StepCursor    !RelativeDirection
  | AtCursor      !RelativeDirection
  | PullItem      !RelativeDirection
  | PopItem       !RelativeDirection
  | ShiftCursor   !RelativeIndex
  | AtIndex       !GaplessIndex
  | GapBufferFail !Strict.Text
  deriving (Eq, Show)

data GapBufferErrorInfo
  = GapBufferErrorInfo
    { theErrorCursorBefore :: !VectorIndex
    , theErrorCursorAfter  :: !VectorIndex
    , theErrorBufferAlloc  :: !VectorSize
    , theErrorBufferRange  :: !Range
    , theErrorFunction     :: !GapBufferError
    }
  deriving (Eq, Show)

ppGapBufferErrorInfo :: GapBufferErrorInfo -> Strict.Text
ppGapBufferErrorInfo info = Strict.pack $ unlines $
  let showInt f = show (f info) in
  [ show (theErrorFunction info)
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

----------------------------------------------------------------------------------------------------

-- | A 'LineIndex' and 'CharIndex' to indicate the location of a character in a 'TextBufferState'.
data TextPoint
  = TextPoint
  { theTextPointRow :: !LineIndex
  , theTextPointColumn :: !CharIndex
  }
  deriving (Eq, Ord)

instance Bounded TextPoint where
  minBound = TextPoint minBound minBound
  maxBound = TextPoint maxBound maxBound

textPointRow :: Lens' TextPoint LineIndex
textPointRow = lens theTextPointRow $ \ a b -> a{ theTextPointRow = b }

textPointColumn :: Lens' TextPoint CharIndex
textPointColumn = lens theTextPointColumn $ \ a b -> a{ theTextPointColumn = b }

----------------------------------------------------------------------------------------------------

-- | A data type to delimit text between a starting and ending @index@. The @index@ is polymorphic
-- because you may specify an index in a few different ways.
--   1. by 'LineIndex'
--   2. by 'CharIndex'
--   3. by 'TextPoint' ('LineIndex' and 'CharIndex')
data TextRange index = TextRange{ theTextRangeStart :: !index, theTextRangeEnd :: !index }
  deriving (Eq, Ord)

instance Show index => Show (TextRange index) where
  show (TextRange{theTextRangeStart=start,theTextRangeEnd=end}) = show start <> ".." <> show end

textRangeStart :: Lens' (TextRange index) index
textRangeStart = lens theTextRangeStart $ \ a b -> a{ theTextRangeStart = b }

textRangeEnd :: Lens' (TextRange index) index
textRangeEnd = lens theTextRangeEnd $ \ a b -> a{ theTextRangeEnd = b }

-- | Indicates whether 'theTextRangeStart' is greater than or equal to 'theTextRangeEnd', which
-- means the range selects lines or characters in an increasing direction. If this function
-- evaluates to 'False' it indicates that the 'TextRange' selects lines or characters in a
-- decreasing direction.
textRangeIsForward :: Ord n => TextRange n -> Bool
textRangeIsForward r = (r ^. textRangeStart) <= (r ^. textRangeEnd)
