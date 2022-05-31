module VecEdit.Print.DisplayInfo
  ( DisplayInfo(..), LinePrinter, showAsText, displayInfoShow,
  ) where

import qualified Data.Text as Strict
import qualified Data.Text.Lazy as Lazy

----------------------------------------------------------------------------------------------------

-- | A type of function that dumps 'Strict.Text' into a stream, file handle, or buffer. This
-- function is expected to print exactly the characters given and nothing else (no terminating line
-- break).
type LinePrinter = Strict.Text -> IO ()

-- | Class for showing lists of things to a display, such as a file handle, or a mutable text
-- buffer.
class DisplayInfo e where { displayInfo :: LinePrinter -> e -> IO () }

instance DisplayInfo Strict.Text where { displayInfo = displayInfoShow; }
instance DisplayInfo Lazy.Text where { displayInfo = displayInfoShow; }
instance DisplayInfo String where { displayInfo = displayInfoShow; }

-- | Use the instance for 'Show' to instantiate the 'displayInfo' function.
displayInfoShow :: Show a => LinePrinter -> a -> IO ()
displayInfoShow p = p . Strict.pack . show

-- | Like the 'show' function, but constructs a lazy 'Lazy.Text' value instead of a 'String'. The
-- reason for building lazy 'Lazy.Text' values is usually because when passing these values to a
-- 'LinePrinter' you must first perform a conversion to a strict 'Strict.Text' value, which (as I
-- understand) allows the optimizing compiler to decide how best to allocate memory for chunks of
-- lazy 'Lazy.Text' when buffering to a strict 'Strict.Text'.
showAsText :: Show a => a -> Lazy.Text
showAsText = Lazy.pack . show
