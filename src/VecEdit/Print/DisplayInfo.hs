module VecEdit.Print.DisplayInfo
  ( DisplayInfo(..), LinePrinter, showAsText, showAsLazyText,
    displayInfoShow, displayInfoPrint,
    ralign6,
  ) where

import Control.Arrow ((>>>))
import qualified Data.Text as Strict
import qualified Data.Text.IO as Strict
import qualified Data.Text.Lazy as Lazy

----------------------------------------------------------------------------------------------------

-- | A type of function that dumps 'Strict.Text' into a stream, file handle, or buffer. This
-- function is expected to print exactly the characters given and nothing else (no terminating line
-- break).
type LinePrinter = Strict.Text -> IO ()

-- | Class for showing lists of things to a display, such as a file handle, or a mutable text
-- buffer.
class DisplayInfo e where { displayInfo :: LinePrinter -> e -> IO () }

instance DisplayInfo () where { displayInfo _ () = pure (); }
instance DisplayInfo Strict.Text where { displayInfo = displayInfoShow; }
instance DisplayInfo Lazy.Text where { displayInfo putStr = mapM_ putStr . Lazy.toChunks; }
instance DisplayInfo String where { displayInfo = displayInfoShow; }

instance DisplayInfo ok => DisplayInfo (Maybe ok) where
  displayInfo putStr = \ case
    Nothing -> putStr "\nNONE\n"
    Just ok -> displayInfo putStr ok >> putStr "\nOK\n"

instance (DisplayInfo err, DisplayInfo ok) => DisplayInfo (Either err ok) where
  displayInfo putStr = \ case
    Left err -> displayInfo putStr err >> putStr "\nERROR\n"
    Right ok -> displayInfo putStr ok  >> putStr "\nOK\n"

-- | Use the instance for 'Show' to instantiate the 'displayInfo' function.
displayInfoShow :: Show a => LinePrinter -> a -> IO ()
displayInfoShow p = p . Strict.pack . show

-- | Calls 'displayInfo' with the 'Strict.putStr' function as the 'LinePrinter'.
--
-- __NOTE:__ when using GHCi, it can be useful to automatically print
-- instances of the 'DisplayInfo' class using this function. Simply
-- include this parameter when invoking GHCi, or use the `:set` directive in the REPL:
--
-- @
-- ghci -interactive-print VecEdit.Print.DisplayInfo.displayInfoShow ...
-- @
displayInfoPrint :: DisplayInfo a => a -> IO ()
displayInfoPrint = displayInfo Strict.putStr

-- | Like the 'show' function, but constructs a lazy 'Lazy.Text' value instead of a 'String'. The
-- reason for building lazy 'Lazy.Text' values is usually because when passing these values to a
-- 'LinePrinter' you must first perform a conversion to a strict 'Strict.Text' value, which (as I
-- understand) allows the optimizing compiler to decide how best to allocate memory for chunks of
-- lazy 'Lazy.Text' when buffering to a strict 'Strict.Text'.
showAsText :: Show a => a -> Strict.Text
showAsText = Strict.pack . show

-- | Like 'showAsText' but constructs a 'Lazy.Text' value.
showAsLazyText :: Show a => a -> Lazy.Text
showAsLazyText = Lazy.fromStrict . showAsText

----------------------------------------------------------------------------------------------------

-- | This function is useful for printing list element numbers (or line numbers) when printing a
-- list of elements to a console log with each element printed on a single line.
--
-- Maximum of 6 base-10 digits indented such that least significant bits align on the right when
-- printed in a column. The string returned by this function is always at least 6 characters in
-- length, and can be more if the 'Int' value given is greater or equal to 1,000,000. This is used
-- for printing indicies for small vectors, so it is reasonable to expect numbers less than
-- 1,000,000. If you want a more general base-10 'Int' printer, consider using the "Text.Printf"
-- module instead of this one.
ralign6 :: Integral i => i -> String
ralign6 = fromIntegral >>> \ case
  i | i < 10 -> "     " <> show i
  i | i < 100 -> "    " <> show i
  i | i < 1000 -> "   " <> show i
  i | i < 10000 -> "  " <> show i
  i | i < 100000 -> " " <> show i
  i               ->       show (i :: Int)
