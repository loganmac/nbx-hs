{-| A print driver and some formatting functions.
-}
{-# LANGUAGE NoImplicitPrelude #-}
module Print where

import           Universum

import qualified Data.Text             as T
import qualified System.Console.ANSI   as Term
import           System.Console.Pretty (Color (..), Style (..), color, style)
import qualified Text.Regex            as Regex

--------------------------------------------------------------------------------
-- CONSTANTS

-- | indentation to use before a header
headerIndent :: Text
headerIndent = spaces 2

-- | indentation to use before a task
taskIndent :: Text
taskIndent = spaces 4

-- | indentation to use before the output of a task
taskOutputIndent :: Text
taskOutputIndent = spaces 6

-- | a string with the given number of spaces
spaces :: Int -> Text
spaces n = T.replicate n " "

--------------------------------------------------------------------------------
-- CLEAR PRINT

-- | Helper to clear then print (since we do so much re-writing)
clearPrint :: Text -> IO ()
clearPrint x = Term.clearLine >> putTextLn x

--------------------------------------------------------------------------------
-- HEADERS

type Header = Text -> IO ()

-- | Prints a header that describes a group of tasks
header :: Text -> IO ()
header str = do
  clearPrint ""
  clearPrint $ headerIndent <> styledHeader
  clearPrint ""
  where
    styledHeader = style Bold . color Cyan $ str <> " :"
--------------------------------------------------------------------------------
-- SPINNER

newtype SpinnerTheme = SpinnerTheme Text

-- | Characters to use for the spinner
unixSpinner :: SpinnerTheme
unixSpinner = SpinnerTheme "⠋⠙⠹⠸⠼⠴⠦⠧⠇⠏"

-- | spinner that works on windows
windowsSpinner :: SpinnerTheme
windowsSpinner = SpinnerTheme "||||//----\\\\"

-- | Prints a spinner next to the given prompt
spinner :: SpinnerTheme -> Int -> Text -> IO ()
spinner (SpinnerTheme theme) pos prompt =
  clearPrint $ taskIndent <> styledSpinner

  where
    styledSpinner :: Text
    styledSpinner =
      style Bold . color Yellow
      $ T.cons spinIcon $ " " <> styledPrompt <> "\n"

    styledPrompt :: Text
    styledPrompt =
      style Underline . color Yellow $ prompt

    spinIcon :: Char
    spinIcon =
      T.index theme $ mod pos (length theme)


-- | Move to the spinner
toSpinner :: IO ()
toSpinner = do
  Term.cursorUpLine 2
  Term.setCursorColumn 0

--------------------------------------------------------------------------------
-- PRINTING

-- | Clears the last line, prints a new last line, then clears the spinner
output :: Text -> IO ()
output str = do
  Term.cursorUpLine 1
  clearPrint $ taskOutputIndent <> str
  toSpinner

out :: Text -> IO ()
out = output . style Faint . strip

err :: Text -> IO ()
err = output . style Normal . color Red . strip

-- | Prints the success message
success :: Text -> IO ()
success x = printResult . style Bold . color Green $ "✓ " <> x

-- | Prints the message as a failure (red with an x)
failure :: Text -> [Text] -> IO ()
failure task buffer = do
  printResult . style Bold . color Red $ "✖ " <> task
  clearPrint "" -- explicitly to clear the line
  clearPrint $ headerIndent <> styledErrorHeader
  clearPrint "" -- explicitly to clear the line

  forM_ (reverse buffer) $ \x -> clearPrint $ taskIndent <> styleStackTrace x
  clearPrint ""
  where
    styleStackTrace = style Normal . color Red

    styledErrorHeader =
      style Bold . style Reverse . color Red
      $ "Error executing task '" <> task <> "':"

-- | clears the spinner then prints the string in its place
printResult :: Text -> IO ()
printResult x = do
  toSpinner
  clearPrint $ taskIndent <> x
  Term.clearLine

-- | removes terminal control sequences from the string
strip :: Text -> Text
strip x = toText $ Regex.subRegex ansiEscape (toString x) ""
  where
    ansiEscape = Regex.mkRegex "\x1b[[][?0123456789]*;?[?0123456789]*[ABEFHJRSTfminsulhp]|\r|\n"
