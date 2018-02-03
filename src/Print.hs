{-| A print driver and some formatting functions.
-}
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
-- HEADER

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
-- FORMAT OUTPUT

-- | formats a normal output
formatOut :: Text -> Text
formatOut = style Faint . strip

-- | formats an error
formatErr :: Text -> Text
formatErr = style Normal . color Red . strip

-- | formats a success string
formatSuccess :: Text -> Text
formatSuccess x = style Bold . color Green $ "✓ " <> x

-- | formats a failure string
formatFailure :: Text -> Text
formatFailure x = style Bold . color Red $ "✖ " <> x

-- | removes terminal control sequences from the string
strip :: Text -> Text
strip x = toText $ Regex.subRegex ansiEscape (toString x) ""
  where
    ansiEscape = Regex.mkRegex "\x1b[[][?0123456789]*;?[?0123456789]*[ABEFHJRSTfminsulhp]|\r|\n"

--------------------------------------------------------------------------------
-- PRINTING

-- | Clears the last line, prints a new last line, then clears the spinner
output :: Text -> IO ()
output str = do
  Term.cursorUpLine 1
  clearPrint $ taskOutputIndent <> str
  toSpinner

-- | Prints the success message
success :: Text -> IO ()
success = printResult

-- | Prints the message as a failure (red with an x)
failure :: Text -> Text -> [Text] -> IO ()
failure task msg buffer = do
  printResult msg
  clearPrint "" -- explicitly to clear the line
  clearPrint $ headerIndent <> styledErrorHeader
  clearPrint "" -- explicitly to clear the line

  forM_ (reverse buffer) $ \x -> clearPrint $ taskIndent <> x
  clearPrint ""
  where
    styledErrorHeader =
      style Bold . style Reverse . color Red
      $ "Error executing task '" <> task <> "':"

-- | clears the spinner then prints the string in its place
printResult :: Text -> IO ()
printResult x = do
  toSpinner
  clearPrint $ taskIndent <> x
  Term.clearLine


