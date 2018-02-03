{-| A print driver and some formatting functions.
-}
module Print where

import           Prelude             ((!!))
import           Universum

import qualified System.Console.ANSI as Term
import qualified Text.Regex          as Regex

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
spaces n = toText $ replicate n ' '

--------------------------------------------------------------------------------
-- HEADER

type Header = Text -> IO ()

-- | Prints a header that describes a group of tasks
header :: Text -> IO ()
header s = do
  putTextLn ""
  putTextLn $ headerIndent <> (style Bold . fgColor Cyan $ (s <> " :"))
  putTextLn ""

--------------------------------------------------------------------------------
-- SPINNER

newtype SpinnerTheme = SpinnerTheme String

-- | Characters to use for the spinner
unixSpinner :: SpinnerTheme
unixSpinner = SpinnerTheme "⠋⠙⠹⠸⠼⠴⠦⠧⠇⠏"

-- | spinner that works on windows
windowsSpinner :: SpinnerTheme
windowsSpinner = SpinnerTheme "||||//----\\\\"

-- | Prints a spinner next to the given prompt
spinner :: SpinnerTheme -> Int -> Text -> IO ()
spinner (SpinnerTheme theme) pos prompt = do
  Term.clearLine
  putText taskIndent
  putTextLn $
    style Bold . fgColor Yellow $
      toText [theme !! mod pos (length theme)] <>
        " " <> (style Underline . fgColor Yellow $ prompt)
  putTextLn ""

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
formatErr = style Normal . fgColor Red . strip

-- | formats a success string
formatSuccess :: Text -> Text
formatSuccess str = style Bold . fgColor Green $ "✓ " <> str

-- | formats a failure string
formatFailure :: Text -> Text
formatFailure str = style Bold . fgColor Red $ "✖ " <> str

-- | removes terminal control sequences from the string
strip :: Text -> Text
strip str = toText $ Regex.subRegex ansiEscape (toString str) ""
  where
    ansiEscape = Regex.mkRegex "\x1b[[][?0123456789]*;?[?0123456789]*[ABEFHJRSTfminsulhp]|\r|\n"

--------------------------------------------------------------------------------
-- PRINTING

-- | Clears the last line, prints a new last line, then clears the spinner
output :: Text -> IO ()
output str = do
  Term.cursorUpLine 1
  Term.clearLine
  putTextLn $ taskOutputIndent <> str
  toSpinner

-- | Prints the success message
success :: Text -> IO ()
success = printResult

-- | Prints the message as a failure (red with an x)
failure :: Text -> Text -> [Text] -> IO ()
failure task msg buffer = do
  printResult msg
  putTextLn ""
  putTextLn $ headerIndent <>
    (style Bold . style Reverse . fgColor Red $ "Error executing task '" <> task <> "':")
  putTextLn ""

  forM_ (reverse buffer) $ \x -> putTextLn $ taskIndent <> x
  putTextLn ""

-- | clears the spinner then prints the string in its place
printResult :: Text -> IO ()
printResult str = do
  toSpinner
  Term.clearLine
  putTextLn $ taskIndent <> str
  Term.clearLine

--------------------------------------------------------------------------------
-- COLORS/STYLES

data Section = Foreground | Background

-- | Colors for an ANSI terminal
data Color = Black | Red | Green | Yellow | Blue | Magenta | Cyan | White | Default
  deriving (Enum)

-- | SGR paramaters, aka text styles for an ANSI terminal
data Style
  = Normal | Bold | Faint | Italic
  | Underline | SlowBlink | ColoredNormal | Reverse
  deriving (Enum)

-- | Helper to set foreground color
fgColor :: Color -> Text -> Text
fgColor = colorize Foreground

-- | Helper to set background color
bgColor :: Color -> Text -> Text
bgColor = colorize Background

colorize :: Section -> Color -> Text -> Text
colorize section color str =
  "\x1b[" <> sectionNum <>
  show (fromEnum color)
  <> "m" <>
  str <>
  "\x1b[0m"
  where
    sectionNum = case section of
      Foreground -> "9"
      Background -> "4"

-- | Wrap the text in the escape codes to format according to the color and style
style :: Style -> Text -> Text
style style' str =
    "\x1b[" <>             -- escape code
    show (fromEnum style') -- style
    <> "m" <>              -- delim
    str <>                 -- string
    "\x1b[0m"              -- reset
