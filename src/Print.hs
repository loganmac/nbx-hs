{-| A print driver and some formatting functions.
-}
module Print where

import           Control.Monad       (forM_)
import qualified System.Console.ANSI as Term
import qualified Text.Regex          as Regex

--------------------------------------------------------------------------------
-- CONSTANTS

-- | indentaion to use before a header
headerIndent :: String
headerIndent = spaces 2

-- | indentation to use before a task
taskIndent :: String
taskIndent = spaces 4

-- | indentation to use before the output of a task
taskOutputIndent :: String
taskOutputIndent = spaces 6

-- | a string with the given number of spaces
spaces :: Int -> String
spaces n = replicate n ' '

--------------------------------------------------------------------------------
-- HEADER

type Header = String -> IO ()

-- | Prints a header that describes a group of tasks
header :: String -> IO ()
header s = do
  putStrLn ""
  putStrLn $ headerIndent ++ (style Bold . color Cyan $ (s ++ " :"))
  putStrLn ""

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
spinner :: SpinnerTheme -> Int -> String -> IO ()
spinner (SpinnerTheme theme) pos prompt = do
  Term.clearLine
  putStr taskIndent
  putStrLn $
    style Bold $ color Yellow $
      theme !! mod pos (length theme) : ' ' :
      (style Underline $ color Yellow $ prompt)
  putStrLn ""

-- | Move to the spinner
toSpinner :: IO ()
toSpinner = do
  Term.cursorUpLine 2
  Term.setCursorColumn 0

--------------------------------------------------------------------------------
-- FORMAT OUTPUT

-- | formats a normal output
formatOut :: String -> String
formatOut = style Faint . strip

-- | formats an error
formatErr :: String -> String
formatErr = style Normal . color Red . strip

-- | formats a success string
formatSuccess :: String -> String
formatSuccess str = style Bold . color Green $ "✓ " ++ str

-- | formats a failure string
formatFailure :: String -> String
formatFailure str = style Bold . color Red $ "✖ " ++ str

-- | removes terminal control sequences from the string
strip :: String -> String
strip str = Regex.subRegex ansiEscape str ""
  where
    ansiEscape = Regex.mkRegex "\x1b[[][?0123456789]*;?[?0123456789]*[ABEFHJRSTfminsulhp]|\r|\n"

--------------------------------------------------------------------------------
-- PRINTING

-- | Clears the last line, prints a new last line, then clears the spinner
output :: String -> IO ()
output str = do
  Term.cursorUpLine 1
  Term.clearLine
  putStrLn (taskOutputIndent ++ str)
  toSpinner

-- | Prints the success message
success :: String -> IO ()
success = printResult

-- | Prints the message as a failure (red with an x)
failure :: String -> String -> [String] -> IO ()
failure task failure buffer = do
  printResult failure
  putStrLn ""
  putStrLn $ headerIndent ++
    (style Bold . style Reverse . color Red $ "Error executing task '" ++ task ++ "':")
  putStrLn ""

  forM_ (reverse buffer) (\x -> putStrLn $ taskIndent ++ x)
  putStrLn ""

-- | clears the spinner then prints the string in its place
printResult :: String -> IO ()
printResult str = do
  toSpinner
  Term.clearLine
  putStrLn $ taskIndent ++ str
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
color :: Color -> String -> String
color = colorize Foreground

-- | Helper to set background color
bgColor :: Color -> String -> String
bgColor = colorize Background

colorize :: Section -> Color -> String -> String
colorize section color str =
  "\x1b[" ++ sectionNum ++
  show (fromEnum color)
  ++ "m" ++
  str ++
  "\x1b[0m"
  where
    sectionNum = case section of
      Foreground -> "9"
      Background -> "4"

-- | Wrap the text in the escape codes to format according to the color and style
style :: Style -> String -> String
style style str =
    "\x1b[" ++            -- escape code
    show (fromEnum style) -- style
    ++ "m" ++             -- delim
    str ++                -- string
    "\x1b[0m"             -- reset
