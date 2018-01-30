{-| A print driver and some formatting functions.
-}
module Print where

import           Concurrency         (millisecond, sleep)
import           Control.Monad       (forM_)
import qualified System.Console.ANSI as Term

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

-- | how many milliseconds between spinner frames
spinnerInterval :: Int
spinnerInterval = 50

-- | a string with the given number of spaces
spaces :: Int -> String
spaces n = replicate n ' '

--------------------------------------------------------------------------------
-- HEADER/SPINNER

-- | Prints a header that describes a group of tasks
header :: String -> IO ()
header s = do
  putStrLn ""
  putStrLn $ headerIndent ++ (blueBold $ s ++ " :")
  putStrLn ""

-- | Prints a spinner next to the given prompt
spinner :: Int -> String -> IO ()
spinner pos prompt = do
  putStr taskIndent
  putStrLn $
    yellowBold $
      theme !! (mod pos $ length theme) : ' ' : (yellowUnderline prompt)
  putStrLn ""
  where
    theme = "⠋⠙⠹⠸⠼⠴⠦⠧⠇⠏"

-- | Move to the spinner
toSpinner :: IO ()
toSpinner = do
  Term.cursorUpLine 2
  Term.setCursorColumn 0

--------------------------------------------------------------------------------
-- RESULTS

-- | Prints the message as a success (green with a check)
success :: String -> IO ()
success str =
  printResult $ greenBold $ taskIndent ++ "✓ " ++ str

-- | Prints the message as a failure (red with an x)
failure :: String -> [String] -> IO ()
failure str buffer = do
  printResult $ redBold $ taskIndent ++ "✖ " ++ str
  putStrLn $ "\n" ++ headerIndent ++
    (bold $ redReverse $ "Error executing task '" ++ str ++ "':\n")
  forM_ buffer printBufferLine
  where
    printBufferLine x = putStrLn $ taskIndent ++ x

-- | clears the spinner then prints the string in its place
printResult :: String -> IO ()
printResult str = do
  toSpinner
  Term.clearLine
  putStrLn str

--------------------------------------------------------------------------------
-- STDOUT/STDERR

-- | Clears the last line, prints a new last line, then clears the spinner
output :: String -> IO ()
output str = do
  Term.cursorUpLine 1
  Term.clearLine
  putStrLn (taskOutputIndent ++ str)
  toSpinner

-- | formats a normal output
out :: String -> String
out = lightGreen . strip

-- | formats an error
err :: String -> String
err = lightRed . strip

strip :: String -> String
strip =
  filter nonAlpha
  where
    nonAlpha x = let o = fromEnum x in o > 31 && o < 126

--------------------------------------------------------------------------------
-- WAIT

-- | go to the spinner, then wait
wait :: IO ()
wait = do
 toSpinner
 sleep spinnerInterval millisecond

--------------------------------------------------------------------------------
-- FORMATTING

-- | Colors for an ANSI terminal
data Color = Black | Red | Green | Yellow | Blue | Magenta | Cyan | White | Default
  deriving (Enum)

-- | SGR paramaters, aka text styles for an ANSI terminal
data Style = Normal | Bold | Faint | Italic | Underline | SlowBlink | ColoredNormal | Reverse
  deriving (Enum)

-- | Helper to make a text green & bold
greenBold :: String -> String
greenBold = style Green Default Bold

-- | Helpler to make a text red & bold
redBold :: String -> String
redBold = style Red Default Bold

-- | Helper for light green text
lightGreen :: String -> String
lightGreen = style Default Default Faint

-- | Helper for light red text
lightRed :: String -> String
lightRed = style Red Default ColoredNormal

-- | Helper to make a text blue & bold
blueBold :: String -> String
blueBold = style Cyan Default Bold

-- | Help to make a text background red and foreground the background color
redReverse :: String -> String
redReverse = style Red Default Reverse

-- | Help to make a text background yellow and underlined
yellowUnderline :: String -> String
yellowUnderline = style Yellow Default Underline

-- | Help to make a text background yellow and bold
yellowBold :: String -> String
yellowBold = style Yellow Default Bold

-- | Helper to underline
underline :: String -> String
underline = style Default Default Underline

-- | Helper to bold
bold :: String -> String
bold = style Default Default Bold

-- | Helper to reverse
reverse :: String -> String
reverse = style Default Default Reverse

-- | Wrap the text in the escape codes to format according to the color and style
style :: Color -> Color -> Style -> String -> String
style foreground background style str =
    "\x1b[9" ++                               -- escape code
    (show $ fromEnum foreground) ++           -- foreground color
    ";4" ++ (show $ fromEnum background) ++   -- background color
    ";" ++ (show $ fromEnum style) ++ "m" ++  -- style
    str ++                                    -- string
    "\x1b[0m"                                 -- reset
