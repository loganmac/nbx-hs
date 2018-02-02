{-| A print driver and some formatting functions.
-}
module Print where

import           Control.Monad       (forM_)
import qualified System.Console.ANSI as Term
import qualified Text.Regex          as Regex

--------------------------------------------------------------------------------
-- CONSTANTS

-- | Characters to use for the spinner
spinnerTheme :: String
spinnerTheme = "⠋⠙⠹⠸⠼⠴⠦⠧⠇⠏"

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

-- | Prints a spinner next to the given prompt
spinner :: Int -> String -> IO ()
spinner pos prompt = do
  Term.clearLine
  putStr taskIndent
  putStrLn $
    style Bold . color Yellow $
      spinnerTheme !! mod pos (length spinnerTheme) :
        ' ' : (style Underline. color Yellow $ prompt)
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
formatFailure str = style Bold. color Red $ "✖ " ++ str

-- | removes terminal control sequences from the string
strip :: String -> String
strip str = Regex.subRegex ansiEscape str ""
  where
    ansiEscape = Regex.mkRegex "\\x1b[[][?0123456789]*;?[?0123456789]*[ABEFHJRSTfminsulhp]"

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

--------------------------------------------------------------------------------
-- TRUE COLOR EXPERIMENT

data TrueColor = TrueColor Int Int Int

nbxDarkBlue = TrueColor 40 48 65

nbxLightBlue = TrueColor 40 166 255

nbxGreen = TrueColor 122 170 170

nbxRed = TrueColor 252 87 94

nbxYellow = TrueColor 255 243 164

trueUnderline str = "\x1b[38;1m"++ "\x1b[38;4m"++ str ++ "\x1b[0m" ++ "\x1b[48;2;40;48;65m"

trueBold str = "\x1b[38;1m"++ str ++ "\x1b[0m" ++ "\x1b[48;2;40;48;65m"

trueReverse str = "\x1b[38;7m"++ "\x1b[38;1m"++ str ++ "\x1b[0m" ++ "\x1b[48;2;40;48;65m"

tcColorize :: TrueColor -> String -> String
tcColorize (TrueColor r g b) str =
  "\x1b[38;2;" ++               -- escape code
  show r ++ ";" ++              -- red
  show g ++ ";" ++              -- green
  show b ++ "m" ++              -- blue
  str -- ++                        -- inner string
  -- "\x1b[0m"                     -- reset

setBg :: TrueColor -> IO ()
setBg (TrueColor r g b) = do
  forM_ [0..50] $ \x ->
    putStrLn $
      "\x1b[48;2;" ++               -- escape code
      show r ++ ";" ++              -- red
      show g ++ ";" ++              -- green
      show b ++ "m"                 -- blue
  Term.cursorUpLine 45

tcOut :: String -> String
tcOut = tcColorize nbxLightBlue

tcErr :: String -> String
tcErr = tcColorize nbxRed

tcSpinner :: Int -> String -> IO ()
tcSpinner pos prompt = do
  Term.clearLine
  putStr taskIndent
  putStrLn $
    tcColorize nbxYellow $ trueBold $
      spinnerTheme !! mod pos (length spinnerTheme) : ' ' : trueUnderline prompt
  putStrLn ""

tcSuccess :: String -> IO ()
tcSuccess str =
  printResult $ trueBold $ tcColorize nbxGreen $ taskIndent ++ "✓ " ++ str

tcFailure :: String -> [String] -> IO ()
tcFailure str buffer = do
  printResult $ trueBold $ tcColorize nbxRed $ taskIndent ++ "✖ " ++ str
  putStrLn $ "\n" ++ headerIndent ++
    trueReverse (tcColorize nbxRed $ "Error executing task '" ++ str ++ "':\n")
  forM_ (reverse buffer) printBufferLine
  putStrLn ""
  where
    printBufferLine x = putStrLn $ taskIndent ++ x

tcHeader :: String -> IO ()
tcHeader s = do
  forM_ [0..20]
    $ \x -> do
        Term.clearLine
        putStrLn "\x1b[48;2;40;48;65m"

  Term.cursorUpLine 20
  Term.clearLine
  putStrLn $ headerIndent ++ trueBold (tcColorize nbxLightBlue (s ++ " :"))
  Term.clearLine
  putStrLn ""
