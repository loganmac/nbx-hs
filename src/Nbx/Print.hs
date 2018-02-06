{-| A print driver and some formatting functions.
-}
module Nbx.Print where

import           Universum

import           Control.Concurrent    (threadDelay)
import qualified Data.Text             as T
import           Data.Time.Clock.POSIX (POSIXTime, getPOSIXTime)
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
printSpinner :: SpinnerTheme -> Int -> Text -> IO ()
printSpinner (SpinnerTheme theme) pos prompt =
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

spinner :: Task -> IO Task
spinner task = do
  -- TODO: parameterize the spinner theme
  printSpinner unixSpinner (spinPos task) (name task)
  now <- getPOSIXTime
  let lastSpinTime = lastSpin task
      lastSpinPos = spinPos task

  -- advance the spinner if it's been enough time
  if now - lastSpinTime >= 0.05
  then pure task {spinPos = lastSpinPos+1, lastSpin = now}
  else pure task

--------------------------------------------------------------------------------
-- PRINTING

data Task = Task
            { name     :: Text
            , spinPos  :: Int
            , lastSpin :: POSIXTime
            , stack    :: [Text]
            }

-- | Create a new task with an emtpy stack and a zeroed out spinner
createTask :: Text -> Task
createTask taskName =
  Task { name = taskName, spinPos = 0, lastSpin = 0, stack = [] }

-- | Clears the last line, prints a new last line, then clears the spinner
output :: Task -> Text -> IO Task
output task str = do
  t <- spinner task
  let task' = t { stack = str : stack t}
  Term.cursorUpLine 1
  clearPrint $ taskOutputIndent <> str
  toSpinner
  pure task'

-- | Updates the spinner then sleeps
handleNothing :: Task -> IO Task
handleNothing task = do
  t <- spinner task
  threadDelay 1000 -- 1ms
  toSpinner
  pure t

-- | clears the spinner then prints the string in its place
printResult :: Text -> IO ()
printResult x = do
  clearPrint $ taskIndent <> x
  Term.clearLine

-- | callback to handle output
handleOut :: Task -> Text -> IO Task
handleOut task = output task . style Faint . strip

-- | callback to handle error
handleErr :: Task -> Text -> IO Task
handleErr task = output task . style Normal . color Red . strip

-- | callback to handle task success
handleSuccess :: Task -> IO ()
handleSuccess task = printResult . style Bold . color Green $ "✓ " <> name task

-- | callback to handle task failure
handleFailure :: Task -> IO ()
handleFailure task = do
  printResult . style Bold . color Red $ "✖ " <> name task
  clearPrint "" -- explicitly to clear the line
  clearPrint $ headerIndent <> styledErrorHeader
  clearPrint "" -- explicitly to clear the line

  forM_ (reverse $ stack task) $
    \x -> clearPrint $ taskIndent <> x
  clearPrint "" -- explicitly to clear the line
  where
    styledErrorHeader =
      style Bold . style Reverse . color Red
      $ "Error executing task '" <> name task <> "':"

-- | removes terminal control sequences from the string
strip :: Text -> Text
strip x = toText $ Regex.subRegex ansiEscape (toString x) ""
  where
    ansiEscape = Regex.mkRegex "\x1b[[][?0123456789]*;?[?0123456789]*[ABEFHJRSTfminsulhp]|\r|\n"
