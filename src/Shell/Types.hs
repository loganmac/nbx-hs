module Shell.Types where

import           Concurrency (Chan)

-- | DisplayDriver is a collection of functions that
-- describe what to do on process output
data DisplayDriver = DisplayDriver
  { formatOut    :: String -> String
  , formatErr    :: String -> String
  , spinner      :: Int -> String -> IO ()
  , printOutput  :: String -> IO ()
  , printSuccess :: String -> IO ()
  , printFailure :: String -> [String] -> IO ()
  , printWait    :: IO ()
  }

-- | The output of running an external process
data Output = Msg String | Err String | Success | Failure Int

-- | Processor has an input channel (for sending commands)
-- and an output channel (for reading the output)
data Processor = Processor (Chan String) (Chan Output)

-- | The type for a partially applied `Processor.run`
type Shell = (String -> String -> IO ())

-- | Task is the description of an external process
type Task = String

-- | Cmd is the external command like `cat foo` to run
type Cmd = String
