module Shell.Types where

import           Shell.Concurrency (Chan)

-- | Driver is a collection of functions that
-- describe what to do on process output
data Driver = Driver
  { formatOut     :: String -> String
  , formatErr     :: String -> String
  , formatSuccess :: String -> String
  , formatFailure :: String -> String
  , spinner       :: Int -> String -> IO ()
  , handleOutput  :: String -> IO ()
  , handleSuccess :: String -> IO ()
  , handleFailure :: String -> String -> [String] -> IO ()
  , toSpinner     :: IO ()
  }

-- | The output of running an external process
data Output = Msg String | Err String | Success | Failure Int

-- | Processor has an input channel (for sending commands)
-- and an output channel (for reading the output)
data Processor = Processor (Chan String) (Chan Output)

-- | The type for a partially applied `Processor.run`
type Shell = (Task -> Cmd -> IO ())

-- | Task is the description of an external process
type Task = String

-- | Cmd is the external command like `cat foo` to run
type Cmd = String
