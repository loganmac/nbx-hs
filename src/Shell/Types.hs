module Shell.Types where

import           Universum
import           Shell.Concurrency (Chan)

-- | Driver is a collection of functions that
-- describe what to do on process output
data Driver = Driver
  { formatOut     :: Text -> Text
  , formatErr     :: Text -> Text
  , formatSuccess :: Text -> Text
  , formatFailure :: Text -> Text
  , spinner       :: Int -> Text -> IO ()
  , handleOutput  :: Text -> IO ()
  , handleSuccess :: Text -> IO ()
  , handleFailure :: Text -> Text -> [Text] -> IO ()
  , toSpinner     :: IO ()
  }

-- | The output of running an external process
data Output = Msg Text | Err Text | Success | Failure Int

-- | Processor has an input channel (for sending commands)
-- and an output channel (for reading the output)
data Processor = Processor (Chan Text) (Chan Output)

-- | The type for a partially applied `Processor.run`
type Shell = (Task -> Cmd -> IO ())

-- | Task is the description of an external process
type Task = Text

-- | Cmd is the external command like `cat foo` to run
type Cmd = Text
