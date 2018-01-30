module Processor.Types where

import           Concurrency (Chan)

-- | The output of running an external process
data Output = Msg String | Err String | Success | Failure Int

-- | Processor has an input channel (for sending commands) and an output channel (for reading the output)
data Processor = Processor (Chan String) (Chan Output)

-- | Task is the description of an external process
type Task = String

-- | Cmd is the external command like `cat foo` to run
type Cmd = String
