{-| Processor is a thread that runs external processes, and can await the response.
-}
module Shell
(new, Shell, DisplayDriver(..))
where

import           Concurrency    (Chan, Lock, done, maybeReceive, millisecond,
                                 newChan, newLock, receive, second, send, sleep,
                                 spawn, wait)
import           Shell.Internal (processor, run)
import           Shell.Types    (Cmd, DisplayDriver (..), Output (..),
                                 Processor (..), Shell, Task)

-- | creates a new processor to run external processes in,
-- then partially applies it to `Shell.Internal.run`, returing a `Shell`
new :: DisplayDriver -> IO Shell
new printer = do
  input <- newChan
  output <- newChan
  let p = Processor input output
  _ <- spawn $ processor p
  pure $ run p printer
