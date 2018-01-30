{-| Shell is a thread that runs external processes, and can do things
based on the response.
-}
module Shell
(new, Shell, DisplayDriver(..))
where

import           Shell.Internal (mkProcessor, processor, run)
import           Shell.Types    (Cmd, DisplayDriver (..), Output (..),
                                 Processor (..), Shell, Task)

-- | creates a new processor to run external processes in,
-- then partially applies it and the display driver
-- to `Shell.Internal.run`, returing a `Shell`
new :: DisplayDriver -> IO Shell
new printer = do
  p <- mkProcessor
  pure $ run p printer
