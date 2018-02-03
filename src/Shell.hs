{-| Shell is a thread that runs external processes, and can do things
based on the response.
-}
module Shell
(Driver(..), Shell, new)
where

import           Universum
import           Shell.Internal (mkProcessor, run)
import           Shell.Types (Driver(..), Shell)

-- | creates a new processor to run external processes in,
-- then partially applies it and the display driver
-- to `Shell.Internal.run`, returing a `Shell`
new :: Driver -> IO Shell
new driver = do
  p <- mkProcessor
  pure $ run p driver
