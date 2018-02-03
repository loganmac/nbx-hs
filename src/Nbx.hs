{-# LANGUAGE NoImplicitPrelude #-}
module Nbx
(Command(..) , execute)
where

import qualified Nbx.Print as Print
import           Shell     (Shell)
import qualified Shell
import           Universum

--------------------------------------------------------------------------------
-- COMMANDS

-- | Commands that are recognized and parsed by the CLI
data Command
  = Main
  | Init
  | Push
  | Status
  | Setup
  | Implode
  | Version

--------------------------------------------------------------------------------
-- COMMAND EXECUTION

-- | execute a command
execute :: Command -> IO ()
execute cmd = do
                            -- here we would do things like check the config,
                            -- read .nbx.yml, etc.
  let header = Print.header -- define a function to print headers
  shell <- Shell.new driver -- create a way to run external processes

  case cmd of
    Main    -> putTextLn "For help, run 'nbx -h'."
    Init    -> putTextLn "INIT!"
    Push    -> pushCmd  shell header
    Status  -> putTextLn "STATUS!"
    Setup   -> putTextLn "SETUP!"
    Implode -> putTextLn "IMPLODE!"
    Version -> putTextLn "NBX version 0.0.1"

-- | run the push command
-- > nbx push
pushCmd :: Shell -> Print.Header -> IO ()
pushCmd shell header = do

  header "Setting up concert"

  shell "Preparing show"   "./test-scripts/bash/noisy-good.sh"
  shell "Setting up stage" "./test-scripts/bash/good-with-warn.sh"
  shell "Inviting guests"  "./test-scripts/bash/good.sh"

  header "Let the show begin"

  shell "Opening gates" "./test-scripts/bash/good.sh"
  shell "Starting show" "./test-scripts/bash/bad.sh"
  shell "Shouldn't run" "./test-scripts/bash/good.sh"

--------------------------------------------------------------------------------
-- SHELL DISPLAY DRIVER

-- | a display driver that pretty-prints output from processes with a spinner
driver :: Shell.Driver
driver = Shell.Driver
  { Shell.spinnerFps    = 20
  , Shell.toSpinner     = Print.toSpinner
  , Shell.spinner       = Print.spinner Print.unixSpinner
  , Shell.sleepDuration = 1 * 1000 -- 1 ms
  , Shell.handleOut     = Print.out
  , Shell.handleErr     = Print.err
  , Shell.handleSuccess = Print.success
  , Shell.handleFailure = Print.failure
  }

-- TODO: Alternate drivers

-- | a windows display driver
-- windowsDisplayDriver :: Shell.Driver
-- windowsDisplayDriver = displayDriver {Shell.spinner = Print.spinner Print.windowsSpinner}

-- -- | a display driver that just logs everything out
-- verboseDriver :: Shell.Driver
-- verboseDriver = Shell.Driver
--   { Shell.toSpinner     = pure ()
--   , Shell.handleOut     = putTextLn
--   , Shell.handleErr     = putTextLn
--   , Shell.handleSuccess = \_str         -> pure ()
--   , Shell.handleFailure = \_task _buf   -> pure ()
--   , Shell.spinner       = \_pos _prompt -> pure ()
--   }
