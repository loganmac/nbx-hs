module Nbx
(Command(..) , execute)
where

import qualified Nbx.Print as Print
import qualified Shellout
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
  shell <- Shellout.new driver -- create a way to run external processes

  case cmd of
    Main    -> putTextLn "For help, run 'nbx -h'."
    Init    -> putTextLn "INIT!"
    Push    -> pushCmd shell header
    Status  -> putTextLn "STATUS!"
    Setup   -> putTextLn "SETUP!"
    Implode -> putTextLn "IMPLODE!"
    Version -> putTextLn "NBX version 0.0.1"

-- | run the push command
-- > nbx push
pushCmd :: Shellout.Shell -> Print.Header -> IO ()
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

-- -- | a shell driver that pretty-prints output from processes with a spinner
driver :: Shellout.Driver Print.Task
driver = Shellout.Driver
  { Shellout.initialState  = Print.createTask
  , Shellout.handleNothing = Print.handleNothing
  , Shellout.handleOut     = Print.handleOut
  , Shellout.handleErr     = Print.handleErr
  , Shellout.handleSuccess = Print.handleSuccess
  , Shellout.handleFailure = Print.handleFailure
  }

-- TODO: Alternate drivers

-- | a windows display driver
-- windowsDriver :: Shellout.Driver Print.Task
-- windowsDriver = driver {Shellout.spinner = Print.spinner Print.windowsSpinner}

-- | a display driver that just logs everything out
-- verboseDriver :: Shellout.Driver Print.Task
-- verboseDriver = Shellout.Driver
--   { Shellout.initialState  = Print.createTask
--   , Shellout.handleNothing = \task     -> threadDelay 1000 >> pure task
--   , Shellout.handleOut     = \task txt -> putTextLn txt >> pure task
--   , Shellout.handleErr     = \task txt -> putTextLn txt >> pure task
--   , Shellout.handleSuccess = \_task    -> pure ()
--   , Shellout.handleFailure = \task     -> putTextLn $ (Print.name task) <> " failed."
--   }
