module Main where

import           Universum
import           Command (Command (..), parse)
import qualified Print
import           Shell   (Shell)
import qualified Shell

main :: IO ()
main = do
                                        -- here we would do things like check the config,
                                        -- read .nbx.yml, etc.
  let header = Print.header             -- define a function to print headers
  shell <- Shell.new displayDriver      -- create a way to run external processes

  -- TODO: turn this into verbose mode
  -- let header x = do
  --       putTextLn ""
  --       putTextLn x
  --       putTextLn ""
  -- shell <- Shell.new verboseDriver

  cmd   <- parse                        -- parse the command from the CLI
  execute shell header cmd              -- execute it, passing the shell and command

--------------------------------------------------------------------------------
-- COMMAND EXECUTION

-- | execute a command
execute :: Shell -> Print.Header -> Command -> IO ()
execute shell header cmd =
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
displayDriver :: Shell.Driver
displayDriver = Shell.Driver
  { Shell.formatOut     = Print.formatOut
  , Shell.formatErr     = Print.formatErr
  , Shell.formatSuccess = Print.formatSuccess
  , Shell.formatFailure = Print.formatFailure
  , Shell.spinner       = Print.spinner Print.unixSpinner
  , Shell.handleOutput  = Print.output
  , Shell.handleSuccess = Print.success
  , Shell.handleFailure = Print.failure
  , Shell.toSpinner     = Print.toSpinner
  }

-- | a windows display driver
windowsDisplayDriver :: Shell.Driver
windowsDisplayDriver = displayDriver {Shell.spinner = Print.spinner Print.windowsSpinner}

-- | a display driver that just logs everything out
verboseDriver :: Shell.Driver
verboseDriver = Shell.Driver
  { Shell.formatOut     = identity
  , Shell.formatErr     = identity
  , Shell.formatSuccess = identity
  , Shell.formatFailure = identity
  , Shell.toSpinner     = pure ()
  , Shell.handleOutput  = putTextLn
  , Shell.handleSuccess = \_str             -> pure ()
  , Shell.handleFailure = \_task _fail _buf -> pure ()
  , Shell.spinner       = \_pos _prompt     -> pure ()
  }
