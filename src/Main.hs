module Main where

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
  --       putStrLn ""
  --       putStrLn x
  --       putStrLn ""
  -- shell <- Shell.new verboseDriver

  cmd   <- parse                        -- parse the command from the CLI
  execute shell header cmd              -- execute it, passing the shell and command

--------------------------------------------------------------------------------
-- COMMAND EXECUTION

-- | execute a command
execute :: Shell -> Print.Header -> Command -> IO ()
execute shell header cmd =
  case cmd of
    Main    -> putStrLn "For help, run 'nbx -h'."
    Init    -> putStrLn "INIT!"
    Push    -> pushCmd  shell header
    Status  -> putStrLn "STATUS!"
    Setup   -> putStrLn "SETUP!"
    Implode -> putStrLn "IMPLODE!"
    Version -> putStrLn "NBX version 0.0.1"

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
displayDriver = Shell.Driver
  { Shell.formatOut     = Print.formatOut
  , Shell.formatErr     = Print.formatErr
  , Shell.formatSuccess = Print.formatSuccess
  , Shell.formatFailure = Print.formatFailure
  , Shell.spinner       = Print.spinner
  , Shell.handleOutput  = Print.output
  , Shell.handleSuccess = Print.success
  , Shell.handleFailure = Print.failure
  , Shell.toSpinner     = Print.toSpinner
  }

-- | a display driver that just logs everything out
verboseDriver = Shell.Driver
  { Shell.formatOut     = id
  , Shell.formatErr     = id
  , Shell.formatSuccess = id
  , Shell.formatFailure = id
  , Shell.toSpinner     = pure ()
  , Shell.handleOutput  = putStrLn
  , Shell.handleSuccess = \str           -> pure ()
  , Shell.handleFailure = \task fail buf -> pure ()
  , Shell.spinner       = \pos prompt    -> pure ()
  }
