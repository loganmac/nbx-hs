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
displayDriver = Shell.DisplayDriver
  { Shell.formatOut    = Print.out
  , Shell.formatErr    = Print.err
  , Shell.spinner      = Print.spinner
  , Shell.printOutput  = Print.output
  , Shell.printSuccess = Print.success
  , Shell.printFailure = Print.failure
  , Shell.toSpinner    = Print.toSpinner
  }

-- | a display driver that just logs everything out
verboseDriver = Shell.DisplayDriver
  { Shell.formatOut    = id
  , Shell.formatErr    = id
  , Shell.toSpinner    = pure ()
  , Shell.printOutput  = putStrLn
  , Shell.printSuccess = \str        -> pure ()
  , Shell.printFailure = \str buf    -> pure ()
  , Shell.spinner      = \pos prompt -> pure ()
  }
