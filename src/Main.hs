module Main where

import           Command (Command (..), parse)
import qualified Print
import           Shell   (Shell)
import qualified Shell

main :: IO ()
main = do
                              -- here we would do things like check the config,
                              -- read .nbx.yml, etc.
  let printer = Shell.DisplayDriver
        { Shell.formatterOut   = Print.out
        , Shell.formatterErr   = Print.err
        , Shell.printerSpinner = Print.spinner
        , Shell.printerOutput  = Print.output
        , Shell.printerSuccess = Print.success
        , Shell.printerFailure = Print.failure
        , Shell.printerWait    = Print.wait
        }
  shell <- Shell.new printer  -- create a way to run external processes
  cmd   <- parse              -- parse the command from the CLI
  execute shell cmd           -- execute it, passing the shell and command

-- | execute a command
execute :: Shell -> Command -> IO ()
execute shell cmd =
  case cmd of
    Main    -> putStrLn "For help, run 'nbx -h'."
    Init    -> putStrLn "INIT!"
    Push    -> pushCmd shell
    Status  -> putStrLn "STATUS!"
    Setup   -> putStrLn "SETUP!"
    Implode -> putStrLn "IMPLODE!"
    Version -> putStrLn "NBX version 0.0.1"

-- | run the push command
-- > nbx push
pushCmd :: Shell -> IO ()
pushCmd shell = do

  Print.header "Setting up concert"

  shell "Preparing show"   "./test-scripts/good.sh"
  shell "Setting up stage" "./test-scripts/good.sh"
  shell "Inviting guests"  "./test-scripts/good.sh"

  Print.header "Let the show begin"

  shell "Opening gates" "./test-scripts/good.sh"
  shell "Starting show" "./test-scripts/good-then-bad.sh"
  shell "Shouldn't run" "./test-scripts/good.sh"
