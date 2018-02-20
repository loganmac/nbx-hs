module Nbx
  ( verifyDocker
  , readConfig
  , push
  ) where

import qualified Nbx.Config as Config
import qualified Nbx.Print  as Print
import qualified Shellout
import qualified Turtle     as T
import           Universum

--------------------------------------------------------------------------------
-- NBX FUNCTIONS
-- | Ensures that docker is installed and running, otherwise prints an error
-- and then exits.
verifyDocker :: IO ()
verifyDocker = do
  path <- T.which "docker"
  case path of
    Nothing -> do
      putTextLn
        "Cannot find Docker. You must have Docker installed and on \
        \your PATH to use this tool."
      exitFailure
    Just _ -> do
      exitCode <- T.shell "docker ps >/dev/null 2>&1" empty
      case exitCode of
        T.ExitFailure _ -> do
          putTextLn
            "Docker is either not running or not working properly. \
            \Please ensure Docker is running and working correctly to use NBX."
          exitFailure
        T.ExitSuccess -> pure ()

-- | Reads the nbx.yml file from the project.
readConfig :: IO ()
readConfig = do
  _ <- Config.parseFile "./nbx.yml"
  pure ()
  -- let services = Config.nbxFileServices settings in
  -- for_ services $ \service -> putTextLn $ Config.serviceName service

-- | > nbx push
push :: Text -> IO ()
push _target = do
  -- let header = Print.header -- define a function to print headers
  -- shell <- Shellout.new driver -- create a way to run external processes
  -- header "Building Images"
  -- shell "Building Image" "docker build"
  let header = Print.header     -- define a function to print headers
  shell <- Shellout.new driver  -- create a way to run external processes

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
-- | a shell driver that pretty-prints output from processes with a spinner
driver :: Shellout.Driver Print.Task
driver =
  Shellout.Driver
  { Shellout.initialState = Print.createTask
  , Shellout.handleNothing = Print.handleNothing
  , Shellout.handleOut = Print.handleOut
  , Shellout.handleErr = Print.handleErr
  , Shellout.handleSuccess = Print.handleSuccess
  , Shellout.handleFailure = Print.handleFailure
  }
-- TODO: Alternate drivers
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
