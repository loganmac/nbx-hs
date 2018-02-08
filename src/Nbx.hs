{-# LANGUAGE ScopedTypeVariables #-}
module Nbx
(readConfig, push)
where

import qualified Data.Yaml        as Yaml
import qualified Nbx.Config       as Config
import qualified Nbx.Print        as Print
import qualified Shellout
import qualified Text.Show.Pretty as P
import           Universum

--------------------------------------------------------------------------------
-- NBX FUNCTIONS

readConfig :: IO ()
readConfig = do
  -- here we might do things like check the config,
  -- read .nbx.yml, etc.

  (parsed::Either Yaml.ParseException Config.NbxFile) <- Yaml.decodeFileEither "./nbx.yml"
  case parsed of
    Left err -> do
      putTextLn $ "Invalid configuration found while parsing nbx.yml:"
      putStrLn $ Yaml.prettyPrintParseException err
      exitFailure
    Right settings ->
      P.pPrint settings
      -- let services = Config.nbxFileServices settings in
      -- for_ services $ \service -> putTextLn $ Config.serviceName service

-- | > nbx push
push :: IO ()
push = do
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
driver = Shellout.Driver
  { Shellout.initialState  = Print.createTask
  , Shellout.handleNothing = Print.handleNothing
  , Shellout.handleOut     = Print.handleOut
  , Shellout.handleErr     = Print.handleErr
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
