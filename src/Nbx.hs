module Nbx
(readConfig, push)
where

import qualified Nbx.Config as Config
import qualified Nbx.Print  as Print
import qualified Shellout
import           Universum

--------------------------------------------------------------------------------
-- NBX FUNCTIONS

readConfig :: IO ()
readConfig = do
  -- here we might do things like check the config,
  _ <- Config.parseFile "./nbx.yml"
  pure ()

      -- let services = Config.nbxFileServices settings in
      -- for_ services $ \service -> putTextLn $ Config.serviceName service

verifyHasDocker :: IO ()
verifyHasDocker = do
  putTextLn "Checking..."
  putTextLn "Has docker!~"

-- | > nbx push
push :: Text -> IO ()
push target = do
  let header = Print.header     -- define a function to print headers
  shell <- Shellout.new driver  -- create a way to run external processes

  shell "Building Image" "docker build"


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
