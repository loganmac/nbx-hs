module Nbx.Config where

import           Universum             hiding (Container, bool)

-- import           Data.HashMap.Strict   (HashMap)
import           Data.Vector           (Vector)
import           Data.Yaml.Combinators

data NbxFile = NbxFile
  { nbxFileConfig   :: Maybe Config
  , nbxFileServices :: (Vector Service)
  , nbxFileDatums   :: Maybe (Vector Datum)
  } deriving (Show)

data Config = Config
  { configRemotes :: (Vector Remote)
  } deriving (Show)

data Remote = Remote
  { remoteName     :: Text
  , remoteEnv      :: Text
  , remotePath     :: Text
  , remoteProvider :: Text
  } deriving (Show)

data Service = Service
  { serviceName :: Text
  , serviceDev  :: Maybe Dev
  , serviceLive :: Maybe Live
  , serviceEnv  :: Maybe (Vector Env)
  } deriving (Show)

data Dev = Dev
  { devImage        :: Text
  , devContainers   :: Maybe (Vector Container)
  , devAliases      :: Maybe (Vector Text)
  , devDependencies :: Maybe (Vector Text)
  } deriving (Show)

data Live = Live
  { liveImage        :: Text
  , liveBuild        :: Maybe Build
  , liveContainers   :: Maybe (Vector Container)
  , liveDependencies :: Maybe (Vector Text)
  , liveHttp         :: Maybe Http
  , liveTcp          :: Maybe (Vector Text)
  , liveUdp          :: Maybe (Vector Text)
  } deriving (Show)

data Env = Env
  { envName :: Text
  -- TODO : , envVars :: HashMap Text Text
  } deriving (Show)

data Http = Http
  { httpExpose :: Maybe Text
  , httpSsl    :: Maybe Bool
  , httpHealth :: Maybe Text
  , httpRoutes :: Maybe (Vector Text)
  } deriving (Show)

data Container = Container
  { containerName    :: Text
  , containerCommand :: Text
  , containerImage   :: Maybe Text
  } deriving (Show)

data Build = Build
  { buildImage :: Text
  , buildSteps :: (Vector Text)
  , buildCopy  :: (Vector Copy)
  } deriving (Show)

data Copy = Copy
  { copyFrom :: Text
  , copyTo   :: Text
  } deriving (Show)

data Datum = Datum
  { datumName  :: Text
  , datumImage :: Text
  , datumDir   :: Text
  } deriving (Show)

nbxFileParser :: Parser NbxFile
nbxFileParser =
    object $ NbxFile
    <$> optField "config" configParser
    <*> field "services" (array serviceParser)
    <*> optField "data" (array datumParser)

configParser :: Parser Config
configParser =
    object $ Config
    <$> field "remotes" (array remoteParser)

remoteParser :: Parser Remote
remoteParser =
    object $ Remote
    <$> field "name" string
    <*> field "env" string
    <*> field "path" string
    <*> field "provider" string

serviceParser :: Parser Service
serviceParser =
    object $ Service
    <$> field "name" string
    <*> optField "dev" devParser
    <*> optField "live" liveParser
    <*> optField "env" (array envParser)

devParser :: Parser Dev
devParser =
    object $ Dev
    <$> field "image" string
    <*> optField "containers" (array containerParser)
    <*> optField "aliases" (array string)
    <*> optField "dependencies" (array string)

liveParser :: Parser Live
liveParser =
    object $ Live
    <$> field "image" string
    <*> optField "build" buildParser
    <*> optField "containers" (array containerParser)
    <*> optField "dependencies" (array string)
    <*> optField "http" httpParser
    <*> optField "tcp" (array string)
    <*> optField "udp" (array string)

envParser :: Parser Env
envParser =
  object $ Env
  <$> field "name" string
  -- TODO : <*> field "vars" object

httpParser :: Parser Http
httpParser =
  object $ Http
  <$> optField "expose" string
  <*> optField "ssl" bool
  <*> optField "health" string
  <*> optField "routes" (array string)

containerParser :: Parser Container
containerParser =
    object $ Container
    <$> field "name" string
    <*> field "command" string
    <*> optField "image" string

buildParser :: Parser Build
buildParser =
    object $ Build
    <$> field "image" string
    <*> field "steps" (array string)
    <*> field "copy" (array copyParser)

copyParser :: Parser Copy
copyParser =
    object $ Copy
    <$> field "from" string
    <*> field "to" string

datumParser :: Parser Datum
datumParser =
    object $ Datum
    <$> field "name" string
    <*> field "image" string
    <*> field "dir" string
