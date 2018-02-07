module Nbx.Config where

import           Universum  hiding (Container)

import           Data.Aeson

data NbxFile = NbxFile
  { nbxFileConfig   :: Maybe Config
  , nbxFileServices :: [Service]
  , nbxFileDatums   :: Maybe [Datum]
  } deriving (Show)

data Config = Config
  { configRemotes :: [Remote]
  } deriving (Show)

data Remote = Remote
  { remoteName     :: Text
  , remoteEnv      :: Text
  , remotePath     :: Text
  , remoteProvider :: Text
  } deriving (Show)

data Service = Service
  { serviceName :: Text
  , serviceLive :: Maybe Live
  , serviceDev  :: Maybe Dev
  } deriving (Show)

data Dev = Dev
  { devImage        :: Text
  , devContainers   :: Maybe [Container]
  , devAliases      :: Maybe [Text]
  , devDependencies :: Maybe [Text]
  } deriving (Show)

data Live = Live
  { liveImage        :: Text
  , liveBuild        :: Maybe Build
  , liveContainers   :: Maybe [Container]
  , liveDependencies :: Maybe [Text]
  } deriving (Show)

data Container = Container
  { containerName    :: Text
  , containerCommand :: Text
  , containerImage   :: Maybe Text
  } deriving (Show)

data Build = Build
  { buildImage :: Text
  , buildSteps :: [Text]
  , buildCopy  :: [Copy]
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

instance FromJSON NbxFile where
  parseJSON = withObject "nbxFile" $ \o ->
    NbxFile
    <$> o .:? "config"
    <*> o .: "services"
    <*> o .:? "data"

instance FromJSON Config where
  parseJSON = withObject "config" $ \o ->
    Config
    <$> o.: "remotes"

instance FromJSON Remote where
  parseJSON = withObject "remote" $ \o ->
    Remote
    <$> o .: "name"
    <*> o .: "env"
    <*> o .: "path"
    <*> o .: "provider"

instance FromJSON Service where
  parseJSON = withObject "service" $ \o ->
    Service
    <$> o .: "name"
    <*> o .:? "live"
    <*> o .:? "dev"

instance FromJSON Dev where
  parseJSON = withObject "dev" $ \o ->
    Dev
    <$> o .: "image"
    <*> o .:? "containers"
    <*> o .:? "aliases"
    <*> o .:? "dependencies"

instance FromJSON Live where
  parseJSON = withObject "live" $ \o ->
    Live
    <$> o .: "image"
    <*> o .:? "build"
    <*> o .:? "containers"
    <*> o .:? "dependencies"

instance FromJSON Container where
  parseJSON = withObject "container" $ \o ->
    Container
    <$> o .: "name"
    <*> o .: "command"
    <*> o .:? "image"

instance FromJSON Build where
  parseJSON = withObject "build" $ \o ->
    Build
    <$> o .: "image"
    <*> o .: "steps"
    <*> o .: "copy"

instance FromJSON Copy where
  parseJSON = withObject "copy" $ \o ->
    Copy
    <$> o .: "from"
    <*> o .: "to"

instance FromJSON Datum where
  parseJSON = withObject "data" $ \o ->
    Datum
    <$> o .: "name"
    <*> o .: "image"
    <*> o .: "dir"
