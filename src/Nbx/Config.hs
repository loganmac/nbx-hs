module Nbx.Config where

import           Prelude

import           Data.Aeson
import qualified Data.HashMap.Strict as HM
import qualified Data.Text           as T

data NbxFile = NbxFile
  { nbxFileRegistries :: Maybe [Registry]
  , nbxFileServices   :: [Service]
  , nbxFileDatums     :: Maybe [Datum]
  } deriving (Show)

data Registry = Registry
  { registryName :: T.Text
  , registryPath :: T.Text
  } deriving (Show)

data Service = Service
  { serviceName :: T.Text
  , serviceDev  :: Maybe Dev
  , serviceLive :: Maybe Live
  } deriving (Show)

data Dev = Dev
  { devImage        :: T.Text
  , devContainers   :: Maybe [Process]
  , devAliases      :: Maybe [T.Text]
  , devDependencies :: Maybe [T.Text]
  } deriving (Show)

data Live = Live
  { liveImage        :: T.Text
  , liveBuild        :: Maybe Build
  , liveProcesses    :: Maybe [Process]
  , liveDependencies :: Maybe [T.Text]
  , liveHttp         :: Maybe Http
  , liveTcp          :: Maybe [T.Text]
  , liveUdp          :: Maybe [T.Text]
  } deriving (Show)

data Http = Http
  { httpExpose :: Maybe T.Text
  , httpSsl    :: Maybe Bool
  , httpHealth :: Maybe T.Text
  , httpRoutes :: Maybe [T.Text]
  } deriving (Show)

data Process = Process
  { processName    :: T.Text
  , processCommand :: T.Text
  , processImage   :: Maybe T.Text
  } deriving (Show)

data Build = Build
  { buildImage :: T.Text
  , buildSteps :: [T.Text]
  , buildCopy  :: [Copy]
  } deriving (Show)

data Copy = Copy
  { copyFrom :: T.Text
  , copyTo   :: T.Text
  } deriving (Show)

data Datum = Datum
  { datumName   :: T.Text
  , datumImage  :: T.Text
  , datumConfig :: HM.HashMap T.Text T.Text
  , datumDir    :: T.Text
  } deriving (Show)

instance FromJSON NbxFile where
  parseJSON = withObject "nbxFile" $ \o ->
    NbxFile
    <$> o .:? "registries"
    <*> o .: "services"
    <*> o .:? "data"

instance FromJSON Registry where
  parseJSON = withObject "registry" $ \o ->
    Registry
    <$> o .: "name"
    <*> o .: "path"

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
    <*> o .:? "http"
    <*> o .:? "tcp"
    <*> o .:? "udp"

instance FromJSON Http where
  parseJSON = withObject "http" $ \o ->
    Http
    <$> o .:? "expose"
    <*> o .:? "ssl"
    <*> o .:? "health"
    <*> o .:? "routes"

instance FromJSON Process where
  parseJSON = withObject "process" $ \o ->
    Process
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
    <*> o .: "config"
    <*> o .: "dir"
