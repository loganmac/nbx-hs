{-# LANGUAGE ScopedTypeVariables #-}
module Nbx.Config where

import           Universum  hiding (Container)

import qualified Data.Aeson as A
import qualified Data.Yaml  as Yaml

data NbxFile = NbxFile
  { nbxFileDestinations :: [Destination]
  , nbxFileResources    :: Maybe [Resource]
  , nbxFileServices     :: [Service]
  } deriving (Show)

data Destination = Destination
  { destinationId       :: Text
  , destinationAlias    :: Text
  , destinationRegistry :: Text
  } deriving (Show)

data Resource = Resource
  { resourceId      :: Text
  , resourceImage   :: Text
  , resourceOptions :: Maybe A.Object
  } deriving (Show)

data Service = Service
  { serviceId         :: Text
  , serviceContainers :: Maybe [Container]
  , servicePort       :: Maybe Int
  , serviceForceSsl   :: Maybe Bool
  , serviceHealth     :: Maybe Text
  , serviceRoutes     :: Maybe [Text]
  , serviceTcp        :: Maybe [Text]
  , serviceUdp        :: Maybe [Text]
  } deriving (Show)

data Container = Container
  { containerId    :: Text
  , containerImage :: Text
  } deriving (Show)

instance A.FromJSON NbxFile where
  parseJSON = A.withObject "nbxFile" $ \o ->
    NbxFile
    <$> o A..:  "destinations"
    <*> o A..:? "resources"
    <*> o A..:  "services"

instance A.FromJSON Destination where
  parseJSON = A.withObject "destination" $ \o ->
    Destination
    <$> o A..: "id"
    <*> o A..: "alias"
    <*> o A..: "registry"

instance A.FromJSON Resource where
  parseJSON = A.withObject "resource" $ \o ->
    Resource
    <$> o A..:  "id"
    <*> o A..:  "image"
    <*> o A..:? "options"

instance A.FromJSON Service where
  parseJSON = A.withObject "service" $ \o ->
    Service
    <$> o A..:  "id"
    <*> o A..:  "containers"
    <*> o A..:? "htttp_port"
    <*> o A..:? "force_ssl"
    <*> o A..:? "health"
    <*> o A..:? "routes"
    <*> o A..:? "tcp"
    <*> o A..:? "udp"

instance A.FromJSON Container where
  parseJSON = A.withObject "container" $ \o ->
    Container
    <$> o A..: "id"
    <*> o A..: "image"

parseFile :: FilePath -> IO NbxFile
parseFile file = do
  yaml :: Either Yaml.ParseException NbxFile <- Yaml.decodeFileEither file
  case yaml of
    Left err -> do
      putTextLn "Invalid configuration found while parsing nbx.yml:"
      putStrLn $ Yaml.prettyPrintParseException err
      exitFailure
    Right settings ->
      pure settings
