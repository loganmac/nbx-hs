{-# LANGUAGE DeriveGeneric #-}
module Nbx.Config where

import           Data.Aeson          (FromJSON (..), withObject)
import qualified Data.Aeson.Types    as A
import qualified Data.HashMap.Strict as HM
import           Data.Traversable    (for)
import           GHC.Generics
import           Universum

data NbxFile = NbxFile
  { config   :: Maybe Config
  , services :: [ServiceConfig]
  } deriving (Generic, Show)

data Config = Config
  { remotes :: [RemoteConfig]
  } deriving (Generic, Show)

data RemoteConfig = RemoteConfig
  { remoteName :: Text
  , remotePath :: Text
  } deriving (Generic, Show)

data ServiceConfig = ServiceConfig
  { serviceName :: Text
  , serviceLive :: Maybe LiveConfig
  } deriving (Generic, Show)

data LiveConfig = LiveConfig
  { liveImage :: Text
  , liveBuild :: Maybe BuildConfig
  , liveRun   :: Maybe [RunConfig]
  } deriving (Generic, Show)

data RunConfig = RunConfig
  { runName    :: Text
  , runCommand :: Text
  } deriving (Generic, Show)

data BuildConfig = BuildConfig
  { buildImage :: Text
  , buildSteps :: [Text]
  , buildCopy  :: [CopyConfig]
  } deriving (Generic, Show)

data CopyConfig = CopyConfig
  { copyFrom :: Text
  , copyTo   :: Text
  } deriving (Generic, Show)

instance FromJSON NbxFile where
  parseJSON = withObject "nbx.yml" $ \obj ->
    NbxFile
      <$> A.parseFieldMaybe obj "config"
      <*> A.explicitParseField parseServices obj "service"
    where
      parseServices = withObject "service" $ \obj ->
        for (HM.toList obj) $ \(name, svcBody) -> do
          body <- parseJSON svcBody
          let live = A.parseFieldMaybe body "live"
          ServiceConfig <$> pure name <*> live


instance FromJSON Config where
  parseJSON = withObject "config" $ \obj ->
    Config
      <$> A.explicitParseField parseRemotes obj "remotes"
    where
      parseRemotes = withObject "remote" $ \obj->
        for (HM.toList obj) $ \(name, path) ->
          RemoteConfig <$> pure name <*> parseJSON path

instance FromJSON LiveConfig where
  parseJSON = withObject "live" $ \obj ->
    LiveConfig
      <$> A.parseField obj "image"
      <*> A.parseFieldMaybe obj "build"
      <*> A.explicitParseFieldMaybe parseRun obj "run"
    where
      parseRun = withObject "run" $ \obj ->
        for (HM.toList obj) $ \(name, cmd) ->
          RunConfig <$> pure name <*> parseJSON cmd

instance FromJSON BuildConfig where
  parseJSON = withObject "build" $ \obj ->
    BuildConfig
      <$> A.parseField obj "image"
      <*> A.parseField obj "steps"
      <*> A.parseField obj "copy"

instance FromJSON CopyConfig where
  parseJSON = withObject "copy" $ \obj ->
    CopyConfig
      <$> A.parseField obj "from"
      <*> A.parseField obj "to"
