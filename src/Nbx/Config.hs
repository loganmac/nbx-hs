{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
module Nbx.Config where

import           Prelude

import           Data.Aeson   (FromJSON (..), ToJSON (..))
import           Data.Text    (Text)
import           GHC.Generics (Generic (..))

data NbxFile = NbxFile
  { nbxFileConfig   :: Maybe Config
  , nbxFileServices :: [Service]
  , nbxFileDatums   :: [Datum]
  } deriving (ToJSON, FromJSON, Generic, Show)

data Config = Config
  { configRemotes :: [Remote]
  } deriving (ToJSON, FromJSON, Generic, Show)

data Remote = Remote
  { remoteName     :: Text
  , remoteEnv      :: Text
  , remotePath     :: Text
  , remoteProvider :: Text
  } deriving (ToJSON, FromJSON, Generic, Show)

data Service = Service
  { serviceName :: Text
  , serviceLive :: Maybe Live
  , serviceDev  :: Maybe Dev
  } deriving (ToJSON, FromJSON, Generic, Show)

data Dev = Dev
  { devImage        :: Text
  , devContainers   :: Maybe [Container]
  , devAliases      :: Maybe [Text]
  , devDependencies :: Maybe [Text]
  } deriving (ToJSON, FromJSON, Generic, Show)

data Live = Live
  { liveImage        :: Text
  , liveBuild        :: Maybe Build
  , liveContainers   :: Maybe [Container]
  , liveDependencies :: Maybe [Text]
  } deriving (ToJSON, FromJSON, Generic, Show)

data Container = Container
  { containerName    :: Text
  , containerCommand :: Text
  , containerImage   :: Maybe Text
  } deriving (ToJSON, FromJSON, Generic, Show)

data Build = Build
  { buildImage :: Text
  , buildSteps :: [Text]
  , buildCopy  :: [Copy]
  } deriving (ToJSON, FromJSON, Generic, Show)

data Copy = Copy
  { copyFrom :: Text
  , copyTo   :: Text
  } deriving (ToJSON, FromJSON, Generic, Show)

data Datum = Datum
  { datumName  :: Text
  , datumImage :: Text
  , datumDir   :: Text
  } deriving (ToJSON, FromJSON, Generic, Show)
