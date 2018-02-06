{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
module Nbx.Config where

import           Prelude

import           Data.Aeson
import           Data.Aeson.Types (Parser)
import qualified Data.Text        as T
import           Data.Typeable    (Proxy (..), Typeable, typeRep)
import           GHC.Generics     (Generic (..))
import           GHC.Generics     (Rep)

data NbxFile = NbxFile
  { nbxFileConfig   :: Maybe Config
  , nbxFileServices :: [Service]
  , nbxFileDatums   :: Maybe [Datum]
  } deriving (Generic, Show)

data Config = Config
  { configRemotes :: [Remote]
  } deriving (Generic, Show)

data Remote = Remote
  { remoteName     :: T.Text
  , remoteEnv      :: T.Text
  , remotePath     :: T.Text
  , remoteProvider :: T.Text
  } deriving (Generic, Show)

data Service = Service
  { serviceName :: T.Text
  , serviceLive :: Maybe Live
  , serviceDev  :: Maybe Dev
  } deriving (Generic, Show)

data Dev = Dev
  { devImage        :: T.Text
  , devContainers   :: Maybe [Container]
  , devAliases      :: Maybe [T.Text]
  , devDependencies :: Maybe [T.Text]
  } deriving (Generic, Show)

data Live = Live
  { liveImage        :: T.Text
  , liveBuild        :: Maybe Build
  , liveContainers   :: Maybe [Container]
  , liveDependencies :: Maybe [T.Text]
  } deriving (Generic, Show)

data Container = Container
  { containerName    :: T.Text
  , containerCommand :: T.Text
  , containerImage   :: Maybe T.Text
  } deriving (Generic, Show)

data Build = Build
  { buildImage :: T.Text
  , buildSteps :: [T.Text]
  , buildCopy  :: [Copy]
  } deriving (Generic, Show)

data Copy = Copy
  { copyFrom :: T.Text
  , copyTo   :: T.Text
  } deriving (Generic, Show)

data Datum = Datum
  { datumName  :: T.Text
  , datumImage :: T.Text
  , datumDir   :: T.Text
  } deriving (Generic, Show)

instance FromJSON NbxFile where
  parseJSON = parseWithPrefix

instance FromJSON Config where
  parseJSON = parseWithPrefix

instance FromJSON Remote where
  parseJSON = parseWithPrefix

instance FromJSON Service where
  parseJSON = parseWithPrefix

instance FromJSON Dev where
  parseJSON = parseWithPrefix

instance FromJSON Live where
  parseJSON = parseWithPrefix

instance FromJSON Container where
  parseJSON = parseWithPrefix

instance FromJSON Build where
  parseJSON = parseWithPrefix

instance FromJSON Copy where
  parseJSON = parseWithPrefix

instance FromJSON Datum where
  parseJSON = parseWithPrefix

-- | Aeson serializer to generically format the given record based on its type
-- information.
parseWithPrefix :: forall a. (GFromJSON Zero (Rep a), Generic a, Typeable a) => Value -> Parser a
parseWithPrefix = (genericParseJSON . optStripPrefix $ Proxy @a)

optStripPrefix :: forall proxy a. Typeable a => proxy a -> Options
optStripPrefix _ = defaultOptions {fieldLabelModifier = stripPrefix}
  where stripPrefix = camelTo2 '_' . (drop . length . show $ typeRep $ Proxy @a)
