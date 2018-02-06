{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
module Nbx.Config where

import           Prelude

import           Data.Aeson   (FromJSON (..), ToJSON (..))
import           Data.Text    (Text)
import           GHC.Generics (Generic (..))

data NbxFile = NbxFile
  { config   :: Maybe Config
  , services :: [Service]
  , datums   :: [Datum]
  } deriving (ToJSON, FromJSON, Generic, Show)

data Config = Config
  { remotes :: [Remote]
  } deriving (ToJSON, FromJSON, Generic, Show)

data Remote = Remote
  { name     :: Text
  , env      :: Text
  , path     :: Text
  , provider :: Text
  } deriving (ToJSON, FromJSON, Generic, Show)

data Service = Service
  { name :: Text
  , live :: Maybe Live
  , dev  :: Maybe Dev
  } deriving (ToJSON, FromJSON, Generic, Show)

data Dev = Dev
  { image        :: Text
  , containers   :: Maybe [Container]
  , aliases      :: Maybe [Text]
  , dependencies :: Maybe [Text]
  } deriving (ToJSON, FromJSON, Generic, Show)

data Live = Live
  { image        :: Text
  , build        :: Maybe Build
  , containers   :: Maybe [Container]
  , dependencies :: Maybe [Text]
  } deriving (ToJSON, FromJSON, Generic, Show)

data Container = Container
  { name    :: Text
  , command :: Text
  , image   :: Maybe Text
  } deriving (ToJSON, FromJSON, Generic, Show)

data Build = Build
  { image :: Text
  , steps :: [Text]
  , copy  :: [Copy]
  } deriving (ToJSON, FromJSON, Generic, Show)

data Copy = Copy
  { from :: Text
  , to   :: Text
  } deriving (ToJSON, FromJSON, Generic, Show)

data Datum = Datum
  { name  :: Text
  , image :: Text
  , dir   :: Text
  } deriving (ToJSON, FromJSON, Generic, Show)
