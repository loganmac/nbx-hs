{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Nbx.Config where

import           Prelude

import           Data.Aeson          (FromJSON (..), ToJSON (..))
import qualified Data.HashMap.Strict as HM
import           Data.Monoid         ((<>))
import           Data.Text           (Text, pack)
import qualified Data.Yaml           as Yaml
import qualified Data.Yaml.Include   as YamlInclude
import           GHC.Generics        (Generic (..))

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

-- | Attempts to load a given YAML file.
--
-- >>> config <- load "example.yaml"
--
load :: FilePath -> IO (Either Text Yaml.Object)
load f = do
  file <- YamlInclude.decodeFile f
  case file of
    Nothing -> pure $ Left $ "Missing or invalid file " <> pack f <> "."
    Just a  -> pure $ Right $ a

-- | Returns all toplevel keys in a config.
--
-- >>> keys config
-- ["section1","section2"]
--
keys :: Yaml.Object -> [Text]
keys obj = HM.keys obj

-- | Returns the value at the key if it exists
--
-- >>> keys sub
-- ["field1","field2"]
-- >>> get "field1" sub
-- Just "value1"
--
get :: FromJSON a => Text -> Yaml.Object -> Maybe a
get p obj = do
  cfg <- HM.lookup p obj
  Yaml.parseMaybe parseJSON cfg

validateYaml :: FilePath -> IO (Maybe Text)
validateYaml p = do
  file <- load p

  let err = case file of
        Left e -> Just e
        Right fileContents -> do
          let (_cfg :: Maybe Yaml.Object) = get "config" fileContents
              (svcs :: Maybe Yaml.Object) = get "services" fileContents
              (_dts :: Maybe Yaml.Object) = get "datums" fileContents

          if svcs == Nothing
          then Just "cannot find key 'services'"
          else Nothing

  case err of
    Nothing -> pure Nothing
    Just e  -> pure $ Just $ "Error in " <> pack p <> ": " <> e

-- | This is here to adjust how exceptions are printed from Yaml.
parseError :: Yaml.ParseException -> String
parseError (Yaml.AesonException err) = err
parseError err                       = Yaml.prettyPrintParseException err
