module Nbx.Schema where

import           Prelude

import           Data.Aeson.Types    (Parser, Value (..))
import qualified Data.HashMap.Strict as HM
import           Data.List           ((\\))
import           Data.Monoid         ((<>))
import           Data.Text           (Text, intercalate, unpack)

type Constraint = (Text, JSType)
type Schema = HM.HashMap Text JSType

schema :: [Constraint] -> Schema
schema = HM.fromList

infixr 0 !?

(!?) :: a -> b -> (a, b)
(!?) = (,)

data JSType = String | Number | Object | Array


validateSchema :: Text -> Schema -> HM.HashMap Text Value -> Parser a -> Parser a
validateSchema name sch obj success = do
  let diff = (HM.keys obj) \\ (HM.keys sch)
      validKeys = null diff

  if validKeys
  then success
  else keyError name diff
  where
    keyError :: Text -> [Text] -> Parser a
    keyError n diff =
      fail $ unpack $ "Unknown keys for type '" <> n
      <> "': " <> (intercalate ", " diff)


checkKey (sch, invalidKeys) k = do
  let val = HM.lookup k sch
  case val of
    Nothing -> k
