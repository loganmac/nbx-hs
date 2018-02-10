{-# LANGUAGE TupleSections #-}

module Nbx.Parser where

import           Prelude

import           Control.Applicative        (empty)
import           Control.Monad              (void)
-- import           Data.Char                  (isAlphaNum)
import           Data.Monoid                ((<>))
import           Data.Void                  (Void)

import qualified Text.Megaparsec            as MP
import           Text.Megaparsec.Char       as C
import qualified Text.Megaparsec.Char.Lexer as L



data NbxError
  = TopFieldError String
  | DataError String
  | RegistryError String
  | ServiceError String
  | DevError String
  | ContainerError String
  | LiveError String
  | BuildError String
  | CopyError String
  | HttpError String
  | EnvError String
  | TypeError String

  deriving (Eq, Ord, Read, Show)

instance MP.ShowErrorComponent NbxError where
  showErrorComponent err =
    case err of
      TopFieldError fieldName ->
        fieldName -- TODO: turn this into a function for this error
      DataError fieldName ->
        fieldName -- TODO: turn this into a function for this error
      RegistryError fieldName ->
        fieldName -- TODO: turn this into a function for this error
      ServiceError fieldName ->
        fieldName -- TODO: turn this into a function for this error
      DevError fieldName ->
        fieldName -- TODO: turn this into a function for this error
      ContainerError fieldName ->
        fieldName -- TODO: turn this into a function for this error
      LiveError fieldName ->
        fieldName -- TODO: turn this into a function for this error
      BuildError fieldName ->
        fieldName -- TODO: turn this into a function for this error
      CopyError fieldName ->
        fieldName -- TODO: turn this into a function for this error
      HttpError fieldName ->
        fieldName -- TODO: turn this into a function for this error
      EnvError fieldName ->
        fieldName -- TODO: turn this into a function for this error
      TypeError fieldName ->
        fieldName -- TODO: turn this into a function for this error


type Parser = MP.Parsec Void String

lineComment :: Parser ()
lineComment = L.skipLineComment "#"

scn :: Parser ()
scn = L.space C.space1 lineComment empty

sc :: Parser ()
sc = L.space (void $ MP.takeWhile1P Nothing f) lineComment empty
  where
    f x = x == ' ' || x == '\t'

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

field :: String -> Parser String
field name = do
  f <- lexeme (C.string name) MP.<?> "field"
  _ <- char ':'
  pure f

topField :: String -> Parser String
topField x = L.nonIndented scn $ field x

parser :: Parser String
parser = (topField "data" <> topField "registries" <> topField "services")
 <* MP.eof

-- field = label "field"

main :: IO ()
main = do
  -- file <- readFile "nbx.yml"
  -- MP.parseTest' parser file
  MP.parseTest parser "data:\nregistries:\n" --services:"

