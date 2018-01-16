{-# LANGUAGE OverloadedStrings #-}
module Command where

import qualified Command.Stack as Stack
import Turtle

data Command 
  = Main 
  | Stack Stack.Mode Stack.Command
  | Init 
  | Version

parse :: IO Command
parse = do
  options "NBX: the Nanobox CLI" parser

parser :: Parser Command
parser = 
  parseMain
  <|> Stack <$> Stack.parseMode <*> Stack.parseCommands
  <|> parseInit
  <|> parseVersion

parseMain :: Parser Command
parseMain =
  pure Main

parseInit :: Parser Command
parseInit =
  subcommand 
    "init" 
    "Initialize a .nbx.yml file for a project" 
    $ pure Init

parseVersion :: Parser Command
parseVersion = do
  subcommand
    "version"
    "Display version info"
    $ pure Version