{-# LANGUAGE OverloadedStrings #-}
module Command where

import qualified Command.Stack as Stack
import Turtle

-- Top level command types
data Command 
  = Main 
  | Stack Stack.Command
  | Init 
  | Version

-- parse arguments, return a command
parse :: IO Command
parse = do
  options "NBX: the Nanobox CLI" parser

-- parsers that turn arguments into commands
parser :: Parser Command
parser = 
  parseMain
  <|> Stack <$> Stack.parseCommand
  <|> parseInit
  <|> parseVersion

-- parse a raw `nbx`
parseMain :: Parser Command
parseMain =
  pure Main

-- parse `nbx init`
parseInit :: Parser Command
parseInit =
  subcommand 
    "init" 
    "Initialize a .nbx.yml file for a project" 
    $ pure Init

-- parse `nbx version`
parseVersion :: Parser Command
parseVersion = do
  subcommand
    "version"
    "Display version info"
    $ pure Version