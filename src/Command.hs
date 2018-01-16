{-# LANGUAGE OverloadedStrings #-}
module Command where

import qualified Command.Dev as Dev
import qualified Command.Live as Live
import Turtle

data Command 
  = Main 
  | Dev Dev.Command
  | Live Live.Command
  | Init 
  | Version


run :: IO ()
run = do
  cmd <- options "NBX: the Nanobox CLI" parser
  runCommand cmd

runCommand :: Command -> IO ()
runCommand cmd =
  case cmd of
    Main -> mainCommand
    Dev sub -> Dev.runCommand sub
    Live sub -> Live.runCommand sub
    Init -> putStrLn "INIT!!!"
    Version -> displayVersion

parser :: Parser Command
parser = 
  parseMain
  <|> Dev <$> Dev.parse
  <|> Live <$> Live.parse
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

mainCommand :: IO ()
mainCommand = do
  displayVersion
  putStrLn "For help, run 'nbx -h'."

displayVersion ::  IO ()
displayVersion = putStrLn "NBX version 0.0.1"