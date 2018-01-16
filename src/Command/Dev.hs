{-# LANGUAGE OverloadedStrings #-}
module Command.Dev where
import Turtle

data Command = Main
  | Run
  | Console
  | Evar
  | Alias
  | Destroy

runCommand :: Command -> IO ()
runCommand cmd =
  case cmd of
    Main -> putStrLn "DEV!!!"
    Run -> putStrLn "Dev: run!!!"

parse :: Parser Command
parse =
  subcommand 
  "dev" 
  "Run the dev environment" 
  $ parseCommands

parseCommands :: Parser Command
parseCommands =
  pure Main 
  <|> parseRun
  <|> parseConsole

parseRun :: Parser Command
parseRun =
  subcommand 
  "run"
  "run a command in a one-off container within the stack"
  $ pure Run

parseConsole :: Parser Command
parseConsole =
  subcommand
  "console"
  "Attach to a running container within a stack, and run a command inside. Default to 'bash'"
  $ pure Console