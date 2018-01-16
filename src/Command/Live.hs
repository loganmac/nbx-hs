{-# LANGUAGE OverloadedStrings #-}
module Command.Live where
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
    Main -> putStrLn "Live!!!"
    Run -> putStrLn "Live: run!!!"

parse :: Parser Command
parse =
  subcommand 
  "live" 
  "Run the live environment" 
  $ parseCommands

parseCommands :: Parser Command
parseCommands =
  pure Main 
  <|> parseRun

parseRun :: Parser Command
parseRun =
  subcommand 
  "run"
  "run a command in a one-off container within the stack"
  $ pure Run