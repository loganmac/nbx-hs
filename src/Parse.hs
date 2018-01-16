{-# LANGUAGE OverloadedStrings #-}
module Parse where

import qualified Data.Text as Text
import Turtle

-- Top level command types
data Command 
  = Main 
  | Modal Mode ModalCommand
  | Init 
  | Version

-- what mode the modal commands are being run in
data Mode = Dev | Live 
  deriving (Show)

-- a modal command under `nbx live` or `nbx dev`
data ModalCommand 
  = Run RunCommand
  | Logs  
  | Evar
  | Alias
  | Destroy

-- aliases for positional args
type Target = Text
type TargetCommand = Text

-- a command under `run`, either with args or empty.
data RunCommand 
  = Start
  | Console Target
  | Execute Target TargetCommand

-- parse arguments, return a command
command :: IO Command
command = do
  options "NBX: the Nanobox CLI" parser

-- parsers that turn arguments into commands
parser :: Parser Command
parser = 
  mainCmd <|> initCmd <|> modalCmd <|> versionCmd

-- parse a raw `nbx`
mainCmd :: Parser Command
mainCmd =
  pure Main

-- parse `nbx init`
initCmd :: Parser Command
initCmd =
  subcommand 
    "init" 
    "Initialize a .nbx.yml file for a project" 
    $ pure Init

-- parse `nbx version`
versionCmd :: Parser Command
versionCmd = do
  subcommand
    "version"
    "Display version info"
    $ pure Version

-- parses either `nbx dev` or `nbx live`
modalCmd :: Parser Command
modalCmd =
  dev <|> live
  where
    dev = subcommand 
      "dev"
      "Commands for running a dev environment"
      $ Modal Dev <$> stackCmd
    live = subcommand 
      "live"
      "Commands for running a live environment"
      $ Modal Live <$> stackCmd

-- parses subcommands of `nbx dev ___` or `nbx live ___`
-- like `run`, or `logs`, or `destroy`
stackCmd :: Parser ModalCommand
stackCmd =
    run <|> logs <|> destroy
    where
      run = subcommand 
        "run"
        "Run the environment"
        $ Run <$> runCmd
      logs = subcommand
        "logs"
        "View the logs"
        $ pure Logs
      destroy = subcommand
        "destroy"
        "Destroy a stack"
        $ pure Destroy

-- Parses either a start command, (no args)
-- or parses a console/execute command (with args)
runCmd :: Parser RunCommand
runCmd = pure Start <|> consoleOrExecuteCmd

-- Parses for target and a list of [command], then parses to either a
-- Console command, or an execute. If execute, it joins the list with
-- spaces, and parses the list as a single command.
consoleOrExecuteCmd :: Parser RunCommand
consoleOrExecuteCmd =
  argsToCommand <$> parseRunArgs
  where
    parseRunArgs :: Parser (Text, [Text])
    parseRunArgs =
      (,) <$> argText "target" "target to console into" 
          <*> many (argText "command" "command to give target")

    argsToCommand :: (Target, [Text]) -> RunCommand
    argsToCommand (target, args) =
      case args of
        [] -> Console target
        xs -> Execute target (Text.intercalate " " xs)
