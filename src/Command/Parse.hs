{-# LANGUAGE OverloadedStrings #-}
module Command.Parse where

import           Command   (Command (..), ModalCommand (..), Mode (..),
                            RunCommand (..), Target, TargetCommand)
import qualified Data.Text as Text
import           Turtle

-- parse arguments, return a command
command :: IO Command
command =
  options "NBX: the Nanobox CLI" parser

-- parsers that turn arguments into commands
parser :: Parser Command
parser =
  mainCmd
  <|> initCmd
  <|> statusCmd
  <|> pushCmd
  <|> modalCmd
  <|> setupCmd
  <|> implodeCmd
  <|> versionCmd

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

-- parse `nbx status`
statusCmd :: Parser Command
statusCmd =
  subcommand
    "status"
    "Display the status of the NBX platform."
    $ pure Status

-- parse `nbx push`
pushCmd :: Parser Command
pushCmd =
  subcommand
    "push"
    "Publish with nanobox."
    $ pure Push

-- parse `nbx setup`
setupCmd :: Parser Command
setupCmd =
  subcommand
    "setup"
    "Install/configure NBX platform."
    $ pure Setup

-- parse `nbx implode`
implodeCmd :: Parser Command
implodeCmd =
  subcommand
    "implode"
    "Delete NBX and all configuration"
    $ pure Implode

-- parse `nbx version`
versionCmd :: Parser Command
versionCmd =
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
      $ Modal Dev <$> modelSub
    live = subcommand
      "live"
      "Commands for running a live environment"
      $ Modal Live <$> modelSub

-- parses subcommands of `nbx dev ___` or `nbx live ___`
-- like `run`, or `logs`, or `destroy`
modelSub :: Parser ModalCommand
modelSub =
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
