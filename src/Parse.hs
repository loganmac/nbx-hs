{-# LANGUAGE OverloadedStrings #-}
module Parse where

import qualified Command   as C
import qualified Data.Text as Text
import           Turtle


-- parse arguments, return a command
command :: IO C.Command
command =
  options "NBX: the Nanobox CLI" parser

-- parsers that turn arguments into commands
parser :: Parser C.Command
parser =
  mainCmd
  <|> initCmd
  <|> modalCmd
  <|> setupCmd
  <|> implodeCmd
  <|> statusCmd
  <|> versionCmd

-- parse a raw `nbx`
mainCmd :: Parser C.Command
mainCmd =
  pure C.Main

-- parse `nbx init`
initCmd :: Parser C.Command
initCmd =
  subcommand
    "init"
    "Initialize a .nbx.yml file for a project"
    $ pure C.Init

-- parse `nbx setup`
setupCmd :: Parser C.Command
setupCmd =
  subcommand
    "setup"
    "Install/configure NBX platform."
    $ pure C.Setup

-- parse `nbx implode`
implodeCmd :: Parser C.Command
implodeCmd =
  subcommand
    "implode"
    "Delete NBX and all configuration"
    $ pure C.Implode

-- parse `nbx status`
statusCmd :: Parser C.Command
statusCmd =
  subcommand
    "status"
    "Display the status of the NBX platform."
    $ pure C.Status

-- parse `nbx version`
versionCmd :: Parser C.Command
versionCmd =
  subcommand
    "version"
    "Display version info"
    $ pure C.Version

-- parses either `nbx dev` or `nbx live`
modalCmd :: Parser C.Command
modalCmd =
  dev <|> live
  where
    dev = subcommand
      "dev"
      "Commands for running a dev environment"
      $ C.Modal C.Dev <$> modelSub
    live = subcommand
      "live"
      "Commands for running a live environment"
      $ C.Modal C.Live <$> modelSub

-- parses subcommands of `nbx dev ___` or `nbx live ___`
-- like `run`, or `logs`, or `destroy`
modelSub :: Parser C.ModalCommand
modelSub =
    run <|> logs <|> destroy
    where
      run = subcommand
        "run"
        "Run the environment"
        $ C.Run <$> runCmd
      logs = subcommand
        "logs"
        "View the logs"
        $ pure C.Logs
      destroy = subcommand
        "destroy"
        "Destroy a stack"
        $ pure C.Destroy

-- Parses either a start command, (no args)
-- or parses a console/execute command (with args)
runCmd :: Parser C.RunCommand
runCmd = pure C.Start <|> consoleOrExecuteCmd

-- Parses for target and a list of [command], then parses to either a
-- Console command, or an execute. If execute, it joins the list with
-- spaces, and parses the list as a single command.
consoleOrExecuteCmd :: Parser C.RunCommand
consoleOrExecuteCmd =
  argsToCommand <$> parseRunArgs
  where
    parseRunArgs :: Parser (Text, [Text])
    parseRunArgs =
      (,) <$> argText "target" "target to console into"
          <*> many (argText "command" "command to give target")

    argsToCommand :: (C.Target, [Text]) -> C.RunCommand
    argsToCommand (target, args) =
      case args of
        [] -> C.Console target
        xs -> C.Execute target (Text.intercalate " " xs)
