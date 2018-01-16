{-# LANGUAGE OverloadedStrings #-}
module Command.Stack where

import Turtle
import qualified Data.Text as Text

-- a stack command with a mode and a subcommand
data Command = Command Mode Subcommand

-- what mode commands are being run in
data Mode = Dev | Live 
  deriving (Show)

-- a subcommand under `nbx live` or `nbx dev`
data Subcommand 
  = Run RunCommand
  | Evar
  | Alias
  | Destroy
  | Logs
  deriving (Show)

-- aliases for positional args
type Target = Text
type TargetCommand = Text

-- a command under `run`, either with args or empty.
data RunCommand 
  = Start
  | Console Target
  | Execute Target TargetCommand
  deriving (Show)

-- parses either `nbx dev` or `nbx live`
parseCommand :: Parser Command
parseCommand =
  parseDev
  <|> parseLive

-- parses `nbx dev`
parseDev :: Parser Command
parseDev = 
  subcommand 
    "dev"
    "Commands for running a dev environment"
    $ (Command Dev) <$> parseSubcommand

-- parses `nbx live`
parseLive :: Parser Command
parseLive =
  subcommand 
    "live"
    "Commands for running a live environment"
    $ (Command Live) <$> parseSubcommand

-- parses subcommands of `nbx dev ___` or `nbx live ___`
parseSubcommand :: Parser Subcommand
parseSubcommand =
  subcommandGroup "Mode specific commands:"
    [ ( "run"
      , "Run the environment"
      , Run <$> parseRunCommand
      )
    , ( "logs"
      , "View the logs"
      , pure Logs
      )
    , ( "destroy"
      , "Destroy a stack"
      , pure Destroy
      )
    ]

-- Parses either a start command, (no args)
-- or parses a console/execute command (with args)
parseRunCommand :: Parser RunCommand
parseRunCommand =
  pure Start
  <|> parseConsoleExecute

-- Parses for target and a list of [command], then parses to either a
-- Console command, or an execute. If execute, it joins the list with
-- spaces, and parses the list as a single command.
parseConsoleExecute :: Parser RunCommand
parseConsoleExecute =
  argsToCommand <$> parseRunArgs
  where
    parseRunArgs =
      (,) <$> argText "target" "target to console into" 
          <*> many (argText "command" "command to give target")
    argsToCommand (target, args) =
      case args of
        [] -> Console target
        xs -> Execute target (Text.intercalate " " xs)