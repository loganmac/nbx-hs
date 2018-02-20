{-| Commands recognized by the CLI, and how to parse them
-}
module Cli.Commands where

import qualified Data.Text as T
import           Prelude
import           Turtle    (Parser, argText, options, subcommand, (<|>))

--------------------------------------------------------------------------------
-- COMMANDS

-- | Commands that are recognized and parsed by the CLI
data Command
  = Main
  | Init
  | Login
  | Push T.Text
  | Logs T.Text
  | Console T.Text
  | Tunnel T.Text

--------------------------------------------------------------------------------
-- PARSING

-- | parse arguments, return a command
parse :: IO Command
parse =
  options "NBX: the Nanobox CLI" parser

-- | parsers that turn arguments into commands
parser :: Parser Command
parser =
  mainCmd
  <|> initCmd
  <|> loginCmd
  <|> pushCmd
  <|> logsCmd
  <|> consoleCmd
  <|> tunnelCmd

-- | parse a raw `nbx`
mainCmd :: Parser Command
mainCmd =
  pure Main

-- | parse `nbx init`
initCmd :: Parser Command
initCmd =
  subcommand
    "init"
    "Initialize a .nbx.yml file for a project"
    $ pure Init

-- | parse `nbx login`
loginCmd :: Parser Command
loginCmd =
  subcommand
    "login"
    "Login to Nanobox."
    $ pure Login

-- | parse `nbx push`
pushCmd :: Parser Command
pushCmd =
  subcommand
    "push"
    "Publish with nanobox."
    $ Push <$> argText "target" "The target to push to"

-- | parse `nbx logs`
logsCmd :: Parser Command
logsCmd =
  subcommand
    "logs"
    "Read the logs of a target."
    $ Logs <$> argText "target" "The target to view the logs of"

-- | parse `nbx console`
consoleCmd :: Parser Command
consoleCmd =
  subcommand
    "console"
    "Open a console to the target."
    $ Console <$> argText "target" "The target to console to"

-- | parse `nbx tunnel`
tunnelCmd :: Parser Command
tunnelCmd =
  subcommand
    "tunnel"
    "Tunnel into a target."
    $ Tunnel <$> argText "target" "The target to tunnel to"

