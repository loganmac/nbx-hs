{-| Commands recognized by the CLI, and how to parse them
-}
module Command where

import           Universum
import           Turtle    (Parser, options, subcommand, (<|>))


--------------------------------------------------------------------------------
-- COMMANDS

-- | Commands that are recognized and parsed by the CLI
data Command
  = Main
  | Init
  | Push
  | Status
  | Setup
  | Implode
  | Version

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
  <|> statusCmd
  <|> pushCmd
  <|> setupCmd
  <|> implodeCmd
  <|> versionCmd

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

-- | parse `nbx status`
statusCmd :: Parser Command
statusCmd =
  subcommand
    "status"
    "Display the status of the NBX platform."
    $ pure Status

-- | parse `nbx push`
pushCmd :: Parser Command
pushCmd =
  subcommand
    "push"
    "Publish with nanobox."
    $ pure Push

-- | parse `nbx setup`
setupCmd :: Parser Command
setupCmd =
  subcommand
    "setup"
    "Install/configure NBX platform."
    $ pure Setup

-- | parse `nbx implode`
implodeCmd :: Parser Command
implodeCmd =
  subcommand
    "implode"
    "Delete NBX and all configuration"
    $ pure Implode

-- | parse `nbx version`
versionCmd :: Parser Command
versionCmd =
  subcommand
    "version"
    "Display version info"
    $ pure Version
