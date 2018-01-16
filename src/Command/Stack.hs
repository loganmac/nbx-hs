{-# LANGUAGE OverloadedStrings #-}
module Command.Stack where
import Turtle

data Mode = Dev | Live 
  deriving (Show)

data Command 
  = Run RunCommand
  | Evar
  | Alias
  | Destroy
  | Logs
  deriving (Show)

type Target = Text

type TargetCommand = Text

data RunCommand 
  = Start
  | Console Target
  | Execute Target TargetCommand
  deriving (Show)


parseMode :: Parser Mode
parseMode =
  subcommandGroup 
  "Modes:"
  [ ( "dev"
    , "development mode"
    , pure Dev
    )
  , ( "live"
    , "live mode" 
    , pure Live
    )
  ]

parseCommands :: Parser Command
parseCommands =
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

parseRunCommand :: Parser RunCommand
parseRunCommand =
  pure Start
  <|> Console 
    <$> argText "target" "target to console into"
  <|> Execute 
    <$> argText "target" ""
    <*> argText "command" "command to give target"
  