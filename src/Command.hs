module Command where

import           Data.Text (Text)

-- Top level command types
data Command
  = Main
  | Init
  | Push
  | Status
  | Setup
  | Implode
  | Version
  | Modal Mode ModalCommand

-- what mode the modal commands are being run in
data Mode = Dev | Live
  deriving (Show)

-- a modal command under `nbx live` or `nbx dev`
data ModalCommand
  = Logs
  | Destroy
  | Run RunCommand

-- aliases for positional args
type Target = Text
type TargetCommand = Text

-- a command under `run`, either with args or empty.
data RunCommand
  = Start
  | Console Target
  | Execute Target TargetCommand
