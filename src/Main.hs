{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Command       (Command (..), ModalCommand (..), Mode,
                                RunCommand (..))
import qualified Command.Parse as Parse
import           Data.Text     (Text, unpack)
import qualified Process

main :: IO ()
main = do
  -- here we would do things like check the config,
  -- read .nbx.yml, etc.

  cmd <- Parse.command
  execute cmd

-- execute a command
execute :: Command -> IO ()
execute cmd =
  case cmd of
    Main           -> putStrLn "For help, run 'nbx -h'."
    Init           -> putStrLn "INIT!"
    Push           -> Process.run "npm i -g vue-cli" --putStrLn "PUSH!"
    Status         -> putStrLn "STATUS!"
    Setup          -> putStrLn "SETUP!"
    Implode        -> putStrLn "IMPLODE!"
    Version        -> putStrLn "NBX version 0.0.1"
    Modal mode sub -> modalCmd mode sub

-- run a modal subcommand like `nbx dev logs`
modalCmd :: Mode -> ModalCommand -> IO ()
modalCmd mode cmd =
  putStrLn $
  "MODE: " ++
  show mode ++
  "\n" ++
  case cmd of
    Logs    -> "LOGS!"
    Destroy -> "DESTROY!"
    Run sub -> runCmd sub

-- a modal run subcommand like `nbx dev run`
runCmd :: RunCommand -> String
runCmd cmd =
  case cmd of
    Start               -> "START!"
    Console target      -> "CONSOLE: " ++ unpack target
    Execute target sub  -> "TARGET: " ++ unpack target ++ "\n" ++ "EXECUTE: " ++ unpack sub

