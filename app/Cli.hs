module Main where

import qualified Cli.Commands as Commands
import qualified Nbx
import           System.IO    as IO
import           Universum

main :: IO ()
main = do
  IO.hSetEncoding IO.stdout IO.utf8
  IO.hSetEncoding IO.stderr IO.utf8

  -- Nbx.verifyDocker
  -- Nbx.readConfig
  cmd <- Commands.parse
  case cmd of
    Commands.Main         -> putTextLn "NBX version 0.0.1\nFor help, run 'nbx -h'."
    Commands.Init         -> putTextLn "INIT!"
    Commands.Push     tar -> Nbx.push tar
    Commands.Logs    _tar -> putTextLn "LOGS!"
    Commands.Console _tar -> putTextLn "CONSOLE!"
    Commands.Tunnel  _tar -> putTextLn "TUNNEL!"
    Commands.Login        -> putTextLn "LOGIN!"


