module Main where

import qualified Cli.Commands as Commands
import qualified Nbx
import           Universum

main :: IO ()
main = do
  Nbx.readConfig
  cmd <- Commands.parse
  case cmd of
    Commands.Main    -> putTextLn "NBX version 0.0.1\nFor help, run 'nbx -h'."
    Commands.Init    -> putTextLn "INIT!"
    Commands.Push    -> Nbx.push
    Commands.Status  -> putTextLn "STATUS!"
    Commands.Setup   -> putTextLn "SETUP!"
    Commands.Implode -> putTextLn "IMPLODE!"
