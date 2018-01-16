module Run where

import qualified Data.Text as T
import qualified Command as C
import qualified Command.Stack as Stack

command :: C.Command -> IO ()
command cmd =
  case cmd of
    C.Main -> main
    C.Stack mode sub -> putStrLn $ show mode ++ " " ++ stack sub
    C.Init -> putStrLn "INIT!!!"
    C.Version -> displayVersion

main :: IO ()
main = do
  displayVersion
  putStrLn "For help, run 'nbx -h'."

stack :: Stack.Command -> String
stack cmd =
  case cmd of
      Stack.Logs -> "LOGS!"
      Stack.Destroy -> "DESTROY!"
      Stack.Run sub -> stackRun sub

stackRun :: Stack.RunCommand -> String
stackRun cmd =
  case cmd of
    Stack.Start -> "START!"
    Stack.Console target  -> "CONSOLE: " ++ T.unpack target
    Stack.Execute target sub -> "EXECUTE: " ++ T.unpack target ++ ": " ++ T.unpack sub
      
displayVersion ::  IO ()
displayVersion = putStrLn "NBX version 0.0.1"