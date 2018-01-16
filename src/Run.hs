module Run where

import qualified Data.Text as T
import qualified Command as C
import qualified Command.Stack as Stack

-- run a command
command :: C.Command -> IO ()
command cmd =
  case cmd of
    C.Main -> mainCommand
    C.Stack sub -> stack sub
    C.Init -> putStrLn "INIT!!!"
    C.Version -> displayVersion

-- run the main command
mainCommand :: IO ()
mainCommand = do
  displayVersion
  putStrLn "For help, run 'nbx -h'."

-- run a stack command like `nbx dev` or `nbx live`
stack :: Stack.Command -> IO ()
stack (Stack.Command mode cmd) =
  putStrLn $ show mode ++ " " ++ 
    case cmd of
        Stack.Logs -> "LOGS!"
        Stack.Destroy -> "DESTROY!"
        Stack.Run sub -> stackRun sub

-- run a stack run subcommand like `nbx dev run`
stackRun :: Stack.RunCommand -> String
stackRun cmd =
  case cmd of
    Stack.Start -> 
      "START!"
    Stack.Console target  -> 
      "CONSOLE: " ++ T.unpack target
    Stack.Execute target sub -> 
      "EXECUTE: " ++ T.unpack target ++ ": " ++ T.unpack sub

-- display the version of the CLI
displayVersion ::  IO ()
displayVersion = putStrLn "NBX version 0.0.1"