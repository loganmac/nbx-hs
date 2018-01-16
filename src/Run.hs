module Run where

import qualified Data.Text as T
import qualified Parse as P

-- run a command
command :: P.Command -> IO ()
command cmd =
  case cmd of
    P.Main -> 
      mainCmd
    P.Modal mode sub -> 
      putStrLn $ "MODE: " ++ show mode ++ "\n" ++ modalCmd sub
    P.Init -> 
      initCmd
    P.Version -> 
      versionCmd

-- run the main command
mainCmd :: IO ()
mainCmd = do
  versionCmd
  putStrLn "For help, run 'nbx -h'."

-- run a modal command like `nbx dev` or `nbx live`
modalCmd :: P.ModalCommand -> String
modalCmd cmd =
  case cmd of
    P.Logs -> "LOGS!"
    P.Destroy -> "DESTROY!"
    P.Run sub -> runCmd sub

-- run a modal run subcommand like `nbx dev run`
runCmd :: P.RunCommand -> String
runCmd cmd =
  case cmd of
    P.Start -> 
      "START!"
    P.Console target  -> 
      "CONSOLE: " ++ T.unpack target
    P.Execute target sub -> 
      "TARGET: " ++ T.unpack target ++ "\n" ++  
      "EXECUTE: " ++ T.unpack sub

initCmd :: IO ()
initCmd = putStrLn "INIT!!!"
      
-- display the version of the CLI
versionCmd ::  IO ()
versionCmd = putStrLn "NBX version 0.0.1"