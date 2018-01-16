module Run where

import qualified Command   as C
import qualified Data.Text as T

-- run a command
command :: C.Command -> IO ()
command cmd =
  case cmd of
    C.Main ->
      mainCmd
    C.Init ->
      initCmd
    C.Setup ->
      setupCmd
    C.Implode ->
      implodeCmd
    C.Status ->
      statusCmd
    C.Version ->
      versionCmd
    C.Modal mode sub ->
      modalCmd mode sub

-- run the main command
mainCmd :: IO ()
mainCmd = do
  versionCmd
  putStrLn "For help, run 'nbx -h'."

-- run a modal command like `nbx dev` or `nbx live`
modalCmd :: C.Mode -> C.ModalCommand -> IO ()
modalCmd mode cmd =
  putStrLn $ "MODE: " ++ show mode ++ "\n"
  ++ modalSubCmd cmd

-- run a modal subcommand like `nbx dev logs`
modalSubCmd :: C.ModalCommand -> String
modalSubCmd cmd =
  case cmd of
    C.Logs    -> "LOGS!"
    C.Destroy -> "DESTROY!"
    C.Run sub -> runCmd sub

-- run a modal run subcommand like `nbx dev run`
runCmd :: C.RunCommand -> String
runCmd cmd =
  case cmd of
    C.Start ->
      "START!"
    C.Console target  ->
      "CONSOLE: " ++ T.unpack target
    C.Execute target sub ->
      "TARGET: " ++ T.unpack target ++ "\n" ++
      "EXECUTE: " ++ T.unpack sub

-- run the init command `nbx init`
initCmd :: IO ()
initCmd = putStrLn "INIT!"

-- run the setup command `nbx setup`
setupCmd :: IO ()
setupCmd = putStrLn "SETUP!"

-- run the implode command `nbx implode`
implodeCmd :: IO ()
implodeCmd = putStrLn "IMPLODE!"

-- run the status command `nbx status`
statusCmd :: IO ()
statusCmd = putStrLn "STATUS!"

-- display the version of the CLI
versionCmd ::  IO ()
versionCmd = putStrLn "NBX version 0.0.1"
