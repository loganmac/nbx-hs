module Process where

import           System.Exit          (ExitCode (..))
import           System.IO            (hClose, hFlush, hGetLine, hIsEOF,
                                       hPutStrLn)
import           System.Process.Typed (closed, createPipe, getExitCode,
                                       getStderr, getStdin, getStdout,
                                       setStderr, setStdin, setStdout, shell,
                                       withProcess)

run :: String -> IO ()
run cmd = do
  let config = setStdin createPipe
              $ setStdout createPipe
              $ setStderr createPipe
              $ shell cmd

  withProcess config $ \p -> do
    let loop = do
          isLogging   <- not <$> hIsEOF (getStdout p)
          isErroring  <- not <$> hIsEOF (getStderr p)
          if isLogging then do
            out <- hGetLine $ getStdout p
            print out
            loop
          else if isErroring then do
            err <- hGetLine $ getStderr p
            print err
            loop
          else do
            c <- getExitCode p
            maybe loop handleExit c
    loop


handleExit :: ExitCode -> IO ()
handleExit code =
  case code of
    ExitSuccess   -> putStrLn "Success!"
    ExitFailure i -> putStrLn $ "Failure! :( " ++ (show i)
