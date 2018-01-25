module Process where

import           Concurrency          (Chan, newChan, receive, send, spawn)
import           Control.Monad        (unless)
import qualified Data.ByteString      as B
import           GHC.IO.Handle        (Handle)
import           System.Exit          (ExitCode (..))
import           System.IO            (BufferMode (..), hClose, hFlush,
                                       hGetBuffering, hGetLine, hIsEOF,
                                       hPutStrLn, hSetBuffering)
import           System.Process.Typed (Process, closed, createPipe, getStderr,
                                       getStdin, getStdout, setStderr, setStdin,
                                       setStdout, shell, waitExitCode,
                                       withProcess)

type Code = Int
data Msg = Msg B.ByteString | Err B.ByteString | Success | Failure Code

run :: String -> IO ()
run cmd = do
  processOutput <- newChan

  let config = setStdin closed
             $ setStdout createPipe
             $ setStderr createPipe
             $ shell cmd

  withProcess config $ \p -> do
    _ <- spawn $ handleOut processOutput p
    _ <- spawn $ handleErr processOutput p
    _ <- spawn $ handleExit processOutput p
    handleProcessOutput processOutput

handleProcessOutput :: Chan Msg -> IO ()
handleProcessOutput chan = do
  let loop = do
        msg <- receive chan
        case msg of
          Msg m     -> do
            print m
            loop
          Err m     -> do
            print m
            loop
          Success   ->
            putStrLn "Success! :)"
          Failure c ->
            putStrLn $ "Failure! :( " ++ show c
  loop


handleOut :: Chan Msg -> Process stdin Handle stderr -> IO ()
handleOut chan p = do
  let loop = do
        done <- hIsEOF (getStdout p)
        unless done $ do
          out <- B.hGetLine (getStdout p)
          send chan $ Msg out
          loop
  loop

handleErr :: Chan Msg -> Process stdin stdout Handle -> IO ()
handleErr chan p = do
  let loop = do
        done <- hIsEOF (getStderr p)
        unless done $ do
          out <- B.hGetLine (getStderr p)
          send chan $ Err out
          loop
  loop

handleExit :: Chan Msg -> Process stdin stdout stderr -> IO ()
handleExit chan p = do
  let toResult x = case x of
        ExitSuccess   -> Success
        ExitFailure i -> Failure i

  c <- waitExitCode p
  send chan $ toResult c
