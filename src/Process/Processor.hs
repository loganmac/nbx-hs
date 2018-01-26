module Process.Processor where

import           Concurrency          (Chan, Lock, done, newChan, receive, send,
                                       spawn, wait, waitFor)
import           Control.Monad        (unless)
import           Data.ByteString      (ByteString, hGetLine)
import           GHC.IO.Handle        (Handle)
import           Process.Types        (Output (..), Processor (..))
import           System.Exit          (ExitCode (..))
import           System.IO            (BufferMode (..), hIsEOF)
import           System.Process.Typed (Process, closed, createPipe, getStderr,
                                       getStdin, getStdout, setStderr, setStdin,
                                       setStdout, shell, waitExitCode,
                                       withProcess)

init :: IO Processor
init = do
  c <- newChan
  let p = Processor c
  pure p

run :: Processor -> String -> IO ()
run (Processor chan) cmd = do
  let config = setStdin closed
             $ setStdout createPipe
             $ setStderr createPipe
             $ shell cmd

  withProcess config $ \p -> do
    outLock <- wait
    errLock <- wait
    _ <- spawn $ handleOut chan Msg (getStdout p) outLock
    _ <- spawn $ handleOut chan Err (getStderr p) errLock

    c <- waitExitCode p
    waitFor outLock
    waitFor errLock

    let toResult x = case x of
          ExitSuccess   -> Success
          ExitFailure i -> Failure i
    send chan $ toResult c


handleOut :: Chan Output -> (ByteString -> Output) -> Handle -> Lock -> IO ()
handleOut chan wrap h lock = do
  let loop = do
        done <- hIsEOF h
        unless done $ do
          out <- hGetLine h
          send chan $ wrap out
          loop
  loop
  done lock
