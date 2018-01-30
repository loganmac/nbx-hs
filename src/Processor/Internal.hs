module Processor.Internal where

import           Concurrency          (Chan, Lock, done, newLock, receive, send,
                                       spawn, wait)
import           Control.Monad        (unless)
import           GHC.IO.Handle        (Handle)
import           Processor.Types      (Output (..), Processor (..))
import           System.Exit          (ExitCode (..))
import           System.IO            (hGetLine, hIsEOF)
import           System.Process.Typed (Process, closed, createPipe, getStderr,
                                       getStdout, setStderr, setStdin,
                                       setStdout, shell, waitExitCode,
                                       withProcess)

-- | the main loop of the processor that awaits a command, runs it, then loops
processor :: Processor -> IO ()
processor (Processor input output) =
  loop
  where
    loop = do
      cmd <- receive input
      runCmd output cmd
      loop

-- | run the command, putting any stdout, stderr, and exits into the output channel.
-- will wait until stdout and stderr are empty to write the exit code.
runCmd :: Chan Output -> String -> IO ()
runCmd output cmd = do
  let config = setStdin closed
             $ setStdout createPipe
             $ setStderr createPipe
             $ shell cmd

  withProcess config $ \p -> do
    stdoutLock <- newLock
    stderrLock <- newLock
    _ <- spawn $ handleOut output Msg (getStdout p) stdoutLock
    _ <- spawn $ handleOut output Err (getStderr p) stderrLock

    c <- waitExitCode p
    wait stdoutLock
    wait stderrLock

    let toResult x = case x of
          ExitSuccess   -> Success
          ExitFailure i -> Failure i
    send output $ toResult c

-- | read from the handle until it's empty, writing to the result
-- (wrapped in the given wrapper type) to the output channel
-- and then releasing the given lock
handleOut :: Chan Output -> (String -> Output) -> Handle -> Lock -> IO ()
handleOut chan wrap h lock = do
  let loop = do
        done <- hIsEOF h
        unless done $ do
          out <- hGetLine h
          send chan $ wrap out
          loop
  loop
  done lock
