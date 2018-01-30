{-| Internals of the `Shell` package, which describe
    a thread that can run external processes,
    and call functions of the `DisplayDriver`
    on process output.
-}
module Shell.Internal where

import           Concurrency          (Chan, Lock, done, maybeReceive, newLock,
                                       receive, send, spawn, wait)
import           Control.Monad        (unless)
import           GHC.IO.Handle        (Handle)
import           Shell.Types          (Cmd, DisplayDriver (..), Output (..),
                                       Processor (..), Task)
import qualified Shell.Types          as DisplayDriver
import           System.Exit          (ExitCode (..), exitWith)
import           System.IO            (hGetLine, hIsEOF)
import           System.Process.Typed (Process, closed, createPipe, getStderr,
                                       getStdout, setStderr, setStdin,
                                       setStdout, shell, waitExitCode,
                                       withProcess)

--------------------------------------------------------------------------------
-- LOOP

-- | the main loop of the processor that awaits a command, runs it, then loops
processor :: Processor -> IO ()
processor (Processor input output) =
  loop
  where
    loop = do
      cmd <- receive input
      runCmd output cmd
      loop

--------------------------------------------------------------------------------
-- RUN COMMAND

-- | executes the given command in the processor
run :: Processor -> DisplayDriver -> Task -> Cmd -> IO ()
run (Processor input output) printer task cmd = do
  send input cmd
  loop 0 []

  where
    -- setup the DisplayDriver aliases
    spinner       = DisplayDriver.spinner printer
    formatOut     = DisplayDriver.formatOut printer
    formatErr     = DisplayDriver.formatErr printer
    printOutput   = DisplayDriver.printOutput printer
    printSuccess  = DisplayDriver.printSuccess printer
    printFailure  = DisplayDriver.printFailure printer
    printWait     = DisplayDriver.printWait printer

    loop :: Int -> [String] -> IO ()
    loop i buffer = do
      spinner i task
      out <- maybeReceive output
      maybe handleNoMsg handleMsg out

      where

        handleMsg :: Output -> IO ()
        handleMsg msg = case msg of
          Msg m -> do
            let out = formatOut m
            printOutput out
            loop (i + 1) (buffer ++ [out])
          Err m -> do
            let err = formatErr m
            printOutput err
            loop (i + 1) (buffer ++ [err])
          Success ->
            printSuccess task
          Failure c -> do
            printFailure task buffer
            exitWith $ ExitFailure c

        handleNoMsg :: IO ()
        handleNoMsg = do
            printWait
            loop (i + 1) buffer

--------------------------------------------------------------------------------
-- | RESPOND TO RUN COMMAND


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
