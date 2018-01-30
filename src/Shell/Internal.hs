{-| Internals of the `Shell` package, which describe
    a thread that can run external processes,
    and call functions of the `DisplayDriver`
    on process output.
-}
module Shell.Internal where

import           Concurrency           (Chan, Lock, done, maybeReceive, newLock,
                                        receive, send, spawn, wait)
import           Control.Monad         (unless)
import           Data.Time.Clock.POSIX (POSIXTime, getPOSIXTime)
import           GHC.IO.Handle         (Handle)
import           Shell.Types           (Cmd, DisplayDriver (..), Output (..),
                                        Processor (..), Task)
import qualified Shell.Types           as DisplayDriver
import           System.Exit           (ExitCode (..), exitWith)
import           System.IO             (hGetLine, hIsEOF)
import           System.Process.Typed  (Process, closed, createPipe, getStderr,
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
  send input cmd  -- send the command to the Processor thread

  loop 0 0 []     -- start the output loop with spinner zeroed out

  where
    -- setup the DisplayDriver functions from the printer
    spinner       = DisplayDriver.spinner       printer
    formatOut     = DisplayDriver.formatOut     printer
    formatErr     = DisplayDriver.formatErr     printer
    printOutput   = DisplayDriver.printOutput   printer
    printSuccess  = DisplayDriver.printSuccess  printer
    printFailure  = DisplayDriver.printFailure  printer
    printWait     = DisplayDriver.printWait     printer

    loop :: Int -> POSIXTime -> [String] -> IO ()
    loop lastSpinPos lastSpinTime buffer = do
      spinner lastSpinPos task               -- print the spinner
      now <- getPOSIXTime                    -- get the current time
      let (spinPos, spinTime) =
            if now - lastSpinTime >= 0.05    -- if it's been 0.05 seconds
            then (lastSpinPos+1, now)        -- advance the spinner
            else (lastSpinPos, lastSpinTime) -- otherwise not

      out <- maybeReceive output             -- get the output
      maybe
        (handleNoMsg spinPos spinTime)       -- handle if it's nothing
        (handleMsg spinPos spinTime) out     -- handle if there's ouput

      where

        handleMsg :: Int -> POSIXTime -> Output -> IO ()
        handleMsg spinPos spinTime msg = case msg of
          Msg m -> do
            let out       = formatOut m
            let newBuffer = out : buffer
            printOutput out
            loop spinPos spinTime newBuffer
          Err m -> do
            let err       = formatErr m
            let newBuffer = err : buffer
            printOutput err
            loop spinPos spinTime newBuffer
          Success ->
            printSuccess task
          Failure c -> do
            printFailure task buffer
            exitWith $ ExitFailure c

        handleNoMsg :: Int -> POSIXTime -> IO ()
        handleNoMsg spinPos spinTime = do
            printWait
            loop spinPos spinTime buffer

--------------------------------------------------------------------------------
-- | RESPOND TO RUN COMMAND FROM PROCESSOR THREAD

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
