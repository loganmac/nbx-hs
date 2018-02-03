{-| Internals of the `Shell` package, which describe
    a thread that can run external processes,
    and call functions of the `Driver`
    on process output.
-}
module Shell.Internal where

import           Universum

import           Control.Monad         (unless)
import           Data.Time.Clock.POSIX (POSIXTime, getPOSIXTime)
import           GHC.IO.Handle         (Handle)
import           Shell.Concurrency     (Chan, Lock, done, maybeReceive,
                                        millisecond, newChan, newLock, receive,
                                        send, sleep, spawn, wait)
import           Shell.Types           (Cmd, Driver (..), Output (..),
                                        Processor (..), Task)
import qualified Shell.Types           as Driver
import           System.Exit           (ExitCode (..))
import           System.IO             (hGetLine, hIsEOF)
import           System.Process.Typed  (Process, closed, createPipe, getStderr,
                                        getStdout, setStderr, setStdin,
                                        setStdout, shell, waitExitCode,
                                        withProcess)

--------------------------------------------------------------------------------
-- CREATE PROCESSOR

mkProcessor :: IO Processor
mkProcessor = do
  input  <- newChan
  output <- newChan
  let p = Processor input output
  _ <- spawn $ processor p
  pure p

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
run :: Processor -> Driver -> Task -> Cmd -> IO ()
run (Processor input output) driver task cmd = do
  send input cmd  -- send the command to the Processor thread

  loop 0 0 []     -- start the output loop with spinner zeroed out

  where
    -- setup the Driver functions from the driver
    spinner        = Driver.spinner       driver
    formatOut      = Driver.formatOut     driver
    formatErr      = Driver.formatErr     driver
    formatSuccess  = Driver.formatSuccess driver
    formatFailure  = Driver.formatFailure driver
    handleOutput   = Driver.handleOutput  driver
    handleSuccess  = Driver.handleSuccess driver
    handleFailure  = Driver.handleFailure driver
    toSpinner      = Driver.toSpinner     driver

    loop :: Int -> POSIXTime -> [Text] -> IO ()
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
        handleAndLoop :: Int-> POSIXTime -> Text -> IO ()
        handleAndLoop spinPos spinTime str = do
          handleOutput str
          loop spinPos spinTime ([str] <> buffer)

        handleMsg :: Int -> POSIXTime -> Output -> IO ()
        handleMsg spinPos spinTime msg = case msg of
          Msg m   -> handleAndLoop spinPos spinTime $ formatOut m
          Err m   -> handleAndLoop spinPos spinTime $ formatErr m
          Success -> handleSuccess $ formatSuccess task
          Failure c -> do
            handleFailure task (formatFailure task) buffer
            exitWith $ ExitFailure c

        handleNoMsg :: Int -> POSIXTime -> IO ()
        handleNoMsg spinPos spinTime = do
          sleep 50 millisecond
          toSpinner
          loop spinPos spinTime buffer

--------------------------------------------------------------------------------
-- | RESPOND TO RUN COMMAND FROM PROCESSOR THREAD

-- | run the command, putting any stdout, stderr, and exits into the output channel.
-- will wait until stdout and stderr are empty to write the exit code.
runCmd :: Chan Output -> Text -> IO ()
runCmd output cmd = do
  let config = setStdin closed
             $ setStdout createPipe
             $ setStderr createPipe
             $ shell (toString cmd)

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
handleOut :: Chan Output -> (Text -> Output) -> Handle -> Lock -> IO ()
handleOut chan wrap h lock = do
  let loop = do
        done <- hIsEOF h
        unless done $ do
          out <- hGetLine h
          send chan $ wrap (toText out)
          loop
  loop
  done lock
