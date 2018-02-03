{-| Shell is a thread that can run external processes,
    and call functions of the `Driver`
    on process output.
-}
module Shell
(Driver(..), Shell, new)
where

import           Universum

import           Control.Concurrent       (threadDelay)
import           Control.Concurrent.Async (Async, async, link)
import           Control.Concurrent.STM   (TQueue, newTQueue, readTQueue,
                                           tryReadTQueue, writeTQueue)
import           Control.Monad            (unless)
import           Data.Text.IO             (hGetLine)
import           Data.Time.Clock.POSIX    (POSIXTime, getPOSIXTime)
import           GHC.IO.Handle            (Handle)
import           Shell.Types              (Cmd, Driver, Output (..),
                                           Processor (Processor), Task)
import qualified Shell.Types              as Driver
import           System.Exit              (ExitCode (..))
import           System.IO                (hIsEOF)
import           System.Process.Typed     (closed, createPipe, getStderr,
                                           getStdout, setStderr, setStdin,
                                           setStdout, shell, waitExitCode,
                                           withProcess)

--------------------------------------------------------------------------------
-- TYPES

-- | Driver is a collection of functions that
-- describe what to do on process output
data Driver = Driver
  { formatOut     :: Text -> Text
  , formatErr     :: Text -> Text
  , formatSuccess :: Text -> Text
  , formatFailure :: Text -> Text
  , spinner       :: Int -> Text -> IO ()
  , handleOutput  :: Text -> IO ()
  , handleSuccess :: Text -> IO ()
  , handleFailure :: Text -> Text -> [Text] -> IO ()
  , toSpinner     :: IO ()
  }

-- | The output of running an external process
data Output = Msg Text | Err Text | Success | Failure Int

-- | Processor has an input channel (for sending commands)
-- and an output channel (for reading the output)
data Processor = Processor (TQueue Text) (TQueue Output)

-- | The type for a partially applied `Processor.run`
type Shell = (Task -> Cmd -> IO ())

-- | Task is the description of an external process
type Task = Text

-- | Cmd is the external command like `cat foo` to run
type Cmd = Text

--------------------------------------------------------------------------------
-- API

-- | creates a new processor to run external processes in,
-- then partially applies it and the display driver
-- to `Shell.Internal.run`, returing a `Shell`
new :: Driver -> IO Shell
new driver = do
  p <- mkProcessor
  pure $ run p driver

--------------------------------------------------------------------------------
-- CREATE PROCESSOR

mkProcessor :: IO Processor
mkProcessor = do
  input  <- atomically newTQueue
  output <- atomically newTQueue
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
      cmd <- atomically $ readTQueue input
      runCmd output cmd
      loop

--------------------------------------------------------------------------------
-- RUN COMMAND

-- | executes the given command in the processor
run :: Processor -> Driver -> Task -> Cmd -> IO ()
run (Processor input output) driver task cmd = do
  atomically $ writeTQueue input cmd  -- send the command to the Processor thread

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
      spinner lastSpinPos task                 -- print the spinner
      now <- getPOSIXTime                      -- get the current time
      let (spinPos, spinTime) =
            if now - lastSpinTime >= 0.05      -- if it's been 0.05 seconds
            then (lastSpinPos+1, now)          -- advance the spinner
            else (lastSpinPos, lastSpinTime)   -- otherwise not

      out <- atomically $ tryReadTQueue output -- get the output
      maybe
        (handleNoMsg spinPos spinTime)         -- handle if it's nothing
        (handleMsg spinPos spinTime) out       -- handle if there's ouput

      where
        handleAndLoop :: Int-> POSIXTime -> Text -> IO ()
        handleAndLoop spinPos spinTime str = do
          handleOutput str
          loop spinPos spinTime (str : buffer)

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
          threadDelay $ 50 * 1000 -- sleep 50 ms
          toSpinner
          loop spinPos spinTime buffer

--------------------------------------------------------------------------------
-- | RESPOND TO RUN COMMAND FROM PROCESSOR THREAD

-- | run the command, putting any stdout, stderr, and exits into the output channel.
-- will wait until stdout and stderr are empty to write the exit code.
runCmd :: TQueue Output -> Text -> IO ()
runCmd output cmd = do
  let config = setStdin closed
             $ setStdout createPipe
             $ setStderr createPipe
             $ shell (toString cmd)

  withProcess config $ \p -> do
    stdoutLock <- newEmptyMVar
    stderrLock <- newEmptyMVar
    _ <- spawn $ handleOut output Msg (getStdout p) stdoutLock
    _ <- spawn $ handleOut output Err (getStderr p) stderrLock

    c <- waitExitCode p
    takeMVar stdoutLock -- wait for the locks
    takeMVar stderrLock

    let toResult x = case x of
          ExitSuccess   -> Success
          ExitFailure i -> Failure i

    atomically $ writeTQueue output $ toResult c

-- | read from the handle until it's empty, writing to the result
-- (wrapped in the given wrapper type) to the output channel
-- and then releasing the given lock
handleOut :: TQueue Output -> (Text -> Output) -> Handle -> MVar () -> IO ()
handleOut chan wrap handle lock = do
  let loop = do
        isDone <- hIsEOF handle
        unless isDone $ do
          out <- hGetLine handle
          atomically $ writeTQueue chan $ wrap out
          loop
  loop
  putMVar lock () -- release the lock

--------------------------------------------------------------------------------
-- THREADS

-- | new async thread, linked to calling thread (will rethrow exceptions on linked thread)
spawn :: IO a -> IO (Async a)
spawn x = do
  thread <- async x
  link thread
  pure thread
