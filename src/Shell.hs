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
-- spawns a thread to run the processor loop,
-- then partially applies it and the display driver
-- to `execute`, returing a `Shell` that can send commands
-- to the processor loop.
new :: Driver -> IO Shell
new driver = do
  processor <- Processor <$> newChan <*> newChan
  _         <- spawn $ processorLoop processor
  pure $ execute processor driver

--------------------------------------------------------------------------------
-- RUN COMMAND

-- | executes the given command in the processor
execute :: Processor -> Driver -> Task -> Cmd -> IO ()
execute (Processor input output) driver task cmd = do
  send input cmd                               -- send the command to the Processor thread
  loop 0 0 []                                  -- start the output loop with spinner at 0

  where
    loop :: Int -> POSIXTime -> [Text] -> IO ()
    loop lastSpinPos lastSpinTime buffer = do
      spinner driver lastSpinPos task          -- print the spinner
      now <- getPOSIXTime                      -- get the current time
      let (spinPos, spinTime) =
            if now - lastSpinTime >= 0.05      -- if it's been 0.05 seconds
            then (lastSpinPos+1, now)          -- advance the spinner
            else (lastSpinPos, lastSpinTime)   -- otherwise not

      out <- maybeReceive output
      case out of
        Nothing -> do
          threadDelay $ 50 * 1000              -- sleep 50 ms
          toSpinner driver
          loop spinPos spinTime buffer
        Just (Msg m) -> do
          let formatted = formatOut driver m
          handleOutput driver formatted
          loop spinPos spinTime (formatted : buffer)
        Just (Err m) -> do
          let formatted = formatErr driver m
          handleOutput driver formatted
          loop spinPos spinTime (formatted : buffer)
        Just Success -> do
          let formatted = formatSuccess driver task
          handleSuccess driver formatted
        Just (Failure c) -> do
          let formatted = formatFailure driver task
          handleFailure driver task formatted buffer
          exitWith $ ExitFailure c

--------------------------------------------------------------------------------
-- | RESPOND TO RUN COMMAND FROM PROCESSOR THREAD

-- | run the command, putting any stdout, stderr, and exits into the output channel.
-- will wait until stdout and stderr are empty to write the exit code.
processorLoop :: Processor -> IO ()
processorLoop processor@(Processor input output) = do
  cmd <- receive input

  let config = setStdin closed
             $ setStdout createPipe
             $ setStderr createPipe
             $ shell (toString cmd)

  withProcess config $ \p -> do
    stdoutLock <- newEmptyMVar -- create locks for stdout/stderr
    stderrLock <- newEmptyMVar

    _ <- spawn $ handleOut Msg (getStdout p) stdoutLock
    _ <- spawn $ handleOut Err (getStderr p) stderrLock

    code <- waitExitCode p -- wait for the exit and output locks
    takeMVar stdoutLock
    takeMVar stderrLock

    let result = case code of
          ExitSuccess   -> Success
          ExitFailure i -> Failure i

    send output result

  processorLoop processor

  where
    -- | read from the handle until it's empty, writing the result
    -- (wrapped in the given wrapper type) to the output channel
    -- and then releasing the given lock
    handleOut :: (Text -> Output) -> Handle -> MVar () -> IO ()
    handleOut wrap handle lock = do
      let loop = do
            isDone <- hIsEOF handle
            unless isDone $ do
              out <- hGetLine handle
              send output $ wrap out
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

--------------------------------------------------------------------------------
-- CHANNELS

-- | create a new channel
newChan :: IO (TQueue a)
newChan = atomically newTQueue

-- | read from the channel, blocking with retry
receive :: TQueue a -> IO a
receive = atomically . readTQueue

-- | try to read from the channel, returning Nothing if no value available
maybeReceive :: TQueue a -> IO (Maybe a)
maybeReceive = atomically . tryReadTQueue

-- | write to the channel
send :: TQueue a -> a -> IO ()
send x = atomically . writeTQueue x
