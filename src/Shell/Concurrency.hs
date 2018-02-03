{- | Wraps a few concurrency primitives in friendlier apis and names
-}
module Shell.Concurrency
  ( Async, Chan, Lock
  , spawn, sleep, second, millisecond
  , newChan, receive, maybeReceive, send
  , newLock, wait, done
  ) where

import           Universum                hiding (second)

import           Control.Concurrent       (threadDelay)
import           Control.Concurrent.Async (Async, async, cancel, link)
import           Control.Concurrent.STM   (TQueue, newTQueue,
                                           readTQueue, tryReadTQueue,
                                           writeTQueue)

--------------------------------------------------------------------------------
-- THREADS

-- | new async thread, linked to calling thread (will rethrow exceptions on linked thread)
spawn :: IO a -> IO (Async a)
spawn x = do
  thread <- async x
  link thread
  pure thread

-- | A unit of time to sleep
newtype TimeUnit = TimeUnit Int

-- | thread sleep a multiple of seconds
sleep :: Int -> TimeUnit -> IO ()
sleep n (TimeUnit measure) = threadDelay $ n * measure

-- | one second
second :: TimeUnit
second = TimeUnit 1000000

-- | one millisecond
millisecond :: TimeUnit
millisecond = TimeUnit 1000


--------------------------------------------------------------------------------
-- CHANNELS

-- | a Channel
type Chan a = TQueue a

-- | create a new channel
newChan :: IO (Chan a)
newChan = atomically newTQueue

-- | read from the channel, blocking with retry
receive :: Chan a -> IO a
receive x = atomically $ readTQueue x

-- | try to read from the channel, returning Nothing if no value available
maybeReceive :: Chan a -> IO (Maybe a)
maybeReceive x = atomically $ tryReadTQueue x

-- | write to the channel
send :: Chan a -> a -> IO ()
send x y = atomically $ writeTQueue x y

--------------------------------------------------------------------------------
-- LOCKS

-- | A lock
type Lock = MVar ()

-- | create a new lock
newLock :: IO Lock
newLock = newEmptyMVar

-- | take the lock, "waiting for" it
wait :: Lock -> IO ()
wait = takeMVar

-- | close the lock
done :: Lock -> IO ()
done x = putMVar x ()

