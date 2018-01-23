module Concurrency
  ( Chan, Lock
  , spawn, sleep, second
  , newChan, receive, send
  , wait, waitFor, done
  ) where

import           Control.Concurrent       (MVar, newEmptyMVar, putMVar,
                                           takeMVar, threadDelay)
import           Control.Concurrent.Async (Async, async, link)
import           Control.Concurrent.STM   (TQueue, atomically, newTQueue,
                                           readTQueue, writeTQueue)
-- import           Control.Monad          (forever)

-- THREADS

-- new async thread, linked to calling thread (will rethrow exceptions on linked thread)
spawn :: IO a -> IO (Async a)
spawn x = do
  thread <- async x
  link thread
  pure thread

-- thread sleep
sleep :: Int -> Int -> IO ()
sleep n measure = threadDelay (n * measure)

-- one second
second :: Int
second = 1000000

-- CHANNELS

-- a Channel
type Chan a = TQueue a

-- create a new channel
newChan :: IO (Chan a)
newChan = atomically newTQueue

-- read from the channel
receive :: Chan a -> IO a
receive x = atomically $ readTQueue x

-- write to the channel
send :: Chan a -> a -> IO ()
send x y = atomically $ writeTQueue x y


-- LOCKS
type Lock = MVar ()

-- create a new lock
wait :: IO Lock
wait = newEmptyMVar

-- take the lock, "waiting for" it
waitFor :: Lock -> IO ()
waitFor = takeMVar

-- close the lock
done :: Lock -> IO ()
done x = putMVar x ()


