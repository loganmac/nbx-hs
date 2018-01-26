module Process.Formatter where

import           Concurrency (Chan, Lock, done, newChan, receive, send, spawn,
                              wait, waitFor)

data Formatter = Formatter (Chan Message)

data Message = Indent | Dedent | Log String | Exit Lock

-- Start the formatter
init :: IO Formatter
init = do
  c    <- newChan
  let f = Formatter c
  _    <- spawn $ formatter f
  return f

-- Indent the formatter one level
indent :: Formatter -> IO ()
indent (Formatter c) =
  send c Indent

-- Dedent the formatter one level
dedent :: Formatter -> IO ()
dedent (Formatter c) =
  send c Dedent

-- Log a message from the formatter
log :: Formatter -> String -> IO ()
log (Formatter c) str =
  send c $ Log str

-- Wait for the formatter to stop
stop :: Formatter -> IO ()
stop (Formatter c) = do
  waiter <- wait       -- create a lock
  send c (Exit waiter) -- send it to the formatter
  waitFor waiter       -- wait for it

-- formatter is just a loop of receiving a message and handling it
formatter :: Formatter -> IO ()
formatter (Formatter input) =
  loop 0
  where
    loop lvl = do
      msg    <- receive input
      newLvl <- handleMsg msg lvl
      loop newLvl

-- handleMsg describes how to handle a given message,
-- returning the new indentation level
handleMsg :: Message -> Int -> IO Int
handleMsg msg lvl =
  case msg of
    Indent ->
      return (lvl + 1)
    Dedent ->
      if lvl > 0 then return (lvl - 1) else return lvl
    Log s  -> do
      let indentation = replicate (lvl * 4) ' '
      putStrLn $ indentation ++ s
      return lvl
    Exit waiter -> do
      done waiter -- close waitLock
      return lvl
