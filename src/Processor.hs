{-| Processor is a thread that runs external processes, and can await the response.
-}
module Processor
(new, run, Processor)
where

import           Concurrency        (Chan, Lock, done, maybeReceive,
                                     millisecond, newChan, newLock, receive,
                                     second, send, sleep, spawn, wait)
import qualified Print
import           Processor.Internal (processor)
import           Processor.Types    (Cmd, Output (..), Processor (..), Task)

-- | creates a new processor
new :: IO Processor
new = do
  input <- newChan
  output <- newChan
  let p = Processor input output
  _ <- spawn $ processor p
  pure p

-- | executes the given command in the processor
run :: Processor -> Task -> Cmd -> IO ()
run (Processor input output) task cmd = do
  send input cmd
  loop 0 []

  where
    loop i buffer = do
      Print.spinner i task
      out <- maybeReceive output
      maybe handleNoMsg handleMsg out
      where
        handleMsg msg = case msg of
          Msg m -> do
            let out = Print.out m
            Print.output out >> loop (i + 1) (out : buffer)
          Err m -> do
            let err = Print.err m
            Print.output err >> loop (i + 1) (err : buffer)
          Success ->
            Print.success task
          Failure c ->
            Print.failure task buffer
        handleNoMsg =
            Print.wait  >> loop (i + 1) buffer

