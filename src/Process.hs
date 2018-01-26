module Process
(run, Processor(..), Output(..))
where

import           Concurrency           (Chan, receive, second, sleep, spawn)
import qualified Data.ByteString.Char8 as BC
import qualified Process.Formatter     as F
import qualified Process.Processor     as P
import           Process.Types         (Output (..), Processor (..))

run :: String -> IO ()
run cmd = do
  processor <- P.init
  _ <- spawn $ P.run processor cmd
  handleProcessOutput processor


handleProcessOutput :: Processor -> IO ()
handleProcessOutput (Processor chan) = do
  let loop = do
        msg <- receive chan
        case msg of
          Msg m     -> do
            putStrLn $ "\x1b[32;1m" ++ BC.unpack m ++ "\x1b[0m"
            loop
          Err m     -> do
            putStrLn $ "\x1b[31;7m" ++ BC.unpack m ++ "\x1b[0m"
            loop
          Success   ->
            putStrLn "Success! :)"
          Failure c ->
            putStrLn $ "Failure! :( " ++ show c
  loop
