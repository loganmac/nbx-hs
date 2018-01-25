module Process where

import           Control.Concurrent
import           Control.Concurrent.Async (Concurrently (..))
import qualified Data.Text                as T

import           Data.Conduit
import qualified Data.Conduit.Binary      as CB
import qualified Data.Conduit.List        as CL
import           Data.Conduit.Process     (ClosedStream (..), shell,
                                           streamingProcess,
                                           waitForStreamingProcess)
import qualified Data.Conduit.Text        as CT
import           GHC.IO.Exception         (ExitCode (..))

data Msg = Quit | Msg T.Text | Error T.Text

-- run :: Chan Msg -> Int -> String -> IO ()
-- run chan i cmd = do

run :: String -> IO ()
run cmd = do
  (ClosedStream, fromProcess, fromProcessErr, p) <- streamingProcess (shell cmd)

  let output h action =
        CB.sourceHandle h $$  -- create conduit source from the handle (i.e. stdout, stderr)
        CT.decode CT.utf8 =$= -- decode the bytestring as utf8 into Text
        CT.lines =$=          -- split the text into a stream of lines
        CL.mapM_ action       -- map the stream over the action
        -- CL.mapM_ (writeChan chan . Msg)

  let stdout = output fromProcess (putStrLn . T.unpack)
  let stderr = output fromProcessErr (putStrLn .  (\s -> "\ESC[35;1m" ++ s ++ "\ESC[0m\n") . T.unpack)

  code <- runConcurrently $ -- run all three conduits, and capture exit code.
    Concurrently stdout *>
    Concurrently stderr *>
    Concurrently (waitForStreamingProcess p)

  case code of
    ExitFailure x -> putStrLn "failure! D:"
    ExitSuccess   -> putStrLn "success! :)"

  putStrLn "exit"
  -- writeChan chan Quit
