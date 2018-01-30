{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Command               (Command (..), parse)
import qualified Data.ByteString.Char8 as BC
import           Data.Text             (Text, unpack)
import qualified Print
import qualified Processor             as P

main :: IO ()
main = do
  -- here we would do things like check the config,
  -- read .nbx.yml, etc.
  processor <- P.new
  cmd       <- parse
  execute processor cmd

-- | execute a command
execute :: P.Processor -> Command -> IO ()
execute processor cmd =
  case cmd of
    Main    -> putStrLn "For help, run 'nbx -h'."
    Init    -> putStrLn "INIT!"
    Push    -> pushCmd processor
    Status  -> putStrLn "STATUS!"
    Setup   -> putStrLn "SETUP!"
    Implode -> putStrLn "IMPLODE!"
    Version -> putStrLn "NBX version 0.0.1"

-- | run the push command
-- > nbx push
pushCmd :: P.Processor -> IO ()
pushCmd processor = do
  let task = P.run processor

  Print.header "Setting Up Concert"

  task "Preparing show"   "./test-scripts/good.sh"
  task "Setting up stage" "./test-scripts/good.sh"
  task "Inviting guests"  "./test-scripts/good.sh"
  task "Starting show"    "./test-scripts/bad.sh"

