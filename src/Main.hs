module Main where

import qualified Parse
import qualified Run
-- import           Concurrency (sleep, second)
import qualified Format as F

main :: IO ()
main = do
  -- here we would do things like check the config,
  -- read .nbx.yml, etc.
  formatter <- F.init
  cmd       <- Parse.command

  Run.command formatter cmd

  F.stop formatter
