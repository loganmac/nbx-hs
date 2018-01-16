module Main where

import qualified Parse as Parse
import qualified Run as Run

main :: IO ()
main = do
  -- here we would do things like check the config,
  -- read .nbx.yml, etc.
  cmd <- Parse.command
  Run.command cmd
