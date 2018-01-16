module Main where

import qualified Parse
import qualified Run

main :: IO ()
main = do
  -- here we would do things like check the config,
  -- read .nbx.yml, etc.
  cmd <- Parse.command
  Run.command cmd
