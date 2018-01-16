module Main where

import qualified Command as Command
import qualified Run as Run

main :: IO ()
main = do
  -- here we would do things like check the config,
  -- read .nbx.yml, etc.
  cmd <- Command.parse
  Run.command cmd
  