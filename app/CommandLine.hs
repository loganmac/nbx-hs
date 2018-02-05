module Main where

import           CommandLine.Parse (parse)
import           Nbx               (execute)
import           Prelude

main :: IO ()
main = do
  cmd <- parse
  execute cmd
