module Main where

import           CommandLine.Parse (parse)
import           Nbx               (execute)

main :: IO ()
main = do
  cmd <- parse
  execute cmd
