module Main where

import           Analyses                    (runAll)
import           Config                      (loadConfig)
import           Control.Monad               (mapM_)
import           Control.Parallel.Strategies (parMap, rdeepseq)
import           Data.Log
import           Database                    (search, update)

main :: IO ()
main = do
  config <- loadConfig "./security-log.yaml"
  logs <- search config
  let a = parMap rdeepseq runAll logs
  print a
  mapM_ (\x -> update config (x { analysed = Just True })) logs
  return ()
