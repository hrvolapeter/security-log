module Main where

import           Analyses      (runAll)
import           Config        (Config (), loadConfig)
import           Control.Monad (mapM_)
import           Database      (Log (..), init, search, update)

main :: IO ()
main = do
  config <- loadConfig "./security-log.yaml"
  _ <- Database.init config
  logs <- search config
  print $ map runAll logs
  mapM_ (\x -> update config (x { analysed = True })) logs
  print logs
  return ()
