module Main (main) where

import           Analyses      (runAll)
import           Config        (loadConfig)
import           Control.Monad (mapM_)
import           Data.Log
import           Database      (search, update)
import           Email

main :: IO ()
main = do
  config <- loadConfig "./security-log.yaml"
  logs <- search config
  let incidents = runAll logs
  -- send only when email is set
  sendIncidents config incidents
  mapM_ (\x -> update config (x { analyzed = Just True })) logs
  return ()
