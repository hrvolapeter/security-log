module Main
  ( main
  ) where

import           Analyses                 (runAll)
import           Control.Concurrent       (threadDelay)
import           Control.Monad            (forever, mapM_)
import           Data.Config.Config       (Config (..), loadConfig)
import qualified Data.Config.InputConfig  as CI
import           Data.Config.OutputConfig (OutputConfig (..))
import           Data.Incident
import           Data.Log
import           Data.Text                (unpack)
import           Email
import           Input.Database           (search, update)
import           Input.File.File          (load)
import           System.Posix.Daemonize   (CreateDaemon (..), serviced,
                                           simpleDaemon)

main :: IO ()
main = do
  config <- loadConfig "./security-log.yaml"
  runProgram config
  return ()

runProgram :: Config -> IO ()
runProgram config
  | asDaemon config =
    serviced $
    simpleDaemon
    { program =
        const $
        forever $ do
          application config
          threadDelay 10000000
    }
  | otherwise = application config

application :: Config -> IO ()
application config = do
  logs <- loadLogs config
  let incidents = runAll logs
  handleIncidents config incidents
  -- mapM_ (\x -> update config (x { analyzed = Just True })) logs
  return ()

handleIncidents :: Config -> [Incident] -> IO ()
handleIncidents Config {output = e@Email {}} incs = sendIncidents e incs
handleIncidents Config {output = Std} incs = print incs
handleIncidents Config {output = File path} incs =
  writeFile (unpack path) (show incs)
handleIncidents _ _ = undefined

loadLogs :: Config -> IO [Log]
loadLogs Config {input = e@CI.Elastic {}} = search e
loadLogs Config {input = CI.File a}       = load a
loadLogs _                                = undefined
