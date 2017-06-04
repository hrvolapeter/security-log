{-# LANGUAGE FlexibleContexts #-}

module Main
  ( main
  ) where

import           Analyses                 (runAll)
import           Control.Concurrent       (threadDelay)
import           Control.Monad            (forever, mapM_)
import           Control.Monad.Reader
import           Data.App
import           Data.Config.Config       (Config (..), loadConfig)
import qualified Data.Config.InputConfig  as CI
import           Data.Config.OutputConfig (OutputConfig (..))
import           Data.Incident
import           Data.Log
import           Data.Text                (unpack)
import           Email
import           Input.Database           (search, update)
import           Input.File.File          (load)
import           Prelude                  hiding (log)
import           System.Posix.Daemonize   (CreateDaemon (..), serviced,
                                           simpleDaemon)


main :: IO ()
main = runProgram =<< loadConfig

runProgram :: Config -> IO ()
runProgram = runReaderT (runApp run)

run :: App ()
run = do
  runAsDaemon <- asks asDaemon
  if runAsDaemon then daemonApplication else application

daemonApplication :: App ()
daemonApplication = do
  config <- ask
  liftIO $ serviced $ simpleDaemon
     { program = const $ forever $ do
           (runReaderT $ runApp application) config
           threadDelay 10000000
     }

application :: App ()
application = do
  logs <- handleInput
  let incidents = runAll logs
  handleAnalysed incidents
  handleOutput incidents
  return ()

handleAnalysed :: [Incident] -> App ()
handleAnalysed logs = do
  inputType <- asks input
  case inputType of
    CI.Elastic{} -> mapM_ (\x -> update ((log x) {analyzed = Just True})) logs
    CI.File{}   -> return ()

handleOutput :: [Incident] -> App ()
handleOutput incs = do
  outputType <- asks output
  case outputType of
    Email{}   -> sendIncidents incs
    Std       -> liftIO $ print incs
    File path -> liftIO $ writeFile (unpack path) (show incs)

handleInput :: App [Log]
handleInput = do
  inputType <- asks input
  case inputType of
    CI.Elastic{} -> search
    CI.File{}    -> load
