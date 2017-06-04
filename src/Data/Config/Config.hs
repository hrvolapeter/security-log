{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}


module Data.Config.Config
  ( Config(..)
  , loadConfig
  ) where

import           Control.Exception        (displayException)
import           Data.Aeson               hiding (decode, encode)
import qualified Data.Config.Args         as Arg
import           Data.Config.InputConfig  (InputConfig (..))
import           Data.Config.OutputConfig (OutputConfig (..))
import           Data.Config.ServerType   (ServerType (..))
import           Data.Maybe               (fromMaybe)
import           Data.Monoid              ((<>))
import           Data.Yaml
import           GHC.Generics             (Generic)
import           Prelude                  hiding (readFile, writeFile)
import           System.Console.CmdArgs

---- Data
data Config
  = Config { input      :: InputConfig
           , serverType :: ServerType
           , output     :: OutputConfig
           , asDaemon   :: Bool
           , configPath :: Maybe String
           }
  | NoConfig
  deriving (Generic, Eq, Show)

instance ToJSON Config where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON Config where
  parseJSON = genericParseJSON defaultOptions

instance Monoid Config where
  mempty = NoConfig
  mappend a NoConfig = a
  mappend NoConfig b = b
  mappend _ b =
    Config
    { input = input b
    , output = output b
    , asDaemon = asDaemon b
    , serverType = serverType b
    , configPath = configPath b
    }

----
defaultConfig :: Config
defaultConfig =
  Config
  { input = Elastic {ip = "http://localhost:9200", size = 10}
  , output = Std
  , asDaemon = False
  , serverType = Apache
  , configPath = Nothing
  }

loadConfig :: IO Config
loadConfig = do
  configArgs <- loadArgs
  let configP = fromMaybe Arg.etcConfig (configPath configArgs)
  configFile <- handleMaybeConfig =<< decodeFileEither configP
  return (configArgs <> configFile)

handleMaybeConfig :: Either ParseException Config -> IO Config
handleMaybeConfig (Left ex) = error $ displayException ex
handleMaybeConfig (Right a) = return a

loadArgs :: IO Config
loadArgs = do
  inputArgs <- cmdArgs Arg.defArgs
  return $ defaultConfig {configPath = Arg.configPath inputArgs}

