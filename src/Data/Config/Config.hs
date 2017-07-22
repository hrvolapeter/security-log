{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Config.Config
  ( Config(..)
  , loadConfig
  ) where

import           Control.Exception        (displayException)
import           Data.Aeson               hiding (decode, encode)
import           Data.ByteString
import           Data.Config.InputConfig  (InputConfig (..))
import           Data.Config.OutputConfig (OutputConfig (..))
import           Data.Monoid              ((<>))
import           Data.Text                ()
import           Data.Yaml
import           GHC.Generics             (Generic)
import           Prelude                  hiding (readFile, writeFile)
import           System.Directory         (doesFileExist)

---- Data
data Config
  = Config { input    :: InputConfig
           , output   :: OutputConfig
           , asDaemon :: Bool }
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
    Config {input = input b, output = output b, asDaemon = asDaemon b}

----
defaultConfig :: Config
defaultConfig =
  Config
  { input = Elastic {ip = "http://localhost:9200", size = 10}
  , output = Std
  , asDaemon = False
  }

loadConfig :: FilePath -> IO Config
loadConfig fileName = do
  exists <- doesFileExist fileName
  config <-
    handleMaybeConfig fileName =<<
    if exists
      then decodeFileEither fileName
      else return (Right NoConfig)
  return (defaultConfig <> config)

handleMaybeConfig :: FilePath -> Either ParseException Config -> IO Config
handleMaybeConfig _ (Left ex) = error $ displayException ex
handleMaybeConfig filePath (Right NoConfig) = do
  writeFile filePath $ encode defaultConfig
  return NoConfig
handleMaybeConfig _ (Right a) = return a
