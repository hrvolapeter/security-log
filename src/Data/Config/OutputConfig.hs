{-# LANGUAGE DeriveGeneric #-}

module Data.Config.OutputConfig
  ( OutputConfig(..)
  ) where

import           Data.Aeson   hiding (decode, encode)
import           Data.Text    (Text)
import           GHC.Generics (Generic)
import           Prelude      hiding (readFile, writeFile)

data OutputConfig
  = Email { receiver :: Text
          , sender   :: Text
          }
  | File Text
  | Std
  deriving (Generic, Eq, Show)

instance ToJSON OutputConfig where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON OutputConfig where
  parseJSON = genericParseJSON defaultOptions
