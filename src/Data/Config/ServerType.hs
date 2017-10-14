{-# LANGUAGE DeriveGeneric #-}

module Data.Config.ServerType
  ( ServerType(..)
  ) where

import           Data.Aeson   hiding (decode, encode)
import           GHC.Generics (Generic)
import           Prelude      hiding (readFile, writeFile)

data ServerType
  = Apache
  | Nginx
  deriving (Generic, Eq, Show)

instance ToJSON ServerType where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON ServerType where
  parseJSON = genericParseJSON defaultOptions
