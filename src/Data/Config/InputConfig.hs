{-# LANGUAGE DeriveGeneric #-}
module Data.Config.InputConfig
  ( InputConfig(..)
  ) where

import           Data.Aeson   hiding (decode, encode)
import           Data.Text    (Text)
import           GHC.Generics (Generic)
import           Prelude      hiding (readFile, writeFile)

data InputConfig
  = Elastic { ip   :: Text
            , size :: Int }
  | File Text
  deriving (Generic, Eq, Show)

instance ToJSON InputConfig where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON InputConfig where
  parseJSON = genericParseJSON defaultOptions
