{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.Log
( Log(..)
) where

import           Control.DeepSeq        (NFData, rnf)
import           Data.Aeson             (FromJSON (..), ToJSON (..),
                                         defaultOptions, genericParseJSON,
                                         genericToJSON, object, (.=))
import           Data.Text
import           Database.V5.Bloodhound (DocId (..))
import           GHC.Generics           (Generic)


data Log = Log
  { request  :: Text
  , verb     :: Text
  , response :: Text
  , _id      :: Maybe DocId
  , analysed :: Maybe Bool
  , referrer :: Text
  } deriving (Eq, Generic, Show, NFData)


instance FromJSON Log where
  parseJSON = genericParseJSON defaultOptions

instance ToJSON Log where
  toJSON = genericToJSON defaultOptions


data LogMapping = LogMapping deriving (Eq, Show)

instance ToJSON LogMapping where
  toJSON LogMapping =
    object [ "properties" .= object []]

instance NFData DocId where
  rnf !_ = ()
