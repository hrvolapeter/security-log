{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.Log
  ( Log(..)
  , LogMapping(..)
  , LogMeta(..)
  ) where

import           Control.DeepSeq        (NFData, rnf)
import           Data.Aeson             (FromJSON (..), ToJSON (..), object,
                                         withObject, (.!=), (.:), (.:?), (.=))
import           Data.Text
import           Database.V5.Bloodhound (DocId (..), IndexName (..))
import           GHC.Generics           (Generic)

data Log = Log
  { request  :: Text
  , analyzed :: Maybe Bool
  , meta     :: Maybe LogMeta
  } deriving (Eq, Generic, Show, NFData)

data LogMeta = LogMeta
  { _id    :: DocId
  , _index :: IndexName
  } deriving (Eq, Generic, Show, NFData)

instance FromJSON Log where
  parseJSON =
    withObject "Log" $ \v -> do
      req <- v .: "request"
      ana <- v .:? "analyzed" .!= Nothing
      return Log {request = req, analyzed = ana, meta = Nothing}

instance ToJSON Log where
  toJSON l = object ["request" .= request l, "analyzed" .= analyzed l]

data LogMapping =
  LogMapping
  deriving (Eq, Show)

instance ToJSON LogMapping where
  toJSON LogMapping = object ["properties" .= object []]

instance NFData DocId where
  rnf !_ = ()

instance NFData IndexName where
  rnf !_ = ()
