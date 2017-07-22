{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module Data.Incident
  ( Incident(..)
  ) where

import           Control.Parallel.Strategies (NFData)
import           Data.Log
import           Data.Text                   (Text)
import           GHC.Generics                (Generic)

data Incident = Incident
  { reason :: Text
  , log    :: Log
  } deriving (Eq, Generic, Show, NFData)
