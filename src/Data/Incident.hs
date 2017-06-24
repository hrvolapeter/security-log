{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module Data.Incident
( Incident(..)
) where

import           Control.Parallel.Strategies (NFData)
import           Data.Log
import           GHC.Generics                (Generic)

data Incident = Incident    { reason :: String
                            , log    :: Log
                            } deriving (Eq, Generic, Show, NFData)

