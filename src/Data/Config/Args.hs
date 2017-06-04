{- To ensure GHC evalutes attributes the right number of times we disable the CSE optimisation -}
{-# OPTIONS_GHC -fno-cse #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Data.Config.Args
( Args(..)
, defArgs
, etcConfig
) where

import           System.Console.CmdArgs

newtype Args = Args { configPath :: Maybe String
          } deriving (Show, Data, Typeable)

defArgs :: Args
defArgs = Args { configPath = def }

etcConfig :: String
etcConfig = "/etc/security-log/config.yaml"

