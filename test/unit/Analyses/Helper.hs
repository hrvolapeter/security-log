{-# LANGUAGE OverloadedStrings #-}

module Analyses.Helper
  ( build
  , buildEmpty
  ) where

import           Data.Log
import           Data.Text

build :: Text -> Log
build query = Log {request = "foor?save=" `append` query, meta = Nothing, analyzed = Nothing}

buildEmpty query = Log {request = query, meta = Nothing, analyzed = Nothing}
