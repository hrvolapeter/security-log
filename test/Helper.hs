{-# LANGUAGE OverloadedStrings #-}

module Helper
  ( build
  ) where

import           Data.Log
import           Data.Text

build :: Text -> Log
build query =
  Log
  {request = "foor?save=" `append` query, meta = Nothing, analyzed = Nothing}
