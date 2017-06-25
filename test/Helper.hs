{-# LANGUAGE OverloadedStrings #-}

module Helper
( build
) where

import           Data.Log
import           Data.Text

build :: Text -> Log
build query = Log {request = "foor?save=" `append` query
                    , verb = ""
                    , response = ""
                    , _id = Nothing
                    , analysed = Nothing
                    , referrer = ""
                    }
