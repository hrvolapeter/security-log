{-# LANGUAGE OverloadedStrings #-}

module Analyses.ObjectReferenceSpec
  ( main
  , spec
  ) where

import           Analyses.ObjectReference
import           Data.Log
import           Data.Maybe               (isJust)
import           Helper
import           Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  it "detect directory traversal #1" $
    analyse (build "../") `shouldSatisfy` isJust
  it "detect directory traversal #2" $
    analyse (build "..\\") `shouldSatisfy` isJust
  it "detect /etc access" $ analyse (build "/etc") `shouldSatisfy` isJust
