{-# LANGUAGE OverloadedStrings #-}

module Analyses.XssSpec (main, spec) where

import           Analyses.Xss
import           Data.Log
import           Data.Maybe   (isJust)
import           Helper
import           Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    it "detects script element" $
        analyse (build "<script>alert(1)</script>") `shouldSatisfy` isJust

    it "detects capitalized script element" $
        analyse (build "<SCRIPT>alert(1)</SCRIPT>") `shouldSatisfy` isJust

    it "detects html encoded script element" $
        analyse (build "&#60;&#115;&#99;&#114;&#105;&#112;&#116;>alert(1)</&#115;&#99;&#114;&#105;&#112;&#116;>") `shouldSatisfy` isJust

    it "detects a onload" $
        analyse (build "<a onload=\"\">test</a>") `shouldSatisfy` isJust

    it "detects img script" $
        analyse (build "<img src=\"script(1)\">test</a") `shouldSatisfy` isJust
