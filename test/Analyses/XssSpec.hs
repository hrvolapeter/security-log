{-# LANGUAGE OverloadedStrings #-}

module Analyses.XssSpec (main, spec) where

import           Analyses.Xss
import           Data.Log
import           Data.Maybe   (isJust)
import           Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "analyse" $ do
    it "detects script element" $
        analyse Log {request = "foor?save=<script>alert(1)</script>"
                    , verb = ""
                    , response = ""
                    , _id = Nothing
                    , analysed = Nothing
                    } `shouldSatisfy` isJust

    it "detects capitalized script element" $
        analyse Log {request = "foor?save=<SCRIPT>alert(1)</SCRIPT>"
                    , verb = ""
                    , response = ""
                    , _id = Nothing
                    , analysed = Nothing
                    } `shouldSatisfy` isJust

    it "detects html encoded script element" $
        analyse Log {request = "foor?save=&#60;&#115;&#99;&#114;&#105;&#112;&#116;>alert(1)</&#115;&#99;&#114;&#105;&#112;&#116;>"
                    , verb = ""
                    , response = ""
                    , _id = Nothing
                    , analysed = Nothing
                    } `shouldSatisfy` isJust

    it "detects a onload" $
        analyse Log {request = "/for?send=<a onload=\"\">test</a>"
                    , verb = ""
                    , response = ""
                    , _id = Nothing
                    , analysed = Nothing
                    } `shouldSatisfy` isJust

    it "detects img script" $
        analyse Log {request = "/for?send=<img src=\"script(1)\">test</a>"
                    , verb = ""
                    , response = ""
                    , _id = Nothing
                    , analysed = Nothing
                    } `shouldSatisfy` isJust
