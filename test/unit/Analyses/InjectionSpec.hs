{-# LANGUAGE OverloadedStrings #-}

module Analyses.InjectionSpec
  ( main
  , spec
  ) where

import           Analyses.Helper
import           Analyses.Injection
import           Data.Log
import           Data.Maybe         (isJust, isNothing)
import           Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "sql" $ do
    describe "keywords " $ do
      it "detects SQL select" $ analyse (build "SELECT ") `shouldSatisfy` isJust
      it "detects or" $ analyse (build "or ") `shouldSatisfy` isJust
      it "detects upper case or" $ analyse (build "OR ") `shouldSatisfy` isJust
      it "detects upper case and" $
        analyse (build "AND ") `shouldSatisfy` isJust
      it "detects or" $ analyse (build "|| ") `shouldSatisfy` isJust
      it "detects and" $ analyse (build "%26%26 ") `shouldSatisfy` isJust
      it "detects UNION" $ analyse (build "UNION ") `shouldSatisfy` isJust
      it "detects ORDER BY" $ analyse (build "ORDER BY ") `shouldSatisfy` isJust
    describe "database detection" $ do
      it "detects comments #1" $ analyse (build "/*") `shouldSatisfy` isJust
      it "detects comments #2" $ analyse (build "--") `shouldSatisfy` isJust
      it "detects sql limit" $ analyse (build "LIMIT ") `shouldSatisfy` isJust
      it "detects mysql version" $
        analyse (build "version()") `shouldSatisfy` isJust
      it "detects mssql version" $
        analyse (build "@@version") `shouldSatisfy` isJust
      it "detects oracle, postgres string concetation" $
        analyse (build "'||'") `shouldSatisfy` isJust
    describe "blind injection" $ do
      it "detects substring function" $
        analyse (build "SUBSTRING(") `shouldSatisfy` isJust
      it "detects remote connection" $
        analyse (build "UTL_HTTP.request") `shouldSatisfy` isJust
      it "detects time delay" $ analyse (build "sleep(") `shouldSatisfy` isJust
    describe "detection prevention" $ do
      it "detects multiple space" $
        analyse (build "or 'a'  =    'a'") `shouldSatisfy` isJust
      it "detects newline delimeter" $
        analyse (build "or\n'a'= \n      'a' ") `shouldSatisfy` isJust
      it "detects comment delimeter" $
        analyse (build "/**//**/name/**/LIKE/**/") `shouldSatisfy` isJust
      it "detects urlencoded" $
        analyse
          (build
             "%27%20UNION%20SELECT%20password%20FROM%20Users%20WHERE%20name%3D%") `shouldSatisfy`
        isJust
      it "detects characterencoded" $
        analyse (build "char(") `shouldSatisfy` isJust
      it "detects exec function" $
        analyse (build "EXEC(") `shouldSatisfy` isJust
      it "detects unhex function" $
        analyse (build "unhex(") `shouldSatisfy` isJust
      it "detects exec function" $
        analyse (build "EXEC(") `shouldSatisfy` isJust
      it "shouw allow empty query argument" $
        analyse (build "") `shouldSatisfy` isNothing
  describe "command injection" $ do
    it "detects /bin" $ analyse (build "/bin") `shouldSatisfy` isJust
    it "detect $(" $ analyse (build "$(") `shouldSatisfy` isJust
