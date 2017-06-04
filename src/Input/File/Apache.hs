{-# LANGUAGE OverloadedStrings #-}

module Input.File.Apache
  ( Input.File.Apache.parse
  ) where

import           Data.Log
import           Data.Text.Lazy     (Text)
import qualified Data.Text.Lazy     as TL
import           Text.Parsec
import           Text.Parsec.String

parse :: Text -> [Log]
parse a =
  map
    (\x ->
       Log
       {request = TL.toStrict $ getPath x, analyzed = Nothing, meta = Nothing}) $
  TL.lines a

getPath :: Text -> Text
getPath a =
  case Text.Parsec.parse logLine "(Apache parse)" (TL.unpack a) of
    Left _    -> error $ show a
    Right res -> TL.pack res

bracketedValue :: Parser String
bracketedValue = do
  _ <- char '['
  content <- many (noneOf "]")
  _ <- char ']'
  return content

quotedValue :: Parser String
quotedValue = do
  _ <- char '"'
  content <- many (noneOf "\"")
  _ <- char '"'
  return content

plainValue :: Parser String
plainValue = many1 (noneOf " \n")

logLine :: Parser String
logLine = do
  _ <- plainValue
  _ <- space
  _ <- plainValue
  _ <- space
  _ <- plainValue
  _ <- space
  _ <- bracketedValue
  _ <- space
  req <- quotedValue
  _ <- space
  _ <- plainValue
  _ <- space
  _ <- plainValue
  _ <- space
  _ <- quotedValue
  _ <- space
  _ <- quotedValue
  return req
