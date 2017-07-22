{-# LANGUAGE OverloadedStrings #-}

module Input.File.Apache
  ( Input.File.Apache.parse
  ) where

import           Data.Log
import           Data.Text.Lazy     (Text)
import qualified Data.Text.Lazy     as TL
import           Text.Parsec
import           Text.Parsec.String
import Debug.Trace

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
    Left err  -> undefined
    Right res -> TL.pack res

bracketedValue :: Parser String
bracketedValue = do
  char '['
  content <- many (noneOf "]")
  char ']'
  return content

quotedValue :: Parser String
quotedValue = do
  char '"'
  content <- many (noneOf "\"")
  char '"'
  return content

plainValue :: Parser String
plainValue = many1 (noneOf " \n")

logLine :: Parser String
logLine = do
  ip <- plainValue
  space
  ident <- plainValue
  space
  user <- plainValue
  space
  date <- bracketedValue
  space
  req <- quotedValue
  space
  status <- plainValue
  space
  bytes <- plainValue
  space
  ref <- quotedValue
  space
  ua <- quotedValue
  return req
