{-# LANGUAGE OverloadedStrings #-}

module Input.File.Nginx
  ( Input.File.Nginx.parse
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
  case Text.Parsec.parse logLine "(Nginx parse)" (TL.unpack a) of
    Left _    -> error "could not parse"
    Right res -> TL.pack res

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
  _ <- plainValue
  _ <- space
  _ <- plainValue
  _ <- space
  _ <- plainValue
  _ <- space
  _ <- plainValue
  _ <- space
  _ <- plainValue
  _ <- space
  _ <- plainValue
  _ <- space
  _ <- plainValue
  _ <- space
  _ <- plainValue
  _ <- space
  req <- quotedValue
  _ <- space
-- name
  _ <- quotedValue
  _ <- space
  _ <- plainValue
  _ <- space
  _ <- plainValue
  return req
