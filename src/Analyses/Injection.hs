{-# LANGUAGE OverloadedStrings #-}

module Analyses.Injection
( analyse
) where

import           Data.Incident
import           Data.Log
import           Data.Text                  hiding (strip)
import           Data.Text.Encoding         (encodeUtf8)
import           Data.Text.Internal.Builder (toLazyText)
import           Data.Text.Lazy             (toStrict)
import           HTMLEntities.Decoder       (htmlEncodedText)
import           Network.HTTP.Types.URI     (parseQueryText)

analyse :: Log -> Maybe Incident
analyse a
    | validateQuery decodedQuery = Nothing
    | otherwise = Just Incident { Data.Incident.log = a, reason = "Injection" }
    where
        decodedQuery = parseQueryText . encodeUtf8 . strip . toLower . request $ a

        validateQuery = Prelude.foldr ((&&) . isValid) True

        isValid (_, Nothing) = True
        isValid (_, Just value) = Prelude.foldl (\x y -> x && not (y `isInfixOf` decodeEntity value) ) True disallowed

        decodeEntity :: Text -> Text
        decodeEntity = toStrict . toLazyText . htmlEncodedText

        strip :: Text -> Text
        strip = Data.Text.map (\x -> if (let o = fromEnum x in o > 31 && o < 126) then x else ' ')

disallowed :: [Text]
disallowed = [ "select "
             , "or "
             , "and "
             , "union "
             , "order "
             , "|| "
             , "&& "
             , "/*"
             , "--"
             , "limit "
             , "version("
             , "@@version"
             , "'||'"
             , "substring("
             , "utl_http.request"
             , "sleep("
             , "char("
             , "exec("
             , "unhex("
             , "/bin"
             , "$("
             , "shutdown("
             , "pg_"
             ]
