{-# LANGUAGE OverloadedStrings #-}

module Analyses.ObjectReference
( analyse
) where

import           Data.Incident
import           Data.Log
import           Data.Text
import           Data.Text.Encoding         (encodeUtf8)
import           Data.Text.Internal.Builder (toLazyText)
import           Data.Text.Lazy             (toStrict)
import           HTMLEntities.Decoder       (htmlEncodedText)
import           Network.HTTP.Types.URI     (parseQueryText)

analyse :: Log -> Maybe Incident
analyse a
    | validateQuery decodedQuery = Nothing
    | otherwise = Just Incident { Data.Incident.log = a, reason = "ObjectReference" }
    where
        decodedQuery = parseQueryText . encodeUtf8 . toLower . decodeEntity . request $ a

        validateQuery = Prelude.foldr ((&&) . isValid) True

        isValid (_, Nothing) = True
        isValid (_, Just value) = Prelude.foldl (\x y -> x && not (y `isInfixOf` value) ) True disallowed

        decodeEntity :: Text -> Text
        decodeEntity = toStrict . toLazyText . htmlEncodedText

disallowed :: [Text]
disallowed = [ "/etc"
             , "..\\"
             , "../"
             ]
