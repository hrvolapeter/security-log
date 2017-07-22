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
import           Network.HTTP.Base          (urlDecode)
import           Network.HTTP.Types.URI     (decodePath, queryToQueryText)
import           Prelude                    hiding (concat)

analyse :: Log -> Maybe Incident
analyse a
  | validateQuery && validatePath = Nothing
  | otherwise =
    Just Incident {Data.Incident.log = a, reason = "ObjectReference"}
  where
    uri = decodePath . encodeUtf8 . toLower . decodeEntity . request $ a
    decodedQuery = queryToQueryText $ snd uri
    validateQuery = Prelude.foldr ((&&) . isValid) True decodedQuery
    validatePath =
      Prelude.foldr
        (\y x -> x && not (y `isInfixOf` pathsCombined))
        True
        disallowed
      where
        pathsCombined = concat $ fst uri
    isValid (_, Nothing) = True
    isValid (_, Just value) =
      Prelude.foldl (\x y -> x && not (y `isInfixOf` value)) True disallowed
    decodeEntity :: Text -> Text
    decodeEntity =
      toStrict . toLazyText . htmlEncodedText . pack . urlDecode . unpack

disallowed :: [Text]
disallowed = ["etc", "..", "system32"]
