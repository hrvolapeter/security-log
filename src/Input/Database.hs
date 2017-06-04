{-# LANGUAGE OverloadedStrings #-}

module Input.Database
  ( search
  , update
  ) where

import           Control.Monad.Reader
import           Data.App
import           Data.Config.Config
import qualified Data.Config.InputConfig as IC
import           Data.Log
import           Data.Maybe              (fromJust, mapMaybe)
import           Data.Text               (Text)
import           Database.V5.Bloodhound
import           GHC.Generics            ()
import           Network.HTTP.Client     (defaultManagerSettings)
import           Prelude                 hiding (log)

---- Settings
logIndex :: IndexName
logIndex = IndexName "_all"

mapping :: MappingName
mapping = MappingName "apache-access"

runBloodHound :: Text -> BH IO a -> IO a
runBloodHound ip = withBH defaultManagerSettings (Server ip)

logFilter :: Filter
logFilter = Filter $ QueryBoolQuery BoolQuery { boolQueryMustNotMatch = [TermQuery (Term "analyzed" "true") Nothing]
                                              , boolQueryMustMatch = []
                                              , boolQueryFilter = []
                                              , boolQueryShouldMatch = []
                                              , boolQueryMinimumShouldMatch = Nothing
                                              , boolQueryBoost = Nothing
                                              , boolQueryDisableCoord = Nothing
                                              }


parseLog :: Either EsError (SearchResult Log) -> [Log]
parseLog (Left e) = error $ show e
parseLog (Right a) = mapMaybe parseHit $ hits $ searchHits a
  where
    parseHit :: Hit Log -> Maybe Log
    parseHit x = (\y -> y {Data.Log.meta = Just $ getMeta x}) <$> hitSource x
    getMeta :: Hit a -> LogMeta
    getMeta h = LogMeta {Data.Log._id = hitDocId h, Data.Log._index = hitIndex h}


search :: App [Log]
search = do
    IC.Elastic ip siz <- asks input
    liftIO $ runBloodHound ip $ do
      let searchQuery = (mkSearch Nothing (Just logFilter)) {size = Size siz}
      searchResult <- searchByType logIndex mapping searchQuery
      return $ parseEsResponse searchResult >>= parseLog

update :: Log -> App ()
update log = do
    IC.Elastic ip _ <- asks input
    liftIO $ runBloodHound ip $ do
      let documentSetting =
            IndexDocumentSettings
            { idsVersionControl = NoVersionControl
            , idsParent = Just $ DocumentParent (extractMeta Data.Log._id)
            }
      _ <- updateDocument (extractMeta Data.Log._index) mapping documentSetting log (extractMeta Data.Log._id)
      return ()
  where
    extractMeta a = a . fromJust . meta $ log
