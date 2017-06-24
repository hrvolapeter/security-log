{-# LANGUAGE OverloadedStrings #-}

module Database
( search
, update
) where

import           Config                 (Config (..))
import qualified Control.Monad          as CM
import           Data.Log
import           Data.Maybe             (fromJust, isJust)
import           Data.Text
import           Database.V5.Bloodhound
import           GHC.Generics           (Generic)
import           Network.HTTP.Client    (defaultManagerSettings, responseBody,
                                         responseStatus)


---- Settings

logIndex = IndexName "_all"
mapping = MappingName "apache_access"
indexSettings = IndexSettings (ShardCount 1) (ReplicaCount 0)

search :: Config -> IO [Log]
search cfg = withBH defaultManagerSettings (Server $ ip cfg) $ do
  let filter = Just $ Filter $ TermQuery (Term "verb" "200") Nothing
  let search = (mkSearch Nothing Nothing ) { Database.V5.Bloodhound.size = Size (fromJust $ Config.size cfg) }
  response <- searchByType logIndex mapping search
  parse <- parseEsResponse response
  return $ parseLog parse
    where
      parseLog :: Either EsError (SearchResult Log) -> [Log]
      parseLog (Left ex) = error $ show (errorMessage ex)
      parseLog (Right a) = Prelude.map fromJust $ Prelude.filter isJust $ Prelude.map parseHit $ hits $ searchHits a
        where
          parseHit :: Hit Log -> Maybe Log
          parseHit x = fmap (\y -> y {Data.Log._id = Just $ hitDocId x} ) (hitSource x)


update :: Config -> Log -> IO ()
update cfg log = withBH defaultManagerSettings (Server $ ip cfg) $ do
  let documentSetting = IndexDocumentSettings { idsVersionControl = NoVersionControl
                                              , idsParent = Just $ DocumentParent logId
                                              }
  updateDocument logIndex mapping documentSetting log logId
  return ()
    where
      logId = fromJust $ Data.Log._id log
