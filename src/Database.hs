{-# LANGUAGE OverloadedStrings #-}

module Database
( search
, update
) where

import           Config                 (Config (..))
import           Data.Log
import           Data.Maybe             (fromJust, isJust)
import           Database.V5.Bloodhound
import           GHC.Generics           ()
import           Network.HTTP.Client    (defaultManagerSettings)
import           Prelude                hiding (log)


---- Settings
logIndex :: IndexName
logIndex = IndexName "_all"

mapping :: MappingName
mapping = MappingName "apache_access"

search :: Config -> IO [Log]
search cfg = withBH defaultManagerSettings (Server $ ip cfg) $ do
  let filtered = Just $ Filter $ TermQuery (Term "verb" "200") Nothing
  let searchQuery = (mkSearch Nothing filtered ) { Database.V5.Bloodhound.size = Size (fromJust $ Config.size cfg) }
  searchResult <- searchByType logIndex mapping searchQuery
  parse <- parseEsResponse searchResult
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
  _ <- updateDocument logIndex mapping documentSetting log logId
  return ()
    where
      logId = fromJust $ Data.Log._id log
