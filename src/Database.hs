{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Database
( Database.init
, search
, update
, Log(..)
) where

import           Config                 (Config (..))
import qualified Control.Monad          as CM
import           Data.Aeson             (FromJSON (..), defaultOptions,
                                         genericParseJSON, genericToJSON,
                                         object, (.=))
import           Data.Maybe             (fromJust, isJust)
import           Data.Text
import           Database.V5.Bloodhound
import           GHC.Generics           (Generic)
import           Network.HTTP.Client    (defaultManagerSettings, responseBody,
                                         responseStatus)


---- Settings

logIndex = IndexName "logs"
mapping = MappingName "log"
indexSettings = IndexSettings (ShardCount 1) (ReplicaCount 0)

---- Data

data Log = Log
  { log      :: Text
  , analysed :: Bool
  , id       :: Maybe DocId
  } deriving (Eq, Generic, Show)

instance FromJSON Log where
  parseJSON = genericParseJSON defaultOptions

instance ToJSON Log where
  toJSON = genericToJSON defaultOptions


data LogMapping = LogMapping deriving (Eq, Show)

instance ToJSON LogMapping where
  toJSON LogMapping =
    object [ "properties" .= object []]

----

init :: Config -> IO ()
init cfg = withBH defaultManagerSettings (Server $ ip cfg) $ do
  exists <- indexExists logIndex
  CM.unless exists createNewIndex
  -- is really needed?
  --  _ <- putMapping logIndex mapping LogMapping
  return ()
    where
      createNewIndex :: BH IO ()
      createNewIndex = do
        reply <- createIndex indexSettings logIndex
        CM.unless (isSuccess reply) $ error "Error creating index"
        return ()


search :: Config -> IO [Log]
search cfg = withBH defaultManagerSettings (Server $ ip cfg) $ do
  let filter = Just $ Filter $ TermQuery (Term "analysed" "false") Nothing
  let search = (mkSearch Nothing filter ) { Database.V5.Bloodhound.size = Size (fromJust $ Config.size cfg) }
  response <- searchByType logIndex mapping search
  parse <- parseEsResponse response
  return $ parseLog parse
    where
      parseLog :: Either EsError (SearchResult Log) -> [Log]
      parseLog (Left ex) = error $ show (errorMessage ex)
      parseLog (Right a) = Prelude.map fromJust $ Prelude.filter isJust $ Prelude.map parseHit $ hits $ searchHits a
        where
          parseHit :: Hit Log -> Maybe Log
          parseHit x = fmap (\y -> y {Database.id = Just $ hitDocId x} ) (hitSource x)

update :: Config -> Log -> IO ()
update cfg log = withBH defaultManagerSettings (Server $ ip cfg) $ do
  let documentSetting = IndexDocumentSettings { idsVersionControl = NoVersionControl
                                              , idsParent = Just $ DocumentParent logId
                                              }
  updateDocument logIndex mapping documentSetting log logId
  return ()
    where
      logId = fromJust $ Database.id log
