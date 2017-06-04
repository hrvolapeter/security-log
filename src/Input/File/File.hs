module Input.File.File
  ( load
  ) where

import           Control.Monad.Reader
import           Data.App
import           Data.ByteString              (ByteString)
import           Data.Config.Config
import           Data.Config.InputConfig
import           Data.Config.ServerType       (ServerType (..))
import           Data.Log
import           Data.Maybe                   (maybe)
import           Data.Text                    (unpack)
import           Data.Text.Encoding           (decodeUtf8)
import           Data.Text.Lazy               (Text, fromStrict)
import qualified Input.File.Apache            as IFA
import qualified Input.File.Nginx             as IFN
import qualified System.IO.Streams            as Streams
import qualified System.IO.Streams.ByteString as SISB
import           System.IO.Streams.File       (withFileAsInput)

load :: App [Log]
load = do
  config <- ask
  let File path = input config
  case serverType config of
    Nginx  -> parseWith IFN.parse (unpack path)
    Apache -> parseWith IFA.parse (unpack path)

parseWith :: (Text -> [Log]) -> String -> App [Log]
parseWith f path = liftIO $ do
    line <- withFileAsInput path readLine
    return $ maybe [] (f . fromStrict . decodeUtf8) line

readLine :: Streams.InputStream ByteString -> IO (Maybe ByteString)
readLine b =  SISB.lines b >>= Streams.read
