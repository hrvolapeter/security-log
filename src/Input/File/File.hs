module Input.File.File
  ( load
  ) where

import           Data.Log
import           Data.Text         (Text, unpack)
import qualified Data.Text.Lazy.IO as TLI
import           Input.File.Apache (parse)

load :: Text -> IO [Log]
load path = do
  t <- TLI.readFile (unpack path)
  return $ parse t
