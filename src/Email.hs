{-# LANGUAGE OverloadedStrings #-}

module Email
  ( sendIncidents
  ) where

import           Control.Monad
import           Control.Monad.Reader
import           Data.App
import           Data.Config.Config       (Config (..))
import           Data.Config.OutputConfig (OutputConfig (..))
import           Data.Incident
import           Data.Log
import           Data.Text.Lazy           (Text, concat, fromStrict)
import           Network.Mail.Mime
import           Prelude                  hiding (concat, log)

sendIncidents :: [Incident] -> App ()
sendIncidents [] = return ()
sendIncidents incidents = do
  Email{sender = sEmail, receiver = rEmail} <- asks output
  let sen = Address { addressName = Just "Security Report"
                       , addressEmail = sEmail
                       }
      rec = Address {addressName = Nothing, addressEmail = rEmail}
      body = concat $ map buildMessage incidents
  liftIO $ renderSendMail $ simpleMail' sen rec "Security incidents report" body



buildMessage :: Incident -> Text
buildMessage inc =  concat [ (fromStrict . reason) inc
                           , "\n"
                           , (fromStrict . request . log) inc
                           , "\n\n"
                           ]
