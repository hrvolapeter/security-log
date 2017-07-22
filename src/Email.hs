{-# LANGUAGE OverloadedStrings #-}

module Email
  ( sendIncidents
  ) where

import           Data.Config.OutputConfig (OutputConfig (..))
import           Data.Incident
import           Data.Log
import           Data.Text.Lazy           (concat, fromStrict)
import           Network.Mail.Mime
import           Prelude                  hiding (concat)

sendIncidents :: OutputConfig -> [Incident] -> IO ()
sendIncidents _ [] = return ()
sendIncidents (Email email) inc =
  let sender =
        Address
        { addressName = Just "Security Report"
        , addressEmail = "peterhrvola@Peters-MacBook-Pro.local"
        }
      receiver = Address {addressName = Nothing, addressEmail = email}
      body =
        concat $
        map
          (\i ->
             concat
               [ (fromStrict . reason) i
               , "\n"
               , (fromStrict . request . Data.Incident.log) i
               , "\n\n"
               ])
          inc
  in renderSendMail $
     simpleMail' sender receiver "Security incidents report" body
sendIncidents _ _ = undefined
