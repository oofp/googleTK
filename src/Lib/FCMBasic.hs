{-# LANGUAGE DeriveGeneric #-}

module FCMBasic
 ( FCMSettings (..)
 , FCMMessage (..)
 , FCMNotification (..)
 , sendMessage
 ) where

import Protolude hiding ((&))
import qualified Network.Wreq as NW
import qualified Data.Aeson as DA
import qualified Data.Aeson.Types as DA
import Control.Lens
import Network.HTTP.Types.Status (statusIsSuccessful)
import GHC.Base (String)

newtype FCMSettings = FCMSettings
  { serverKey :: Text -- get it at Firebase/Setting/Cloud Messaging
  }

data FCMNotification = FCMNotification
  { title :: Text
  , body :: Text
  , icon :: Maybe Text
  , click_action :: Maybe Text
  } deriving (Show, Eq, Generic)

data FCMMessage = FCMMessage
  { notification :: FCMNotification
  , time_to_live :: Maybe Int
  , payload :: Maybe DA.Object
  , registration_ids :: [Text]
  } deriving (Show, Eq, Generic)

replaceData :: String -> String
replaceData "payload" = "data"
replaceData a = a

customOptions :: DA.Options
customOptions = DA.defaultOptions
  { DA.fieldLabelModifier = replaceData
  }

instance DA.ToJSON FCMNotification

instance DA.ToJSON FCMMessage where
    toJSON     = DA.genericToJSON customOptions
    toEncoding = DA.genericToEncoding customOptions


sendMessage :: FCMSettings -> FCMMessage -> IO (Either NW.Status NW.Status)
sendMessage fcmSetting fcmMsg = do
  let opts = NW.defaults & NW.header "Authorization" .~ [encodeUtf8 ("key="<>serverKey fcmSetting)]
      msgVal =DA.toJSON fcmMsg
  resp <- NW.postWith opts "https://fcm.googleapis.com/fcm/send" msgVal
  putStrLn ("Resp:" <> show resp::Text)
  let respStatus = resp ^. NW.responseStatus
  return $ (if statusIsSuccessful respStatus then Right else Left) respStatus
