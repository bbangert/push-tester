--------------------------------------------------------------------------------
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
module PushClient
    ( mkMessage
    , receiveMessage
    , sendReceiveMessage
    , Message (..)
    , ChannelUpdate (..)
    ) where

--------------------------------------------------------------------------------
import           Data.Aeson           (FromJSON, ToJSON, eitherDecode, encode,
                                       genericParseJSON, genericToJSON,
                                       parseJSON, toJSON)
import           Data.Aeson.Types     (defaultOptions, fieldLabelModifier,
                                       omitNothingFields)
import           Data.Text            (Text)
import           GHC.Generics
import qualified Network.WebSockets   as WS

-- Generic Message construct for easy JSON encode/decode
data Message = Message { messageType  :: String
                       , uaid         :: Maybe Text
                       , channelIDs   :: Maybe [String]
                       , channelID    :: Maybe String
                       , status       :: Maybe Int
                       , pushEndpoint :: Maybe String
                       , updates      :: Maybe [ChannelUpdate]
                       } deriving (Show, Eq, Generic)
instance ToJSON Message where
    toJSON = genericToJSON $ defaultOptions { omitNothingFields = True }
instance FromJSON Message

data ChannelUpdate = ChannelUpdate { cu_channelID :: String
                                   , cu_version   :: Int
                                   } deriving (Show, Eq, Generic)
instance ToJSON ChannelUpdate where
    toJSON = genericToJSON $ defaultOptions { fieldLabelModifier = drop 3 }
instance FromJSON ChannelUpdate where
    parseJSON = genericParseJSON $ defaultOptions { fieldLabelModifier = drop 3 }

-- Make an empty message with the default Uaid
mkMessage :: Message
mkMessage = Message "" Nothing Nothing Nothing Nothing Nothing Nothing

receiveMessage :: WS.ClientApp Message
receiveMessage conn = do
    d <- WS.receiveData conn
    either (\e -> fail $ "Error decoding: " ++ e) return $ eitherDecode d

sendReceiveMessage :: Message -> WS.ClientApp Message
sendReceiveMessage msg conn = do
  let eMsg = encode msg
  WS.sendTextData conn eMsg
  receiveMessage conn
