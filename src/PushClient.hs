{-# OPTIONS_GHC -funbox-strict-fields #-}
--------------------------------------------------------------------------------
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
module PushClient
    ( mkMessage
    , receiveMessage
    , sendMessage
    , sendReceiveMessage
    , Message (..)
    , ChannelUpdate (..)
    ) where

--------------------------------------------------------------------------------
import           Data.Aeson         (FromJSON, ToJSON, eitherDecode, encode,
                                     genericParseJSON, genericToJSON, parseJSON,
                                     toJSON)
import           Data.Aeson.Types   (defaultOptions, fieldLabelModifier,
                                     omitNothingFields)
import           Data.Text          (Text)
import           GHC.Generics
import qualified Network.WebSockets as WS
import           System.Timeout     (timeout)

-- Generic Message construct for easy JSON encode/decode
data Message = Message { messageType  :: !String
                       , uaid         :: !(Maybe Text)
                       , channelIDs   :: !(Maybe [String])
                       , channelID    :: !(Maybe String)
                       , status       :: !(Maybe Int)
                       , pushEndpoint :: !(Maybe String)
                       , updates      :: !(Maybe [ChannelUpdate])
                       } deriving (Show, Eq, Generic)
instance ToJSON Message where
    toJSON = genericToJSON $ defaultOptions { omitNothingFields = True }
instance FromJSON Message

data ChannelUpdate = ChannelUpdate { cu_channelID :: !String
                                   , cu_version   :: !Int
                                   , cu_data      :: !(Maybe String)
                                   } deriving (Show, Eq, Generic)
instance ToJSON ChannelUpdate where
    toJSON = genericToJSON $ defaultOptions { fieldLabelModifier = drop 3
                                            , omitNothingFields = True }
instance FromJSON ChannelUpdate where
    parseJSON = genericParseJSON $ defaultOptions { fieldLabelModifier = drop 3 }

-- Make an empty message with the default Uaid
mkMessage :: Message
mkMessage = Message "" Nothing Nothing Nothing Nothing Nothing Nothing

receiveMessage :: WS.ClientApp Message
receiveMessage conn = do
    d <- timeout (25*1000000) $ WS.receiveData conn
    case d of
        Nothing -> fail "Timeout waiting for data"
        Just result ->
            either (\e -> fail $ "Error decoding: " ++ e) return $ eitherDecode result

sendReceiveMessage :: Message -> WS.ClientApp Message
sendReceiveMessage msg conn = sendMessage msg conn >> receiveMessage conn

sendMessage :: Message -> WS.ClientApp ()
sendMessage msg conn = do
    result <- timeout (25*1000000) $ WS.sendTextData conn eMsg
    case result of
        Nothing -> fail "Timeout sending data"
        Just _ -> return ()
  where
    eMsg = encode msg
