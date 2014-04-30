--------------------------------------------------------------------------------
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
module PushClient
    ( mkMessage
    , defaultUaid
    , defaultChannelID
    , receiveMessage
    , sendReceiveMessage
    , Message (..)
    , ChannelUpdate (..)
    ) where

--------------------------------------------------------------------------------
import           Control.Concurrent   (forkIO)
import           Control.Exception    (assert)
import           Control.Monad        (void)
import           Control.Monad.Trans  (liftIO)
import           Data.Aeson           (FromJSON, ToJSON, eitherDecode, encode,
                                       genericParseJSON, genericToJSON,
                                       parseJSON, toJSON)
import           Data.Aeson.Types     (defaultOptions, fieldLabelModifier,
                                       omitNothingFields)
import qualified Data.ByteString.Lazy as BL
import           Data.Maybe           (fromJust)
import           Data.Text            (Text)
import           GHC.Generics
import qualified Network.WebSockets   as WS
import           Network.Wreq         (put)

-- Generic Message construct for easy JSON encode/decode
data Message = Message { messageType  :: String
                       , uaid         :: Maybe Text
                       , channelIDs   :: Maybe [String]
                       , channelID    :: Maybe Text
                       , status       :: Maybe Int
                       , pushEndpoint :: Maybe String
                       , updates      :: Maybe [ChannelUpdate]
                       } deriving (Show, Generic)
instance ToJSON Message where
    toJSON = genericToJSON $ defaultOptions { omitNothingFields = True }
instance FromJSON Message

data ChannelUpdate = ChannelUpdate { cu_channelID :: String
                                   , cu_version   :: Int
                                   } deriving (Show, Generic)
instance ToJSON ChannelUpdate where
    toJSON = genericToJSON $ defaultOptions { fieldLabelModifier = drop 3 }
instance FromJSON ChannelUpdate where
    parseJSON = genericParseJSON $ defaultOptions { fieldLabelModifier = drop 3 }

-- Make an empty message with the default Uaid
mkMessage :: Message
mkMessage = Message "" Nothing Nothing Nothing Nothing Nothing Nothing

defaultUaid :: Text
defaultUaid = "decafbad-0000-0000-0000-000000000000"

defaultChannelID :: Text
defaultChannelID = "deadbeef-0000-0000-0000-000000000000"

receiveMessage :: WS.ClientApp Message
receiveMessage conn = do
    d <- WS.receiveData conn
    liftIO $ BL.putStrLn $ BL.append "<<< Recv'd:: " d
    either (\e -> fail $ "Error decoding: " ++ e) return $ eitherDecode d

sendReceiveMessage :: Message -> WS.ClientApp Message
sendReceiveMessage msg conn = WS.sendTextData conn (encode msg) >> receiveMessage conn

-- sendHello :: WS.ClientApp ()
-- sendHello conn = do
--     liftIO $ putStrLn ">>> Sending 'hello'"
--     let helloMsg = mkMessage { messageType="hello", channelIDs=Just [] }
--     msg <- sendReceiveMessage helloMsg conn
--     assert (uaid msg == Just defaultUaid) $ return ()
--
-- registerChid :: WS.ClientApp String
-- registerChid conn = do
--     liftIO $ putStrLn ">>> Registering channel"
--     let regMsg = mkMessage { messageType="register", channelID=Just defaultChannelID }
--     msg <- sendReceiveMessage regMsg conn
--     assert (channelID msg == Just defaultChannelID)
--            (return . fromJust $ pushEndpoint msg)
--
-- readUpdateNotification :: WS.ClientApp ()
-- readUpdateNotification conn = do
--     msg <- receiveMessage conn
--     assert (messageType msg == "notification") (return ())
--     let Just chUpdates = updates msg
--     assert (length chUpdates == 1) $ do
--         liftIO $ putStrLn "SUCCESS!!! Exiting..."
--         return ()
--
-- unregisterChid :: WS.ClientApp ()
-- unregisterChid conn = do
--     liftIO $ putStrLn ">>> Unregistering channel"
--     let unregMsg = encode $ mkMessage { messageType="unregister", channelID=Just defaultChannelID }
--     WS.sendTextData conn unregMsg
--     msg <- receiveMessage conn
--     assert (status msg == Just 200 && messageType msg == "unregister") (return ())

--------------------------------------------------------------------------------
-- app :: WS.ClientApp ()
-- app conn = do
--     putStrLn "Connected!"
--     sendHello conn
--
--     -- Register our channelID and get the endpoint
--     endpoint <- registerChid conn
--
--     -- Send a notification in
--     forkIO $ void $ put endpoint ("version=" :: BL.ByteString)
--
--     -- Read the notification
--     readUpdateNotification conn
--
--     -- Unregister for the channel
--     unregisterChid conn
--
--     WS.sendClose conn ("Bye!" :: Text)
--     liftIO $ putStrLn "Smoke test was successful"
--
-- --------------------------------------------------------------------------------
-- main :: IO ()
-- main = WS.runClientWith
--             "localhost" 8080 "/"
--             WS.defaultConnectionOptions
--             [("Origin", "localhost:8080")] app
