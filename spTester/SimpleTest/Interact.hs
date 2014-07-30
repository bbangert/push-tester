{-# LANGUAGE OverloadedStrings #-}

module SimpleTest.Interact
    ( -- * Interaction type and commands
      Interaction
    , runInteraction
    , helo
    , register
    , unregister
    , sendPushNotification
    , wait
    , randomChannelId

      -- * Datatypes for interactions
    , Uaid
    , ChannelIDs
    , ChannelID
    , Endpoint
    , Version
    , Storage
    , Config
    , newStorage
    , newConfig
    ) where

import           Control.Applicative        ((<$>))
import           Control.Concurrent         (forkIO, threadDelay)
import           Control.Monad              (void)
import           Control.Monad.Reader       (ReaderT, ask, runReaderT)
import           Control.Monad.State.Strict (StateT, get, put, runStateT)
import           Control.Monad.Trans        (liftIO)
import qualified Data.ByteString.Lazy       as BL
import qualified Data.Map.Strict            as Map
import           Data.String                (fromString)
import qualified Network.WebSockets         as WS
import qualified Network.Wreq               as Wreq
import           Test.QuickCheck            (arbitrary)
import           Test.QuickCheck.Gen        (Gen, generate)

import           PushClient                 (Message (..), mkMessage,
                                             receiveMessage, sendReceiveMessage)

import           SimpleTest.Types           (ChannelID, ChannelIDs, Endpoint,
                                             Uaid, Version)
import           SimpleTest.Util            (ValidChannelID (..))

type Result = String

-- | Interaction datatypes
type VariableStorage = Map.Map String Result

data Storage = Storage
    { variables :: VariableStorage
    } deriving (Show, Eq)

data Config = IConfig
    { iconn :: WS.Connection
    }

newStorage :: Storage
newStorage = Storage Map.empty

newConfig :: WS.Connection -> Config
newConfig = IConfig

-- | Interaction monad transformer for a simplePush interaction
type Interaction = ReaderT Config (StateT Storage IO)

-- | Run a complete websocket client interaction
runInteraction :: Interaction a -> Config -> Storage -> IO (a, Storage)
runInteraction interaction config = runStateT (runReaderT interaction config)

{-  * Utility Methods for raw message sending/recieving interactions

-}

-- | Send a message and get a message in response
sendRecieve :: Message -> Interaction Message
sendRecieve msg = do
    conn <- iconn <$> ask
    liftIO (sendReceiveMessage msg conn) >>= return

-- | Get a message
getMessage :: Interaction Message
getMessage = do
    conn <- iconn <$> ask
    liftIO (receiveMessage conn) >>= return

{-  * Basic SimplePush style interaction commands

-}

-- | Say helo to a remote server
helo :: Uaid -> ChannelIDs -> Interaction Message
helo uid cids = sendRecieve heloMsg
  where
    heloMsg = mkMessage {messageType="hello", uaid=uid, channelIDs=cids}

-- | Register a channel ID with a remote server
register :: ChannelID -> Interaction Message
register cid = sendRecieve registerMsg
  where
    registerMsg = mkMessage {messageType="register", channelID=cid}

-- | Unregister a channel ID with a remote server
unregister :: ChannelID -> Interaction Message
unregister cid = sendRecieve unregisterMsg
  where
    unregisterMsg = mkMessage {messageType="unregister", channelID=cid}

sendPushNotification :: Endpoint -> Version -> Interaction Message
sendPushNotification endpoint ver = do
    liftIO $ send endpoint ver
    getMessage

-- | Wait for a given amount of seconds
wait :: Int -> Interaction ()
wait i = liftIO $ threadDelay (i * 1000000)

-- | Generate a random valid channelID
randomChannelId :: Interaction ChannelID
randomChannelId = do
    (ValidChannelID cid) <- liftIO $ generate (arbitrary :: Gen ValidChannelID)
    return cid

{-  * Utility methods for parsing messages and generating components

-}

-- | Send a PUT request to a notification point
send :: String -> Version -> IO ()
send ep ver = void $ forkIO $ void $ Wreq.put ep $ serializeVersion ver

-- | Serialize the version to a bytestring for sending
serializeVersion :: Version -> BL.ByteString
serializeVersion Nothing = "version="
serializeVersion (Just ver) = BL.append "version=" $ esc ver
  where esc = fromString . show
