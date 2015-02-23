{-# OPTIONS_GHC -funbox-strict-fields #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module SimpleTest.Interact
    ( -- * TestInteraction type and commands
      TestInteraction
    , runTestInteraction
    , withConnection
    , getSetting

      -- * WebsocketInteraction type and commands
    , WebsocketInteraction
    , helo
    , register
    , unregister
    , ping
    , ack
    , sendPushNotification

      -- ** General MonadIO helpers
    , wait
    , randomChannelId
    , randomElement
    , randomNumber
    , randomChoice
    , randomData
    , assert

      -- ** WebsocketInteraction Message manipulation commands
    , getEndpoint

      -- * Datatypes for interactions
    , Uaid
    , ChannelIDs
    , ChannelID
    , Endpoint
    , Version
    , Storage
    , Config
    , ClientTracker(..)
    , TestConfig(..)
    , newStorage
    , newConfig
    , newClientTracker
    , eatExceptions
    ) where

import           Control.Applicative        ((<$>))
import           Control.Concurrent         (threadDelay)
import qualified Control.Exception          as E
import           Control.Monad              (void, when)
import           Control.Monad.IO.Class     (MonadIO)
import           Control.Monad.Reader       (ReaderT, ask, runReaderT)
import           Control.Monad.State.Strict (MonadState (get, put), StateT,
                                             runStateT)
import           Control.Monad.Trans        (liftIO)
import           Data.ByteString            (ByteString)
import qualified Data.ByteString.Char8      as BC
import qualified Data.ByteString.Lazy       as BL
import           Data.IORef
import qualified Data.Map.Strict            as Map
import           Data.Maybe                 (fromJust, fromMaybe, isJust)
import qualified Data.Sequence              as S
import           Data.Time.Clock.POSIX      (getPOSIXTime)
import qualified Network.Metric             as Metric
import qualified Network.WebSockets         as WS
import           Network.Wreq               (FormParam ((:=)))
import qualified Network.Wreq.Session       as Wreq
import           Test.QuickCheck            (arbitrary, vectorOf)
import           Test.QuickCheck.Gen        (Gen, choose, elements, generate)

import           PushClient                 (ChannelUpdate (..), Message (..),
                                             mkMessage, receiveMessage,
                                             sendMessage, sendReceiveMessage)

import           SimpleTest.Types           (ChannelID, ChannelIDs, Data,
                                             Endpoint, Notification (..), Uaid,
                                             Version)
import           SimpleTest.Util            (ValidChannelID (..))

----------------------------------------------------------------

type Result = String

-- | WebsocketInteraction datatypes
type VariableStorage = Map.Map String Result

data Storage = Storage !VariableStorage

data Config = IConfig
    { iconn    :: !WS.Connection
    , iStat    :: !Metric.AnySink
    , iSession :: !Wreq.Session
    }

data ClientTracker = ClientTracker
    { attempting :: IORef Int
    , maxClients :: !Int
    }

data TestConfig = TC
    { tcHostip      :: String
    , tcPort        :: Int
    , tcTracker     :: ClientTracker
    , tcInitStorage :: Storage
    , tcStatsd      :: Metric.AnySink
    , tcSession     :: Wreq.Session
    , tcSettings    :: Map.Map String Int
    }

-- | WebsocketInteraction monad transformer for a simplePush interaction
type WebsocketInteraction = ReaderT Config (StateT Storage IO)

-- | Testing monad transformer for a simplePush interaction
type TestInteraction = StateT TestConfig IO

----------------------------------------------------------------

newStorage :: Storage
newStorage = Storage Map.empty

newConfig :: WS.Connection -> Metric.AnySink -> Wreq.Session -> Config
newConfig = IConfig

newClientTracker :: Int -> IO ClientTracker
newClientTracker m = do
    attempts <- newIORef 0
    return $ ClientTracker attempts m

-- | Run a complete websocket client interaction
runWebsocketInteraction :: WebsocketInteraction a
                        -> Config
                        -> Storage
                        -> IO (a, Storage)
runWebsocketInteraction interaction config = runStateT (runReaderT interaction config)

-- | Run a test interaction
runTestInteraction :: TestInteraction a -> TestConfig -> IO (a, TestConfig)
runTestInteraction = runStateT

----------------------------------------------------------------

{-  * TestInteraction commands

-}

-- | Run an interaction in a test interaction
withConnection :: WebsocketInteraction a -> TestInteraction a
withConnection interaction = do
    tc@TC{..} <- get
    let runClient = WS.runClientWith tcHostip tcPort "/"
                    WS.defaultConnectionOptions (originHeader tcHostip tcPort)
    (resp, store) <- liftIO $ runClient $ \conn -> do
        let config = newConfig conn tcStatsd tcSession
        runWebsocketInteraction interaction config tcInitStorage
    put $ tc { tcInitStorage = store }
    return resp
  where
    originHeader host port = [("Origin", BC.concat ["http://", BC.pack host,
                                                    ":", BC.pack $ show port])]

-- | Get an env setting from the map
getSetting :: String -> TestInteraction Int
getSetting name = do
    TC{..} <- get
    return (fromJust $ Map.lookup name tcSettings)

----------------------------------------------------------------

{-  * Statistics helpers

-}

-- | Times an interaction and returns its result after sending the timer metric
--   recorded over the supplied metric sink.
withTimer :: ByteString             -- ^ Metric namespace
          -> ByteString             -- ^ Metric bucket name
          -> Metric.AnySink         -- ^ Metric sink to use
          -> WebsocketInteraction a          -- ^ WebsocketInteraction to run
          -> WebsocketInteraction a
withTimer namespace bucket sink op = do
    now <- liftIO getPOSIXTime
    opVal <- op
    done <- liftIO getPOSIXTime
    let tDiff = realToFrac $ done - now
        diff  = tDiff * 1000
    liftIO $ eatExceptions $ Metric.push sink $ metricTimer diff
    return opVal
  where
    metricTimer = Metric.Timer namespace bucket

-- | Send a counter increment
incrementCounter :: ByteString      -- ^ Metric namespace
                 -> ByteString      -- ^ Metric bucket name
                 -> Integer         -- ^ Count to increment by
                 -> WebsocketInteraction ()
incrementCounter namespace bucket count = do
    sink <- iStat <$> ask
    liftIO $ eatExceptions $ Metric.push sink counter
    return ()
  where
    counter = Metric.Counter namespace bucket count

----------------------------------------------------------------

{-  * Utility Methods for raw message sending/recieving interactions

-}

-- | Send a message and get a message in response
sendRecieve :: Message -> WebsocketInteraction Message
sendRecieve msg = do
    conn <- iconn <$> ask
    liftIO $ sendReceiveMessage msg conn

-- | Send a message without a response
send :: Message -> WebsocketInteraction ()
send msg = do
  conn <- iconn <$> ask
  void $ liftIO $ sendMessage msg conn

-- | Get a message
getMessage :: WebsocketInteraction Message
getMessage = do
    conn <- iconn <$> ask
    liftIO (receiveMessage conn)

getEndpoint :: Message -> String
getEndpoint = fromJust . pushEndpoint

----------------------------------------------------------------

{-  * Validity assertions to ensure operations work properly

-}

assert :: (Show a, MonadIO m) => (Bool, a) -> String -> m ()
assert (True, _) _ = return ()
assert (False, obj) msg = do
    liftIO $ putStrLn $ "Assert failed: " ++ msg ++ " \tObject: " ++ show obj
    fail "Abort"

assertStatus200 :: MonadIO m => Message -> m ()
assertStatus200 msg = assert (msgStatus == 200, msg) "message status not 200."
  where
    msgStatus = fromJust $ status msg

assertEndpointMatch :: MonadIO m => ChannelID -> Message -> m ()
assertEndpointMatch cid msg = do
    assert (length cids == 1, cids) "channel updates is longer than 1."
    assert (updateCid == cid', (updateCid, cid')) "channel ID mismatch."
  where
    cids = fromJust $ updates msg
    cid' = fromJust cid
    updateCid = cu_channelID $ head cids

----------------------------------------------------------------

{-  * Basic SimplePush style websocket interaction commands

-}

-- | Say helo to a remote server
helo :: Uaid -> ChannelIDs -> WebsocketInteraction Message
helo uid cids = sendRecieve heloMsg
  where
    heloMsg = mkMessage {messageType="hello", uaid=uid, channelIDs=cids}

-- | Register a channel ID with a remote server
register :: ChannelID -> WebsocketInteraction Message
register cid = do
    msg <- sendRecieve registerMsg
    assertStatus200 msg
    return msg
  where
    registerMsg = mkMessage {messageType="register", channelID=cid}

-- | Unregister a channel ID with a remote server
unregister :: ChannelID -> WebsocketInteraction Message
unregister cid = sendRecieve unregisterMsg
  where
    unregisterMsg = mkMessage {messageType="unregister", channelID=cid}

-- | Ack a notification
ack :: Message -> WebsocketInteraction ()
ack msg = send ackMsg
  where
    ackMsg = mkMessage {messageType="ack", updates=basicChans (updates msg)}
    -- Strip out the data from the ack
    basicChans = fmap $ map (\update -> update { cu_data=Nothing })


sendPushNotification :: (ChannelID, Endpoint) -> Notification -> WebsocketInteraction Message
sendPushNotification (cid, endpoint) notif@Notification{..} = do
    sess <- iSession <$> ask
    sink <- iStat <$> ask
    msg <- withTimer "push_test.update" "latency" sink $ do
            sendNotification sess endpoint notif
            getMessage
    when hasData $
        incrementCounter "push_test.notification.throughput" "bytes" msgLen
    assertEndpointMatch cid msg
    return msg
  where
    hasData = isJust notifData
    msgLen = toInteger . length $ fromJust notifData

ping :: WebsocketInteraction Bool
ping = do
    conn <- iconn <$> ask
    liftIO $ WS.sendTextData conn ("{}" :: BL.ByteString)
    (d :: BL.ByteString) <- liftIO $ WS.receiveData conn
    return $ d == "{}"

----------------------------------------------------------------

{-  * MonadIO utility methods

-}

-- | Wait for a given amount of seconds
wait :: MonadIO m => Int -> m ()
wait i = liftIO $ threadDelay (i * 1000000)

-- | Generate a random valid channelID
randomChannelId :: MonadIO m => m ChannelID
randomChannelId = do
    (ValidChannelID cid) <- liftIO $ generate (arbitrary :: Gen ValidChannelID)
    return cid

-- | Choose from a list randomly
randomElement :: MonadIO m => [a] -> m a
randomElement xs = liftIO $ generate (elements xs)

randomNumber :: MonadIO m => (Int, Int) -> m Int
randomNumber (l, u) = liftIO $ generate $ choose (l, u)

randomChoice :: MonadIO m => S.Seq a -> m a
randomChoice vec = do
    i <- randomNumber (0, S.length vec - 1)
    return $ S.index vec i

hexChar :: Gen Char
hexChar = elements (['a'..'f'] ++ ['0'..'9'])

randomData :: MonadIO m => Int -> m String
randomData len = liftIO $ generate $ vectorOf len hexChar

----------------------------------------------------------------

{-  * Utility methods for parsing messages and generating components

-}

-- | Send a PUT request to a notification point
sendNotification :: MonadIO m => Wreq.Session -> String -> Notification -> m ()
sendNotification sess ep notif = liftIO $ eatExceptions $ Wreq.put sess ep encNotif
  where encNotif = serializeNotification notif

-- | Serialize the notification to a bytestring for sending
serializeNotification :: Notification -> [FormParam]
serializeNotification (Notification ver Nothing) = [serializeVersion ver]
serializeNotification (Notification ver dat) = [encVer, encData]
  where
    encVer = serializeVersion ver
    encData = serializeData dat

-- | Serialize the version to a bytestring for sending
serializeVersion :: Version -> FormParam
serializeVersion Nothing = "version" := ("" :: String)
serializeVersion (Just ver) = "version" := ver

-- | Serialize the data to a bytestring
serializeData :: Data -> FormParam
serializeData dat = "data" := fromMaybe ("" :: String) dat

eatExceptions :: IO a -> IO ()
eatExceptions m = void m `E.catch` \(_ :: E.SomeException) -> return ()
