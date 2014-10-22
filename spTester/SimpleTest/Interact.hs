{-# OPTIONS_GHC -funbox-strict-fields #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module SimpleTest.Interact
    ( -- * Interaction type and commands
      Interaction
    , runInteraction
    , helo
    , register
    , unregister
    , ping
    , sendPushNotification

      -- ** Interaction helpers
    , wait
    , randomChannelId
    , randomElement
    , randomNumber
    , randomChoice

      -- ** Interaction Message manipulation commands
    , getEndpoint

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
import qualified Control.Exception          as E
import           Control.Monad              (void)
import           Control.Monad.Reader       (ReaderT, ask, runReaderT)
import           Control.Monad.State.Strict (StateT, runStateT)
import           Control.Monad.Trans        (liftIO)
import           Data.ByteString            (ByteString)
import qualified Data.ByteString.Lazy       as BL
import qualified Data.Map.Strict            as Map
import           Data.Maybe                 (fromJust)
import qualified Data.Sequence              as S
import           Data.String                (fromString)
import           Data.Time.Clock.POSIX      (getPOSIXTime)
import qualified Network.Metric             as Metric
import qualified Network.WebSockets         as WS
import qualified Network.Wreq.Session       as Wreq
import           Test.QuickCheck            (arbitrary)
import           Test.QuickCheck.Gen        (Gen, choose, elements, generate)

import           PushClient                 (ChannelUpdate (..), Message (..),
                                             mkMessage, receiveMessage,
                                             sendReceiveMessage)

import           SimpleTest.Types           (ChannelID, ChannelIDs, Endpoint,
                                             Uaid, Version)
import           SimpleTest.Util            (ValidChannelID (..))

----------------------------------------------------------------

type Result = String

-- | Interaction datatypes
type VariableStorage = Map.Map String Result

data Storage = Storage
    { _stVariables :: !VariableStorage
    }

data Config = IConfig
    { iconn    :: !WS.Connection
    , iStat    :: !Metric.AnySink
    , iSession :: !Wreq.Session
    }

-- | Interaction monad transformer for a simplePush interaction
type Interaction = ReaderT Config (StateT Storage IO)

newStorage :: Storage
newStorage = Storage Map.empty

newConfig :: WS.Connection -> Metric.AnySink -> Wreq.Session -> Config
newConfig = IConfig

-- | Run a complete websocket client interaction
runInteraction :: Interaction a -> Config -> Storage -> IO (a, Storage)
runInteraction interaction config = runStateT (runReaderT interaction config)

----------------------------------------------------------------

{-  * Statistics helpers

-}

-- | Times an interaction and returns its result after sending the timer metric
--   recorded over the supplied metric sink.
withTimer :: ByteString             -- ^ Metric namespace
          -> ByteString             -- ^ Metric bucket name
          -> Metric.AnySink         -- ^ Metric sink to use
          -> Interaction a          -- ^ Interaction to run
          -> Interaction a
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

----------------------------------------------------------------

{-  * Utility Methods for raw message sending/recieving interactions

-}

-- | Send a message and get a message in response
sendRecieve :: Message -> Interaction Message
sendRecieve msg = do
    conn <- iconn <$> ask
    liftIO (sendReceiveMessage msg conn)

-- | Get a message
getMessage :: Interaction Message
getMessage = do
    conn <- iconn <$> ask
    liftIO (receiveMessage conn)

getEndpoint :: Message -> String
getEndpoint = fromJust . pushEndpoint

----------------------------------------------------------------

{-  * Validity assertions to ensure operations work properly

-}

assert :: Show a => (Bool, a) -> String -> Interaction ()
assert (True, _) _ = return ()
assert (False, obj) msg = do
    liftIO $ putStrLn $ "Assert failed: " ++ msg ++ " \tObject: " ++ show obj
    fail "Abort"

assertStatus200 :: Message -> Interaction ()
assertStatus200 msg = assert (msgStatus == 200, msg) "message status not 200."
  where
    msgStatus = fromJust $ status msg

assertEndpointMatch :: ChannelID -> Message -> Interaction ()
assertEndpointMatch cid msg = do
    assert (length cids == 1, cids) "channel updates is longer than 1."
    let updateCid = cu_channelID $ head cids
    assert (updateCid == cid', (updateCid, cid')) "channel ID mismatch."
  where
    cids = fromJust $ updates msg
    cid' = fromJust cid

----------------------------------------------------------------

{-  * Basic SimplePush style interaction commands

-}

-- | Say helo to a remote server
helo :: Uaid -> ChannelIDs -> Interaction Message
helo uid cids = sendRecieve heloMsg
  where
    heloMsg = mkMessage {messageType="hello", uaid=uid, channelIDs=cids}

-- | Register a channel ID with a remote server
register :: ChannelID -> Interaction Message
register cid = do
    msg <- sendRecieve registerMsg
    assertStatus200 msg
    return msg
  where
    registerMsg = mkMessage {messageType="register", channelID=cid}

-- | Unregister a channel ID with a remote server
unregister :: ChannelID -> Interaction Message
unregister cid = sendRecieve unregisterMsg
  where
    unregisterMsg = mkMessage {messageType="unregister", channelID=cid}

sendPushNotification :: (ChannelID, Endpoint) -> Version -> Interaction Message
sendPushNotification (cid, endpoint) ver = do
    sess <- iSession <$> ask
    sink <- iStat <$> ask
    msg <- withTimer "simplepush.client" "pushNotification" sink $ do
            liftIO $ send sess endpoint ver
            getMessage
    assertEndpointMatch cid msg
    return msg

ping :: Interaction Bool
ping = do
    conn <- iconn <$> ask
    liftIO $ WS.sendTextData conn ("{}" :: BL.ByteString)
    (d :: BL.ByteString) <- liftIO $ WS.receiveData conn
    return $ d == "{}"

-- | Wait for a given amount of seconds
wait :: Int -> Interaction ()
wait i = liftIO $ threadDelay (i * 1000000)

-- | Generate a random valid channelID
randomChannelId :: Interaction ChannelID
randomChannelId = do
    (ValidChannelID cid) <- liftIO $ generate (arbitrary :: Gen ValidChannelID)
    return cid

-- | Choose from a list randomly
randomElement :: [a] -> Interaction a
randomElement xs = liftIO $ generate (elements xs)

randomNumber :: (Int, Int) -> Interaction Int
randomNumber (l, u) = liftIO $ generate $ choose (l, u)

randomChoice :: S.Seq a -> Interaction a
randomChoice vec = do
    i <- randomNumber (0, S.length vec - 1)
    return $ S.index vec i

----------------------------------------------------------------

{-  * Utility methods for parsing messages and generating components

-}

-- | Send a PUT request to a notification point
send :: Wreq.Session -> String -> Version -> IO ()
send sess ep ver =
    void $ forkIO $ eatExceptions $ Wreq.put sess ep $ serializeVersion ver

-- | Serialize the version to a bytestring for sending
serializeVersion :: Version -> BL.ByteString
serializeVersion Nothing = "version="
serializeVersion (Just ver) = BL.append "version=" $ esc ver
  where esc = fromString . show

eatExceptions :: IO a -> IO ()
eatExceptions m = void m `E.catch` \(_ :: E.SomeException) -> return ()
