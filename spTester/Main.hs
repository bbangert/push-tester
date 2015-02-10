{-# OPTIONS_GHC -funbox-strict-fields #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Control.Applicative  ((<$>))
import           Control.Arrow        (second)
import           Control.Concurrent   (forkIO, runInUnboundThread, threadDelay)
import qualified Control.Exception    as E
import           Control.Monad        (forever, replicateM_, void, when)
import           Control.Monad.Trans  (liftIO)
import           Data.IORef
import           Data.List.Split      (splitOn)
import qualified Data.Map.Strict      as Map
import           Data.Maybe           (fromJust, isNothing)
import           Data.Sequence        ((|>))
import qualified Data.Sequence        as S
import qualified Network.Metric       as Metric
import qualified Network.Wreq.Session as Wreq
import           System.Environment   (getArgs)
import           System.Posix.Env     (getEnv)
import           Text.Printf          (printf)

import           PushClient           (ChannelUpdate (..), Message (..))
import           SimpleTest.Interact
import           SimpleTest.Types     (Notification (..))

----------------------------------------------------------------

-- | Main function to parse arguments and kick off the tester
main :: IO ()
main = getArgs >>= parseArguments >>= maybe (return ()) runTester

parseArguments :: [String] -> IO (Maybe (TestConfig, TestInteraction (), Int))
parseArguments [ip, port, spawnCount, strategy, statsdHost] = do
    let [sHostname, sPort] = splitOn ":" statsdHost
        portNum = fromInteger (read sPort :: Integer)
        maxC = read spawnCount
        interaction = Map.lookup strategy interactions

    when (isNothing interaction) $ fail "Bad interaction lookup"

    sink <- Metric.open Metric.Statsd Nothing sHostname portNum
    sess <- Wreq.withSession return

    clientTracker <- newClientTracker maxC

    let defaultSettings = Map.fromList [("NOTIFICATION_DELAY", 5),
                                        ("NOTIFICATION_COUNT", 20),
                                        ("PING_DELAY", 20),
                                        ("PING_COUNT", 10),
                                        ("RECONNECT_DELAY", 5)]
    let vars = Map.keys defaultSettings
    envSettings <- zip vars <$> mapM getEnv vars
    let actualSettings = Map.fromList $ readSecond $ dropNothings envSettings
        settingsMap = Map.union actualSettings defaultSettings

    let tconfig = TC ip (read port) clientTracker newStorage sink sess settingsMap
    return $ Just (tconfig, fromJust interaction, maxC)
  where
    dropNothings = filter (isNothing . snd)
    readSecond = map (second (read . fromJust))
parseArguments _ = do
    putStrLn "Usage: spTester IP PORT SPAWN_COUNT [basic|ping|channels|reconnecter|datasender] STATSDHOST:STATSDPORT"
    return Nothing

-- | Watches client tracking to echo data to stdout
watcher :: ClientTracker -> IO () -> IO ()
watcher ClientTracker{..} spawn = forever $ do
    threadDelay (5*1000000)
    attempts <- readIORef attempting
    printf "Clients Connected: %s\n" (show attempts)
    let spawnCount = maxClients - attempts
    incRef spawnCount attempting
    replicateM_ spawnCount spawn

runTester :: (TestConfig, TestInteraction (), Int) -> IO ()
runTester (tc@TC{..}, interaction, maxConnections) = runInUnboundThread $ do
    replicateM_ maxConnections spawn
    watcher tcTracker spawn
  where
    att = attempting tcTracker
    inc = incRef 1 att
    dec = decRef att
    spawn = void . forkIO . eatExceptions $ E.bracket_ inc dec go
    go = runTestInteraction interaction tc


----------------------------------------------------------------

incRef :: Int -> IORef Int -> IO ()
incRef v ref = void $ atomicModifyIORef' ref (\x -> (x+v, ()))

decRef :: IORef Int -> IO ()
decRef ref = void $ atomicModifyIORef' ref (\x -> (x-1, ()))

----------------------------------------------------------------

{- * SimplePush Client WebsocketInteractions

-}

emptyNotification :: Notification
emptyNotification = Notification Nothing Nothing

-- | Mapping of all the valid interactions we support
interactions :: Map.Map String (TestInteraction ())
interactions = Map.fromList
    [ ("basic",    basic)
    , ("ping",     pingDeliver)
    , ("channels", channelMonster)
    , ("reconnecter", reconnecter)
    , ("datasender", dataSender)
    ]

-- | A helper interaction that starts a new registration and returns a new
--   endpoint for a channelID
setupNewEndpoint :: WebsocketInteraction (ChannelID, Endpoint)
setupNewEndpoint = do
    cid <- randomChannelId
    endpoint <- getEndpoint <$> register cid
    return (cid, endpoint)


afterDelay :: Int -> Int -> Int -> WebsocketInteraction a
           -> WebsocketInteraction Int
afterDelay now lastUse delay action
    | now-lastUse > delay = action >> return now
    | otherwise           = return lastUse

-- | Basic single channel registration that sends a notification to itself
--  every 5 seconds and never pings
basic :: TestInteraction ()
basic = do
    notifDelay <- getSetting "NOTIFICATION_DELAY"
    withConnection $ do
        void $ helo Nothing (Just [])
        endpoint <- setupNewEndpoint
        forever $ do
            msg <- sendPushNotification endpoint emptyNotification
            ack msg
            wait notifDelay

-- | Delivers a notification once every 10 seconds, pings every 20 seconds
pingDeliver :: TestInteraction ()
pingDeliver = do
    [notifDelay, pingDelay] <-
        mapM getSetting ["NOTIFICATION_DELAY", "PING_DELAY"]
    withConnection $ do
        void $ helo Nothing (Just [])
        endpoint <- setupNewEndpoint
        loop 0 endpoint 0 0 notifDelay pingDelay
  where
    loop :: Int -> (ChannelID, Endpoint)
         -> Int -> Int
         -> Int -> Int
         -> WebsocketInteraction ()
    loop count endpoint lastPing lastNotif notifDelay pingDelay = do
        lastPing' <- afterDelay count lastPing pingDelay ping
        lastNotif' <- afterDelay count lastNotif notifDelay $
            sendPushNotification endpoint emptyNotification >>= ack
        wait 1
        loop (count+1) endpoint lastPing' lastNotif' notifDelay pingDelay

-- | Registers a new channel ID every 10 seconds
--   Chooses a channel randomly every 5 seconds for delivery and sends a push
channelMonster :: TestInteraction ()
channelMonster = withConnection $ do
    void $ helo Nothing (Just [])
    loop 0 S.empty
  where
    loop :: Int -> S.Seq (ChannelID, Endpoint) -> WebsocketInteraction ()
    loop count endpoints = do
        endpoints' <- updatedEndpoints
        endpoint <- randomChoice endpoints'
        msg <- sendPushNotification endpoint emptyNotification
        ack msg
        wait 5
        loop (count+5) endpoints'
      where
        -- Returns endpoints to use, prepending a new one every 10 seconds
        updatedEndpoints
            | shouldAddEndpoint = prependEndpoint <$> setupNewEndpoint
            | otherwise         = return endpoints
        shouldAddEndpoint = count `mod` 10 == 0
        prependEndpoint = (endpoints |>)

-- | Registers a channel, sends a notification every 5 seconds,
--   reconnects every 20
reconnecter :: TestInteraction ()
reconnecter = do
    (uid, cid, endpoint) <- withConnection $ do
        msg <- helo Nothing (Just [])
        cid <- randomChannelId
        endpoint <- getEndpoint <$> register cid
        return (uaid msg, cid, endpoint)
    reconnectLoop (uid, cid, endpoint)
  where
    reconnectLoop :: (Uaid, ChannelID, Endpoint) -> TestInteraction ()
    reconnectLoop (uid, cid, endpoint) = do
        [reconDelay, notifDelay, notifCount] <-
            mapM getSetting ["RECONNECT_DELAY", "NOTIFICATION_DELAY",
                             "NOTIFICATION_COUNT"]
        sendNotifications notifDelay notifCount
        wait reconDelay
        reconnectLoop (uid, cid, endpoint)
      where
        failMsg :: String
        failMsg = concat ["Failed to retain UAID: ", show uid, " Cid: ",
                          show cid]
        notificationLoop :: Int -> Int -> WebsocketInteraction ()
        notificationLoop _ 0 = return ()
        notificationLoop delay count = do
            msg <- sendPushNotification (cid, endpoint) emptyNotification
            ack msg
            wait delay
            notificationLoop delay (count-1)
        sendNotifications delay count = withConnection $ do
            msg <- helo uid (Just [fromJust cid])
            assert (uid==uaid msg, msg) failMsg
            notificationLoop delay count

-- | Registers a channel, sends a notification with data every 5 seconds,
--   pings every 20
dataSender :: TestInteraction ()
dataSender = withConnection $ do
    void $ helo Nothing (Just [])
    endpoint <- setupNewEndpoint
    loop 0 endpoint
  where
    loop :: Int -> (ChannelID, Endpoint) -> WebsocketInteraction ()
    loop count endpoint = do
        len <- randomNumber (10, 4000)
        dat <- randomData len
        msg <- sendPushNotification endpoint (Notification Nothing (Just dat))
        ack msg
        let msgData = fromJust . cu_data . head . fromJust $ updates msg
        assert (dat==msgData, msg) "Data failed to match"
        if count == 20 then do
            void ping
            wait 5
            loop 5 endpoint
        else do
            wait 5
            loop (count+5) endpoint
