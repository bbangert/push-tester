{-# OPTIONS_GHC -funbox-strict-fields #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}

module Main where

import           Control.Applicative     ((<$>))
import           Control.Arrow           (second)
import           Control.Concurrent      (forkIO, runInUnboundThread,
                                          threadDelay)
import qualified Control.Exception       as E
import           Control.Monad           (forever, replicateM_, void, when)
import           Data.IORef
import           Data.List.Split         (splitOn)
import qualified Data.Map.Strict         as Map
import           Data.Maybe              (fromJust, isJust, isNothing)
import           Data.Sequence           ((|>))
import qualified Data.Sequence           as S
import           Network.HTTP.Client     (Manager, ManagerSettings (..),
                                          newManager)
import           Network.HTTP.Client.TLS (tlsManagerSettings)
import qualified Network.Metric          as Metric
import qualified Network.Wreq.Session    as Wreq
import           System.Environment      (getArgs)
import           System.Posix.Env        (getEnv)
import           Text.Printf             (printf)

import           PushClient              (ChannelUpdate (..), Message (..))
import           SimpleTest.Interact
import           SimpleTest.Types        (Notification (..))

----------------------------------------------------------------

-- | Main function to parse arguments and kick off the tester
main :: IO ()
main = getArgs >>= parseArguments >>= maybe (return ()) runTester

defaultSettings :: Map.Map String Int
defaultSettings = Map.fromList [("NOTIFICATION_DELAY", 5),
                                ("NOTIFICATION_COUNT", 20),
                                ("NOTIFICATION_MAX_SIZE", 4096),
                                ("PING_DELAY", 20),
                                ("PING_COUNT", 10),
                                ("RECONNECT_DELAY", 5),
                                ("CONNECTION_COUNT", 10),
                                ("LAUNCH_AT_ONCE", 1000)]

loadEnvSettings :: IO (Map.Map String Int)
loadEnvSettings = mergedMap . filterValid . zip vars <$> mapM getEnv vars
  where
    mergedMap lst = Map.union (convertedMap lst) defaultSettings
    convertedMap lst = Map.fromList $ map convertSetting lst
    convertSetting = second $ read . fromJust
    filterValid = filter (isJust . snd)
    vars = Map.keys defaultSettings

parseArguments :: [String] -> IO (Maybe (TestConfig, TestInteraction (), Manager))
parseArguments [ip, port, spawnCount, strategy, statsdHost] = do
    let [sHostname, sPort] = splitOn ":" statsdHost
        portNum = fromInteger (read sPort :: Integer)
        maxC = read spawnCount
        interaction = Map.lookup strategy interactions

    -- Must specify an interaction
    when (isNothing interaction) $ fail "Bad interaction lookup"

    -- Load the environment settings
    settingsMap <- loadEnvSettings
    let mgrSettings = tlsManagerSettings {
          managerConnCount = settingsMap Map.! "CONNECTION_COUNT"
        , managerResponseTimeout = Just (30*1000000)
        }

    -- Setup some common objects
    sink <- Metric.open Metric.Statsd Nothing sHostname portNum
    sess <- Wreq.withSession return
    mgr <- newManager mgrSettings
    clientTracker <- newClientTracker maxC

    -- Create the main config object
    let tconfig = TC ip (read port) clientTracker newStorage sink sess settingsMap

    return $ Just (tconfig, fromJust interaction, mgr)
parseArguments _ = do
    putStrLn "Usage: spTester IP PORT SPAWN_COUNT [basic|ping|channels|reconnecter|datasender] STATSDHOST:STATSDPORT"
    return Nothing

-- | Watches client tracking to echo data to stdout
watcher :: ClientTracker -> Int -> IO () -> IO ()
watcher ClientTracker{..} atMost spawn = forever $ do
    attempts <- readIORef attempting
    printf "Clients Connected: %s\n" (show attempts)
    let spawnCount = min (maxClients - attempts) atMost
    when (spawnCount > 0) $
        printf "Spinning up %s instances\n" (show spawnCount)
    incRef spawnCount attempting
    replicateM_ spawnCount $ do
        spawn
        threadDelay 10000
    wait 5

runTester :: (TestConfig, TestInteraction (), Manager) -> IO ()
runTester (tc@TC{..}, interaction, mgr) =
    runInUnboundThread $ watcher tcTracker atMost spawn
  where
    atMost = tcSettings Map.! "LAUNCH_AT_ONCE"
    att = attempting tcTracker
    dec = decRef att
    spawn = void . forkIO . void $ E.finally go dec
    go = void $ do
        sess <- Wreq.withSessionManager mgr return
        let newTc = tc { tcSession = sess }
        runTestInteraction interaction newTc

----------------------------------------------------------------

incRef :: Int -> IORef Int -> IO ()
incRef v ref = atomicModifyIORef' ref (\x -> (x+v, ()))

decRef :: IORef Int -> IO ()
decRef ref = atomicModifyIORef' ref (\x -> (x-1, ()))

----------------------------------------------------------------

{- * SimplePush Client WebsocketInteractions

-}

type Now = Int
type Delay = Int
type LastUsed = Int
type PossibleInteraction = Now -> LastUsed -> WebsocketInteraction LastUsed

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

-- | Helper function that determines if the supplied action should be run based
--   on whether or not enough of a delay has passed
--   Returns the time to use as the new 'lastUsed' value.
afterDelay :: WebsocketInteraction a -> Delay -> PossibleInteraction
afterDelay action delay now lastUse
    | now-lastUse >= delay = action >> return now
    | otherwise            = return lastUse

-- | Helper to create a push notification PossibleInteraction
pushAction :: (ChannelID, Endpoint) -> Delay -> PossibleInteraction
pushAction endpoint = afterDelay push
  where
    push = sendPushNotification endpoint emptyNotification >>= ack

-- | Loop helper that will iterate forever, running the list of interactions and
--   then waiting 1 second.
runLoop :: [PossibleInteraction] -> WebsocketInteraction ()
runLoop actions = go 0 $ zip actions (repeat 0)
  where
    go :: Now -> [(PossibleInteraction, LastUsed)] -> WebsocketInteraction ()
    go now actionList = do
        actionList' <- mapM (runIt now) actionList
        wait 1
        go (now+1) actionList'
    runIt now (action, used) = (action,) <$> action now used

-- | Basic single channel registration that sends a notification to itself
--  every 5 seconds and never pings
basic :: TestInteraction ()
basic = do
    notifDelay <- getSetting "NOTIFICATION_DELAY"
    withConnection $ do
        void $ helo Nothing (Just [])
        endpoint <- setupNewEndpoint
        runLoop [pushAction endpoint notifDelay]

-- | Delivers a notification once every 10 seconds, pings every 20 seconds
pingDeliver :: TestInteraction ()
pingDeliver = do
    [notifDelay, pingDelay] <-
        mapM getSetting ["NOTIFICATION_DELAY", "PING_DELAY"]
    withConnection $ do
        void $ helo Nothing (Just [])
        endpoint <- setupNewEndpoint
        runLoop [afterDelay ping pingDelay, pushAction endpoint notifDelay]

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
        sendPushNotification endpoint emptyNotification >>= ack
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
            sendPushNotification (cid, endpoint) emptyNotification >>= ack
            wait delay
            notificationLoop delay (count-1)
        sendNotifications delay count = withConnection $ do
            msg <- helo uid (Just [fromJust cid])
            assert (uid==uaid msg, msg) failMsg "uaid_reset"
            notificationLoop delay count

-- | Registers a channel, sends a notification with data every 5 seconds,
--   pings every 20
dataSender :: TestInteraction ()
dataSender = do
    [notifDelay, notifMaxSize, pingDelay] <-
        mapM getSetting ["NOTIFICATION_DELAY", "NOTIFICATION_MAX_SIZE",
                         "PING_DELAY"]
    withConnection $ do
        void $ helo Nothing (Just [])
        endpoint <- setupNewEndpoint
        let pingAction = afterDelay ping pingDelay
            notifAction = afterDelay (push notifMaxSize endpoint) notifDelay
        runLoop [pingAction, notifAction]
  where
    push maxSize endpoint = do
        len <- randomNumber (10, maxSize)
        dat <- randomData len
        msg <- sendPushNotification endpoint (Notification Nothing (Just dat))
        ack msg
        let msgData = fromJust . cu_data . head . fromJust $ updates msg
        assert (dat==msgData, msg) "Data failed to match" "data_mismatch"
