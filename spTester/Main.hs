{-# OPTIONS_GHC -funbox-strict-fields #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Control.Applicative  ((<$>))
import           Control.Concurrent   (forkIO, runInUnboundThread, threadDelay)
import qualified Control.Exception    as E
import           Control.Monad        (forever, replicateM_, void, when)
import           Data.IORef
import           Data.List.Split      (splitOn)
import qualified Data.Map.Strict      as Map
import           Data.Maybe           (fromJust, isNothing)
import           Data.Sequence        ((|>))
import qualified Data.Sequence        as S
import qualified Network.Metric       as Metric
import qualified Network.Wreq.Session as Wreq
import           System.Environment   (getArgs)
import           Text.Printf          (printf)

import           PushClient           (Message (..))
import           SimpleTest.Interact

----------------------------------------------------------------

-- | Main function to parse arguments and kick off the tester
main :: IO ()
main = getArgs >>= parseArguments >>= maybe (return ()) runTester

parseArguments :: [String] -> IO (Maybe (TestConfig, TestInteraction (), Int))
parseArguments [ip, port, spawnCount, strategy, statsdHost] = do
    let (sHostname:sPort:[]) = splitOn ":" statsdHost
        portNum = fromInteger (read sPort :: Integer)
        maxC = read spawnCount
        interaction = Map.lookup strategy interactions

    when (isNothing interaction) $ fail "Bad interaction lookup"

    sink <- Metric.open Metric.Statsd Nothing sHostname portNum
    sess <- Wreq.withSession return

    clientTracker <- newClientTracker maxC

    let tconfig = TC ip (read port) clientTracker newStorage sink sess
    return $ Just (tconfig, fromJust interaction, maxC)
parseArguments _ = do
    putStrLn "Usage: spTester IP PORT SPAWN_COUNT [basic|ping|channels] STATSDHOST:STATSDPORT"
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

-- | Mapping of all the valid interactions we support
interactions :: Map.Map String (TestInteraction ())
interactions = Map.fromList
    [ ("basic",    basic)
    , ("ping",     pingDeliver)
    , ("channels", channelMonster)
    , ("reconnecter", reconnecter)
    ]

-- | A helper interaction that starts a new registration and returns a new
--   endpoint for a channelID
setupNewEndpoint :: WebsocketInteraction (ChannelID, Endpoint)
setupNewEndpoint = do
    cid <- randomChannelId
    endpoint <- getEndpoint <$> register cid
    return (cid, endpoint)

-- | Basic single channel registration that sends a notification to itself
--  every 5 seconds and never pings
basic :: TestInteraction ()
basic = withConnection $ do
    void $ helo Nothing (Just [])
    endpoint <- setupNewEndpoint
    forever $ do
        void $ sendPushNotification endpoint Nothing
        wait 5

-- | Delivers a notification once every 10 seconds, pings every 20 seconds
pingDeliver :: TestInteraction ()
pingDeliver = withConnection $ do
    void $ helo Nothing (Just [])
    endpoint <- setupNewEndpoint
    loop 0 endpoint
  where
    loop :: Int -> (ChannelID, Endpoint) -> WebsocketInteraction ()
    loop count endpoint = do
        void $ sendPushNotification endpoint Nothing
        if count == 20 then do
            void ping
            wait 10
            loop 10 endpoint
        else do
            wait 10
            loop (count+10) endpoint

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
        void $ sendPushNotification endpoint Nothing
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
        sendNotifications
        reconnectLoop (uid, cid, endpoint)
      where
        notificationLoop :: Int -> WebsocketInteraction ()
        notificationLoop 20 = return ()
        notificationLoop count = do
            void $ sendPushNotification (cid, endpoint) Nothing
            wait 5
            notificationLoop (count+5)
        sendNotifications = withConnection $ do
            msg <- helo uid (Just [fromJust cid])
            let failMsg = concat ["Failed to retain UAID: ", show uid,
                                  " Cid: ", show cid]
            assert (uid==uaid msg, msg) failMsg
            notificationLoop 0
