{-# OPTIONS_GHC -funbox-strict-fields #-}

{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Control.Applicative   ((<$>))
import           Control.Concurrent    (forkIO, runInUnboundThread, threadDelay)
import qualified Control.Exception     as E
import           Control.Monad         (forever, replicateM_, void, when)
import qualified Data.ByteString.Char8 as BC
import           Data.IORef
import           Data.List.Split       (splitOn)
import qualified Data.Map.Strict       as Map
import           Data.Maybe            (fromJust, isNothing)
import           Data.Sequence         ((|>))
import qualified Data.Sequence         as S
import qualified Network.Metric        as Metric
import qualified Network.WebSockets    as WS
import qualified Network.Wreq.Session  as Wreq
import           System.Environment    (getArgs)
import           Text.Printf           (printf)

import           SimpleTest.Interact

data ClientTracker = ClientTracker
    { attempting :: IORef Int
    , connected  :: IORef Int
    , maxClients :: !Int
    }

data TestConfig = TC
    { tcHostip      :: String
    , tcPort        :: Int
    , tcTracker     :: ClientTracker
    , tcInitStorage :: Storage
    , tcStatsd      :: Metric.AnySink
    , tcSession     :: Wreq.Session
    }

newClientTracker :: Int -> IO ClientTracker
newClientTracker m = do
    attempts <- newIORef 0
    conns  <- newIORef 0
    return $ ClientTracker attempts conns m

eatExceptions :: IO a -> IO ()
eatExceptions m = void m `E.catch` \(_ :: E.SomeException) -> return ()

exceptionToUsage :: IO a -> IO ()
exceptionToUsage m = void m `E.catch` \(_ :: E.SomeException) -> do
    putStrLn "Usage: spTester IP PORT SPAWN_COUNT [basic|ping|channels] STATSDHOST:STATSDPORT"
    return ()

watcher :: ClientTracker -> IO () -> IO ()
watcher (ClientTracker att conns m) spawn = forever $ do
    threadDelay (5*1000000)
    count <- readIORef conns
    attempts <- readIORef att
    printf "Clients Connected: %s\n" (show count)
    let spawnCount = m - attempts
    incRef spawnCount att
    replicateM_ spawnCount spawn

main :: IO ()
main = runInUnboundThread $ exceptionToUsage $ do
    [ip, port, spawnCount, strategy, statsdHost] <- getArgs
    let (sHostname:sPort:[]) = splitOn ":" statsdHost
        portNum = fromInteger $ (read sPort :: Integer)
        maxC = read spawnCount
        interaction = Map.lookup strategy interactions

    when (isNothing interaction) $ fail "Bad interation lookup"

    sink <- Metric.open Metric.Statsd "" sHostname portNum
    sess <- Wreq.withSession return

    clientTracker <- newClientTracker maxC

    let tconfig = TC ip (read port) clientTracker newStorage sink sess

    let spawn = startWs tconfig $ fromJust interaction
    incRef maxC (attempting clientTracker)
    replicateM_ maxC spawn
    watcher clientTracker spawn

startWs :: TestConfig -> Interaction () -> IO ()
startWs tc@(TC host port tracker _ _ _) i =
    void . forkIO $ E.finally safeSpawn $ decRef (attempting tracker)
  where
    safeSpawn = eatExceptions $ spawn
    spawn = WS.runClientWith host port "/" WS.defaultConnectionOptions
              [("Origin", BC.concat ["http://", BC.pack host, ":", BC.pack $ show port])]
              $ interactionTester tc i

interactionTester :: TestConfig -> Interaction a -> WS.ClientApp ()
interactionTester (TC _ _ (ClientTracker _ conns _) storage sink sess) i conn = do
    incRef 1 conns
    E.finally runit $ decRef conns
  where
    config = newConfig conn sink sess
    runit = void $ runInteraction i config storage

incRef :: Int -> IORef Int -> IO ()
incRef v ref = void $ atomicModifyIORef' ref (\x -> (x+v, ()))

decRef :: IORef Int -> IO ()
decRef ref = void $ atomicModifyIORef' ref (\x -> (x-1, ()))

{- * SimplePush Client Interactions

-}

interactions :: Map.Map String (Interaction ())
interactions = Map.fromList
    [ ("basic",    basic)
    , ("ping",     pingDeliver)
    , ("channels", channelMonster)
    ]

-- | A helper interaction that starts a new registration and returns a new
--   endpoint for a channelID
setupNewEndpoint :: Interaction (ChannelID, Endpoint)
setupNewEndpoint = do
    cid <- randomChannelId
    endpoint <- getEndpoint <$> register cid
    return (cid, endpoint)

-- | Basic single channel registration that sends a notification to itself
--  every 5 seconds and never pings
basic :: Interaction ()
basic = do
    _ <- helo Nothing (Just [])
    ep <- setupNewEndpoint
    forever $ do
        _ <- sendPushNotification ep Nothing
        wait 5

-- | Delivers a notification once every 10 seconds, pings every 20 seconds
pingDeliver :: Interaction ()
pingDeliver = do
    _ <- helo Nothing (Just [])
    endpoint <- setupNewEndpoint
    loop 0 endpoint
  where
    loop :: Int -> (ChannelID, Endpoint) -> Interaction ()
    loop count endpoint = do
        _ <- sendPushNotification endpoint Nothing
        if count == 20 then do
            _ <- ping
            wait 10
            loop 10 endpoint
        else do
            wait 10
            loop (count+10) endpoint

-- | Registers a new channel ID every 10 seconds
--   Chooses a channel randomly every 5 seconds for delivery and sends a push
channelMonster :: Interaction ()
channelMonster = do
    _ <- helo Nothing (Just [])
    loop 0 S.empty
  where
    loop :: Int -> S.Seq (ChannelID, Endpoint) -> Interaction ()
    loop count eps = do
        eps' <- if (count `mod` 10 == 0) then do
                    endpoint <- setupNewEndpoint
                    return $ eps |> endpoint
                else return eps
        i <- randomNumber (0, S.length eps' - 1)
        let ep = S.index eps' i
        _ <- sendPushNotification ep Nothing
        wait 5
        loop (count+5) eps'
