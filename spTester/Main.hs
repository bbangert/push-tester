{-# OPTIONS_GHC -funbox-strict-fields #-}

{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Control.Applicative   ((<$>))
import           Control.Concurrent    (forkIO, runInUnboundThread, threadDelay)
import qualified Control.Exception     as E
import           Control.Monad         (forever, replicateM_, void)
import           Control.Monad.Trans   (liftIO)
import qualified Data.Sequence as S
import           Crypto.Random         (newGenIO)
import           Data.ByteString       (ByteString)
import qualified Data.ByteString.Char8 as BC
import           Data.IORef
import Data.Sequence ((|>))
import qualified Network.WebSockets    as WS
import           System.Environment    (getArgs)
import           Test.QuickCheck       (arbitrary)
import           Test.QuickCheck.Gen   (Gen, generate)
import           Text.Printf           (printf)

import           SimpleTest.Interact
import           SimpleTest.Types      (Endpoint)
import           SimpleTest.Util       (ValidChannelID (..))

data ClientTracker = ClientTracker
    { attempting :: IORef Int
    , connected  :: IORef Int
    , maxClients :: !Int
    }

newClientTracker :: Int -> IO ClientTracker
newClientTracker m = do
    attempts <- newIORef 0
    conns  <- newIORef 0
    return $ ClientTracker attempts conns m

eatExceptions :: IO a -> IO ()
eatExceptions m = void m `E.catch` \(_ :: E.SomeException) -> return ()

watcher :: ClientTracker -> IO () -> IO ()
watcher (ClientTracker att conns m) spawn = forever $ do
    threadDelay (5*1000000)
    count <- readIORef conns
    attempts <- readIORef att
    printf "Clients Connected: %s\n" (show count)
    let spawnCount = m - count
    incRef spawnCount att
    replicateM_ spawnCount spawn

main :: IO ()
main = runInUnboundThread $ do
    [ip, port, spawnCount] <- getArgs
    let maxC = read spawnCount
    clientTracker <- newClientTracker maxC

    storage <- newStorage

    let spawn = startWs ip (read port) clientTracker pingDeliver storage
    incRef maxC (attempting clientTracker)
    replicateM_ maxC spawn
    watcher clientTracker spawn

startWs :: String -> Int -> ClientTracker -> Interaction () -> Storage -> IO ()
startWs host port tracker i storage =
    void . forkIO $ E.finally safeSpawn $ decRef (attempting tracker)
  where
    safeSpawn = eatExceptions $ spawn
    spawn = WS.runClientWith host port "/" WS.defaultConnectionOptions
              [("Origin", BC.concat [BC.pack host, ":", BC.pack $ show port])]
              $ interactionTester tracker i storage

interactionTester :: ClientTracker -> Interaction a -> Storage -> WS.ClientApp ()
interactionTester (ClientTracker att conns _) i storage conn = do
    incRef 1 conns
    E.finally runit $ decRef conns
  where
    config = newConfig conn
    runit = void $ runInteraction i config storage

incRef :: Int -> IORef Int -> IO ()
incRef v ref = void $ atomicModifyIORef' ref (\x -> (x+v, ()))

decRef :: IORef Int -> IO ()
decRef ref = void $ atomicModifyIORef' ref (\x -> (x-1, ()))

{- * SimplePush Client Interactions

-}

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
    helo Nothing (Just [])
    (_, endpoint) <- setupNewEndpoint
    forever $ do
        sendPushNotification endpoint Nothing
        wait 5

-- | Delivers a notification once every 10 seconds, pings every 20 seconds
pingDeliver :: Interaction ()
pingDeliver = do
    helo Nothing (Just [])
    (_, endpoint) <- setupNewEndpoint
    loop 0 endpoint
  where
    loop count endpoint = do
        sendPushNotification endpoint Nothing
        if count == 20 then do
            ping
            wait 10
            loop 10 endpoint
        else do
            wait 10
            loop (count+10) endpoint

-- | Registers a new channel ID every 10 seconds
--   Chooses a channel randomly every 5 seconds for delivery and sends a push
channelMonster :: Interaction ()
channelMonster = do
    helo Nothing (Just [])
    loop 0 S.empty
  where
    loop count eps = do
        eps' <- if (count `mod` 10 == 0) then do
                    (_, endpoint) <- setupNewEndpoint
                    return $ eps |> endpoint
                else return eps
        index <- randomNumber (0, S.length eps' - 1)
        let ep = S.index eps' index
        sendPushNotification ep Nothing
        wait 5
        loop (count+5) eps'
