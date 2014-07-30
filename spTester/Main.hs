{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Control.Concurrent    (forkIO, runInUnboundThread, threadDelay)
import qualified Control.Exception     as E
import           Control.Monad         (forever, replicateM_, void)
import           Control.Monad.Trans   (liftIO)
import           Crypto.Random         (newGenIO)
import           Data.ByteString       (ByteString)
import qualified Data.ByteString.Char8 as BC
import           Data.IORef
import qualified Network.WebSockets    as WS
import           System.Environment    (getArgs)
import           Test.QuickCheck       (arbitrary)
import           Test.QuickCheck.Gen   (Gen, generate)
import           Text.Printf           (printf)

import           SimpleTest.Interact
import           SimpleTest.Util       (ValidChannelID(..))

data ClientTracker = ClientTracker
    { attempting :: IORef Int
    , connected  :: IORef Int
    , maxClients :: Int
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
    printf "Clients Connected: %s \tAttempting: %s" (show count) (show attempts)
    let spawnCount = m - attempts
    incRef spawnCount att
    replicateM_ spawnCount spawn

main :: IO ()
main = runInUnboundThread $ do
    [ip, port, spawnCount] <- getArgs
    let maxC = read spawnCount
    clientTracker <- newClientTracker maxC

    let spawn = startWs ip (read port) clientTracker basicInteraction
    incRef maxC (attempting clientTracker)
    replicateM_ maxC spawn
    watcher clientTracker spawn

startWs :: String -> Int -> ClientTracker -> Interaction () -> IO ()
startWs host port tracker i =
    void . forkIO $ E.finally safeSpawn $ decRef (attempting tracker)
  where
    safeSpawn = eatExceptions $ spawn
    spawn = WS.runClientWith host port "/" WS.defaultConnectionOptions
              [("Origin", BC.concat [BC.pack host, ":", BC.pack $ show port])]
              $ interactionTester (connected tracker) i

interactionTester :: IORef Int -> Interaction a -> WS.ClientApp ()
interactionTester c i conn = do
    incRef 1 c
    E.finally runit $ decRef c
  where
    storage = newStorage
    config = newConfig conn
    runit = void $ runInteraction i config storage

incRef :: Int -> IORef Int -> IO ()
incRef v ref = void $ atomicModifyIORef' ref (\x -> (x+v, ()))

decRef :: IORef Int -> IO ()
decRef ref = void $ atomicModifyIORef' ref (\x -> (x-1, ()))

basicInteraction :: Interaction ()
basicInteraction = do
    helo Nothing Nothing
    (ValidChannelID cid) <- liftIO $ generate (arbitrary :: Gen ValidChannelID)
    register cid
    return ()
