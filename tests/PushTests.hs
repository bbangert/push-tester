{-# LANGUAGE OverloadedStrings #-}

module PushTests
  ( tests
  ) where

import           Test.Framework                       (Test, testGroup)
import           Test.Framework.Providers.HUnit       (testCase)
import           Test.Framework.Providers.QuickCheck2 (testProperty)
import           Control.Monad.Trans  (liftIO)
import           Test.HUnit                           (Assertion, (@=?))
import           Test.QuickCheck                      (Arbitrary (..), Gen, Property, (==>))
import           Test.QuickCheck.Monadic              (assert, monadicIO)

import qualified Network.WebSockets                   as WS

import           PushClient

tests :: Test
tests = testGroup "PushTests"
  [ testCase "hello server" testHello
  , testProperty "hello prop" prop_hello ]

testHello :: Assertion
testHello = withPushServer $ \conn -> do
  let helloMsg = mkMessage { messageType="hello", uaid=Just defaultUaid, channelIDs=Just [] }
  msg <- sendReceiveMessage helloMsg conn
  uaid msg @=? Just defaultUaid
  status msg @=? Just 200

prop_hello :: Message -> Property
prop_hello msg = isHelloMessage msg ==> monadicIO test
  where test = withPushServer $ \conn -> do
        rmsg <- sendReceiveMessage msg conn
        assert $ uaid rmsg == Just (uaid msg)
        assert $ status msg == Just 200

isHelloMessage :: Message -> Bool
isHelloMessage (Message "hello" _ (Just _) Nothing Nothing Nothing Nothing) = True
isHelloMessage _ = False

withPushServer :: WS.ClientApp a -> IO a
withPushServer app = WS.runClientWith
                        "localhost" 8080 "/"
                        WS.defaultConnectionOptions
                        [("Origin", "localhost:8080")] app
