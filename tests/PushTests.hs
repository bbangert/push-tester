{-# LANGUAGE OverloadedStrings    #-}

module PushTests
  ( tests
  ) where

import           Data.Text                            (Text)
import qualified Data.Text                            as T
import           Test.Framework                       (Test, testGroup)
import           Test.Framework.Providers.HUnit       (testCase)
import           Test.Framework.Providers.QuickCheck2 (testProperty)
import           Test.HUnit                           (Assertion, (@=?))
import           Test.QuickCheck                      (Property,
                                                       (==>))
import           Test.QuickCheck.Monadic              (assert, monadicIO, run)

import           PushClient
import           Util

tests :: Test
tests = testGroup "PushTests"
  [ testProperty "hello prop" prop_hello
  , testProperty "smoke-test" prop_smoketest
  ]

prop_hello :: HelloMessage -> Property
prop_hello msg = monadicIO $ do
  rmsg <- run $ withPushServer $ sendReceiveMessage msg
  assert $ uaid rmsg == uaid msg
  assert $ status rmsg == Just 200

prop_smoketest :: ValidUaid -> ValidChannelID -> Property
prop_smoketest (ValidUaid hex) (ValidChannelID cid) =
  [ Hello hex (Just [])
  , Register cid
  , SendNotification Nothing Nothing]
  `resultsIn`
  [ HelloSuccess hex (Just [])
  , RegisterSuccess cid
  , NotificationUpdate $ mkUpdates [(cid, 1)]]
