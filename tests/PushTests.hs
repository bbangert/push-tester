{-# LANGUAGE OverloadedStrings #-}

module PushTests
  ( tests
  ) where

import           Test.Framework                       (Test, testGroup)
import           Test.Framework.Providers.QuickCheck2 (testProperty)
import           Test.QuickCheck                      (Property)

import           Util

tests :: Test
tests = testGroup "PushTests"
  [ testProperty "hello prop" prop_hello
  , testProperty "smoke-test" prop_smoketest
  ]

prop_hello :: AnyUaid -> Property
prop_hello (AnyUaid uid) = resultsIn
  [(Hello uid (Just []), HelloSuccess uid Nothing)]

prop_smoketest :: ValidUaid -> ValidChannelID -> Property
prop_smoketest (ValidUaid hex) (ValidChannelID cid) =
  resultsIn [
    (Hello hex (Just []),              HelloSuccess hex Nothing)
  , (Register cid,                     RegisterSuccess cid)
  , (SendNotification Nothing Nothing, NotificationUpdate 1)
  , (UnRegister cid,                   UnRegisterSuccess)
  ]
