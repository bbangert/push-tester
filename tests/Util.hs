{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Util
	( HelloMessage
	) where

import           Control.Applicative ((<$>), (<*>))
import           Data.String         (fromString)
import           Test.QuickCheck     (Arbitrary (..), Gen, arbitrary,
                                      elements, listOf1, oneof)

import           PushClient

type HelloMessage = Message

instance Arbitrary HelloMessage where
  arbitrary = do
    arbUaid <- fromString <$> uaidGen
    return mkMessage {messageType="hello", uaid=Just arbUaid, channelIDs=Just []}

uaidGen :: Gen String
uaidGen = listOf1 hexChar

hexChar :: Gen Char
hexChar = (elements (['A'..'F'] ++ ['a'..'f'] ++ ['0'..'9'] ++ "-"))

randomMessageType :: Gen String
randomMessageType = elements ["hello", "register", "unregister", "ping"]
