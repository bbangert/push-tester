{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Util
  ( resultsIn
  , ValidUaid (..)
  , ValidChannelID (..)
  , Action (..)
  , Result (..)
  , withPushServer
  , AnyUaid (..)
  ) where

import           Control.Applicative     ((<$>))
import           Control.Concurrent      (forkIO)
import           Control.Monad           (liftM2, void)
import qualified Data.ByteString.Lazy    as BL
import           Data.Maybe              (fromJust)
import           Data.String             (fromString)
import           Data.Text               (Text)
import           Network.Wreq            (put)
import           Test.QuickCheck         (Arbitrary (..), Gen, Property,
                                          arbitrary, elements, listOf1,
                                          vectorOf)
import           Test.QuickCheck.Monadic (assert, monadicIO, run)

import qualified Network.WebSockets      as WS

import           PushClient

type Uaid = Maybe Text
type ChannelIDs = Maybe [String]
type ChannelID = Maybe String
type Version = Maybe Int
type Endpoint = Maybe String
type Updates = Int

newtype AnyUaid = AnyUaid Uaid deriving (Show)
newtype ValidUaid = ValidUaid Uaid deriving (Show)
newtype ValidChannelID = ValidChannelID ChannelID deriving (Show)

data Action = Hello Uaid ChannelIDs
            | Register ChannelID
            | UnRegister ChannelID
            | SendNotification Endpoint Version
            deriving (Show, Eq)

data Result = HelloSuccess Uaid ChannelIDs
             | RegisterSuccess ChannelID
             | UnRegisterSuccess
             | NotificationUpdate Updates
             | BadResponse Message
             | BadRequest String
             deriving (Show, Eq)

instance Arbitrary AnyUaid where
  arbitrary = AnyUaid . Just . fromString <$> uaidGen

instance Arbitrary ValidUaid where
  arbitrary = ValidUaid . Just . fromString <$> vectorOf 32 hexChar

instance Arbitrary ValidChannelID where
  arbitrary = ValidChannelID . Just <$> vectorOf 32 hexChar



uaidGen :: Gen String
uaidGen = listOf1 hexChar

hexChar :: Gen Char
hexChar = elements (['a'..'f'] ++ ['0'..'9'])
