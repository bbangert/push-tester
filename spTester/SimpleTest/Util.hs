{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TypeSynonymInstances #-}

module SimpleTest.Util
  ( ValidUaid (..)
  , ValidChannelID (..)
  , AnyUaid (..)
  ) where

import           Control.Applicative     ((<$>))
import           Data.String             (fromString)
import           Test.QuickCheck         (Arbitrary (..), Gen,
                                          arbitrary, elements, listOf1,
                                          vectorOf)

import           SimpleTest.Types (ChannelID, Uaid)

newtype AnyUaid = AnyUaid Uaid deriving (Show)
newtype ValidUaid = ValidUaid Uaid deriving (Show)
newtype ValidChannelID = ValidChannelID ChannelID deriving (Show)

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
