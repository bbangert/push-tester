{-# OPTIONS_GHC -funbox-strict-fields #-}

module SimpleTest.Types
    (
      -- * Basic SimpleTest Types
      Uaid
    , ChannelIDs
    , ChannelID
    , Version
    , Endpoint
    , Updates
    ) where

import           Data.Text (Text)

type Uaid = Maybe Text
type ChannelIDs = Maybe [String]
type ChannelID = Maybe String
type Version = Maybe Int
type Endpoint = String
type Updates = Int
