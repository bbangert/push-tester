{-# OPTIONS_GHC -funbox-strict-fields #-}
{-# LANGUAGE FlexibleInstances #-}

module SimpleTest.Types
    (
      -- * Basic SimpleTest Types
      Uaid
    , ChannelIDs
    , ChannelID
    , Version
    , Endpoint
    , Updates
    , Notification (..)
    , Data
    ) where

import qualified Data.ByteString           as S
import           Data.Text                 (Text)
import           Network.HTTP.Client       (Request (method))
import qualified Network.HTTP.Client       as HTTP
import           Network.HTTP.Types.Method (methodPut)
import           Network.Wreq.Types        (FormParam ((:=)),
                                            Putable (putPayload),
                                            renderFormValue)


type Uaid = Maybe Text
type ChannelIDs = Maybe [String]
type ChannelID = Maybe String
type Version = Maybe Int
type Endpoint = String
type Updates = Int
type Data = Maybe String

data Notification = Notification Version Data deriving (Show)


instance Putable [(S.ByteString, S.ByteString)] where
    putPayload ps req = return $ (HTTP.urlEncodedBody ps req) { method = methodPut }

instance Putable [FormParam] where
    putPayload ps = putPayload (map f ps)
        where f (a := b) = (a, renderFormValue b)

instance Putable FormParam where
    putPayload p = putPayload [p]
