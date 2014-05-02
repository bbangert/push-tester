{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Util
  ( HelloMessage
  , resultsIn
  , ValidUaid (..)
  , ValidChannelID (..)
  , Action (..)
  , Result (..)
  , withPushServer
  , mkUpdates
  ) where

import           Control.Applicative     ((<$>), (<*>))
import           Control.Concurrent      (forkIO)
import           Control.Monad           (void)
import           Control.Monad           (liftM2)
import qualified Data.ByteString.Lazy    as BL
import           Data.Maybe              (fromJust)
import           Data.String             (fromString)
import           Data.Text               (Text)
import           Network.Wreq            (put)
import           Test.QuickCheck         (Arbitrary (..), Gen, Property,
                                          arbitrary, elements, listOf1, oneof, resize)
import           Test.QuickCheck.Monadic (assert, monadicIO, run)

import qualified Network.WebSockets      as WS

import           PushClient

type Uaid = Maybe Text
type ChannelIDs = Maybe [String]
type ChannelID = Maybe String
type Version = Maybe Int
type HelloMessage = Message
type Endpoint = Maybe String
type Updates = Maybe [ChannelUpdate]

newtype ValidUaid = ValidUaid Uaid deriving (Show)
newtype ValidChannelID = ValidChannelID ChannelID deriving (Show)

instance Arbitrary HelloMessage where
  arbitrary = do
    arbUaid <- fromString <$> uaidGen
    return mkMessage {messageType="hello", uaid=Just arbUaid, channelIDs=Just []}

instance Arbitrary ValidUaid where
  arbitrary = ValidUaid . Just . fromString <$> uaidGen

instance Arbitrary ValidChannelID where
  arbitrary = ValidChannelID . Just <$> (resize 16 $ listOf1 hexChar)

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

mkUpdates :: [(Maybe String, Int)] -> Updates
mkUpdates [] = Just []
mkUpdates ((Just cid, ver):xs) = liftM2 (:) (Just $ ChannelUpdate cid ver) (mkUpdates xs)

noEndpoints :: String
noEndpoints = "No endpoint supplied, and no prior channelID register call"

serializeVersion :: Version -> BL.ByteString
serializeVersion Nothing = "version="
serializeVersion (Just ver) = BL.append "version=" (esc ver)
  where esc = fromString . show

perform :: [String] -> [Action] -> WS.ClientApp [Result]
perform _ [] _ = return []
perform eps (a:as) conn =
  case a of
    Register _ -> do
      msg <- sendReceiveMessage (newMsg a) conn
      let ep = fromJust $ pushEndpoint msg
      liftM2 (:) (return $ parseResult msg) (perform (eps++[ep]) as conn)
    SendNotification e ver ->
      case e of
        Nothing ->
          if length eps == 0
            then liftM2 (:) (return $ BadRequest noEndpoints) (perform eps as conn)
            else do
              let (endpoint:es) = eps
              _ <- send endpoint $ serializeVersion ver
              liftM2 (:) (parseResult <$> receiveMessage conn) (perform es as conn)
        Just endpoint -> do
          _ <- send endpoint $ serializeVersion ver
          liftM2 (:) (parseResult <$> receiveMessage conn) (perform eps as conn)
    _ -> performIt
  where
    performIt = liftM2 (:) (parseResult <$> sendReceiveMessage (newMsg a) conn)
                           (perform eps as conn)
    newMsg (Hello uid cids) = mkMessage {messageType="hello", uaid=uid, channelIDs=cids}
    newMsg (Register cid)   = mkMessage {messageType="register", channelID=cid}
    newMsg (UnRegister cid) = mkMessage {messageType="unregister", channelID=cid}
    send endpoint version = forkIO $ void $ put endpoint version

parseResult :: Message -> Result
parseResult m@Message { status = Nothing }            = BadResponse m
parseResult m@Message { status = Just x }
  | x /= 200                                          = BadResponse m
parseResult Message { messageType = "hello",
                      uaid = uid, channelIDs = cids } = HelloSuccess uid cids
parseResult Message { messageType = "register",
                      channelID=cid }                 = RegisterSuccess cid
parseResult Message { messageType = "unregister"}     = UnRegisterSuccess
parseResult Message { messageType = "notification",
                      updates = us}                   = NotificationUpdate us
parseResult msg                                       = BadResponse msg

resultsIn :: [Action] -> [Result] -> Property
resultsIn actions results = monadicIO $ do
  results' <- run $ withPushServer $ perform [] actions
  assert $ results == results'

uaidGen :: Gen String
uaidGen = listOf1 hexChar

hexChar :: Gen Char
hexChar = (elements (['A'..'F'] ++ ['a'..'f'] ++ ['0'..'9'] ++ "-"))

randomMessageType :: Gen String
randomMessageType = elements ["hello", "register", "unregister", "ping"]

withPushServer :: WS.ClientApp a -> IO a
withPushServer app = WS.runClientWith
                        "localhost" 8080 "/"
                        WS.defaultConnectionOptions
                        [("Origin", "localhost:8080")] app
