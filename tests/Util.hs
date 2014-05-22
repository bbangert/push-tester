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

resultsIn :: [(Action, Result)] -> Property
resultsIn lst = monadicIO $ do
  let (actions, results) = unzip lst
  results' <- run $ withPushServer $ perform [] actions
  assert $ results == results'

perform :: [String] -> [Action] -> WS.ClientApp [Result]
perform _ [] _ = return []
perform eps (a:as) conn =
  case a of
    Register _ -> do
      msg <- sendReceiveMessage (newMsg a) conn
      let ep = fromJust $ pushEndpoint msg
      liftM2 (:) (return $ parseResult msg) (perform (eps++[ep]) as conn)
    SendNotification Nothing ver
      | null eps -> liftM2 (:) (return $ BadRequest noEndpoints) (perform eps as conn)
      | otherwise -> do
        let (endpoint:es) = eps
        send endpoint ver
        liftM2 (:) (parseResult <$> receiveMessage conn) (perform es as conn)
    SendNotification (Just endpoint) ver -> send endpoint ver >>
      liftM2 (:) (parseResult <$> receiveMessage conn) (perform eps as conn)
    _ -> go
  where
    go = liftM2 (:) (parseResult <$> sendReceiveMessage (newMsg a) conn)
                    (perform eps as conn)

send :: String -> Version -> IO ()
send ep ver = void $ forkIO $ void $ put ep $ serializeVersion ver

noEndpoints :: String
noEndpoints = "No endpoint supplied, and no prior channelID register call"

serializeVersion :: Version -> BL.ByteString
serializeVersion Nothing = "version="
serializeVersion (Just ver) = BL.append "version=" $ esc ver
  where esc = fromString . show

newMsg :: Action -> Message
newMsg (Hello uid cids) = mkMessage {messageType="hello", uaid=uid, channelIDs=cids}
newMsg (Register cid)   = mkMessage {messageType="register", channelID=cid}
newMsg (UnRegister cid) = mkMessage {messageType="unregister", channelID=cid}

parseResult :: Message -> Result
parseResult m@Message { status = Just x }
  | x /= 200                                          = BadResponse m
parseResult Message { messageType = "hello",
                      uaid = uid, channelIDs = cids } = HelloSuccess uid cids
parseResult Message { messageType = "register",
                      channelID=cid }                 = RegisterSuccess cid
parseResult Message { messageType = "unregister"}     = UnRegisterSuccess
parseResult Message { messageType = "notification",
                      updates = us}                   = NotificationUpdate (length . fromJust $ us)
parseResult msg                                       = BadResponse msg

uaidGen :: Gen String
uaidGen = listOf1 hexChar

hexChar :: Gen Char
hexChar = elements (['a'..'f'] ++ ['0'..'9'])

withPushServer :: WS.ClientApp a -> IO a
withPushServer = WS.runClientWith "localhost" 8080 "/" WS.defaultConnectionOptions
                  [("Origin", "localhost:8080")]
