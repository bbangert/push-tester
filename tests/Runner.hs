module Runner
  (
  ) where

import           Test.QuickCheck.Monadic (assert, monadicIO, run)
import qualified Network.WebSockets      as WS

class Action where
  -- | Run an action with the given state and transport, return a result
  -- and the new state
  run :: (Eq a) => s -> t -> IO (a, s)

class ResultRunner t where
  resultsIn :: (Action a, Eq b) => s -> t -> [(a, b)] -> Property

instance ResultRunner (WS.ClientApp a) where
  resultsIn initState transport lst = monadicIO $ do
    let (actions, results) = unzip lst
    results' <- run $ transport $ perform initState actions



perform :: (Action a, Eq b) => s -> [a] -> t -> IO [b]
perform state []

resultsIn :: (Action a, Eq b) => s -> [(a, b)] -> Property
resultsIn initState lst = monadicIO $ do
  let (actions, results) = unzip lst
  results' <- run $ withPushServer $ perform initState actions
  assert $ results == results'


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

withPushServer :: WS.ClientApp a -> IO a
withPushServer = WS.runClientWith "localhost" 8080 "/" WS.defaultConnectionOptions
                  [("Origin", "localhost:8080")]
