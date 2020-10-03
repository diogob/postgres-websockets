{-|
Module      : PostgresWebsockets.Middleware
Description : PostgresWebsockets WAI middleware, add functionality to any WAI application.

Allow websockets connections that will communicate with the database through LISTEN/NOTIFY channels.
-}
{-# LANGUAGE DeriveGeneric #-}

module PostgresWebsockets.Middleware
  ( postgresWsMiddleware
  ) where

import Protolude
import Data.Time.Clock (UTCTime)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds, posixSecondsToUTCTime)
import Control.Concurrent.AlarmClock (newAlarmClock, setAlarm)
import qualified Hasql.Notifications as H
import qualified Hasql.Pool as H
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.WebSockets as WS
import qualified Network.WebSockets as WS

import qualified Data.Aeson as A
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.HashMap.Strict as M
import qualified Data.Text.Encoding.Error as T

import PostgresWebsockets.Broadcast (onMessage)
import PostgresWebsockets.Claims ( ConnectionInfo, validateClaims )
import PostgresWebsockets.Context ( Context(..) )
import PostgresWebsockets.Config (AppConfig(..))
import qualified PostgresWebsockets.Broadcast as B


data Event =
    WebsocketMessage
  | ConnectionOpen
  deriving (Show, Eq, Generic)

data Message = Message
  { claims  :: A.Object
  , event   :: Event
  , payload :: Text
  , channel :: Text
  } deriving (Show, Eq, Generic)

instance A.ToJSON Event
instance A.ToJSON Message

-- | Given a secret, a function to fetch the system time, a Hasql Pool and a Multiplexer this will give you a WAI middleware.
postgresWsMiddleware :: Context -> Wai.Middleware
postgresWsMiddleware =
  WS.websocketsOr WS.defaultConnectionOptions . wsApp

-- private functions
jwtExpirationStatusCode :: Word16
jwtExpirationStatusCode = 3001

-- when the websocket is closed a ConnectionClosed Exception is triggered
-- this kills all children and frees resources for us
wsApp :: Context -> WS.ServerApp
wsApp Context{..} pendingConn =
  ctxGetTime >>= validateClaims requestChannel (configJwtSecret ctxConfig) (toS jwtToken) >>= either rejectRequest forkSessions
  where
    hasRead m = m == ("r" :: ByteString) || m == ("rw" :: ByteString)
    hasWrite m = m == ("w" :: ByteString) || m == ("rw" :: ByteString)

    rejectRequest :: Text -> IO ()
    rejectRequest msg = do
      putErrLn $ "Rejecting Request: " <> msg
      WS.rejectRequest pendingConn (toS msg)

    -- the URI has one of the two formats - /:jwt or /:channel/:jwt
    pathElements = BS.split '/' $ BS.drop 1 $ WS.requestPath $ WS.pendingRequest pendingConn
    jwtToken = 
      case length pathElements `compare` 1 of
        GT -> headDef "" $ tailSafe pathElements
        _ -> headDef "" pathElements
    requestChannel =
      case length pathElements `compare` 1 of
        GT -> Just $ headDef "" pathElements
        _ -> Nothing
    forkSessions :: ConnectionInfo -> IO ()
    forkSessions (chs, mode, validClaims) = do
          -- We should accept only after verifying JWT
          conn <- WS.acceptRequest pendingConn
          -- Fork a pinging thread to ensure browser connections stay alive
          WS.withPingThread conn 30 (pure ()) $ do
            case M.lookup "exp" validClaims of
              Just (A.Number expClaim) -> do
                connectionExpirer <- newAlarmClock $ const (WS.sendCloseCode conn jwtExpirationStatusCode ("JWT expired" :: ByteString))
                setAlarm connectionExpirer (posixSecondsToUTCTime $ realToFrac expClaim)
              Just _ -> pure ()
              Nothing -> pure ()

            let sendNotification msg channel = sendMessageWithTimestamp $ websocketMessageForChannel msg channel
                sendMessageToDatabase = sendToDatabase ctxPool (configListenChannel ctxConfig)
                sendMessageWithTimestamp = timestampMessage ctxGetTime >=> sendMessageToDatabase
                websocketMessageForChannel = Message validClaims WebsocketMessage
                connectionOpenMessage = Message validClaims ConnectionOpen

            case configMetaChannel ctxConfig of
              Nothing -> pure ()
              Just ch -> sendMessageWithTimestamp $ connectionOpenMessage (toS $ BS.intercalate "," chs) ch

            when (hasRead mode) $
              forM_ chs $ flip (onMessage ctxMulti) $ WS.sendTextData conn . B.payload

            when (hasWrite mode) $
              notifySession conn sendNotification chs

            waitForever <- newEmptyMVar
            void $ takeMVar waitForever

-- Having both channel and claims as parameters seem redundant
-- But it allows the function to ignore the claims structure and the source
-- of the channel, so all claims decoding can be coded in the caller
notifySession :: WS.Connection -> (Text -> Text -> IO ()) -> [ByteString] -> IO ()
notifySession wsCon sendToChannel chs =
  withAsync (forever relayData) wait
  where
    relayData = do
      msg <- WS.receiveData wsCon
      forM_ chs (sendToChannel msg . toS)

sendToDatabase :: H.Pool -> Text -> Message -> IO ()
sendToDatabase pool dbChannel =
  notify . jsonMsg
  where
    notify = void . H.notifyPool pool dbChannel . toS
    jsonMsg = BL.toStrict . A.encode

timestampMessage :: IO UTCTime -> Message -> IO Message
timestampMessage getTime msg@Message{..} = do
  time <- utcTimeToPOSIXSeconds <$> getTime
  return $ msg{ claims = M.insert "message_delivered_at" (A.Number $ realToFrac time) claims}
