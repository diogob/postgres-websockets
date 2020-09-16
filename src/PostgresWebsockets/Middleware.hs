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

data Message = Message
  { claims  :: A.Object
  , channel :: Text
  , payload :: Text
  } deriving (Show, Eq, Generic)

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

            when (hasRead mode) $
              forM_ chs $ flip (onMessage ctxMulti) $ WS.sendTextData conn . B.payload

            when (hasWrite mode) $
              let sendNotifications = void . H.notifyPool ctxPool (configListenChannel ctxConfig) . toS
              in notifySession validClaims conn ctxGetTime sendNotifications chs

            waitForever <- newEmptyMVar
            void $ takeMVar waitForever

-- Having both channel and claims as parameters seem redundant
-- But it allows the function to ignore the claims structure and the source
-- of the channel, so all claims decoding can be coded in the caller
notifySession :: A.Object
              -> WS.Connection
              -> IO UTCTime
              -> (ByteString -> IO ())
              -> [ByteString]
              -> IO ()
notifySession claimsToSend wsCon getTime send chs =
  withAsync (forever relayData) wait
  where
    relayData = do 
      msg <- WS.receiveData wsCon
      forM_ chs (relayChannelData msg . toS)

    relayChannelData msg ch = do
      claims' <- claimsWithTime ch
      send $ jsonMsg ch claims' msg

    -- we need to decode the bytestring to re-encode valid JSON for the notification
    jsonMsg :: Text -> M.HashMap Text A.Value -> ByteString -> ByteString
    jsonMsg ch cl = BL.toStrict . A.encode . Message cl ch . decodeUtf8With T.lenientDecode

    claimsWithTime :: Text -> IO (M.HashMap Text A.Value)
    claimsWithTime ch = do
      time <- utcTimeToPOSIXSeconds <$> getTime
      return $ M.insert "message_delivered_at" (A.Number $ realToFrac time) (claimsWithChannel ch)

    claimsWithChannel ch = M.insert "channel" (A.String ch) claimsToSend
