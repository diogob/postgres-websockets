{-| PostgresWebsockets Middleware, composing this allows postgrest to create
    websockets connections that will communicate with the database through LISTEN/NOTIFY channels.
-}
{-# LANGUAGE DeriveGeneric #-}

module PostgresWebsockets
  ( postgresWsMiddleware
  -- * Re-exports
  , newHasqlBroadcaster
  , newHasqlBroadcasterOrError
  ) where

import qualified Hasql.Pool                     as H
import qualified Network.Wai                    as Wai
import qualified Network.Wai.Handler.WebSockets as WS
import qualified Network.WebSockets             as WS
import           Protolude

import qualified Data.Aeson                     as A
import qualified Data.ByteString.Char8          as BS
import qualified Data.ByteString.Lazy           as BL
import qualified Data.HashMap.Strict            as M
import qualified Data.Text.Encoding.Error       as T
import           Data.Time.Clock.POSIX          (getPOSIXTime)
import           PostgresWebsockets.Broadcast          (Multiplexer, onMessage)
import qualified PostgresWebsockets.Broadcast          as B
import           PostgresWebsockets.Claims
import           PostgresWebsockets.Database
import           PostgresWebsockets.HasqlBroadcast     (newHasqlBroadcaster,
                                                 newHasqlBroadcasterOrError)

data Message = Message
  { claims  :: A.Object
  , channel :: Text
  , payload :: Text
  } deriving (Show, Eq, Generic)

instance A.ToJSON Message

-- | Given a secret, a function to fetch the system time, a Hasql Pool and a Multiplexer this will give you a WAI middleware.
postgresWsMiddleware :: ByteString -> ByteString -> H.Pool -> Multiplexer -> Wai.Application -> Wai.Application
postgresWsMiddleware =
  WS.websocketsOr WS.defaultConnectionOptions `compose` wsApp
  where
    compose = (.) . (.) . (.) . (.)

-- private functions

-- when the websocket is closed a ConnectionClosed Exception is triggered
-- this kills all children and frees resources for us
wsApp :: ByteString -> ByteString -> H.Pool -> Multiplexer -> WS.ServerApp
wsApp dbChannel secret pool multi pendingConn =
  validateClaims requestChannel secret (toS jwtToken) >>= either rejectRequest forkSessions
  where
    hasRead m = m == ("r" :: ByteString) || m == ("rw" :: ByteString)
    hasWrite m = m == ("w" :: ByteString) || m == ("rw" :: ByteString)
    rejectRequest = WS.rejectRequest pendingConn . encodeUtf8
    -- the URI has one of the two formats - /:jwt or /:channel/:jwt 
    pathElements = BS.split '/' $ BS.drop 1 $ WS.requestPath $ WS.pendingRequest pendingConn
    jwtToken
      | length pathElements > 1 = headDef "" $ tailSafe pathElements
      | length pathElements <= 1 = headDef "" pathElements
    requestChannel
      | length pathElements > 1 = Just $ headDef "" pathElements
      | length pathElements <= 1 = Nothing
    forkSessions (ch, mode, validClaims) = do
          -- role claim defaults to anon if not specified in jwt
          -- We should accept only after verifying JWT
          conn <- WS.acceptRequest pendingConn
          -- Fork a pinging thread to ensure browser connections stay alive
          WS.forkPingThread conn 30

          when (hasRead mode) $
            onMessage multi ch $ WS.sendTextData conn . B.payload

          when (hasWrite mode) $
            let sendNotifications = void . notifyPool pool dbChannel
            in notifySession validClaims (toS ch) conn sendNotifications

          waitForever <- newEmptyMVar
          void $ takeMVar waitForever

-- Having both channel and claims as parameters seem redundant
-- But it allows the function to ignore the claims structure and the source
-- of the channel, so all claims decoding can be coded in the caller
notifySession :: A.Object
              -> Text
              -> WS.Connection
              -> (ByteString -> IO ())
              -> IO ()
notifySession claimsToSend ch wsCon send =
  withAsync (forever relayData) wait
  where
    relayData = jsonMsgWithTime >>= send

    jsonMsgWithTime = liftA2 jsonMsg claimsWithTime (WS.receiveData wsCon)

    -- we need to decode the bytestring to re-encode valid JSON for the notification
    jsonMsg :: M.HashMap Text A.Value -> ByteString -> ByteString
    jsonMsg cl = BL.toStrict . A.encode . Message cl ch . decodeUtf8With T.lenientDecode

    claimsWithChannel = M.insert "channel" (A.String ch) claimsToSend
    claimsWithTime :: IO (M.HashMap Text A.Value)
    claimsWithTime = do
      time <- getPOSIXTime
      return $ M.insert "message_delivered_at" (A.Number $ fromRational $ toRational time) claimsWithChannel
