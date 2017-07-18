{-| PostgRESTWS Middleware, composing this allows postgrest to create
    websockets connections that will communicate with the database through LISTEN/NOTIFY channels.
-}
{-# LANGUAGE DeriveGeneric #-}

module PostgRESTWS
  ( postgrestWsMiddleware
  -- * Re-exports
  , newHasqlBroadcaster
  , newHasqlBroadcasterOrError
  ) where

import           Protolude
import qualified Network.Wai                    as Wai
import qualified Network.Wai.Handler.WebSockets as WS
import qualified Network.WebSockets             as WS
import qualified Hasql.Pool                     as H

import qualified Data.Text.Encoding.Error       as T

import           Data.Time.Clock.POSIX          (POSIXTime)
import qualified Data.Aeson                     as A
import qualified Data.ByteString                as BS
import qualified Data.ByteString.Lazy           as BL

import PostgRESTWS.Claims
import PostgRESTWS.Database
import PostgRESTWS.Broadcast (Multiplexer, onMessage)
import PostgRESTWS.HasqlBroadcast (newHasqlBroadcaster, newHasqlBroadcasterOrError)
import qualified PostgRESTWS.Broadcast as B

data Message = Message
  { claims :: A.Object
  , payload :: Text
  } deriving (Show, Eq, Generic)

instance A.ToJSON Message

-- | Given a secret, a function to fetch the system time, a Hasql Pool and a Multiplexer this will give you a WAI middleware.
postgrestWsMiddleware :: Maybe PgIdentifier -> ByteString -> IO POSIXTime -> H.Pool -> Multiplexer -> Wai.Application -> Wai.Application
postgrestWsMiddleware =
  WS.websocketsOr WS.defaultConnectionOptions `compose` wsApp
  where
    compose = (.) . (.) . (.) . (.) . (.)

-- private functions

-- when the websocket is closed a ConnectionClosed Exception is triggered
-- this kills all children and frees resources for us
wsApp :: Maybe PgIdentifier -> ByteString -> IO POSIXTime -> H.Pool -> Multiplexer -> WS.ServerApp
wsApp mAuditChannel secret getTime pqCon multi pendingConn =
  getTime >>= forkSessionsWhenTokenIsValid . validateClaims secret jwtToken
  where
    forkSessionsWhenTokenIsValid = either rejectRequest forkSessions
    hasRead m = m == ("r" :: ByteString) || m == ("rw" :: ByteString)
    hasWrite m = m == ("w" :: ByteString) || m == ("rw" :: ByteString)
    rejectRequest = WS.rejectRequest pendingConn . encodeUtf8
    -- the first char in path is '/' the rest is the token
    jwtToken = decodeUtf8 $ BS.drop 1 $ WS.requestPath $ WS.pendingRequest pendingConn

    forkSessions (channel, mode, validClaims) = do
          -- role claim defaults to anon if not specified in jwt
          -- We should accept only after verifying JWT
          conn <- WS.acceptRequest pendingConn
          -- Fork a pinging thread to ensure browser connections stay alive
          WS.forkPingThread conn 30

          when (hasRead mode) $
            onMessage multi channel $ WS.sendTextData conn . B.payload

          when (hasWrite mode) $
            notifySession validClaims conn (error "need to implement send notification")
            -- send = notifyPool pool channel
            
          waitForever <- newEmptyMVar
          void $ takeMVar waitForever

-- Having both channel and claims as parameters seem redundant
-- But it allows the function to ignore the claims structure and the source
-- of the channel, so all claims decoding can be coded in the caller
notifySession :: A.Object
                  -> WS.Connection
                  -> (ByteString -> IO ())
                  -> IO ()
notifySession claimsToSend wsCon send =
  withAsync (forever relayData) wait
  where
    relayData = WS.receiveData wsCon >>= (void . send . jsonMsg)
    -- we need to decode the bytestring to re-encode valid JSON for the notification
    jsonMsg = BL.toStrict . A.encode . Message claimsToSend . decodeUtf8With T.lenientDecode
