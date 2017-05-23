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
import PostgRESTWS.Broadcast (Multiplexer, onMessage, readTChan)
import PostgRESTWS.HasqlBroadcast (newHasqlBroadcaster, newHasqlBroadcasterOrError)
import qualified PostgRESTWS.Broadcast as B

data Message = Message
  { claims :: A.Object
  , payload :: Text
  } deriving (Show, Eq, Generic)

instance A.ToJSON Message

-- | Given a secret, a function to fetch the system time, a Hasql Pool and a Multiplexer this will give you a WAI middleware.
postgrestWsMiddleware :: ByteString -> IO POSIXTime -> H.Pool -> Multiplexer -> Wai.Application -> Wai.Application
postgrestWsMiddleware =
  WS.websocketsOr WS.defaultConnectionOptions `compose` wsApp
  where
    compose = (.) . (.) . (.) . (.)

-- private functions

-- when the websocket is closed a ConnectionClosed Exception is triggered
-- this kills all children and frees resources for us
wsApp :: ByteString -> IO POSIXTime -> H.Pool -> Multiplexer -> WS.ServerApp
wsApp secret getTime pqCon multi pendingConn =
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
            onMessage multi channel (\ch ->
              forever $ atomically (readTChan ch) >>= WS.sendTextData conn . B.payload)

          notifySessionFinished <- if hasWrite mode
            then forkAndWait $ forever $ notifySession channel validClaims pqCon conn
            else newMVar ()
          takeMVar notifySessionFinished

-- Having both channel and claims as parameters seem redundant
-- But it allows the function to ignore the claims structure and the source
-- of the channel, so all claims decoding can be coded in the caller
notifySession :: BS.ByteString
                    -> A.Object
                    -> H.Pool
                    -> WS.Connection
                    -> IO ()
notifySession channel claimsToSend pool wsCon =
  WS.receiveData wsCon >>= (void . send . jsonMsg)
  where
    send = notifyPool pool channel
    -- we need to decode the bytestring to re-encode valid JSON for the notification
    jsonMsg = BL.toStrict . A.encode . Message claimsToSend . decodeUtf8With T.lenientDecode

forkAndWait :: IO () -> IO (MVar ())
forkAndWait io = do
  mvar <- newEmptyMVar
  void $ forkFinally io (\_ -> putMVar mvar ())
  return mvar
