{-# LANGUAGE DeriveGeneric #-}

module PostgRESTWS
  ( postgrestWsMiddleware
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

data Message = Message A.Object Text deriving (Show, Eq, Generic)

instance A.ToJSON Message

postgrestWsMiddleware :: Maybe ByteString -> IO POSIXTime -> H.Pool -> Multiplexer -> Wai.Application -> Wai.Application
postgrestWsMiddleware = WS.websocketsOr WS.defaultConnectionOptions `compose` wsApp
  where
    compose = (.) . (.) . (.) . (.)

-- when the websocket is closed a ConnectionClosed Exception is triggered
-- this kills all children and frees resources for us
wsApp :: Maybe ByteString -> IO POSIXTime -> H.Pool -> Multiplexer -> WS.ServerApp
wsApp mSecret getTime pqCon multi pendingConn = getTime >>= forkSessionsWhenTokenIsValid . validateClaims mSecret jwtToken
  where
    forkSessionsWhenTokenIsValid = either rejectRequest forkSessions
    hasRead m = m == ("r" :: ByteString) || m == ("rw" :: ByteString)
    hasWrite m = m == ("w" :: ByteString) || m == ("rw" :: ByteString)
    rejectRequest = WS.rejectRequest pendingConn . encodeUtf8
    toBS :: Show a => a -> ByteString
    toBS = show
    -- the first char in path is '/' the rest is the token
    jwtToken = decodeUtf8 $ BS.drop 1 $ WS.requestPath $ WS.pendingRequest pendingConn
    forkSessions (channel, mode, claims) = do
          -- role claim defaults to anon if not specified in jwt
          -- We should accept only after verifying JWT
          conn <- WS.acceptRequest pendingConn
          -- Fork a pinging thread to ensure browser connections stay alive
          WS.forkPingThread conn 30
          -- each websocket needs its own listen connection to avoid
          -- handling of multiple waiting threads in the same connection

          when (hasRead mode) $
            onMessage multi channel (\ch ->
              (atomically $ readTChan ch) >>= WS.sendTextData conn . toBS)
          -- all websockets share a single connection to NOTIFY
          notifySessionFinished <- if hasWrite mode
            then forkAndWait $ forever $ notifySession channel claims pqCon conn
            else newMVar ()
          takeMVar notifySessionFinished

-- private functions

-- Having both channel and claims as parameters seem redundant
-- But it allows the function to ignore the claims structure and the source
-- of the channel, so all claims decoding can be coded in the caller
notifySession :: BS.ByteString
                    -> A.Object
                    -> H.Pool
                    -> WS.Connection
                    -> IO ()
notifySession channel claims pool wsCon =
  WS.receiveData wsCon >>= (void . send . jsonMsg)
  where
    send = notify pool channel
    -- we need to decode the bytestring to re-encode valid JSON for the notification
    jsonMsg = BL.toStrict . A.encode . Message claims . decodeUtf8With T.lenientDecode

forkAndWait :: IO () -> IO (MVar ())
forkAndWait io = do
  mvar <- newEmptyMVar
  void $ forkFinally io (\_ -> putMVar mvar ())
  return mvar
