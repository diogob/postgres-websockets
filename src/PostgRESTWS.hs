{-# LANGUAGE DeriveGeneric #-}

module PostgRESTWS
  ( postgrestWsApp
  ) where

import           Protolude
import           GHC.IORef
import qualified Hasql.Pool                     as H
import qualified Network.Wai                    as Wai
import qualified Network.Wai.Handler.WebSockets as WS
import qualified Network.WebSockets             as WS
import           PostgREST.App                  as PGR
import           PostgREST.Config               as PGR
import           PostgREST.Types                as PGR

import qualified Data.Text.Encoding.Error       as T

import           Data.Time.Clock.POSIX          (POSIXTime)
import qualified Database.PostgreSQL.LibPQ      as PQ
import qualified Data.Aeson                     as A
import qualified Data.ByteString                as BS
import qualified Data.ByteString.Lazy           as BL

import PostgRESTWS.Claims

data Message = Message A.Object Text deriving (Show, Eq, Generic)

instance A.ToJSON Message

postgrestWsApp :: PGR.AppConfig
                    -> IORef PGR.DbStructure
                    -> H.Pool
                    -> PQ.Connection
                    -> IO POSIXTime
                    -> Wai.Application
postgrestWsApp conf refDbStructure pool pqCon getTime =
  WS.websocketsOr WS.defaultConnectionOptions (wsApp conf getTime pqCon) $ postgrest conf refDbStructure pool getTime
-- when the websocket is closed a ConnectionClosed Exception is triggered
-- this kills all children and frees resources for us
wsApp :: PGR.AppConfig -> IO POSIXTime -> PQ.Connection -> WS.ServerApp
wsApp conf getTime pqCon pendingConn = getTime >>= forkSessionsWhenTokenIsValid . validateClaims (configJwtSecret conf) jwtToken
  where
    forkSessionsWhenTokenIsValid = either rejectRequest forkSessions
    hasRead m = m == ("r" :: ByteString) || m == ("rw" :: ByteString)
    hasWrite m = m == ("w" :: ByteString) || m == ("rw" :: ByteString)
    rejectRequest = WS.rejectRequest pendingConn . encodeUtf8
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
          listenSessionFinished <- if hasRead mode
            then forkAndWait $ listenSession channel conf conn
            else newMVar ()
          -- all websockets share a single connection to NOTIFY
          notifySessionFinished <- if hasWrite mode
            then forkAndWait $ forever $ notifySession channel claims pqCon conn
            else newMVar ()
          takeMVar listenSessionFinished
          takeMVar notifySessionFinished

-- private functions

-- Having both channel and claims as parameters seem redundant
-- But it allows the function to ignore the claims structure and the source
-- of the channel, so all claims decoding can be coded in the caller
notifySession :: BS.ByteString
                    -> A.Object
                    -> PQ.Connection
                    -> WS.Connection
                    -> IO ()
notifySession channel claims pqCon wsCon =
  WS.receiveData wsCon >>= (notify . jsonMsg)
  where
    notify mesg = void $ PQ.exec pqCon ("NOTIFY " <> channel <> ", '" <> mesg <> "'")
    -- we need to decode the bytestring to re-encode valid JSON for the notification
    jsonMsg = BL.toStrict . A.encode . Message claims . decodeUtf8With T.lenientDecode

listenSession :: BS.ByteString
                    -> PGR.AppConfig
                    -> WS.Connection
                    -> IO ()
listenSession channel conf wsCon = do
  pqCon <- PQ.connectdb pgSettings
  listen pqCon
  waitForNotifications pqCon
  where
    waitForNotifications = forever . fetch
    listen con = void $ PQ.exec con $ "LISTEN " <> channel
    pgSettings = toS $ configDatabase conf
    fetch con = do
      mNotification <- PQ.notifies con
      case mNotification of
        Nothing -> do
          mfd <- PQ.socket con
          case mfd of
            Nothing  -> panic "Error checking for PostgreSQL notifications"
            Just fd -> do
              (waitRead, _) <- threadWaitReadSTM fd
              atomically waitRead
              void $ PQ.consumeInput con
        Just notification ->
          WS.sendTextData wsCon $ PQ.notifyExtra notification

forkAndWait :: IO () -> IO (MVar ())
forkAndWait io = do
  mvar <- newEmptyMVar
  void $ forkFinally io (\_ -> putMVar mvar ())
  return mvar
