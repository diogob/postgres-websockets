module PostgRESTWS
  ( postgrestWsApp
  ) where

import qualified Network.Wai                     as Wai
import qualified Network.WebSockets              as WS
import qualified Network.Wai.Handler.WebSockets  as WS
import           PostgREST.App    as PGR
import           PostgREST.Config as PGR
import           PostgREST.Types  as PGR
import qualified Hasql.Pool as H
import GHC.IORef

import qualified Data.Text as T
import qualified Data.Text.Encoding as T

import Control.Monad (forever, void, when)
import Data.Time.Clock.POSIX (getPOSIXTime)
import PostgREST.Auth (jwtClaims)
import qualified Data.HashMap.Strict           as M
import qualified Database.PostgreSQL.LibPQ     as PQ
import           Data.String.Conversions              (cs)

import qualified Data.ByteString as BS
import Data.Monoid
import qualified Data.Aeson as A

import Control.Concurrent (forkIO, threadWaitReadSTM)
import GHC.Conc (atomically)

postgrestWsApp :: PGR.AppConfig
                    -> IORef PGR.DbStructure
                    -> H.Pool
                    -> PQ.Connection
                    -> Wai.Application

postgrestWsApp conf refDbStructure pool pqCon =
  WS.websocketsOr WS.defaultConnectionOptions wsApp $ postgrest conf refDbStructure pool
  where
    -- when the websocket is closed a ConnectionClosed Exception is triggered
    -- this kills all children and frees resources for us
    wsApp :: WS.ServerApp
    wsApp pendingConn = do
      time <- getPOSIXTime
      let
        claimsOrExpired = jwtClaims jwtSecret jwtToken time
      case claimsOrExpired of
        Left e -> rejectRequest e
        Right claims -> do
              -- role claim defaults to anon if not specified in jwt
              -- We should accept only after verifying JWT
              conn <- WS.acceptRequest pendingConn
              -- each websocket needs its own listen connection to avoid
              -- handling of multiple waiting threads in the same connection
              when (hasRead claims) $
                void $ forkIO $ listenSession (channel claims) conf conn
              -- all websockets share a single connection to NOTIFY
              when (hasWrite claims) $
                forever $ notifySession (channel claims) pqCon conn
      where
        claimAsBS name cl = let A.String s = (cl M.! name) in T.encodeUtf8 s
        channel = claimAsBS ("channel" :: T.Text)
        mode = claimAsBS ("mode" :: T.Text)
        hasRead cl = mode cl == "r" || mode cl == "rw"
        hasWrite cl = mode cl == "w" || mode cl == "rw"
        rejectRequest = WS.rejectRequest pendingConn . T.encodeUtf8
        jwtSecret = configJwtSecret conf
        -- the first char in path is '/' the rest is the token
        jwtToken = T.decodeUtf8 $ BS.drop 1 $ WS.requestPath $ WS.pendingRequest pendingConn

notifySession :: BS.ByteString
                    -> PQ.Connection
                    -> WS.Connection
                    -> IO ()
notifySession channel pqCon wsCon =
  WS.receiveData wsCon >>= notify
  where
    notify msg = void $ PQ.exec pqCon ("NOTIFY " <> channel <> ", '" <> msg <> "'")

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
    pgSettings = cs $ configDatabase conf
    fetch con = do
      mNotification <- PQ.notifies con
      case mNotification of
        Nothing -> do
          mfd <- PQ.socket con
          case mfd of
            Nothing  -> error "Error checking for PostgreSQL notifications"
            Just fd -> do
              (waitRead, _) <- threadWaitReadSTM fd
              atomically waitRead
              void $ PQ.consumeInput con
        Just notification ->
          WS.sendTextData wsCon $ PQ.notifyExtra notification
