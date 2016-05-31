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

import Control.Concurrent (forkIO, threadWaitReadSTM, killThread)
import GHC.Conc (atomically)


postgrestWsApp :: PGR.AppConfig
                    -> IORef PGR.DbStructure
                    -> H.Pool
                    -> PQ.Connection
                    -> Wai.Application

postgrestWsApp conf refDbStructure pool pqCon =
  WS.websocketsOr WS.defaultConnectionOptions wsApp $ postgrest conf refDbStructure pool
  where
    wsApp :: WS.ServerApp
    wsApp pendingConn = do
      time <- getPOSIXTime
      print jwtToken
      let
        claimsOrExpired = jwtClaims jwtSecret jwtToken time
      case claimsOrExpired of
        Left e -> rejectRequest e
        Right claims -> do
              -- role claim defaults to anon if not specified in jwt
              -- We should accept only after verifying JWT
              conn <- WS.acceptRequest pendingConn
              putStrLn "WS session with claims:"
              print claims
              when (hasRead claims) $
                void $ forkIO $ listenSession (channel claims) conf conn
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
        jwtToken = T.decodeUtf8 $ BS.drop 1 $ WS.requestPath $ WS.pendingRequest pendingConn

notifySession :: BS.ByteString
                    -> PQ.Connection
                    -> WS.Connection
                    -> IO ()
notifySession channel pqCon wsCon = do
  putStrLn "Checking WS notification..."
  clientMessage <- WS.receiveData wsCon
  _ <- PQ.exec pqCon ("NOTIFY " <> channel <> ", '" <> clientMessage <> "'")
  print $ "client -> server: " <> clientMessage

listenSession :: BS.ByteString
                    -> PGR.AppConfig
                    -> WS.Connection
                    -> IO ()
listenSession channel conf wsCon = do
  pqCon <- PQ.connectdb pgSettings
  _ <- PQ.exec pqCon $ "LISTEN " <> channel
  putStrLn "Listening to server channel..."
  forever $ fetch pqCon
  where
    pgSettings = cs $ configDatabase conf
    fetch con = do
      mNotification <- PQ.notifies con
      putStrLn "Checking PG notification..."
      print mNotification
      case mNotification of
        Nothing -> do
          putStrLn "Notification queue empty"
          mfd <- PQ.socket con
          case mfd of
            Nothing  -> error "test"
            Just fd -> do
              (waitRead, _) <- threadWaitReadSTM fd
              atomically waitRead
              void $ PQ.consumeInput con
        Just notification -> do
          print $ "server -> client: " <> PQ.notifyExtra notification
          WS.sendTextData wsCon $ PQ.notifyExtra notification
