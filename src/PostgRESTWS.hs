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

import Control.Monad (forever)
import Data.Maybe (fromMaybe)
import Network.HTTP.Types.Header (hAuthorization)
import Data.Time.Clock.POSIX     (getPOSIXTime, POSIXTime)
import PostgREST.Auth            (jwtClaims, claimsToSQL)
import qualified Data.HashMap.Strict           as M
import qualified Database.PostgreSQL.LibPQ     as PQ

import           GHC.Conc           ( atomically )
import           Control.Concurrent ( threadWaitReadSTM )
import Data.Monoid

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
      let
        claimsOrExpired = jwtClaims jwtSecret tokenStr time
      case claimsOrExpired of
        Left e -> rejectRequest e
        Right claims -> do
              -- role claim defaults to anon if not specified in jwt
              -- We should accept only after verifying JWT
              conn <- WS.acceptRequest pendingConn
              putStrLn "WS session..."
              forever $ sessionHandler pqCon conn
      where
        rejectRequest = WS.rejectRequest pendingConn . T.encodeUtf8
        jwtSecret = configJwtSecret conf
        headers = WS.requestHeaders $ WS.pendingRequest pendingConn
        lookupHeader = flip lookup headers
        auth = fromMaybe "" $ lookupHeader hAuthorization
        tokenStr = case T.split (== ' ') (T.decodeUtf8 auth) of
                    ("Bearer" : t : _) -> t
                    _                  -> ""

sessionHandler :: PQ.Connection -> WS.Connection -> IO ()
sessionHandler pqCon wsCon = do
  _ <- PQ.exec pqCon "LISTEN server"
  putStrLn "Listening to server channel..."
  mNotification <- PQ.notifies pqCon
  putStrLn "Checking PG notification..."
  print mNotification
  case mNotification of
    Nothing -> putStrLn "Notification queue empty"
    Just notification -> do
      print $ "server -> client: " <> PQ.notifyExtra notification
      WS.sendTextData wsCon $ PQ.notifyExtra notification
  putStrLn "Checking WS notification..."
  clientMessage <- WS.receiveData wsCon
  _ <- PQ.exec pqCon ("NOTIFY client, '" <> clientMessage <> "'")
  print $ "client -> server: " <> clientMessage
