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

postgrestWsApp :: PGR.AppConfig
                    -> IORef PGR.DbStructure
                    -> H.Pool
                    -> Wai.Application
postgrestWsApp conf refDbStructure pool =
  WS.websocketsOr WS.defaultConnectionOptions wsApp $ postgrest conf refDbStructure pool
  where
    wsApp :: WS.ServerApp
    wsApp pendingConn = do
      time <- getPOSIXTime
      let
        claimsOrExpired = jwtClaims jwtSecret tokenStr time
      case claimsOrExpired of
        Left e -> rejectRequest e
        Right claims ->
          if M.null claims && not (T.null tokenStr)
            then rejectRequest "Invalid JWT"
            else do
              -- role claim defaults to anon if not specified in jwt
              -- We should accept only after verifying JWT
              conn <- WS.acceptRequest pendingConn
              forever $ sessionHandler conn
      where
        rejectRequest = WS.rejectRequest pendingConn . T.encodeUtf8
        jwtSecret = configJwtSecret conf
        sessionHandler = chooseSession headers
        headers = WS.requestHeaders $ WS.pendingRequest pendingConn
        lookupHeader = flip lookup headers
        auth = fromMaybe "" $ lookupHeader hAuthorization
        tokenStr = case T.split (== ' ') (T.decodeUtf8 auth) of
                    ("Bearer" : t : _) -> t
                    _                  -> ""

sessionRecorder :: WS.Connection -> IO ()
sessionRecorder = undefined

sessionPlayer :: WS.Connection -> IO ()
sessionPlayer = undefined

chooseSession :: WS.Headers -> WS.Connection -> IO ()
chooseSession = undefined
