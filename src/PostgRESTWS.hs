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

import qualified Data.Text                      as T
import qualified Data.Text.Encoding             as T
import qualified Data.Text.Encoding.Error       as T

import qualified Data.HashMap.Strict            as M
import           Data.Time.Clock.POSIX          (POSIXTime, getPOSIXTime)
import qualified Database.PostgreSQL.LibPQ      as PQ
import           PostgREST.Auth                 (jwtClaims)

import qualified Data.Aeson                     as A
import qualified Data.ByteString                as BS
import qualified Data.ByteString.Lazy           as BL
import           Web.JWT                        (Secret)

data Message = Message A.Object T.Text deriving (Show, Eq, Generic)

instance A.ToJSON Message

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
        claimsOrError = validateClaims jwtSecret jwtToken time
      case claimsOrError of
        Left e -> rejectRequest e
        Right (channel, mode, claims) -> do
              -- role claim defaults to anon if not specified in jwt
              -- We should accept only after verifying JWT
              conn <- WS.acceptRequest pendingConn
              -- Fork a pinging thread to ensure browser connections stay alive
              WS.forkPingThread conn 30
              -- each websocket needs its own listen connection to avoid
              -- handling of multiple waiting threads in the same connection
              when (hasRead mode) $
                void $ forkIO $ listenSession channel conf conn
              -- all websockets share a single connection to NOTIFY
              when (hasWrite mode) $
                forever $ notifySession channel claims pqCon conn
      where
        hasRead m = m == ("r" :: ByteString) || m == ("rw" :: ByteString)
        hasWrite m = m == ("w" :: ByteString) || m == ("rw" :: ByteString)
        rejectRequest = WS.rejectRequest pendingConn . T.encodeUtf8
        jwtSecret = configJwtSecret conf
        -- the first char in path is '/' the rest is the token
        jwtToken = T.decodeUtf8 $ BS.drop 1 $ WS.requestPath $ WS.pendingRequest pendingConn

-- private functions

type Claims = M.HashMap Text A.Value
type ConnectionInfo = (ByteString, ByteString, Claims)

validateClaims :: Secret -> Text -> POSIXTime -> Either Text ConnectionInfo
validateClaims jwtSecret jwtToken time = do
  cl <- jwtClaims jwtSecret jwtToken time
  jChannel <- claimAsJSON "channel" cl
  jMode <- claimAsJSON "mode" cl
  channel <- value2BS jChannel
  mode <- value2BS jMode
  Right (channel, mode, cl)
  where
    value2BS val = case val of
      A.String s -> Right $ T.encodeUtf8 s
      _ -> Left "claim is not string value"
    claimAsJSON :: Text -> Claims -> Either Text A.Value
    claimAsJSON name cl = case M.lookup name cl of
      Just el -> Right el
      Nothing -> Left (name <> " not in claims")

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
    jsonMsg = BL.toStrict . A.encode . Message claims . T.decodeUtf8With T.lenientDecode

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
