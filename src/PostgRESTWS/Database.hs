module PostgRESTWS.Database
  ( notify
  , onNotification
  ) where

import Protolude
import Hasql.Pool (Pool, UsageError, use)
import Hasql.Session (sql)
import qualified Database.PostgreSQL.LibPQ      as PQ
import Data.Either.Combinators

import PostgRESTWS.Types

notify :: Pool -> ByteString -> ByteString -> IO (Either Error ())

notify pool channel mesg =
   mapError <$> use pool (sql ("NOTIFY " <> channel <> ", '" <> mesg <> "'"))
   where
     mapError :: Either UsageError () -> Either Error ()
     mapError = mapLeft (NotifyError . show)

onNotification :: ByteString -> ByteString -> (ByteString -> IO()) -> IO ()
onNotification channel pgSettings sendNotification =
  openNotificationConnection channel pgSettings >>= waitForNotifications sendNotification

openNotificationConnection :: ByteString -> ByteString -> IO PQ.Connection
openNotificationConnection channel pgSettings = do
  pqCon <- PQ.connectdb $ toS pgSettings
  listen pqCon
  return pqCon
  where
    listen con = void $ PQ.exec con $ "LISTEN " <> channel

waitForNotifications :: (ByteString -> IO()) -> PQ.Connection -> IO ()
waitForNotifications sendNotification = forever . fetch
  where
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
           sendNotification $ PQ.notifyExtra notification
