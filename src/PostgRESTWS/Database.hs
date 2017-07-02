{-| This module encapsulates knowledge about the SQL commands and the Hasql interface.
-}
module PostgRESTWS.Database
  ( notifyPool
  , notify
  , listen
  , unlisten
  , waitForNotifications
  , PgIdentifier
  , toPgIdentifier
  ) where

import Protolude
import Hasql.Pool (Pool, UsageError, use)
import Hasql.Session (sql, run)
import qualified Hasql.Session as S
import Hasql.Connection (Connection, withLibPQConnection)
import qualified Database.PostgreSQL.LibPQ      as PQ
import Data.Either.Combinators
import qualified Data.ByteString.Char8 as B
import Data.ByteString.Search (replace)
newtype Error = NotifyError Text

newtype PgIdentifier = PgIdentifier ByteString deriving (Show)

fromPgIdentifier :: PgIdentifier -> ByteString
fromPgIdentifier (PgIdentifier bs) = bs

toPgIdentifier :: ByteString -> PgIdentifier
toPgIdentifier x = PgIdentifier $ "\"" <> strictlyReplaceQuotes (trimNullChars x) <> "\""
  where
    trimNullChars :: ByteString -> ByteString
    trimNullChars = B.takeWhile (/= '\x0')
    strictlyReplaceQuotes :: ByteString -> ByteString
    strictlyReplaceQuotes = toS . replace "\"" ("\"\"" :: ByteString)

-- | Given a Hasql Pool, a channel and a message sends a notify command to the database
notifyPool :: Pool -> PgIdentifier -> ByteString -> IO (Either Error ())
notifyPool pool channel mesg =
   mapError <$> use pool (sql ("NOTIFY " <> fromPgIdentifier channel <> ", '" <> mesg <> "'"))
   where
     mapError :: Either UsageError () -> Either Error ()
     mapError = mapLeft (NotifyError . show)

-- | Given a Hasql Connection, a channel and a message sends a notify command to the database
notify :: Connection -> PgIdentifier -> ByteString -> IO (Either Error ())
notify con channel mesg =
   mapError <$> run (sql ("NOTIFY " <> fromPgIdentifier channel <> ", '" <> mesg <> "'")) con
   where
     mapError :: Either S.Error () -> Either Error ()
     mapError = mapLeft (NotifyError . show)

-- | Given a Hasql Connection and a channel sends a listen command to the database
listen :: Connection -> PgIdentifier -> IO ()
listen con channel =
  void $ withLibPQConnection con execListen
  where
    execListen pqCon = void $ PQ.exec pqCon $ "LISTEN " <> fromPgIdentifier channel

-- | Given a Hasql Connection and a channel sends a unlisten command to the database
unlisten :: Connection -> PgIdentifier -> IO ()
unlisten con channel =
  void $ withLibPQConnection con execListen
  where
    execListen pqCon = void $ PQ.exec pqCon $ "UNLISTEN " <> fromPgIdentifier channel


{- | Given a function that handles notifications and a Hasql connection forks a thread that listens on the database connection and calls the handler everytime a message arrives.

   The message handler passed as first argument needs two parameters channel and payload.
-}

waitForNotifications :: (ByteString -> ByteString -> IO()) -> Connection -> IO ()
waitForNotifications sendNotification con =
  withLibPQConnection con $ void . forkIO . forever . pqFetch
  where
    pqFetch pqCon = do
      mNotification <- PQ.notifies pqCon
      case mNotification of
        Nothing -> do
          mfd <- PQ.socket pqCon
          case mfd of
            Nothing  -> panic "Error checking for PostgreSQL notifications"
            Just fd -> do
              void $ threadWaitRead fd
              void $ PQ.consumeInput pqCon
        Just notification ->
           sendNotification (PQ.notifyRelname notification) (PQ.notifyExtra notification)
