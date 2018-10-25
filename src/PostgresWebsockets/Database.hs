{-| This module encapsulates knowledge about the SQL commands and the Hasql interface.
-}
module PostgresWebsockets.Database
  ( notifyPool
  , notify
  , listen
  , unlisten
  , waitForNotifications
  , PgIdentifier
  , toPgIdentifier
  ) where

import Protolude hiding (replace)
import Hasql.Pool (Pool, UsageError, use)
import Hasql.Session (sql, run, statement)
import qualified Hasql.Session as S
import qualified Hasql.Statement as HST
import Hasql.Connection (Connection, withLibPQConnection)
import qualified Hasql.Decoders as HD
import qualified Hasql.Encoders as HE
import qualified Database.PostgreSQL.LibPQ      as PQ
import Data.Either.Combinators
import qualified Data.ByteString.Char8 as B
import Data.ByteString.Search (replace)
import Data.Functor.Contravariant (contramap)

newtype Error = NotifyError Text

-- | A wrapped bytestring that represents a properly escaped and quoted PostgreSQL identifier
newtype PgIdentifier = PgIdentifier ByteString deriving (Show)

-- | Given a PgIdentifier returns the wrapped bytestring
fromPgIdentifier :: PgIdentifier -> ByteString
fromPgIdentifier (PgIdentifier bs) = bs

-- | Given a bytestring returns a properly quoted and escaped PgIdentifier
toPgIdentifier :: ByteString -> PgIdentifier
toPgIdentifier x = PgIdentifier $ "\"" <> strictlyReplaceQuotes (trimNullChars x) <> "\""
  where
    trimNullChars :: ByteString -> ByteString
    trimNullChars = B.takeWhile (/= '\x0')
    strictlyReplaceQuotes :: ByteString -> ByteString
    strictlyReplaceQuotes = toS . replace "\"" ("\"\"" :: ByteString)

-- | Given a Hasql Pool, a channel and a message sends a notify command to the database
notifyPool :: Pool -> ByteString -> ByteString -> IO (Either Error ())
notifyPool pool channel mesg =
   mapError <$> use pool (statement (toS channel, toS mesg) callStatement)
   where
     mapError :: Either UsageError () -> Either Error ()
     mapError = mapLeft (NotifyError . show)
     callStatement = HST.Statement ("SELECT pg_notify" <> "($1, $2)") encoder HD.unit False
     encoder = contramap fst (HE.param HE.text) <> contramap snd (HE.param HE.text)

-- | Given a Hasql Connection, a channel and a message sends a notify command to the database
notify :: Connection -> PgIdentifier -> ByteString -> IO (Either Error ())
notify con channel mesg =
   mapError <$> run (sql ("NOTIFY " <> fromPgIdentifier channel <> ", '" <> mesg <> "'")) con
   where
     mapError :: Either S.QueryError () -> Either Error ()
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
  withLibPQConnection con $ void . forever . pqFetch
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
