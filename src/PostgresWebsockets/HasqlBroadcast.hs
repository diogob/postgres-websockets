-- |
-- Module      : PostgresWebsockets.Broadcast
-- Description : Build a Hasql.Notifications based producer 'Multiplexer'.
--
-- Uses Broadcast module adding database as a source producer.
-- This module provides a function to produce a 'Multiplexer' from a Hasql 'Connection'.
-- The producer issues a LISTEN command upon Open commands and UNLISTEN upon Close.
module PostgresWebsockets.HasqlBroadcast
  ( newHasqlBroadcaster,
    newHasqlBroadcasterOrError,
    -- re-export
    acquire,
    relayMessages,
    relayMessagesForever,
  )
where

import Control.Retry (RetryStatus (..), capDelay, exponentialBackoff, retrying)
import Data.Aeson (Value (..), decode)
import qualified Data.Aeson.KeyMap as JSON
import qualified Data.Aeson.Key as Key

import Data.Either.Combinators (mapBoth)
import Data.Function (id)
import GHC.Show
import Hasql.Connection
import qualified Hasql.Decoders as HD
import qualified Hasql.Encoders as HE
import Hasql.Notifications
import qualified Hasql.Session as H
import qualified Hasql.Statement as H
import PostgresWebsockets.Broadcast
import Protolude hiding (putErrLn, show, toS)
import Protolude.Conv

-- | Returns a multiplexer from a connection URI, keeps trying to connect in case there is any error.
--   This function also spawns a thread that keeps relaying the messages from the database to the multiplexer's listeners
newHasqlBroadcaster :: IO () -> Text -> Int -> Maybe Int -> ByteString -> IO Multiplexer
newHasqlBroadcaster onConnectionFailure ch maxRetries checkInterval = newHasqlBroadcasterForConnection . tryUntilConnected maxRetries
  where
    newHasqlBroadcasterForConnection = newHasqlBroadcasterForChannel onConnectionFailure ch checkInterval

-- | Returns a multiplexer from a connection URI or an error message on the left case
--   This function also spawns a thread that keeps relaying the messages from the database to the multiplexer's listeners
newHasqlBroadcasterOrError :: IO () -> Text -> ByteString -> IO (Either ByteString Multiplexer)
newHasqlBroadcasterOrError onConnectionFailure ch =
  acquire >=> (sequence . mapBoth (toSL . show) (newHasqlBroadcasterForConnection . return))
  where
    newHasqlBroadcasterForConnection = newHasqlBroadcasterForChannel onConnectionFailure ch Nothing

tryUntilConnected :: Int -> ByteString -> IO Connection
tryUntilConnected maxRetries =
  fmap (either (panic "Failure on connection retry") id) . retryConnection
  where
    retryConnection conStr = retrying retryPolicy shouldRetry (const $ acquire conStr)
    maxDelayInMicroseconds = 32000000
    firstDelayInMicroseconds = 1000000
    retryPolicy = capDelay maxDelayInMicroseconds $ exponentialBackoff firstDelayInMicroseconds
    shouldRetry :: RetryStatus -> Either ConnectionError Connection -> IO Bool
    shouldRetry RetryStatus {..} con =
      case con of
        Left err -> do
          putErrLn $ "Error connecting notification listener to database: " <> (toS . show) err
          pure $ rsIterNumber < maxRetries - 1
        _ -> return False

-- | Returns a multiplexer from a channel and an IO Connection, listen for different database notifications on the provided channel using the connection produced.
--
--   This function also spawns a thread that keeps relaying the messages from the database to the multiplexer's listeners
--
--   To listen on channels *chat*
--
--   @
--   import Protolude
--   import PostgresWebsockets.HasqlBroadcast
--   import PostgresWebsockets.Broadcast
--   import Hasql.Connection
--
--   main = do
--    conOrError <- H.acquire "postgres://localhost/test_database"
--    let con = either (panic . show) id conOrError :: Connection
--    multi <- newHasqlBroadcaster con
--
--    onMessage multi "chat" (\ch ->
--      forever $ fmap print (atomically $ readTChan ch)
--   @
newHasqlBroadcasterForChannel :: IO () -> Text -> Maybe Int -> IO Connection -> IO Multiplexer
newHasqlBroadcasterForChannel onConnectionFailure ch checkInterval getCon = do
  multi <- newMultiplexer openProducer $ const onConnectionFailure
  case checkInterval of
    Just i -> superviseMultiplexer multi i shouldRestart
    _ -> pure ()
  void $ relayMessagesForever multi
  return multi
  where
    toMsg :: Text -> Text -> Message
    toMsg c m = case decode (toS m) of
      Just v -> Message (channelDef c v) m
      Nothing -> Message c m

    lookupStringDef :: Text -> Text -> Value -> Text
    lookupStringDef key d (Object obj) =
      case lookupDefault (String $ toS d) key obj of
        String s -> toS s
        _ -> toS d
    lookupStringDef _ d _ = toS d

    lookupDefault d key obj = fromMaybe d $ JSON.lookup (Key.fromText key) obj

    channelDef = lookupStringDef "channel"
    shouldRestart = do
      con <- getCon
      not <$> isListening con ch

    openProducer msgQ = do
      con <- getCon
      listen con $ toPgIdentifier ch
      waitForNotifications
        (\c m -> atomically $ writeTQueue msgQ $ toMsg (toS c) (toS m))
        con

putErrLn :: Text -> IO ()
putErrLn = hPutStrLn stderr

isListening :: Connection -> Text -> IO Bool
isListening con ch = do
  resultOrError <- H.run session con
  pure $ fromRight False resultOrError
  where
    session = H.statement chPattern isListeningStatement
    chPattern = "listen%" <> ch <> "%"

isListeningStatement :: H.Statement Text Bool
isListeningStatement =
  H.Statement sql encoder decoder True
  where
    sql = "select exists (select * from pg_stat_activity where datname = current_database() and query ilike $1);"
    encoder = HE.param $ HE.nonNullable HE.text
    decoder = HD.singleRow (HD.column (HD.nonNullable HD.bool))