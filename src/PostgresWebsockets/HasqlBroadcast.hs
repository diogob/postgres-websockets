{-| Uses Broadcast module adding database as a source producer.
    This module provides a function to produce a 'Multiplexer' from a Hasql 'Connection'.
    The producer issues a LISTEN command upon Open commands and UNLISTEN upon Close.
-}
module PostgresWebsockets.HasqlBroadcast
  ( newHasqlBroadcaster
  , newHasqlBroadcasterOrError
  -- re-export
  , acquire
  , relayMessages
  , relayMessagesForever
  ) where

import Protolude hiding (putErrLn)

import Hasql.Connection
import Hasql.Notifications
import Data.Aeson              (decode, Value(..))
import Data.HashMap.Lazy       (lookupDefault)
import Data.Either.Combinators (mapBoth)
import Data.Function           (id)
import Control.Retry           (RetryStatus, retrying, capDelay, exponentialBackoff)

import PostgresWebsockets.Broadcast

{- | Returns a multiplexer from a connection URI, keeps trying to connect in case there is any error.
   This function also spawns a thread that keeps relaying the messages from the database to the multiplexer's listeners
-}
newHasqlBroadcaster :: IO () -> Text -> ByteString -> IO Multiplexer
newHasqlBroadcaster onConnectionFailure ch = newHasqlBroadcasterForConnection . tryUntilConnected
  where
    newHasqlBroadcasterForConnection = newHasqlBroadcasterForChannel onConnectionFailure ch

{- | Returns a multiplexer from a connection URI or an error message on the left case
   This function also spawns a thread that keeps relaying the messages from the database to the multiplexer's listeners
-}
newHasqlBroadcasterOrError :: IO () -> Text -> ByteString -> IO (Either ByteString Multiplexer)
newHasqlBroadcasterOrError onConnectionFailure ch =
  acquire >=> (sequence . mapBoth show (newHasqlBroadcasterForConnection . return))
  where
    newHasqlBroadcasterForConnection = newHasqlBroadcasterForChannel onConnectionFailure ch

tryUntilConnected :: ByteString -> IO Connection
tryUntilConnected =
  fmap (either (panic "Failure on connection retry") id) . retryConnection
  where
    retryConnection conStr = retrying retryPolicy shouldRetry (const $ acquire conStr)
    maxDelayInMicroseconds = 32000000
    firstDelayInMicroseconds = 1000000
    retryPolicy = capDelay maxDelayInMicroseconds $ exponentialBackoff firstDelayInMicroseconds
    shouldRetry :: RetryStatus -> Either ConnectionError Connection -> IO Bool
    shouldRetry _ con =
      case con of
        Left err -> do
          putErrLn $ "Error connecting notification listener to database: " <> show err
          return True
        _ -> return False

{- | Returns a multiplexer from a channel and an IO Connection, listen for different database notifications on the provided channel using the connection produced.

   This function also spawns a thread that keeps relaying the messages from the database to the multiplexer's listeners

   To listen on channels *chat*

   @
   import Protolude
   import PostgresWebsockets.HasqlBroadcast
   import PostgresWebsockets.Broadcast
   import Hasql.Connection

   main = do
    conOrError <- H.acquire "postgres://localhost/test_database"
    let con = either (panic . show) id conOrError :: Connection
    multi <- newHasqlBroadcaster con

    onMessage multi "chat" (\ch ->
      forever $ fmap print (atomically $ readTChan ch)
   @
-}
newHasqlBroadcasterForChannel :: IO () -> Text -> IO Connection -> IO Multiplexer
newHasqlBroadcasterForChannel onConnectionFailure ch getCon = do
  multi <- newMultiplexer openProducer $ const onConnectionFailure
  void $ relayMessagesForever multi
  return multi
  where
    toMsg :: ByteString -> ByteString -> Message
    toMsg c m = case decode (toS m) of
                   Just v -> Message (channelDef c v) m
                   Nothing -> Message c m

    lookupStringDef :: Text -> ByteString -> Value -> ByteString
    lookupStringDef key d (Object obj) =
      case lookupDefault (String $ toS d) key obj of
        String s -> toS s
        _ -> d
    lookupStringDef _ d _ = d
    channelDef = lookupStringDef "channel"
    openProducer msgs = do
      con <- getCon
      listen con $ toPgIdentifier ch
      waitForNotifications
        (\c m-> atomically $ writeTQueue msgs $ toMsg c m)
        con

putErrLn :: Text -> IO ()
putErrLn = hPutStrLn stderr
