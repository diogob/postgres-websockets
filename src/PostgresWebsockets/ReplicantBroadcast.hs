-- |
-- Module      : PostgresWebsockets.ReplicantBroadcast
-- Description : Build a Replicant based producer 'Multiplexer'.
--
-- Uses Broadcast module adding database as a source producer.
-- This module provides a function to produce a 'Multiplexer' from a Replicant 'Connection'.
module PostgresWebsockets.ReplicantBroadcast
  ( newReplicantBroadcaster,
    -- re-export
    relayMessages,
    relayMessagesForever,
  )
where

import APrelude
import Data.Aeson (encode)
import qualified Database.PostgreSQL.Replicant as PGR
import PostgresWebsockets.Broadcast

-- | Returns a multiplexer from a connection URI, keeps trying to connect in case there is any error.
--   This function also spawns a thread that keeps relaying the messages from the database to the multiplexer's listeners
newReplicantBroadcaster :: IO () -> Int -> Maybe Int -> ByteString -> IO Multiplexer
newReplicantBroadcaster onConnectionFailure _maxRetries = newReplicantBroadcasterForChanges onConnectionFailure

--

-- | Returns a multiplexer from a channel and an IO Connection, listen for different database notifications on the provided channel using the connection produced.
--
--   This function also spawns a thread that keeps relaying the messages from the database to the multiplexer's listeners
--
--   To listen on channels *chat*
--
--   @
--   import Protolude
--   import PostgresWebsockets.ReplicantBroadcast
--   import PostgresWebsockets.Broadcast
--   import Replicant.Connection
--
--   main = do
--    conOrError <- H.acquire "postgres://localhost/test_database"
--    let con = either (panic . show) id conOrError :: Connection
--    multi <- newReplicantBroadcaster con
--
--    onMessage multi "chat" (\ch ->
--      forever $ fmap print (atomically $ readTChan ch)
--   @
newReplicantBroadcasterForChanges :: IO () -> Maybe Int -> ByteString -> IO Multiplexer
newReplicantBroadcasterForChanges onConnectionFailure checkInterval _conURI = do
  multi <- newMultiplexer openProducer $ const onConnectionFailure
  case checkInterval of
    Just i -> superviseMultiplexer multi i shouldRestart
    _ -> pure ()
  void $ relayMessagesForever multi
  return multi
  where
    toMsg :: Text -> Message
    toMsg = Message "database.changes"

    shouldRestart =
      pure False -- TODO implement this properly
    openProducer msgQ = do
      let settings =
            PGR.PgSettings
              "replicant"
              Nothing
              "postgres_ws_test"
              "localhost"
              "5432"
              "second_test"
              "100"
      void $ do
        print ("forking replicant producer" :: Text)
        forkIO $
          PGR.withLogicalStream settings $ \changePayload -> do
            print ("Change received!" :: Text)
            print $ encode changePayload
            writeMessage $ decodeUtf8 $ showBS $ encode changePayload
            case changePayload of
              PGR.InformationMessage _infoMsg ->
                pure Nothing
              PGR.ChangeMessage change ->
                pure . Just $ PGR.changeNextLSN change
      where
        writeMessage m = atomically $ writeTQueue msgQ $ toMsg m

