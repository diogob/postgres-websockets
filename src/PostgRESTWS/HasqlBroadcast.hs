{-| Uses Broadcast module adding database as a source producer.
    This module provides a function to produce a 'Multiplexer' from a Hasql 'Connection'.
    The producer issues a LISTEN command upon Open commands and UNLISTEN upon Close.
-}
module PostgRESTWS.HasqlBroadcast
  ( newHasqlBroadcaster
  , newHasqlBroadcasterOrError
  -- re-export
  , acquire
  , relayMessages
  , relayMessagesForever
  ) where

import Protolude

import Hasql.Connection
import Data.Either.Combinators (mapBoth)

import PostgRESTWS.Database
import PostgRESTWS.Broadcast

{- | Returns a multiplexer from a connection URI or an error message on the left case
   This function also spawns a thread that keeps relaying the messages from the database to the multiplexer's listeners
-}

newHasqlBroadcasterOrError :: ByteString -> IO (Either ByteString Multiplexer)
newHasqlBroadcasterOrError =
  acquire >=> (sequence . mapBoth show newHasqlBroadcaster)

{- | Returns a multiplexer from a connection, listen for different database notification channels using that connection.

   This function also spawns a thread that keeps relaying the messages from the database to the multiplexer's listeners

   To listen on channels *chat*

   @
   import Protolude
   import PostgRESTWS.HasqlBroadcast
   import PostgRESTWS.Broadcast
   import Hasql.Connection

   main = do
    conOrError <- H.acquire "postgres://localhost/test_database"
    let con = either (panic . show) id conOrError :: Connection
    multi <- newHasqlBroadcaster con

    onMessage multi "chat" (\ch ->
      forever $ fmap print (atomically $ readTChan ch)
   @

-}
newHasqlBroadcaster :: Connection -> IO Multiplexer
newHasqlBroadcaster con = do
  multi <-
    newMultiplexer (\cmds msgs-> do
    waitForNotifications
      (\c m-> atomically $ writeTQueue msgs $ Message c m)
      con
    forever $ do
      cmd <- atomically $ readTQueue cmds
      case cmd of
        Open ch -> listen con ch
        Close ch -> unlisten con ch
    ) (\_ -> return ())
  void $ relayMessagesForever multi
  return multi
