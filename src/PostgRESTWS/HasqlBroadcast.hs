{-|
Module      : PostgRESTWS.HasqlBroadcast
Description : Uses Broadcast module adding database as a source producer.

This module provides a function to produce a 'Multiplexer' from a Hasql 'Connection'.
The producer issues a LISTEN command upon Open commands and UNLISTEN upon Close.

-}
module PostgRESTWS.HasqlBroadcast
  ( newHasqlBroadcaster
  -- re-export
  , acquire
  , relayMessages
  , relayMessagesForever
  ) where

import Protolude

import Hasql.Connection

import PostgRESTWS.Database
import PostgRESTWS.Broadcast

{- | Returns a multiplexer from a connection, listen for different database notification channels using that connection.

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
newHasqlBroadcaster con = newMultiplexer (\cmds msgs-> do
    waitForNotifications
      (\c m-> atomically $ writeTQueue msgs $ Message c m)
      con
    forever $ do
      cmd <- atomically $ readTQueue cmds
      case cmd of
        Open ch -> listen con ch
        Close ch -> unlisten con ch
    ) (\_ -> return ())
