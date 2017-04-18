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
