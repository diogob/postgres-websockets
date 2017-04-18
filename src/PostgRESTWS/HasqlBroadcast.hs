module PostgRESTWS.HasqlBroadcast
  ( newHasqlBroadcaster
  -- re-export
  , acquire
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
      traceM $ "Waiting for command..."
      cmd <- atomically $ readTQueue cmds
      traceM $ "cmd: " <> show cmd
      case cmd of
        Open ch -> listen con ch
        Close ch -> unlisten con ch
      traceM $ "cmd: " <> show cmd <> " executed! "
    ) (\_ -> return ())
