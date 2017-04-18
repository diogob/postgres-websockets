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
newHasqlBroadcaster con = newMultiplexer relayCommands (\_ -> return ())
  where
    {- we need to ensure we are listening for database
    notifications before locking this thread on the forever
    call, hence the seq -}
    relayCommands cmds msgs =
      seq
        (waitForNotifications (writeNotificationToQueue msgs) con)
        forever $ readCommand cmds >>= executeCommand con
    writeNotificationToQueue queue ch m =
      atomically $ writeTQueue queue $ Message ch m
    readCommand cmds = atomically $ readTQueue cmds
    executeCommand dbCon cmd =
      case cmd of
        Open ch -> listen dbCon ch
        Close ch -> unlisten dbCon ch
