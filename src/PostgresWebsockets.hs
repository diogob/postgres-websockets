{-| PostgresWebsockets main library interface
    All function necessary to start a fully functionaing service should be re-exported here.
-}
{-# LANGUAGE DeriveGeneric #-}

module PostgresWebsockets
  ( module Exports
  , newHasqlBroadcaster
  ) where

import           PostgresWebsockets.Middleware as Exports
import           PostgresWebsockets.HasqlBroadcast (newHasqlBroadcaster)
