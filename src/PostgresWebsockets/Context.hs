-- |
-- Module      : PostgresWebsockets.Context
-- Description : Produce a context capable of running postgres-websockets sessions
module PostgresWebsockets.Context
  ( Context (..),
    mkContext,
  )
where

import Control.AutoUpdate
  ( defaultUpdateSettings,
    mkAutoUpdate,
    updateAction,
  )
import Data.Time.Clock (UTCTime, getCurrentTime)
import qualified Hasql.Pool as P
import qualified Hasql.Pool.Config as P
import PostgresWebsockets.Broadcast (Multiplexer)
import PostgresWebsockets.Config (AppConfig (..))
import PostgresWebsockets.HasqlBroadcast (newHasqlBroadcaster)
import Protolude hiding (toS)
import Protolude.Conv
import PostgresWebsockets.ReplicantBroadcast (newReplicantBroadcaster)

data Context = Context
  { ctxConfig :: AppConfig,
    ctxPool :: P.Pool,
    ctxNotifications :: Multiplexer,
    ctxChanges :: Multiplexer,
    ctxGetTime :: IO UTCTime
  }

-- | Given a configuration and a shutdown action (performed when the Multiplexer's listen connection dies) produces the context necessary to run sessions
mkContext :: AppConfig -> IO () -> IO Context
mkContext conf@AppConfig {..} shutdownServer = do
  pool <- P.acquire config
  pure (Context conf pool)
    <*> newHasqlBroadcaster shutdown (toS configListenChannel) configRetries configReconnectInterval pgSettings
    <*> newReplicantBroadcaster shutdown configRetries configReconnectInterval pool ""
    <*> mkGetTime
  where

    config = P.settings [P.staticConnectionSettings pgSettings]
    shutdown =
      maybe
        shutdownServer
        (const $ putText "Producer thread is dead")
        configReconnectInterval
    mkGetTime :: IO (IO UTCTime)
    mkGetTime = mkAutoUpdate defaultUpdateSettings {updateAction = getCurrentTime}
    pgSettings = toS configDatabase
