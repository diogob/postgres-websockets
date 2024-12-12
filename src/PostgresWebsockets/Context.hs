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

data Context = Context
  { ctxConfig :: AppConfig,
    ctxPool :: P.Pool,
    ctxMulti :: Multiplexer,
    ctxGetTime :: IO UTCTime
  }

-- | Given a configuration and a shutdown action (performed when the Multiplexer's listen connection dies) produces the context necessary to run sessions
mkContext :: AppConfig -> IO () -> IO Context
mkContext conf@AppConfig {..} shutdownServer = do
  Context conf
    <$> P.acquire config
    <*> newHasqlBroadcaster shutdown (toS configListenChannel) configRetries configReconnectInterval pgSettings
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
