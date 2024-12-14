-- |
-- Module      : PostgresWebsockets.Context
-- Description : Produce a context capable of running postgres-websockets sessions
module PostgresWebsockets.Context
  ( Context (..),
    mkContext,
  )
where

import APrelude
import Control.AutoUpdate
  ( defaultUpdateSettings,
    mkAutoUpdate,
    updateAction,
  )
import Data.Time.Clock (UTCTime, getCurrentTime)
import qualified Hasql.Pool as P
import qualified Hasql.Pool.Config as P
import qualified Hasql.Connection.Setting as C
import qualified Hasql.Connection.Setting.Connection as C
import PostgresWebsockets.Broadcast (Multiplexer)
import PostgresWebsockets.Config (AppConfig (..))
import PostgresWebsockets.HasqlBroadcast (newHasqlBroadcaster)
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
  Context conf
    <$> P.acquire config
    <*> newHasqlBroadcaster shutdown configListenChannel configRetries configReconnectInterval pgSettings
    <*> newReplicantBroadcaster shutdown configRetries configReconnectInterval ""
    <*> mkGetTime
  where
    config = P.settings [P.staticConnectionSettings [C.connection $ C.string $ decodeUtf8 pgSettings]]
    shutdown =
      maybe
        shutdownServer
        (const $ putStrLn "Producer thread is dead")
        configReconnectInterval
    mkGetTime :: IO (IO UTCTime)
    mkGetTime = mkAutoUpdate defaultUpdateSettings {updateAction = getCurrentTime}
    pgSettings = encodeUtf8 configDatabase
