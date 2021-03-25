{-|
Module      : PostgresWebsockets.Context
Description : Produce a context capable of running postgres-websockets sessions
-}
module PostgresWebsockets.Context
        ( Context (..)
        , mkContext
        ) where

import Protolude hiding (toS)
import Protolude.Conv
import Data.Time.Clock (UTCTime, getCurrentTime)
import Control.AutoUpdate       ( defaultUpdateSettings
                                , mkAutoUpdate
                                , updateAction
                                )
import qualified Hasql.Pool as P

import PostgresWebsockets.Config ( AppConfig(..) )
import PostgresWebsockets.HasqlBroadcast (newHasqlBroadcaster)
import PostgresWebsockets.Broadcast (Multiplexer)

data Context = Context {
    ctxConfig :: AppConfig
  , ctxPool :: P.Pool
  , ctxMulti :: Multiplexer
  , ctxGetTime :: IO UTCTime
  }

-- | Given a configuration and a shutdown action (performed when the Multiplexer's listen connection dies) produces the context necessary to run sessions
mkContext :: AppConfig -> IO () -> IO Context
mkContext conf@AppConfig{..} shutdown = do
  Context conf
    <$> P.acquire (configPool, 10, pgSettings)
    <*> newHasqlBroadcaster shutdown (toS configListenChannel) configRetries pgSettings
    <*> mkGetTime
  where
    mkGetTime :: IO (IO UTCTime)
    mkGetTime = mkAutoUpdate defaultUpdateSettings {updateAction = getCurrentTime}
    pgSettings = toS configDatabase
