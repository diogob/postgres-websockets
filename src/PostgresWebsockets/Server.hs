{-|
Module      : PostgresWebsockets.Server
Description : Functions to start a full stand-alone PostgresWebsockets server.
-}

module PostgresWebsockets.Server
        ( serve 
        ) where

import           Protolude
import           PostgresWebsockets.Middleware
import           PostgresWebsockets.Config
import           PostgresWebsockets.HasqlBroadcast (newHasqlBroadcaster)

import qualified Hasql.Pool as P
import           Network.Wai.Application.Static
import           Data.Time.Clock (UTCTime, getCurrentTime)
import Control.AutoUpdate       ( defaultUpdateSettings
                                , mkAutoUpdate
                                , updateAction
                                )
import           Network.Wai (Application, responseLBS)
import           Network.HTTP.Types (status200)
import           Network.Wai.Handler.Warp
import           Network.Wai.Middleware.RequestLogger (logStdout)

-- | Start a stand-alone warp server using the parameters from AppConfig and a opening a database connection pool.
-- | Returns a MVar that shuts down the server once written to. 
serve :: AppConfig -> IO (MVar ())
serve conf = do
  shutdownSignal <- newEmptyMVar
  let listenChannel = toS $ configListenChannel conf
      pgSettings = toS (configDatabase conf)
      waitForShutdown cl = void $ forkIO (takeMVar shutdownSignal >> cl >> die "Shutting server down...")
      appSettings = warpSettings waitForShutdown conf

  putStrLn $ ("Listening on port " :: Text) <> show (configPort conf)

  let shutdown = putErrLn ("Broadcaster connection is dead" :: Text) >> putMVar shutdownSignal ()
  pool <- P.acquire (configPool conf, 10, pgSettings)
  multi <- newHasqlBroadcaster shutdown listenChannel pgSettings
  getTime <- mkGetTime

  runSettings appSettings $
    postgresWsMiddleware getTime listenChannel (configJwtSecret conf) pool multi $
    logStdout $ maybe dummyApp staticApp' (configPath conf)

  pure shutdownSignal

  where
    mkGetTime :: IO (IO UTCTime)
    mkGetTime = mkAutoUpdate defaultUpdateSettings {updateAction = getCurrentTime}
    staticApp' :: Text -> Application
    staticApp' = staticApp . defaultFileServerSettings . toS
    dummyApp :: Application
    dummyApp _ respond =
        respond $ responseLBS status200 [("Content-Type", "text/plain")] "Hello, Web!"
