module Main where

import           Protolude hiding (replace)
import           PostgresWebsockets
import           PostgresWebsockets.Config  (AppConfig (..),
                                                       prettyVersion,
                                                       loadConfig)

import           Data.String                          (IsString (..))
import qualified Hasql.Pool                           as P
import           Network.Wai.Application.Static
import Data.Time.Clock (UTCTime, getCurrentTime)
import Control.AutoUpdate       ( defaultUpdateSettings
                                , mkAutoUpdate
                                , updateAction
                                )

import           Network.Wai (Application, responseLBS)
import           Network.HTTP.Types (status200)
import           Network.Wai.Handler.Warp
import           Network.Wai.Middleware.RequestLogger (logStdout)
import           System.IO                            (BufferMode (..),
                                                       hSetBuffering)
main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  hSetBuffering stdin  LineBuffering
  hSetBuffering stderr NoBuffering

  putStrLn $ ("postgres-websockets " :: Text)
               <> prettyVersion
               <> " / Connects websockets to PostgreSQL asynchronous notifications."

  conf <- loadConfig
  shutdownSignal <- newEmptyMVar
  let host = configHost conf
      port = configPort conf
      listenChannel = toS $ configListenChannel conf
      pgSettings = toS (configDatabase conf)
      waitForShutdown cl = void $ forkIO (takeMVar shutdownSignal >> cl >> die "Shutting server down...")

      appSettings = setHost ((fromString . toS) host)
                  . setPort port
                  . setServerName (toS $ "postgres-websockets/" <> prettyVersion)
                  . setTimeout 3600
                  . setInstallShutdownHandler waitForShutdown
                  . setGracefulShutdownTimeout (Just 5)
                  $ defaultSettings

  putStrLn $ ("Listening on port " :: Text) <> show (configPort conf)

  let shutdown = putErrLn ("Broadcaster connection is dead" :: Text) >> putMVar shutdownSignal ()
  pool <- P.acquire (configPool conf, 10, pgSettings)
  multi <- newHasqlBroadcaster shutdown listenChannel pgSettings
  getTime <- mkGetTime

  runSettings appSettings $
    postgresWsMiddleware getTime listenChannel (configJwtSecret conf) pool multi $
    logStdout $ maybe dummyApp staticApp' (configPath conf)

  where
    mkGetTime :: IO (IO UTCTime)
    mkGetTime = mkAutoUpdate defaultUpdateSettings {updateAction = getCurrentTime}
    staticApp' :: Text -> Application
    staticApp' = staticApp . defaultFileServerSettings . toS
    dummyApp :: Application
    dummyApp _ respond =
        respond $ responseLBS status200 [("Content-Type", "text/plain")] "Hello, Web!"
