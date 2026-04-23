-- |
-- Module      : PostgresWebsockets.Server
-- Description : Functions to start a full stand-alone PostgresWebsockets server.
module PostgresWebsockets.Server
  ( serve,
  )
where

import APrelude
import Network.HTTP.Types (status200)
import Network.Wai (Application, responseLBS)
import Network.Wai.Application.Static (defaultFileServerSettings, staticApp)
import Network.Wai.Handler.Warp (runSettings)
import Network.Wai.Handler.WarpTLS (runTLS, tlsSettings)
import Network.Wai.Middleware.RequestLogger (logStdout)
import PostgresWebsockets.Config (AppConfig (..), warpSettings)
import PostgresWebsockets.Context (mkContext)
import PostgresWebsockets.Middleware (postgresWsMiddleware)

-- | Start a stand-alone warp server using the parameters from AppConfig and a opening a database connection pool.
serve :: AppConfig -> IO ()
serve conf@AppConfig {..} = do
  shutdownSignal <- newEmptyMVar
  putStrLn $ "Listening on port " <> show configPort

  let shutdown = putErrLn ("Broadcaster connection is dead" :: Text) >> putMVar shutdownSignal ()
  ctx <- mkContext conf shutdown

  putStrLn "Listening to changes..."
  let waitForShutdown cl = void $ forkIO (takeMVar shutdownSignal >> cl)
      appSettings = warpSettings waitForShutdown conf
      app = postgresWsMiddleware ctx $ logStdout $ maybe dummyApp staticApp' configPath

  case (configCertificateFile, configKeyFile) of
    (Just certificate, Just key) -> runTLS (tlsSettings (unpack certificate) (unpack key)) appSettings app
    _ -> runSettings appSettings app

  die "Shutting down server..."
  where
    staticApp' :: Text -> Application
    staticApp' = staticApp . defaultFileServerSettings . unpack
    dummyApp :: Application
    dummyApp _ respond =
      respond $ responseLBS status200 [("Content-Type", "text/plain")] "Hello, Web!"
