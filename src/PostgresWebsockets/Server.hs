{-|
Module      : PostgresWebsockets.Server
Description : Functions to start a full stand-alone PostgresWebsockets server.
-}
module PostgresWebsockets.Server
        ( serve 
        ) where

import Protolude
import Network.Wai.Application.Static ( staticApp, defaultFileServerSettings )
import Network.Wai (Application, responseLBS)
import Network.HTTP.Types (status200)
import Network.Wai.Handler.Warp ( runSettings )
import Network.Wai.Middleware.RequestLogger (logStdout)

import PostgresWebsockets.Middleware ( postgresWsMiddleware )
import PostgresWebsockets.Config ( AppConfig(..), warpSettings )
import PostgresWebsockets.Context ( mkContext )

-- | Start a stand-alone warp server using the parameters from AppConfig and a opening a database connection pool.
serve :: AppConfig -> IO ()
serve conf@AppConfig{..} = do
  shutdownSignal <- newEmptyMVar
  let waitForShutdown cl = void $ forkIO (takeMVar shutdownSignal >> cl)
      appSettings = warpSettings waitForShutdown conf

  putStrLn $ ("Listening on port " :: Text) <> show configPort

  let shutdown = putErrLn ("Broadcaster connection is dead" :: Text) >> putMVar shutdownSignal ()
  ctx <- mkContext conf shutdown

  runSettings appSettings $
    postgresWsMiddleware ctx $
    logStdout $ maybe dummyApp staticApp' configPath
  die "Shutting down server..."

  where
    staticApp' :: Text -> Application
    staticApp' = staticApp . defaultFileServerSettings . toS
    dummyApp :: Application
    dummyApp _ respond =
        respond $ responseLBS status200 [("Content-Type", "text/plain")] "Hello, Web!"
