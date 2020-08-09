module Main where

import           Protolude hiding (replace)
import           PostgresWebsockets
import           PostgresWebsockets.Config  (AppConfig (..),
                                                       prettyVersion,
                                                       readOptions)

import qualified Data.ByteString                      as BS
import qualified Data.ByteString.Base64               as B64
import           Data.String                          (IsString (..))
import           Data.Text                            (pack, replace, strip, stripPrefix)
import           Data.Text.Encoding                   (encodeUtf8, decodeUtf8)
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

  conf <- loadSecretFile =<< readOptions
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

loadSecretFile :: AppConfig -> IO AppConfig
loadSecretFile conf = extractAndTransform secret
  where
    secret   = decodeUtf8 $ configJwtSecret conf
    isB64     = configJwtSecretIsBase64 conf

    extractAndTransform :: Text -> IO AppConfig
    extractAndTransform s =
      fmap setSecret $ transformString isB64 =<<
        case stripPrefix "@" s of
          Nothing       -> return . encodeUtf8 $ s
          Just filename -> chomp <$> BS.readFile (toS filename)
      where
        chomp bs = fromMaybe bs (BS.stripSuffix "\n" bs)

    -- Turns the Base64url encoded JWT into Base64
    transformString :: Bool -> ByteString -> IO ByteString
    transformString False t = return t
    transformString True t =
      case B64.decode $ encodeUtf8 $ strip $ replaceUrlChars $ decodeUtf8 t of
        Left errMsg -> panic $ pack errMsg
        Right bs    -> return bs

    setSecret bs = conf {configJwtSecret = bs}

    -- replace: Replace every occurrence of one substring with another
    replaceUrlChars =
      replace "_" "/" . replace "-" "+" . replace "." "="
