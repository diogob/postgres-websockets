module Main where

import           Protolude
import           PostgRESTWS
import           Config                               (AppConfig (..),
                                                       PgVersion (..),
                                                       minimumPgVersion,
                                                       prettyVersion,
                                                       readOptions)

import           Data.ByteString.Base64               (decode)
import           Data.String                          (IsString (..))
import           Data.Text                            (stripPrefix, pack, replace)
import           Data.Text.Encoding                   (encodeUtf8, decodeUtf8)
import           Data.Text.IO                         (readFile)
import           Data.Time.Clock.POSIX                (getPOSIXTime)
import qualified Hasql.Query                          as H
import qualified Hasql.Session                        as H
import qualified Hasql.Decoders                       as HD
import qualified Hasql.Encoders                       as HE
import qualified Hasql.Pool                           as P
import Network.Wai.Application.Static

import           Network.Wai.Handler.Warp
import           Network.Wai.Middleware.RequestLogger (logStdout)
import           System.IO                            (BufferMode (..),
                                                       hSetBuffering)
import           Control.AutoUpdate

isServerVersionSupported :: H.Session Bool
isServerVersionSupported = do
  ver <- H.query () pgVersion
  return $ ver >= pgvNum minimumPgVersion
 where
  pgVersion =
    H.statement "SELECT current_setting('server_version_num')::integer"
      HE.unit (HD.singleRow $ HD.value HD.int4) False

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  hSetBuffering stdin  LineBuffering
  hSetBuffering stderr NoBuffering

  conf <- loadSecretFile =<< readOptions
  let host = configHost conf
      port = configPort conf
      pgSettings = toS (configDatabase conf)
      appSettings = setHost ((fromString . toS) host)
                  . setPort port
                  . setServerName (toS $ "postgrest/" <> prettyVersion)
                  . setTimeout 3600
                  $ defaultSettings

  putStrLn $ ("Listening on port " :: Text) <> show (configPort conf)

  pool <- P.acquire (configPool conf, 10, pgSettings)

  -- ask for the OS time at most once per second
  getTime <- mkAutoUpdate
    defaultUpdateSettings { updateAction = getPOSIXTime }

  multi <- newHasqlBroadcaster pgSettings

  runSettings appSettings $
    postgrestWsMiddleware (configJwtSecret conf) getTime pool multi $
    logStdout $ staticApp $ defaultFileServerSettings $ toS $ configPath conf

loadSecretFile :: AppConfig -> IO AppConfig
loadSecretFile conf = extractAndTransform secret
  where
    secret   = decodeUtf8 $ configJwtSecret conf
    isB64     = configJwtSecretIsBase64 conf

    extractAndTransform :: Text -> IO AppConfig
    extractAndTransform s =
      fmap setSecret $ transformString isB64 =<<
        case stripPrefix "@" s of
            Nothing       -> return s
            Just filename -> readFile (toS filename)

    transformString :: Bool -> Text -> IO ByteString
    transformString False t = return . encodeUtf8 $ t
    transformString True  t =
      case decode (encodeUtf8 $ replaceUrlChars t) of
        Left errMsg -> panic $ pack errMsg
        Right bs    -> return bs

    setSecret bs = conf { configJwtSecret = bs }

    replaceUrlChars = replace "_" "/" . replace "-" "+" . replace "." "="
