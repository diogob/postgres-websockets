module Main where

import           Protolude hiding (replace)
import           PostgRESTWS
import           Config                               (AppConfig (..),
                                                       PgVersion (..),
                                                       minimumPgVersion,
                                                       prettyVersion,
                                                       readOptions)

import qualified Data.ByteString                      as BS
import qualified Data.ByteString.Base64               as B64
import           Data.String                          (IsString (..))
import           Data.Text                            (pack, replace, strip, stripPrefix)
import           Data.Text.Encoding                   (encodeUtf8, decodeUtf8)
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
  multi <- newHasqlBroadcaster pgSettings

  runSettings appSettings $
    postgrestWsMiddleware (toS <$> configAuditChannel conf) (configJwtSecret conf) getPOSIXTime pool multi $
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
