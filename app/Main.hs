module Main where

import           Protolude
import           PostgRESTWS
import           PostgRESTWS.Broadcast
import           PostgRESTWS.HasqlBroadcast
import           Config                               (AppConfig (..),
                                                       PgVersion (..),
                                                       minimumPgVersion,
                                                       prettyVersion,
                                                       readOptions)

import           Data.Function                        (id)
import           Data.ByteString.Base64               (decode)
import           Data.String                          (IsString (..))
import           Data.Text                            (stripPrefix, pack, replace)
import           Data.Text.Encoding                   (encodeUtf8, decodeUtf8)
import           Data.Text.IO                         (readFile)
import           Data.Time.Clock.POSIX                (getPOSIXTime)
import qualified Hasql.Connection                     as H
import qualified Hasql.Query                          as H
import qualified Hasql.Session                        as H
import qualified Hasql.Decoders                       as HD
import qualified Hasql.Encoders                       as HE
import qualified Hasql.Pool                           as P
import Network.Wai
import Network.HTTP.Types

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
  conOrError <- H.acquire pgSettings
  let con = either (panic . show) id conOrError :: H.Connection

  -- ask for the OS time at most once per second
  getTime <- mkAutoUpdate
    defaultUpdateSettings { updateAction = getPOSIXTime }

  multi <- newHasqlBroadcaster con
  void $ relayMessagesForever multi

  runSettings appSettings $
    postgrestWsMiddleware (configJwtSecret conf) getTime pool multi $
    logStdout (\_ respond ->
      respond $ responseLBS status200 [("Content-Type", "text/plain")] "PostgREST-WS is running!"
    )

loadSecretFile :: AppConfig -> IO AppConfig
loadSecretFile conf = extractAndTransform mSecret
  where
    mSecret   = decodeUtf8 <$> configJwtSecret conf
    isB64     = configJwtSecretIsBase64 conf

    extractAndTransform :: Maybe Text -> IO AppConfig
    extractAndTransform Nothing  = return conf
    extractAndTransform (Just s) =
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

    setSecret bs = conf { configJwtSecret = Just bs }

    replaceUrlChars = replace "_" "/" . replace "-" "+" . replace "." "="
