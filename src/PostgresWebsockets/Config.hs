-- |
-- Module      : PostgresWebsockets.Config
-- Description : Manages PostgresWebsockets configuration options.
--
-- This module provides a helper function to read the command line
-- arguments using  the AppConfig type to store
-- them.  It also can be used to define other middleware configuration that
-- may be delegated to some sort of external configuration.
module PostgresWebsockets.Config
  ( prettyVersion,
    loadConfig,
    warpSettings,
    AppConfig (..),
  )
where

import APrelude
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as B64
import Data.String (IsString (..))
import Data.Text (intercalate, pack, replace, strip, stripPrefix)
import Data.Version (versionBranch)
import Env
import Network.Wai.Handler.Warp
import Paths_postgres_websockets (version)

-- | Config file settings for the server
data AppConfig = AppConfig
  { configDatabase :: Text,
    configPath :: Maybe Text,
    configHost :: Text,
    configPort :: Int,
    configListenChannel :: Text,
    configMetaChannel :: Maybe Text,
    configJwtSecret :: ByteString,
    configJwtSecretIsBase64 :: Bool,
    configPool :: Int,
    configRetries :: Int,
    configReconnectInterval :: Maybe Int,
    configCertificateFile :: Maybe Text,
    configKeyFile :: Maybe Text
  }
  deriving (Show)

-- | User friendly version number
prettyVersion :: Text
prettyVersion = intercalate "." $ map showText $ versionBranch version

-- | Load all postgres-websockets config from Environment variables. This can be used to use just the middleware or to feed into warpSettings
loadConfig :: IO AppConfig
loadConfig =
  readOptions
    >>= verifyTLSConfig
    >>= loadSecretFile
    >>= loadDatabaseURIFile

-- | Given a shutdown handler and an AppConfig builds a Warp Settings to start a stand-alone server
warpSettings :: (IO () -> IO ()) -> AppConfig -> Settings
warpSettings waitForShutdown AppConfig {..} =
  setHost (fromString $ unpack configHost)
    . setPort configPort
    . setServerName ("postgres-websockets/" <> encodeUtf8 prettyVersion)
    . setTimeout 3600
    . setInstallShutdownHandler waitForShutdown
    . setGracefulShutdownTimeout (Just 5)
    $ defaultSettings

-- private

-- | Function to read and parse options from the environment
readOptions :: IO AppConfig
readOptions =
  Env.parse (header "You need to configure some environment variables to start the service.") $
    AppConfig
      <$> var (str <=< nonempty) "PGWS_DB_URI" (help "String to connect to PostgreSQL")
      <*> optional (var str "PGWS_ROOT_PATH" (help "Root path to serve static files, unset to disable."))
      <*> var str "PGWS_HOST" (def "*4" <> helpDef show <> help "Address the server will listen for websocket connections")
      <*> var auto "PGWS_PORT" (def 3000 <> helpDef show <> help "Port the server will listen for websocket connections")
      <*> var str "PGWS_LISTEN_CHANNEL" (def "postgres-websockets-listener" <> helpDef show <> help "Master channel used in the database to send or read messages in any notification channel")
      <*> optional (var str "PGWS_META_CHANNEL" (help "Websockets channel used to send events about the server state changes."))
      <*> var str "PGWS_JWT_SECRET" (help "Secret used to sign JWT tokens used to open communications channels")
      <*> var auto "PGWS_JWT_SECRET_BASE64" (def False <> helpDef show <> help "Indicate whether the JWT secret should be decoded from a base64 encoded string")
      <*> var auto "PGWS_POOL_SIZE" (def 10 <> helpDef show <> help "How many connection to the database should be used by the connection pool")
      <*> var auto "PGWS_RETRIES" (def 5 <> helpDef show <> help "How many times it should try to connect to the database on startup before exiting with an error")
      <*> optional (var auto "PGWS_CHECK_LISTENER_INTERVAL" (helpDef show <> help "Interval for supervisor thread to check if listener connection is alive. 0 to disable it."))
      <*> optional (var str "PGWS_CERTIFICATE_FILE" (helpDef show <> help "Certificate file to serve secure websockets connection (wss)."))
      <*> optional (var str "PGWS_KEY_FILE" (helpDef show <> help "Key file to serve secure websockets connection (wss)."))

verifyTLSConfig :: AppConfig -> IO AppConfig
verifyTLSConfig conf@AppConfig {..} = do
  when (isJust configCertificateFile /= isJust configKeyFile) $
    panic "PGWS_TLS_CERTIFICATE and PGWS_TLS_KEY must be set in tandem"
  pure conf

loadDatabaseURIFile :: AppConfig -> IO AppConfig
loadDatabaseURIFile conf@AppConfig {..} =
  case stripPrefix "@" configDatabase of
    Nothing -> pure conf
    Just filename -> setDatabase . strip . pack <$> readFile (unpack filename)
  where
    setDatabase uri = conf {configDatabase = uri}

loadSecretFile :: AppConfig -> IO AppConfig
loadSecretFile conf = extractAndTransform secret
  where
    secret = decodeUtf8 $ configJwtSecret conf
    isB64 = configJwtSecretIsBase64 conf

    extractAndTransform :: Text -> IO AppConfig
    extractAndTransform s =
      fmap setSecret $
        transformString isB64
          =<< case stripPrefix "@" s of
            Nothing -> return . encodeUtf8 $ s
            Just filename -> chomp <$> BS.readFile (unpack filename)
      where
        chomp bs = fromMaybe bs (BS.stripSuffix "\n" bs)

    -- Turns the Base64url encoded JWT into Base64
    transformString :: Bool -> ByteString -> IO ByteString
    transformString False t = return t
    transformString True t =
      case B64.decode $ encodeUtf8 $ strip $ replaceUrlChars $ decodeUtf8 t of
        Left errMsg -> panic $ pack errMsg
        Right bs -> return bs

    setSecret bs = conf {configJwtSecret = bs}

    -- replace: Replace every occurrence of one substring with another
    replaceUrlChars =
      replace "_" "/" . replace "-" "+" . replace "." "="
