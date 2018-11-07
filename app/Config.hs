{-|
Module      : Config
Description : Manages PostgresWebsockets configuration options.

This module provides a helper function to read the command line
arguments using the optparse-applicative and the AppConfig type to store
them.  It also can be used to define other middleware configuration that
may be delegated to some sort of external configuration.

It currently includes a hardcoded CORS policy but this could easly be
turned in configurable behaviour if needed.

Other hardcoded options such as the minimum version number also belong here.
-}
module Config ( prettyVersion
                        , readOptions
                        , minimumPgVersion
                        , PgVersion (..)
                        , AppConfig (..)
                        )
       where

import Env
import           Data.Text                   (intercalate)
import           Data.Version                (versionBranch)
import           Paths_postgres_websockets   (version)
import           Protolude hiding            (intercalate, (<>))

-- | Config file settings for the server
data AppConfig = AppConfig {
    configDatabase          :: Text
  , configPath              :: Text
  , configHost              :: Text
  , configPort              :: Int
  , configListenChannel     :: Text
  , configJwtSecret         :: ByteString
  , configJwtSecretIsBase64 :: Bool
  , configPool              :: Int
  }

-- | User friendly version number
prettyVersion :: Text
prettyVersion = intercalate "." $ map show $ versionBranch version

-- | Function to read and parse options from the environment
readOptions :: IO AppConfig
readOptions =
    Env.parse (header "You need to configure some environment variables to start the service.") $
      AppConfig <$> var (str <=< nonempty) "PGWS_DB_URI"  (help "String to connect to PostgreSQL")
                <*> var str "PGWS_ROOT_PATH" (def "./" <> helpDef show <> help "Root path to serve static files")
                <*> var str "PGWS_HOST" (def "*4" <> helpDef show <> help "Address the server will listen for websocket connections")
                <*> var auto "PGWS_PORT" (def 3000 <> helpDef show <> help "Port the server will listen for websocket connections")
                <*> var str "PGWS_LISTEN_CHANNEL" (def "postgres-websockets-listener" <> helpDef show <> help "Master channel used in the database to send or read messages in any notification channel")
                <*> var str "PGWS_JWT_SECRET" (help "Secret used to sign JWT tokens used to open communications channels")
                <*> var auto "PGWS_JWT_SECRET_BASE64" (def False <> helpDef show <> help "Indicate whether the JWT secret should be decoded from a base64 encoded string")
                <*> var auto "PGWS_POOL_SIZE" (def 10 <> helpDef show <> help "How many connection to the database should be used by the connection pool")

data PgVersion = PgVersion {
  pgvNum  :: Int32
, pgvName :: Text
}

-- | Tells the minimum PostgreSQL version required by this version of PostgresWebsockets
minimumPgVersion :: PgVersion
minimumPgVersion = PgVersion 90300 "9.3"
