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

import           System.IO.Error             (IOError)
import           Control.Applicative
import qualified Data.Configurator           as C
import qualified Data.Configurator.Parser    as C
import qualified Data.Configurator.Types     as C
import           Data.Monoid
import           Data.Scientific             (floatingOrInteger)
import           Data.Text                   (intercalate, lines)
import           Data.Text.Encoding          (encodeUtf8)
import           Data.Version                (versionBranch)
import           Options.Applicative hiding  (str)
import           Paths_postgres_websockets   (version)
import           System.IO                   (hPrint)
import           Text.Heredoc
import           Text.PrettyPrint.ANSI.Leijen hiding ((<>), (<$>))
import qualified Text.PrettyPrint.ANSI.Leijen as L
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

-- | Function to read and parse options from the command line
readOptions :: IO AppConfig
readOptions = do
  -- First read the config file path from command line
  cfgPath <- customExecParser parserPrefs opts
  -- Now read the actual config file
  conf <- catch
    (C.readConfig =<< C.load [C.Required cfgPath])
    configNotfoundHint

  let (mAppConf, errs) = flip C.runParserM conf $ do
        -- db ----------------
        cDbUri    <- C.key "db-uri"
        cPool     <- fromMaybe 10 . join . fmap coerceInt <$> C.key "db-pool"
        -- server ------------
        cPath     <- C.key "server-root"
        cHost     <- fromMaybe "*4" . mfilter (/= "") <$> C.key "server-host"
        cPort     <- fromMaybe 3000 . join . fmap coerceInt <$> C.key "server-port"
        cAuditC   <- C.key "audit-channel"
        cChannel  <- case cAuditC of
          Just c -> fromMaybe c . mfilter (/= "") <$> C.key "listen-channel"
          Nothing -> C.key "listen-channel"
        -- jwt ---------------
        cJwtSec   <- C.key "jwt-secret"
        cJwtB64   <- fromMaybe False <$> C.key "secret-is-base64"

        return $ AppConfig cDbUri cPath cHost cPort cChannel (encodeUtf8 cJwtSec) cJwtB64 cPool

  case mAppConf of
    Nothing -> do
      forM_ errs $ hPrint stderr
      exitFailure
    Just appConf ->
      return appConf

 where
  coerceInt :: (Read i, Integral i) => C.Value -> Maybe i
  coerceInt (C.Number x) = rightToMaybe $ floatingOrInteger x
  coerceInt (C.String x) = readMaybe $ toS x
  coerceInt _            = Nothing

  opts = info (helper <*> pathParser) $
           fullDesc
           <> progDesc (
               "postgres-websockets "
               <> toS prettyVersion
               <> " / Connects websockets to PostgreSQL asynchronous notifications."
             )
           <> footerDoc (Just $
               text "Example Config File:"
               L.<> nest 2 (hardline L.<> exampleCfg)
             )

  parserPrefs = prefs showHelpOnError

  configNotfoundHint :: IOError -> IO a
  configNotfoundHint e = die $ "Cannot open config file:\n\t" <> show e

  missingKeyHint :: C.KeyError -> IO a
  missingKeyHint (C.KeyError n) =
    die $
      "Required config parameter \"" <> n <> "\" is missing or of wrong type.\n"

  exampleCfg :: Doc
  exampleCfg = vsep . map (text . toS) . lines $
    [str|db-uri = "postgres://user:pass@localhost:5432/dbname"
        |db-pool = 10
        |
        |server-root = "./client-example"
        |server-host = "*4"
        |server-port = 3000
        |listen-channel = 3000
        |
        |## choose a secret to enable JWT auth
        |## (use "@filename" to load from separate file)
        |# jwt-secret = "foo"
        |# secret-is-base64 = false
        |]


pathParser :: Parser FilePath
pathParser =
  strArgument $
    metavar "FILENAME" <>
    help "Path to configuration file"

data PgVersion = PgVersion {
  pgvNum  :: Int32
, pgvName :: Text
}

-- | Tells the minimum PostgreSQL version required by this version of PostgresWebsockets
minimumPgVersion :: PgVersion
minimumPgVersion = PgVersion 90300 "9.3"
