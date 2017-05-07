{-|
Module      : Config
Description : Manages PostgRESTWS configuration options.

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
import qualified Data.ByteString             as B
import qualified Data.Configurator           as C
import qualified Data.Configurator.Types     as C
import           Data.Monoid
import           Data.Text                   (intercalate, lines)
import           Data.Text.Encoding          (encodeUtf8)
import           Data.Text.IO                (hPutStrLn)
import           Data.Version                (versionBranch)
import           Options.Applicative hiding  (str)
import           Paths_postgrest_ws             (version)
import           Text.Heredoc
import           Text.PrettyPrint.ANSI.Leijen hiding ((<>), (<$>))
import qualified Text.PrettyPrint.ANSI.Leijen as L
import           Protolude hiding            (intercalate, (<>))

-- | Config file settings for the server
data AppConfig = AppConfig {
    configDatabase          :: Text
  , configHost              :: Text
  , configPort              :: Int

  , configJwtSecret         :: Maybe B.ByteString
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
    (C.load [C.Required cfgPath])
    configNotfoundHint

  handle missingKeyHint $ do
    -- db ----------------
    cDbUri    <- C.require conf "db-uri"
    cPool     <- C.lookupDefault 10 conf "db-pool"
    -- server ------------
    cHost     <- C.lookupDefault "*4" conf "server-host"
    cPort     <- C.lookupDefault 3000 conf "server-port"
    -- jwt ---------------
    cJwtSec   <- C.lookup conf "jwt-secret"
    cJwtB64   <- C.lookupDefault False conf "secret-is-base64"

    return $ AppConfig cDbUri cHost cPort (encodeUtf8 <$> cJwtSec) cJwtB64 cPool

 where
  opts = info (helper <*> pathParser) $
           fullDesc
           <> progDesc (
               "PostgREST "
               <> toS prettyVersion
               <> " / create a REST API to an existing Postgres database"
             )
           <> footerDoc (Just $
               text "Example Config File:"
               L.<> nest 2 (hardline L.<> exampleCfg)
             )

  parserPrefs = prefs showHelpOnError

  configNotfoundHint :: IOError -> IO a
  configNotfoundHint e = do
    hPutStrLn stderr $
      "Cannot open config file:\n\t" <> show e
    exitFailure

  missingKeyHint :: C.KeyError -> IO a
  missingKeyHint (C.KeyError n) = do
    hPutStrLn stderr $
      "Required config parameter \"" <> n <> "\" is missing or of wrong type.\n" <>
      "Documentation for configuration options available at\n" <>
      "\thttp://postgrest.com/en/v0.4/admin.html#configuration\n\n" <>
      "Try the --example-config option to see how to configure PostgREST."
    exitFailure

  exampleCfg :: Doc
  exampleCfg = vsep . map (text . toS) . lines $
    [str|db-uri = "postgres://user:pass@localhost:5432/dbname"
        |db-pool = 10
        |
        |server-host = "*4"
        |server-port = 3000
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

-- | Tells the minimum PostgreSQL version required by this version of PostgRESTWS
minimumPgVersion :: PgVersion
minimumPgVersion = PgVersion 90300 "9.3"
