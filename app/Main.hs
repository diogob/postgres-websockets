{-# LANGUAGE CPP #-}

module Main where


import           Protolude
import           PostgREST.Config                     (AppConfig (..),
                                                       minimumPgVersion,
                                                       prettyVersion,
                                                       readOptions)
import           PostgREST.DbStructure
import           PostgRESTWS

import qualified Hasql.Query                          as H
import qualified Hasql.Session                        as H
import qualified Hasql.Decoders                       as HD
import qualified Hasql.Encoders                       as HE
import qualified Hasql.Pool                           as P
import           Network.Wai.Handler.Warp
import           System.IO                            ( BufferMode (..)
                                                      , hSetBuffering
                                                      )

import           Web.JWT                              (secret)

#ifndef mingw32_HOST_OS
import           System.Posix.Signals
import           Data.IORef
#endif
import qualified Database.PostgreSQL.LibPQ            as PQ
import           Data.Function (id)

isServerVersionSupported :: H.Session Bool
isServerVersionSupported = do
  ver <- H.query () pgVersion
  return $ toInteger ver >= minimumPgVersion
 where
  pgVersion =
    H.statement "SHOW server_version_num"
      HE.unit (HD.singleRow $ HD.value HD.int4) True

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  hSetBuffering stdin  LineBuffering
  hSetBuffering stderr NoBuffering

  conf <- readOptions
  let port = configPort conf
      pgSettings = toS (configDatabase conf)
      appSettings = setPort port
                  . setServerName (toS $ "postgrest/" <> prettyVersion)
                  $ defaultSettings

  unless (secret "secret" /= configJwtSecret conf) $
    putStrLn ("WARNING, running in insecure mode, JWT secret is the default value" :: Text)
  putStrLn $ ("Listening on port " :: Text) <> show (configPort conf)

  pool <- P.acquire (configPool conf, 10, pgSettings)

  result <- P.use pool $ do
    supported <- isServerVersionSupported
    unless supported $ panic (
      "Cannot run in this PostgreSQL version, PostgREST needs at least "
      <> show minimumPgVersion)
    getDbStructure (toS $ configSchema conf)

  refDbStructure <- newIORef $ either (panic . show) id result
  notificationsCon <- PQ.connectdb pgSettings

#ifndef mingw32_HOST_OS
  tid <- myThreadId
  forM_ [sigINT, sigTERM] $ \sig ->
    void $ installHandler sig (Catch $ do
        P.release pool
        throwTo tid UserInterrupt
      ) Nothing

  void $ installHandler sigHUP (
      Catch . void . P.use pool $ do
        s <- getDbStructure (toS $ configSchema conf)
        liftIO $ atomicWriteIORef refDbStructure s
   ) Nothing
#endif
  runSettings appSettings $ postgrestWsApp conf refDbStructure pool notificationsCon
