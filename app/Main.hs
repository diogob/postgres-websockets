{-# LANGUAGE CPP #-}

module Main where


import           PostgREST.App
import           PostgREST.Config                     (AppConfig (..),
                                                       minimumPgVersion,
                                                       prettyVersion,
                                                       readOptions)
import           PostgREST.DbStructure

import           Control.Monad
import           Control.Monad.IO.Class               (liftIO)
import           Data.Monoid                          ((<>))
import           Data.String.Conversions              (cs)
import qualified Hasql.Query                          as H
import qualified Hasql.Session                        as H
import qualified Hasql.Decoders                       as HD
import qualified Hasql.Encoders                       as HE
import qualified Hasql.Pool                           as P
import           Network.Wai.Handler.Warp
import           System.IO                            (BufferMode (..),
                                                       hSetBuffering, stderr,
                                                       stdin, stdout)
import           Web.JWT                              (secret)
import qualified Data.Text as T
import qualified Network.Wai                     as Wai
import qualified Network.WebSockets              as WS
import qualified Network.Wai.Handler.WebSockets  as WS

#ifndef mingw32_HOST_OS
import           System.Posix.Signals
import           Control.Concurrent                   (myThreadId)
import           Data.IORef
import           Control.Exception.Base               (throwTo, AsyncException(..))
#endif

isServerVersionSupported :: H.Session Bool
isServerVersionSupported = do
  ver <- H.query () pgVersion
  return $ read (cs ver) >= minimumPgVersion
 where
  pgVersion =
    H.statement "SHOW server_version_num"
      HE.unit (HD.singleRow $ HD.value HD.text) True

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  hSetBuffering stdin  LineBuffering
  hSetBuffering stderr NoBuffering

  conf <- readOptions
  let port = configPort conf
      pgSettings = cs (configDatabase conf)
      appSettings = setPort port
                  . setServerName (cs $ "postgrest/" <> prettyVersion)
                  $ defaultSettings

  unless (secret "secret" /= configJwtSecret conf) $
    putStrLn "WARNING, running in insecure mode, JWT secret is the default value"
  Prelude.putStrLn $ "Listening on port " ++
    (show $ configPort conf :: String)

  pool <- P.acquire (configPool conf, 10, pgSettings)

  result <- P.use pool $ do
    supported <- isServerVersionSupported
    unless supported $ error (
      "Cannot run in this PostgreSQL version, PostgREST needs at least "
      <> show minimumPgVersion)
    getDbStructure (cs $ configSchema conf)

  refDbStructure <- newIORef $ either (error.show) id result

#ifndef mingw32_HOST_OS
  tid <- myThreadId
  forM_ [sigINT, sigTERM] $ \sig ->
    void $ installHandler sig (Catch $ do
        P.release pool
        throwTo tid UserInterrupt
      ) Nothing

  void $ installHandler sigHUP (
      Catch . void . P.use pool $ do
        s <- getDbStructure (cs $ configSchema conf)
        liftIO $ atomicWriteIORef refDbStructure s
   ) Nothing
#endif
  let app :: Wai.Application
      app = WS.websocketsOr WS.defaultConnectionOptions wsApp $ postgrest conf refDbStructure pool
        where
          wsApp :: WS.ServerApp
          wsApp pending_conn = do
            conn <- WS.acceptRequest pending_conn
            WS.sendTextData conn ("Hello, client!" :: T.Text)
  runSettings appSettings app
