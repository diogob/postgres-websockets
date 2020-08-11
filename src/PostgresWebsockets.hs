{-|
Module      : PostgresWebsockets
Description : PostgresWebsockets main library interface.

These are all function necessary to start a fully functionaing service.
-}
module PostgresWebsockets
  ( prettyVersion
  , loadConfig
  , serve
  , postgresWsMiddleware
  ) where

import           PostgresWebsockets.Middleware
import           PostgresWebsockets.Server
import           PostgresWebsockets.Config
