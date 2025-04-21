-- |
-- Module      : PostgresWebsockets
-- Description : PostgresWebsockets main library interface.
--
-- These are all function necessary to configure and start the server.
module PostgresWebsockets
  ( prettyVersion,
    loadConfig,
    serve,
    postgresWsMiddleware,
  )
where

import PostgresWebsockets.Config (loadConfig, prettyVersion)
import PostgresWebsockets.Middleware (postgresWsMiddleware)
import PostgresWebsockets.Server (serve)
