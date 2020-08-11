{-| PostgresWebsockets main library interface
    All function necessary to start a fully functionaing service should be re-exported here.
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
