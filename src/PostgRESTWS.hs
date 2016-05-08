module PostgRESTWS
  ( postgrestWsApp
  ) where

import qualified Network.Wai                     as Wai
import qualified Network.WebSockets              as WS
import qualified Network.Wai.Handler.WebSockets  as WS
import           PostgREST.App    as PGR
import           PostgREST.Config as PGR
import           PostgREST.Types  as PGR
import qualified Hasql.Pool as H
import GHC.IORef
import Data.Monoid
import qualified Data.Text as T

postgrestWsApp :: PGR.AppConfig
                    -> IORef PGR.DbStructure
                    -> H.Pool
                    -> Wai.Application
postgrestWsApp conf refDbStructure pool =
  WS.websocketsOr WS.defaultConnectionOptions wsApp $ postgrest conf refDbStructure pool
  where
    wsApp :: WS.ServerApp
    wsApp pending_conn = do
      conn <- WS.acceptRequest pending_conn
      WS.sendTextData conn ("Hello, client! Your headers are: " <> T.pack (show $ WS.requestHeaders (WS.pendingRequest pending_conn)))
