module PostgRESTWS
  ( postgrestWsApp
  ) where

import qualified Data.Text as T
import qualified Network.Wai                     as Wai
import qualified Network.WebSockets              as WS
import qualified Network.Wai.Handler.WebSockets  as WS
import           PostgREST.App    as PGR
import           PostgREST.Config as PGR
import           PostgREST.Types  as PGR
import qualified Hasql.Pool as H
import GHC.IORef

postgrestWsApp :: PGR.AppConfig
                    -> IORef PGR.DbStructure
                    -> H.Pool
                    -> Wai.Application
postgrestWsApp conf refDbStructure pool = WS.websocketsOr WS.defaultConnectionOptions wsApp $ postgrest conf refDbStructure pool
  where
    wsApp :: WS.ServerApp
    wsApp pending_conn = do
      conn <- WS.acceptRequest pending_conn
      WS.sendTextData conn ("Hello, client!" :: T.Text)
