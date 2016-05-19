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
import Control.Concurrent (threadDelay)
import Control.Monad (forever)

postgrestWsApp :: PGR.AppConfig
                    -> IORef PGR.DbStructure
                    -> H.Pool
                    -> Wai.Application
postgrestWsApp conf refDbStructure pool =
  WS.websocketsOr WS.defaultConnectionOptions wsApp $ postgrest conf refDbStructure pool
  where
    wsApp :: WS.ServerApp
    wsApp pendingConn = do
      -- We should accept only after verifying JWT
      conn <- WS.acceptRequest pendingConn
      forever $ sessionHandler conn
      where
        sessionHandler = chooseSession headers
        headers = WS.requestHeaders $ WS.pendingRequest pendingConn

sessionRecorder :: WS.Connection -> IO ()
sessionRecorder = undefined

sessionPlayer :: WS.Connection -> IO ()
sessionPlayer = undefined

chooseSession :: WS.Headers -> WS.Connection -> IO ()
chooseSession = undefined
