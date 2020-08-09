module ServerSpec (spec) where

import Protolude

import Data.Function (id)
import Test.Hspec
import PostgresWebsockets

import qualified Hasql.Connection as Hasql
import qualified Hasql.Session as Hasql
import qualified Hasql.Statement as Hasql

import qualified Network.WebSockets as WS
import           Network.Socket (withSocketsDo)

testServerConfig :: AppConfig
testServerConfig = AppConfig 
                    { configDatabase = "postgres://localhost/postgres"
                    , configPath = Nothing
                    , configHost = "*"
                    , configPort = 8080
                    , configListenChannel = "postgres-websockets-test-channel"
                    , configJwtSecret = "reallyreallyreallyreallyverysafe"
                    , configJwtSecretIsBase64 = False
                    , configPool = 10
                    }

startTestServer :: IO ThreadId
startTestServer = forkIO $ serve testServerConfig

withServer :: IO () -> IO ()
withServer action =
  bracket startTestServer
          killThread
          (const action)

sendWsData :: Text -> IO ()
sendWsData msg =
    withSocketsDo $ 
        WS.runClient 
            "localhost" 
            (configPort testServerConfig) 
            "/test/eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJtb2RlIjoicncifQ.auy9z4-pqoVEAay9oMi1FuG7ux_C_9RQCH8-wZgej18" 
            (`WS.sendTextData` msg)

spec :: Spec
spec = around_ withServer $
            describe "serve" $
                it "should be able to send messages to test server" $
                    sendWsData "test data"