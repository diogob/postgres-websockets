module ServerSpec (spec) where

import Protolude

import Data.Function (id)
import Test.Hspec
import PostgresWebsockets

import qualified Hasql.Connection as Hasql
import qualified Hasql.Session as Hasql
import qualified Hasql.Statement as Hasql

import Control.Lens
import Data.Aeson.Lens

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

waitForWsData :: Text -> IO (MVar ByteString)
waitForWsData channel = do
    msg <- newEmptyMVar
    void $ forkIO $
        withSocketsDo $ 
            WS.runClient 
                "localhost" 
                (configPort testServerConfig) 
                "/test/eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJtb2RlIjoicncifQ.auy9z4-pqoVEAay9oMi1FuG7ux_C_9RQCH8-wZgej18" 
                (\c -> do
                    m <- WS.receiveData c
                    putMVar msg m
                )
    pure msg

spec :: Spec
spec = around_ withServer $
            describe "serve" $ do
                it "should be able to send messages to test server" $
                    sendWsData "test data"
                it "should be able to receive messages from test server" $ do
                    msg <- waitForWsData "test"
                    threadDelay 1000
                    sendWsData "test data"
                    msgJson <- takeMVar msg
                    (msgJson ^? key "payload" . _String) `shouldBe` Just "test data"
