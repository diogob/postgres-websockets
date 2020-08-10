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
startTestServer = do
    threadId <- forkIO $ serve testServerConfig
    threadDelay 1000
    pure threadId

withServer :: IO () -> IO ()
withServer action =
  bracket startTestServer
          killThread
          (const action)

sendWsData :: Text -> Text -> IO ()
sendWsData uri msg =
    withSocketsDo $ 
        WS.runClient 
            "localhost" 
            (configPort testServerConfig) 
            (toS uri) 
            (`WS.sendTextData` msg)

testChannel :: Text
testChannel = "/test/eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJtb2RlIjoicncifQ.auy9z4-pqoVEAay9oMi1FuG7ux_C_9RQCH8-wZgej18"

secondaryChannel :: Text
secondaryChannel = "/secondary/eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJtb2RlIjoicncifQ.auy9z4-pqoVEAay9oMi1FuG7ux_C_9RQCH8-wZgej18"

testAndSecondaryChannel :: Text
testAndSecondaryChannel = "/eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJtb2RlIjoicnciLCJjaGFubmVscyI6WyJ0ZXN0Iiwic2Vjb25kYXJ5Il19.7tB2A9MhpY4tyqhfnHNy5FUYw4gwpKtL4UAHBXbNEz4"

waitForWsData :: Text -> IO (MVar ByteString)
waitForWsData uri = do
    msg <- newEmptyMVar
    void $ forkIO $
        withSocketsDo $ 
            WS.runClient 
                "localhost" 
                (configPort testServerConfig) 
                (toS uri) 
                (\c -> do
                    m <- WS.receiveData c
                    putMVar msg m
                )
    threadDelay 1000
    pure msg

spec :: Spec
spec = around_ withServer $
            describe "serve" $ do
                it "should be able to send messages to test server" $
                    sendWsData testChannel "test data"
                it "should be able to receive messages from test server" $ do
                    msg <- waitForWsData testChannel
                    sendWsData testChannel "test data"
                    msgJson <- takeMVar msg
                    (msgJson ^? key "payload" . _String) `shouldBe` Just "test data"
                it "should be able to send messages to multiple channels in one shot" $ do
                    msg <- waitForWsData testChannel
                    secondaryMsg <- waitForWsData secondaryChannel
                    sendWsData testAndSecondaryChannel "test data"
                    msgJson <- takeMVar msg
                    secondaryMsgJson <- takeMVar secondaryMsg

                    (msgJson ^? key "payload" . _String) `shouldBe` Just "test data"
                    (msgJson ^? key "channel" . _String) `shouldBe` Just "test"
                    (secondaryMsgJson ^? key "payload" . _String) `shouldBe` Just "test data"
                    (secondaryMsgJson ^? key "channel" . _String) `shouldBe` Just "secondary"
