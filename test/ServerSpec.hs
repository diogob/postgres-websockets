module ServerSpec (spec) where

import Control.Lens
import Data.Aeson.Lens
import Network.Socket (withSocketsDo)
import qualified Network.WebSockets as WS
import PostgresWebsockets
import PostgresWebsockets.Config
import Protolude
import Test.Hspec

testServerConfig :: AppConfig
testServerConfig =
  AppConfig
    { configDatabase = "postgres://postgres:roottoor@localhost:5432/postgres_ws_test",
      configPath = Nothing,
      configHost = "*",
      configPort = 8080,
      configListenChannel = "postgres-websockets-test-channel",
      configJwtSecret = "reallyreallyreallyreallyverysafe",
      configMetaChannel = Nothing,
      configJwtSecretIsBase64 = False,
      configPool = 10,
      configRetries = 5,
      configReconnectInterval = Nothing,
      configCertificateFile = Nothing,
      configKeyFile = Nothing
    }

startTestServer :: IO ThreadId
startTestServer = do
  threadId <- forkIO $ serve testServerConfig
  threadDelay 500000
  pure threadId

withServer :: IO () -> IO ()
withServer action =
  bracket
    startTestServer
    (\tid -> killThread tid >> threadDelay 500000)
    (const action)

sendWsData :: Text -> Text -> IO ()
sendWsData uri msg =
  withSocketsDo $
    WS.runClient
      "127.0.0.1"
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
  void $
    forkIO $
      withSocketsDo $
        WS.runClient
          "127.0.0.1"
          (configPort testServerConfig)
          (toS uri)
          ( \c -> do
              m <- WS.receiveData c
              putMVar msg m
          )
  threadDelay 10000
  pure msg

waitForMultipleWsData :: Int -> Text -> IO (MVar [ByteString])
waitForMultipleWsData messageCount uri = do
  msg <- newEmptyMVar
  void $
    forkIO $
      withSocketsDo $
        WS.runClient
          "127.0.0.1"
          (configPort testServerConfig)
          (toS uri)
          ( \c -> do
              m <- replicateM messageCount (WS.receiveData c)
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
    it "should be able to receive from multiple channels in one shot" $ do
      msgs <- waitForMultipleWsData 2 testAndSecondaryChannel
      sendWsData testAndSecondaryChannel "test data"
      msgsJson <- takeMVar msgs

      forM_
        msgsJson
        (\msgJson -> (msgJson ^? key "payload" . _String) `shouldBe` Just "test data")
