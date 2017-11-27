module BroadcastSpec (spec) where

import Protolude
import Control.Concurrent.STM.TQueue

import Test.Hspec

import PostgresWebsockets.Broadcast

spec :: Spec
spec = do
  describe "newMultiplexer" $
    it "opens a separate thread for a producer function" $ do
      output <- newTQueueIO :: IO (TQueue ThreadId)

      void $ liftIO $ newMultiplexer (\_ _-> do
        tid <- myThreadId
        atomically $ writeTQueue output tid
        ) (\_ -> return ())

      outMsg <- atomically $ readTQueue output
      myThreadId `shouldNotReturn` outMsg
  describe "relayMessages" $
    it "relays a single message from producer to 1 listener on 1 test channel" $ do
      output <- newTQueueIO :: IO (TQueue Message)
      multi <- liftIO $ newMultiplexer (\_ msgs->
        atomically $ writeTQueue msgs (Message "test" "payload")) (\_ -> return ())
      void $ onMessage multi "test" $ atomically . writeTQueue output

      liftIO $ relayMessages multi

      outMsg <- atomically $ readTQueue output
      outMsg `shouldBe` Message "test" "payload"
  describe "onMessage" $
    it "sends an open command to the producer" $ do
      output <- newTQueueIO :: IO (TQueue Text)
      multi <- liftIO $ newMultiplexer (\cmds _->
        atomically $ do
          cmd <- readTQueue cmds
          writeTQueue output (show cmd :: Text)
        ) (\_ -> return ())

      void $ onMessage multi "new channel" (\_ -> return ())

      outMsg <- atomically $ readTQueue output
      outMsg `shouldBe` "Open \"new channel\""
