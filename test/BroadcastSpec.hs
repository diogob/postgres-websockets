module BroadcastSpec (spec) where

import Protolude
import Control.Concurrent.STM.TChan
import Control.Concurrent.STM.TQueue

import Test.Hspec

import PostgRESTWS.Broadcast

spec :: Spec
spec =
  describe "Broadcast" $
    it "relays a single message from producer to 1 listener on 1 test channel" $ do
      output <- newTQueueIO :: IO (TQueue Message)
      multi <- liftIO $ newMultiplexer (\_ msgs-> atomically $ writeTQueue msgs (Message "test" "payload")) (\_ -> return ())
      void $ onMessage multi "test" (\ch ->
        atomically $ do
          message <- readTChan ch
          writeTQueue output message)
      liftIO $ relayMessages multi
      outMsg <- atomically $ readTQueue output
      outMsg `shouldBe` Message "test" "payload"
