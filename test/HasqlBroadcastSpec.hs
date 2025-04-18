module HasqlBroadcastSpec (spec) where

import APrelude
import Hasql.Notifications
import PostgresWebsockets.Broadcast
import PostgresWebsockets.HasqlBroadcast
import Test.Hspec

spec :: Spec
spec = describe "newHasqlBroadcaster" $ do
  let newConnection connStr =
        either (panic . showText) id
          <$> acquire connStr

  it "relay messages sent to the appropriate database channel" $ do
    multi <- either (panic . showText) id <$> newHasqlBroadcasterOrError (pure ()) "postgres-websockets" "postgres://postgres:roottoor@localhost:5432/postgres_ws_test"
    msg <- liftIO newEmptyMVar
    onMessage multi "test" $ putMVar msg

    con <- newConnection "postgres://postgres:roottoor@localhost:5432/postgres_ws_test"
    void $ notify con (toPgIdentifier "postgres-websockets") "{\"channel\": \"test\", \"payload\": \"hello there\"}"

    readMVar msg `shouldReturn` Message "test" "{\"channel\": \"test\", \"payload\": \"hello there\"}"
