module HasqlBroadcastSpec (spec) where

import Protolude

import           Data.Function                        (id)
import qualified Hasql.Query as H
import qualified Hasql.Session as H
import qualified Hasql.Decoders as HD
import qualified Hasql.Encoders as HE

import Test.Hspec

import PostgresWebsockets.Broadcast
import PostgresWebsockets.HasqlBroadcast

spec :: Spec
spec = describe "newHasqlBroadcaster" $ do
    let booleanQueryShouldReturn con query expected =
          either (panic . show) id
          <$> H.run query con
          `shouldReturn` expected
        newConnection connStr =
            either (panic . show) id
            <$> acquire connStr

    it "start listening on a database connection as we send an Open command" $ do
      con <- newConnection "postgres://localhost/postgres_ws_test"
      multi <- liftIO $ newHasqlBroadcaster "postgres://localhost/postgres_ws_test"

      atomically $ openChannelProducer multi "test channel"
      threadDelay 1000000

      let statement = H.statement "SELECT EXISTS (SELECT 1 FROM pg_stat_activity WHERE query ~* 'LISTEN \\\"test channel\\\"')"
                      HE.unit (HD.singleRow $ HD.value HD.bool) False
          query = H.query () statement
      booleanQueryShouldReturn con query True

    it "stops listening on a database connection as we send a Close command" $ do
      con <- newConnection "postgres://localhost/postgres_ws_test"
      multi <- liftIO $ newHasqlBroadcaster "postgres://localhost/postgres_ws_test"

      atomically $ closeChannelProducer multi "test channel"
      threadDelay 1000000

      let statement = H.statement "SELECT EXISTS (SELECT 1 FROM pg_stat_activity WHERE query ~* 'UNLISTEN \\\"test channel\\\"')"
                      HE.unit (HD.singleRow $ HD.value HD.bool) False
          query = H.query () statement
      booleanQueryShouldReturn con query True
