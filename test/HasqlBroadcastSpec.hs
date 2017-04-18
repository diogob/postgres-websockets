module HasqlBroadcastSpec (spec) where

import Protolude

import           Data.Function                        (id)
import Control.Concurrent.STM.TQueue
import qualified Hasql.Query as H
import qualified Hasql.Session as H
import qualified Hasql.Decoders as HD
import qualified Hasql.Encoders as HE

import Test.Hspec

import PostgRESTWS.Database
import PostgRESTWS.Broadcast
import PostgRESTWS.HasqlBroadcast

spec :: Spec
spec = describe "newHasqlBroadcaster" $
    it "start listening on a database connection as we send an Open command" $ do
      conOrError <- acquire "postgres://localhost/postgrest_test"
      let con = either (panic . show) id conOrError
      multi <- liftIO $ newHasqlBroadcaster con
      atomically $ openChannelProducer multi "test"

      let statement = H.statement "SELECT EXISTS (SELECT 1 FROM pg_stat_activity WHERE query ~* 'LISTEN \"test\"')"
                      HE.unit (HD.singleRow $ HD.value HD.bool) False
          query = H.query () statement
      resOrError <- H.run query con
      let result = either (panic . show) id resOrError
      result `shouldBe` True
