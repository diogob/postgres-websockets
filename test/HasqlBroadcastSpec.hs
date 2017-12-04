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

    it "relay messages sent to the appropriate database channel" $ do
      pending
