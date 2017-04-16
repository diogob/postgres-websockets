module DatabaseSpec (spec) where

import Protolude

import           Data.Function                        (id)
import qualified Hasql.Connection                     as H
import Test.Hspec

import PostgRESTWS.Database

spec :: Spec
spec =
  describe "waitForNotifications" $
    it "does not block the connection ann trigger action upon notification" $ do
      conOrError <- H.acquire "postgres://localhost/postgrest_test"
      let con = either (panic . show) id conOrError :: H.Connection
      notification <- liftIO newEmptyMVar

      waitForNotifications (curry $ putMVar notification) con
      listen con "test"
      void $ notify con "test" "hello there"

      readMVar notification `shouldReturn` ("test", "hello there")
