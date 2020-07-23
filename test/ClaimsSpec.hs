module ClaimsSpec (spec) where

import           Protolude

import qualified Data.HashMap.Strict as M
import           Test.Hspec
import           Data.Aeson          (Value (..) )
import Data.Time.Clock
import           PostgresWebsockets.Claims

spec :: Spec
spec =
  describe "validate claims" $ do
    it "should invalidate an expired token" $ do
      time <- getCurrentTime
      validateClaims Nothing "reallyreallyreallyreallyverysafe"
                   "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJtb2RlIjoiciIsImNoYW5uZWwiOiJ0ZXN0IiwiZXhwIjoxfQ.4rDYiMZFR2WHB7Eq4HMdvDP_BQZVtHIfyJgy0NshbHY" time
                   `shouldReturn` Left "Token expired"
    it "should succeed using a matching token" $ do
      time <- getCurrentTime
      validateClaims Nothing "reallyreallyreallyreallyverysafe"
                   "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJtb2RlIjoiciIsImNoYW5uZWwiOiJ0ZXN0In0.1d4s-at2kWj8OSabHZHTbNh1dENF7NWy_r0ED3Rwf58" time
                   `shouldReturn` Right (["test"], "r", M.fromList[("mode",String "r"),("channels",String "test")])    it "should succeed using a matching token" $ do
      time <- getCurrentTime
      validateClaims Nothing "reallyreallyreallyreallyverysafe"
                   "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJtb2RlIjoiciIsImNoYW5uZWxzIjpbInRlc3QiXSwiZXhwIjoxfQ.jPPRtgz_TSk1Ft5b1JEdwF28yi790wcghePbpLORcBM" time
                   `shouldReturn` Right (["test"], "r", M.fromList[("mode",String "r"),("channels",M.fromList[ "test")])

      time <- getCurrentTime
      validateClaims Nothing "reallyreallyreallyreallyverysafe"
                   "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJtb2RlIjoiciIsImNoYW5uZWxzIjpbInRlc3QiXSwiZXhwIjoxfQ.jPPRtgz_TSk1Ft5b1JEdwF28yi790wcghePbpLORcBM" time
                   `shouldReturn` Right ("test":"test2":[], "r", M.fromList[("mode",String "r"),("channels",M.fromList[ "test", "test2")])
