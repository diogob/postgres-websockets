module ClaimsSpec (spec) where

import           Protolude

import qualified Data.HashMap.Strict as M
import           Test.Hspec
import           Data.Aeson          (Value (..) )

import           PostgRESTWS.Claims

spec :: Spec
spec =
  describe "validate claims"
  $ it "should succeed using a matching token"
  $ validateClaims Nothing "reallyreallyreallyreallyverysafe"
                   "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJtb2RlIjoiciIsImNoYW5uZWwiOiJ0ZXN0In0.1d4s-at2kWj8OSabHZHTbNh1dENF7NWy_r0ED3Rwf58"
                   `shouldReturn` Right ("test", "r", M.fromList[("mode",String "r"),("channel",String "test")])
