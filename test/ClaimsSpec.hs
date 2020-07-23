module ClaimsSpec (spec) where

import           Protolude

import qualified Data.HashMap.Strict as M
import           Test.Hspec
import           Data.Aeson          (Value (..),  toJSON, Array, fromJSON )
import Data.Time.Clock
import           PostgresWebsockets.Claims

secret :: ByteString
secret = "reallyreallyreallyreallyverysafe"
-- it would be nice if we could use JOSE? to generate the test tokens...

spec :: Spec
spec =
  describe "validate claims" $ do
    it "should invalidate an expired token" $ do
      time <- getCurrentTime
      validateClaims Nothing secret
                   "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJtb2RlIjoiciIsImNoYW5uZWwiOiJ0ZXN0IiwiZXhwIjoxfQ.4rDYiMZFR2WHB7Eq4HMdvDP_BQZVtHIfyJgy0NshbHY" time
                   `shouldReturn` Left "Token expired"
    it "requesting a channel that is set by and old style channel claim should work" $ do
      time <- getCurrentTime
      validateClaims (Just (encodeUtf8 "test")) secret
                   "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJtb2RlIjoiciIsImNoYW5uZWwiOiJ0ZXN0In0.1d4s-at2kWj8OSabHZHTbNh1dENF7NWy_r0ED3Rwf58" time
                   `shouldReturn` Right (["test"], "r", M.fromList[("mode",String "r"),("channel",String "test")])
    it "no requesting channel should return all channels in the token" $ do
      time <- getCurrentTime
      validateClaims Nothing secret
                   "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJtb2RlIjoiciIsImNoYW5uZWxzIjpbInRlc3QiXX0.am8O0kbbcVNIpDPJy6ZSb49yE9w9vjlaBHzF3lro3e8" time
                   `shouldReturn` Right (["test"], "r", M.fromList[("mode",String "r"),("channels",  toJSON["test"::Text] )        ])

    it "requesting a chanel from the channels claim shoud return the requested chanel" $ do
      time <- getCurrentTime
      validateClaims (Just (encodeUtf8 "test")) secret
                   "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJtb2RlIjoiciIsImNoYW5uZWxzIjpbInRlc3QiXSwiZXhwIjoxfQ.jPPRtgz_TSk1Ft5b1JEdwF28yi790wcghePbpLORcBM" time
                   `shouldReturn` Right (["test"], "r", M.fromList[("mode",String "r"),("channels",  toJSON ["test"::Text, "test2"] )])
