module ClaimsSpec (spec) where

import APrelude
import Data.Aeson (Value (..), toJSON)
import qualified Data.Aeson.KeyMap as JSON
import Data.Time.Clock
import PostgresWebsockets.Claims
import Test.Hspec
import Prelude

secret :: ByteString
secret = "reallyreallyreallyreallyverysafe"

spec :: Spec
spec =
  describe "validate claims" $ do
    it "should invalidate an expired token" $ do
      time <- getCurrentTime
      validateClaims
        Nothing
        secret
        "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJtb2RlIjoiciIsImNoYW5uZWwiOiJ0ZXN0IiwiZXhwIjoxfQ.4rDYiMZFR2WHB7Eq4HMdvDP_BQZVtHIfyJgy0NshbHY"
        time
        `shouldReturn` Left "Token expired"
    it "request any channel from a token that does not have channels or channel claims should succeed" $ do
      time <- getCurrentTime
      validateClaims
        (Just "test")
        secret
        "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJtb2RlIjoiciJ9.jL5SsRFegNUlbBm8_okhHSujqLcKKZdDglfdqNl1_rY"
        time
        `shouldReturn` Right (["test"], "r", JSON.fromList [("mode", String "r")])
    it "requesting a channel that is set by and old style channel claim should work" $ do
      time <- getCurrentTime
      validateClaims
        (Just "test")
        secret
        "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJtb2RlIjoiciIsImNoYW5uZWwiOiJ0ZXN0In0.1d4s-at2kWj8OSabHZHTbNh1dENF7NWy_r0ED3Rwf58"
        time
        `shouldReturn` Right (["test"], "r", JSON.fromList [("mode", String "r"), ("channel", String "test")])
    it "no requesting channel should return all channels in the token" $ do
      time <- getCurrentTime
      validateClaims
        Nothing
        secret
        "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJtb2RlIjoiciIsImNoYW5uZWxzIjpbInRlc3QiLCJhbm90aGVyIHRlc3QiXX0.b9N8J8tPOPIxxFj5WJ7sWrmcL8ib63i8eirsRZTM9N0"
        time
        `shouldReturn` Right (["test", "another test"], "r", JSON.fromList [("mode", String "r"), ("channels", toJSON ["test" :: Text, "another test" :: Text])])

    it "requesting a channel from the channels claim shoud return only the requested channel" $ do
      time <- getCurrentTime
      validateClaims
        (Just "test")
        secret
        "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJtb2RlIjoiciIsImNoYW5uZWxzIjpbInRlc3QiLCJ0ZXN0MiJdfQ.MumdJ5FpFX4Z6SJD3qsygVF0r9vqxfqhj5J30O32N0k"
        time
        `shouldReturn` Right (["test"], "r", JSON.fromList [("mode", String "r"), ("channels", toJSON ["test" :: Text, "test2"])])

    it "requesting a channel not from the channels claim shoud error" $ do
      time <- getCurrentTime
      validateClaims
        (Just "notAllowed")
        secret
        "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJtb2RlIjoiciIsImNoYW5uZWxzIjpbInRlc3QiLCJ0ZXN0MiJdfQ.MumdJ5FpFX4Z6SJD3qsygVF0r9vqxfqhj5J30O32N0k"
        time
        `shouldReturn` Left "No allowed channels"

    it "requesting a channel with no mode fails" $ do
      time <- getCurrentTime
      validateClaims
        (Just "test")
        secret
        "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJjaGFubmVscyI6WyJ0ZXN0IiwidGVzdDIiXX0.akC1PEYk2DEZtLP2XjC6qXOGZJejmPx49qv-VeEtQYQ"
        time
        `shouldReturn` Left "Missing mode"
