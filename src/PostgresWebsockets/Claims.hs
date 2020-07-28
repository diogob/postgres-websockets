{-| This module provides the JWT claims validation. Since websockets and
    listening connections in the database tend to be resource intensive
    (not to mention stateful) we need claims authorizing a specific channel and
    mode of operation.
-}
module PostgresWebsockets.Claims
  ( ConnectionInfo,validateClaims
  ) where

import           Control.Lens
import qualified Crypto.JOSE.Types   as JOSE.Types
import           Crypto.JWT
import qualified Data.HashMap.Strict as M
import           Protolude
import Data.List
import Data.Time.Clock (UTCTime)
import Data.String (String, fromString)
import qualified Data.Aeson as JSON
import qualified Data.Aeson.Types as JSON


type Claims = M.HashMap Text JSON.Value
type ConnectionInfo = ([ByteString], ByteString, Claims)

{-| Given a secret, a token and a timestamp it validates the claims and returns
    either an error message or a triple containing channel, mode and claims hashmap.
-}
validateClaims
  :: Maybe ByteString
  -> ByteString
  -> LByteString
  -> UTCTime
  -> IO (Either Text ConnectionInfo)
validateClaims requestChannel secret jwtToken time = runExceptT $ do
  cl  <- liftIO $ jwtClaims time (parseJWK secret) jwtToken
  cl' <- case cl of
    JWTClaims  c          -> pure c
    JWTInvalid JWTExpired -> throwError "Token expired"
    _                     -> throwError "Error"
  channels  <-  let chs = claimAsJSONList "channels" cl' in pure $ case claimAsJSON "channel" cl' of
    Just c ->  case chs of
      Just cs ->  nub (c : cs)
      Nothing ->  [c]
    Nothing -> fromMaybe [] chs
  mode <-
    let md = claimAsJSON "mode" cl'
    in case md of
          Just m  -> pure m
          Nothing -> throwError "Missing mode"
  requestedAllowedChannels <- case requestChannel of
    Just rc -> pure $ filter (== rc) channels
    Nothing -> pure channels
  pure (requestedAllowedChannels, mode, cl')

 where
  claimAsJSON :: Text -> Claims -> Maybe ByteString
  claimAsJSON name cl = case M.lookup name cl of
    Just (JSON.String s) -> Just $ encodeUtf8 s
    _ -> Nothing

  claimAsJSONList :: Text -> Claims -> Maybe [ByteString]
  claimAsJSONList name cl = case M.lookup name cl of
    Just channelsJson ->
      case JSON.fromJSON channelsJson :: JSON.Result [Text] of
        JSON.Success channelsList -> Just $ encodeUtf8 <$> channelsList
        _ -> Nothing
    Nothing -> Nothing

{- Private functions and types copied from postgrest

   This code duplication will be short lived since postgrest will migrate towards jose
   Then this library will use jose's verifyClaims and error types.
-}
{-|
  Possible situations encountered with client JWTs
-}
data JWTAttempt = JWTInvalid JWTError
                | JWTMissingSecret
                | JWTClaims (M.HashMap Text JSON.Value)
                deriving Eq

{-|
  Receives the JWT secret (from config) and a JWT and returns a map
  of JWT claims.
-}
jwtClaims :: UTCTime -> JWK -> LByteString -> IO JWTAttempt
jwtClaims _ _ "" = return $ JWTClaims M.empty
jwtClaims time jwk payload = do
  let config = defaultJWTValidationSettings (const True)
  eJwt <- runExceptT $ do
    jwt <- decodeCompact payload
    verifyClaimsAt config jwk time jwt
  return $ case eJwt of
    Left e    -> JWTInvalid e
    Right jwt -> JWTClaims . claims2map $ jwt

{-|
  Internal helper used to turn JWT ClaimSet into something
  easier to work with
-}
claims2map :: ClaimsSet -> M.HashMap Text JSON.Value
claims2map = val2map . JSON.toJSON
 where
  val2map (JSON.Object o) = o
  val2map _          = M.empty

{-|
  Internal helper to generate HMAC-SHA256. When the jwt key in the
  config file is a simple string rather than a JWK object, we'll
  apply this function to it.
-}
hs256jwk :: ByteString -> JWK
hs256jwk key =
  fromKeyMaterial km
    & jwkUse ?~ Sig
    & jwkAlg ?~ JWSAlg HS256
 where
  km = OctKeyMaterial (OctKeyParameters (JOSE.Types.Base64Octets key))

parseJWK :: ByteString -> JWK
parseJWK str =
  fromMaybe (hs256jwk str) (JSON.decode (toS str) :: Maybe JWK)
