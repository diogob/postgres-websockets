{-| This module provides the JWT claims validation. Since websockets and
    listening connections in the database tend to be resource intensive
    (not to mention stateful) we need claims authorizing a specific channel and
    mode of operation.
-}
module PostgresWebsockets.Claims
  ( validateClaims
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
validateClaims :: Maybe ByteString -> ByteString -> LByteString -> UTCTime -> IO (Either Text ConnectionInfo)
validateClaims requestChannel secret jwtToken time =
  runExceptT $ do
    cl <- liftIO $ jwtClaims time (parseJWK secret) jwtToken
    cl' <- case cl of
      JWTClaims c -> pure c
      JWTInvalid JWTExpired -> throwError "Token expired"
      _ -> throwError "Error"
    channel <- claimAsJSON requestChannel "channel" cl'
    channels <- claimAsJSONList requestChannel "channels" cl'
    mode <- claimAsJSON Nothing "mode" cl'
    pure (  channel : channels , mode, cl')

  where
    claimAsJSON :: Maybe ByteString -> Text -> Claims -> ExceptT Text IO ByteString
    claimAsJSON defaultVal name cl = case M.lookup name cl of
      Just (JSON.String s) -> pure $ encodeUtf8 s
      Just _ -> throwError "claim is not string value"
      Nothing -> nonExistingClaim defaultVal name

    claimAsJSONList :: Maybe ByteString -> Text -> Claims -> ExceptT Text IO [ByteString]
    claimAsJSONList defaultVal name cl = case M.lookup name cl of
      Just (JSON.String s) -> pure [encodeUtf8 s]
      Just _ -> throwError "claim is not string value"
      Nothing -> nonExistingClaimList defaultVal name

    nonExistingClaimList :: Maybe ByteString -> Text -> ExceptT Text IO [ByteString]
    nonExistingClaimList Nothing name = throwError (name <> " not in claims")
    nonExistingClaimList (Just defaultVal) _ = pure [defaultVal]

    nonExistingClaim :: Maybe ByteString -> Text -> ExceptT Text IO ByteString
    nonExistingClaim Nothing name = throwError (name <> " not in claims")
    nonExistingClaim (Just defaultVal) _ = pure defaultVal

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
