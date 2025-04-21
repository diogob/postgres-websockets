-- |
-- Module      : PostgresWebsockets.Claims
-- Description : Parse and validate JWT to open postgres-websockets channels.
--
-- This module provides the JWT claims validation. Since websockets and
-- listening connections in the database tend to be resource intensive
-- (not to mention stateful) we need claims authorizing a specific channel and
-- mode of operation.
module PostgresWebsockets.Claims
  ( ConnectionInfo,
    validateClaims,
  )
where

import APrelude
import Control.Lens
import qualified Crypto.JOSE.Types as JOSE.Types
import Crypto.JWT
import qualified Data.Aeson as JSON
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as JSON
import Data.List
import Data.Time.Clock (UTCTime)

type Claims = JSON.KeyMap JSON.Value

type ConnectionInfo = ([Text], Text, Claims)

-- | Given a secret, a token and a timestamp it validates the claims and returns
--    either an error message or a triple containing channel, mode and claims KeyMap.
validateClaims ::
  Maybe Text ->
  ByteString ->
  LByteString ->
  UTCTime ->
  IO (Either Text ConnectionInfo)
validateClaims requestChannel secret jwtToken time = runExceptT $ do
  cl <- liftIO $ jwtClaims time (parseJWK secret) jwtToken
  cl' <- case cl of
    JWTClaims c -> pure c
    JWTInvalid JWTExpired -> throwError "Token expired"
    JWTInvalid err -> throwError $ "Error: " <> showText err
  channels <-
    let chs = claimAsJSONList "channels" cl'
     in pure $ case claimAsJSON "channel" cl' of
          Just c -> case chs of
            Just cs -> nub (c : cs)
            Nothing -> [c]
          Nothing -> fromMaybe [] chs
  mode <-
    let md = claimAsJSON "mode" cl'
     in case md of
          Just m -> pure m
          Nothing -> throwError "Missing mode"
  requestedAllowedChannels <- case (requestChannel, length channels) of
    (Just rc, 0) -> pure [rc]
    (Just rc, _) -> pure $ filter (== rc) channels
    (Nothing, _) -> pure channels
  validChannels <- if null requestedAllowedChannels then throwError "No allowed channels" else pure requestedAllowedChannels
  pure (validChannels, mode, cl')
  where
    claimAsJSON :: Text -> Claims -> Maybe Text
    claimAsJSON name cl = case JSON.lookup (Key.fromText name) cl of
      Just (JSON.String s) -> Just s
      _ -> Nothing

    claimAsJSONList :: Text -> Claims -> Maybe [Text]
    claimAsJSONList name cl = case JSON.lookup (Key.fromText name) cl of
      Just channelsJson ->
        case JSON.fromJSON channelsJson :: JSON.Result [Text] of
          JSON.Success channelsList -> Just channelsList
          _ -> Nothing
      Nothing -> Nothing

-- |
--  Possible situations encountered with client JWTs
data JWTAttempt
  = JWTInvalid JWTError
  | JWTClaims (JSON.KeyMap JSON.Value)
  deriving (Eq)

-- |
--  Receives the JWT secret (from config) and a JWT and returns a map
--  of JWT claims.
jwtClaims :: UTCTime -> JWK -> LByteString -> IO JWTAttempt
jwtClaims _ _ "" = return $ JWTClaims JSON.empty
jwtClaims time jwk' payload = do
  let config = defaultJWTValidationSettings (const True)
  eJwt <- runExceptT $ do
    jwt <- decodeCompact payload
    verifyClaimsAt config jwk' time jwt
  return $ case eJwt of
    Left e -> JWTInvalid e
    Right jwt -> JWTClaims . claims2map $ jwt

-- |
--  Internal helper used to turn JWT ClaimSet into something
--  easier to work with
claims2map :: ClaimsSet -> JSON.KeyMap JSON.Value
claims2map = val2map . JSON.toJSON
  where
    val2map (JSON.Object o) = o
    val2map _ = JSON.empty

-- |
--  Internal helper to generate HMAC-SHA256. When the jwt key in the
--  config file is a simple string rather than a JWK object, we'll
--  apply this function to it.
hs256jwk :: ByteString -> JWK
hs256jwk key =
  fromKeyMaterial km
    & jwkUse ?~ Sig
    & jwkAlg ?~ JWSAlg HS256
  where
    km = OctKeyMaterial (OctKeyParameters (JOSE.Types.Base64Octets key))

parseJWK :: ByteString -> JWK
parseJWK str =
  fromMaybe (hs256jwk str) (JSON.decode (fromStrict str) :: Maybe JWK)
