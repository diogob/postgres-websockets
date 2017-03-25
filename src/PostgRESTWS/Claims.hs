module PostgRESTWS.Claims
  ( validateClaims
  ) where

import           Protolude
import qualified Data.HashMap.Strict           as M
import           PostgREST.Auth                as PGR
import           Web.JWT                       (binarySecret)
import           Data.Aeson                    (Value (..))
import           Data.Time.Clock.POSIX         (POSIXTime)

type Claims = M.HashMap Text Value
type ConnectionInfo = (ByteString, ByteString, Claims)

validateClaims :: Maybe ByteString -> Text -> POSIXTime -> Either Text ConnectionInfo
validateClaims secret jwtToken time = do
  cl <- case jwtClaims jwtSecret jwtToken time of
    PGR.JWTClaims c -> Right c
    _ -> Left "Error"
  jChannel <- claimAsJSON "channel" cl
  jMode <- claimAsJSON "mode" cl
  channel <- value2BS jChannel
  mode <- value2BS jMode
  Right (channel, mode, cl)
  where
    jwtSecret = binarySecret <$> secret
    value2BS val = case val of
      String s -> Right $ encodeUtf8 s
      _ -> Left "claim is not string value"
    claimAsJSON :: Text -> Claims -> Either Text Value
    claimAsJSON name cl = case M.lookup name cl of
      Just el -> Right el
      Nothing -> Left (name <> " not in claims")
