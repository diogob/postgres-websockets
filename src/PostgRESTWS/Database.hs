module PostgRESTWS.Database
  ( notify
  ) where

import Protolude
import Hasql.Pool (Pool, UsageError, use)
import Hasql.Session (sql)
import Data.Either.Combinators

import PostgRESTWS.Types

notify :: Pool -> ByteString -> ByteString -> IO (Either Error ())

notify pool channel mesg =
   mapError <$> use pool (sql ("NOTIFY " <> channel <> ", '" <> mesg <> "'"))
   where
     mapError :: Either UsageError () -> Either Error ()
     mapError = mapLeft (NotifyError . show)
