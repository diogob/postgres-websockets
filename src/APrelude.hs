module APrelude
  ( Text,
    ByteString,
    LByteString,
    Generic,
    fromMaybe,
    putErrLn,
    fromRight,
    isJust,
    decodeUtf8,
    encodeUtf8,
    MVar,
    readMVar,
    swapMVar,
    newMVar,
    STM,
    atomically,
    ThreadId,
    forkFinally,
    forkIO,
    killThread,
    threadDelay,
    (>=>),
    when,
    forever,
    void,
    panic,
    SomeException,
    throwError,
    liftIO,
    runExceptT,
    unpack,
    pack,
    showText,
    showBS,
    LBS.fromStrict,
    stdin,
    stdout,
    stderr,
    hPutStrLn,
    Word16,
    forM,
    forM_,
    takeMVar,
    newEmptyMVar,
    wait,
    headDef,
    tailSafe,
    withAsync,
    putMVar,
    die,
    myThreadId,
    replicateM,
    bracket,
  )
where

import Control.Concurrent (ThreadId, forkFinally, forkIO, killThread, myThreadId, threadDelay)
import Control.Concurrent.Async (wait, withAsync)
import Control.Concurrent.MVar (MVar, newEmptyMVar, newMVar, putMVar, readMVar, swapMVar, takeMVar)
import Control.Concurrent.STM (STM, atomically)
import Control.Exception (Exception, SomeException, bracket, throw)
import Control.Monad (forM, forM_, forever, replicateM, void, when, (>=>))
import Control.Monad.Error.Class (throwError)
import Control.Monad.Except (runExceptT)
import Control.Monad.IO.Class (liftIO)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Either (fromRight)
import Data.Maybe (fromMaybe, isJust, listToMaybe)
import Data.Text (Text, pack, unpack)
import qualified Data.Text as T
import Data.Text.Encoding
import Data.Word (Word16)
import GHC.Generics (Generic)
import System.Exit (die)
import System.IO (hPutStrLn, stderr, stdin, stdout)

showBS :: (Show a) => a -> BS.ByteString
showBS = BS.pack . show

showText :: (Show a) => a -> Text
showText = T.pack . show

type LByteString = LBS.ByteString

-- | Uncatchable exceptions thrown and never caught.
newtype FatalError = FatalError {fatalErrorMessage :: Text}
  deriving (Show)

instance Exception FatalError

panic :: Text -> a
panic a = throw (FatalError a)

putErrLn :: Text -> IO ()
putErrLn = hPutStrLn stderr . unpack

headDef :: a -> [a] -> a
headDef def = fromMaybe def . listToMaybe

tailSafe :: [a] -> [a]
tailSafe = drop 1
