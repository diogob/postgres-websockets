{-| PostgresWebsockets functions to broadcast messages to several listening clients
    This module provides a type called Multiplexer.
    The multiplexer contains a map of channels and a producer thread.

    This module avoids any database implementation details, it is used by HasqlBroadcast where
    the database logic is combined.
-}
module PostgresWebsockets.Broadcast ( Multiplexer (src)
                             , Message (..)
                             , newMultiplexer
                             , onMessage
                             , relayMessages
                             , relayMessagesForever
                             -- * Re-exports
                             , readTQueue
                             , writeTQueue
                             , readTChan
                             ) where

import Protolude
import qualified StmContainers.Map as M
import Control.Concurrent.STM.TChan
import Control.Concurrent.STM.TQueue

import GHC.Show

data Message = Message { channel :: ByteString
                       , payload :: ByteString
                       } deriving (Eq, Show)

data Multiplexer = Multiplexer { channels :: M.Map ByteString Channel
                               , src :: ThreadId
                               , messages :: TQueue Message
                               }

instance Show Multiplexer where
  show Multiplexer{} = "Multiplexer"

data Channel = Channel { broadcast :: TChan Message
                       , listeners :: Integer
                       }

-- | Opens a thread that relays messages from the producer thread to the channels forever
relayMessagesForever :: Multiplexer -> IO ThreadId
relayMessagesForever =  forkIO . forever . relayMessages

-- | Reads the messages from the producer and relays them to the active listeners in their respective channels.
relayMessages :: Multiplexer -> IO ()
relayMessages multi =
  atomically $ do
    m <- readTQueue (messages multi)
    mChannel <- M.lookup (channel m) (channels multi)
    case mChannel of
      Nothing -> return ()
      Just c -> writeTChan (broadcast c) m

newMultiplexer :: (TQueue Message -> IO a)
               -> (Either SomeException a -> IO ())
               -> IO Multiplexer
newMultiplexer openProducer closeProducer = do
  msgs <- newTQueueIO
  m <- liftA2 Multiplexer M.newIO (forkFinally (openProducer msgs) closeProducer)
  return $ m msgs

openChannel ::  Multiplexer -> ByteString -> STM Channel
openChannel multi chan = do
    c <- newBroadcastTChan
    let newChannel = Channel{ broadcast = c
                            , listeners = 0
                            }
    M.insert newChannel chan (channels multi)
    return newChannel

{- |  Adds a listener to a certain multiplexer's channel.
      The listener must be a function that takes a 'TChan Message' and perform any IO action.
      All listeners run in their own thread.
      The first listener will open the channel, when a listener dies it will check if there acquire
      any others and close the channel when that's the case.
-}
onMessage :: Multiplexer -> ByteString -> (Message -> IO()) -> IO ()
onMessage multi chan action = do
  listener <- atomically $ openChannelWhenNotFound >>= addListener
  void $ forkFinally (forever (atomically (readTChan listener) >>= action)) disposeListener
  where
    disposeListener _ = atomically $ do
      mC <- M.lookup chan (channels multi)
      let c = fromMaybe (panic $ "trying to remove listener from non existing channel: " <> toS chan) mC
      M.delete chan (channels multi)
      when (listeners c - 1 > 0) $
        M.insert Channel{ broadcast = broadcast c, listeners = listeners c - 1 } chan (channels multi)
    openChannelWhenNotFound =
      M.lookup chan (channels multi) >>= \case
                                            Nothing -> openChannel multi chan
                                            Just ch -> return ch
    addListener ch = do
      M.delete chan (channels multi)
      let newChannel = Channel{ broadcast = broadcast ch, listeners = listeners ch + 1}
      M.insert newChannel chan (channels multi)
      dupTChan $ broadcast newChannel
