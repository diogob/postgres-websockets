{-|
Module      : PostgRESTWS.Broadcast
Description : PostgRESTWS functions to broadcast messages to several listening clients

This module provides a type called Multiplexer.
The multiplexer contains a map of channels and a producer thread.

This module avoids any database implementation details, it is used by HasqlBroadcast where
the database logic is combined.
-}
module PostgRESTWS.Broadcast ( Multiplexer (src)
                             , Message (..)
                             , SourceCommands (..)
                             , newMultiplexer
                             , onMessage
                             , relayMessages
                             , relayMessagesForever
                             , openChannelProducer
                             , closeChannelProducer
                             -- * Re-exports
                             , readTQueue
                             , writeTQueue
                             , readTChan
                             ) where

import Protolude
import qualified STMContainers.Map as M
import Control.Concurrent.STM.TChan
import Control.Concurrent.STM.TQueue

data SourceCommands = Open ByteString | Close ByteString deriving (Show)
data Message = Message { channel :: ByteString
               , payload :: ByteString
               } deriving (Eq, Show)

data Multiplexer = Multiplexer { channels :: M.Map ByteString Channel
                               , src :: ThreadId
                               , commands :: TQueue SourceCommands
                               , messages :: TQueue Message
                               }

data Channel = Channel { broadcast :: TChan Message
                       , listeners :: Integer
                       , close :: STM ()
                       }

-- | Open a multiplexer's channel
openChannelProducer :: Multiplexer -> ByteString -> STM ()
openChannelProducer multi ch = writeTQueue (commands multi) (Open ch)

-- | Close a multiplexer's channel
closeChannelProducer ::  Multiplexer -> ByteString -> STM ()
closeChannelProducer multi chan = writeTQueue (commands multi) (Close chan)

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

newMultiplexer :: (TQueue SourceCommands -> TQueue Message -> IO a)
               -> (Either SomeException a -> IO ())
               -> IO Multiplexer
newMultiplexer openProducer closeProducer = do
  cmds <- newTQueueIO
  msgs <- newTQueueIO
  m <- liftA2 Multiplexer M.newIO (forkFinally (openProducer cmds msgs) closeProducer)
  return $ m cmds msgs

openChannel ::  Multiplexer -> ByteString -> STM Channel
openChannel multi chan = do
    c <- newBroadcastTChan
    let newChannel = Channel{ broadcast = c
                            , listeners = 0
                            , close = closeChannelProducer multi chan
                            }
    M.insert newChannel chan (channels multi)
    openChannelProducer multi chan
    return newChannel

{- |  Adds a listener to a certain multiplexer's channel.
      The listener must be a function that takes a 'TChan Message' and perform any IO action.
      All listeners run in their own thread.
      The first listener will open the channel, when a listener dies it will check if there acquire
      any others and close the channel when that's the case.
-}
onMessage :: Multiplexer -> ByteString -> (TChan Message -> IO()) -> IO ()
onMessage multi chan action = do
  listener <- atomically $ openChannelWhenNotFound >>= addListener
  void $ forkFinally (action listener) disposeListener
  where
    disposeListener _ = atomically $ do
      mC <- M.lookup chan (channels multi)
      let c = fromMaybe (panic $ "trying to remove listener from non existing channel: " <> toS chan) mC
      M.delete chan (channels multi)
      if listeners c - 1 > 0
        then M.insert Channel{ broadcast = broadcast c, listeners = listeners c - 1, close = close c} chan (channels multi)
        else closeChannelProducer multi chan
    openChannelWhenNotFound =
      M.lookup chan (channels multi) >>= \mC ->
      case mC of
            Nothing -> openChannel multi chan
            Just ch -> return ch
    addListener ch = do
      M.delete chan (channels multi)
      let newChannel = Channel{ broadcast = broadcast ch, listeners = listeners ch + 1, close = close ch}
      M.insert newChannel chan (channels multi)
      dupTChan $ broadcast newChannel
