module PostgRESTWS.Broadcast ( Multiplexer (src)
                             , Message (..)
                             , SourceCommands (..)
                             , newMultiplexer
                             , onMessage
                             , relayMessages
                             , relayMessagesForever
                             , openChannelProducer
                             , closeChannelProducer
                             -- reexports
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

openChannelProducer :: Multiplexer -> ByteString -> STM ()
openChannelProducer multi ch = writeTQueue (commands multi) (Open ch)

closeChannelProducer ::  Multiplexer -> ByteString -> STM ()
closeChannelProducer multi chan = writeTQueue (commands multi) (Close chan)

relayMessagesForever :: Multiplexer -> IO ThreadId
relayMessagesForever =  forkIO . forever . relayMessages

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

onMessage :: Multiplexer -> ByteString -> (TChan Message -> IO()) -> IO ()
onMessage multi chan action = do
  listener <- atomically $ do
    mC <- M.lookup chan (channels multi)
    c <- case mC of
              Nothing -> openChannel multi chan
              Just ch -> return ch
    M.delete chan (channels multi)
    let newChannel = Channel{ broadcast = broadcast c, listeners = listeners c + 1, close = close c}
    M.insert newChannel chan (channels multi)
    dupTChan $ broadcast newChannel
  void $ forkFinally (action listener) (\_ -> atomically $ do
    mC <- M.lookup chan (channels multi)
    let c = fromMaybe (panic $ "trying to remove listener from non existing channel: " <> toS chan) mC
    M.delete chan (channels multi)
    when (listeners c - 1 == 0) $ closeChannelProducer multi chan
    when (listeners c - 1 > 0) $ do
      let newChannel = Channel{ broadcast = broadcast c, listeners = listeners c - 1, close = close c}
      M.insert newChannel chan (channels multi)
    )
