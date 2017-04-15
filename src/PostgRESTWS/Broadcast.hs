module PostgRESTWS.Broadcast ( Multiplexer (src)
                             , Message (..)
                             , SourceCommands (..)
                             , newMultiplexer
                             , onMessage
                             , relayMessages
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

data Multiplexer = Multiplexer { channels :: ChannelMap
                               , src :: ThreadId
                               , commands :: TQueue SourceCommands
                               , messages :: TQueue Message
                               }

type ChannelMap = M.Map ByteString Channel
data Channel = Channel { broadcast :: TChan Message
                       , listeners :: Integer
                       , close :: STM ()
                       }

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
                            , close = closeChannel multi chan
                            }
    M.insert newChannel chan (channels multi)
    writeTQueue (commands multi) (Open chan)
    return newChannel

closeChannel ::  Multiplexer -> ByteString -> STM ()
closeChannel multi chan = do
    M.delete chan (channels multi)
    writeTQueue (commands multi) (Close chan)

onMessage :: Multiplexer -> ByteString -> (TChan Message -> IO()) -> IO ()
onMessage multi chan action = do
  listener <- atomically $ do
    c <- liftA2 fromMaybe (openChannel multi chan) (M.lookup chan (channels multi))
    M.delete chan (channels multi)
    let newChannel = Channel{ broadcast = broadcast c, listeners = listeners c + 1, close = close c}
    M.insert newChannel chan (channels multi)
    dupTChan $ broadcast newChannel
  void $ forkFinally (action listener) (\_ -> atomically $ do
    c <- liftA2 fromMaybe (openChannel multi chan) (M.lookup chan (channels multi))
    M.delete chan (channels multi)
    when (listeners c - 1 == 0) $ closeChannel multi chan
    when (listeners c - 1 > 0) $ do
      let newChannel = Channel{ broadcast = broadcast c, listeners = listeners c - 1, close = close c}
      M.insert newChannel chan (channels multi)
    )
