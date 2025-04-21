{-# LANGUAGE DeriveGeneric #-}

-- |
-- Module      : PostgresWebsockets.Broadcast
-- Description : Distribute messages from one producer to several consumers.
--
-- PostgresWebsockets functions to broadcast messages to several listening clients
-- This module provides a type called Multiplexer.
-- The multiplexer contains a map of channels and a producer thread.
--
-- This module avoids any database implementation details, it is used by HasqlBroadcast where
-- the database logic is combined.
module PostgresWebsockets.Broadcast
  ( Multiplexer,
    Message (..),
    newMultiplexer,
    onMessage,
    relayMessages,
    relayMessagesForever,
    superviseMultiplexer,

    -- * Re-exports
    readTQueue,
    writeTQueue,
    readTChan,
  )
where

import APrelude
import Control.Concurrent.STM.TChan
import Control.Concurrent.STM.TQueue
import qualified Data.Aeson as A
import qualified StmContainers.Map as M

data Message = Message
  { channel :: Text,
    payload :: Text
  }
  deriving (Eq, Show)

data Multiplexer = Multiplexer
  { channels :: M.Map Text Channel,
    messages :: TQueue Message,
    producerThreadId :: MVar ThreadId,
    reopenProducer :: IO ThreadId
  }

data MultiplexerSnapshot = MultiplexerSnapshot
  { channelsSize :: Int,
    messageQueueEmpty :: Bool,
    producerId :: Text
  }
  deriving (Generic)

data Channel = Channel
  { broadcast :: TChan Message,
    listeners :: Integer
  }

instance A.ToJSON MultiplexerSnapshot

-- | Given a multiplexer derive a type that can be printed for debugging or logging purposes
takeSnapshot :: Multiplexer -> IO MultiplexerSnapshot
takeSnapshot multi =
  MultiplexerSnapshot <$> size <*> e <*> (pack <$> thread)
  where
    size = atomically $ M.size $ channels multi
    thread = show <$> readMVar (producerThreadId multi)
    e = atomically $ isEmptyTQueue $ messages multi

-- | Opens a thread that relays messages from the producer thread to the channels forever
relayMessagesForever :: Multiplexer -> IO ThreadId
relayMessagesForever = forkIO . forever . relayMessages

-- | Reads the messages from the producer and relays them to the active listeners in their respective channels.
relayMessages :: Multiplexer -> IO ()
relayMessages multi =
  atomically $ do
    m <- readTQueue (messages multi)
    mChannel <- M.lookup (channel m) (channels multi)
    case mChannel of
      Nothing -> return ()
      Just c -> writeTChan (broadcast c) m

newMultiplexer ::
  (TQueue Message -> IO a) ->
  (Either SomeException a -> IO ()) ->
  IO Multiplexer
newMultiplexer openProducer closeProducer = do
  msgs <- newTQueueIO
  let forkNewProducer = forkFinally (openProducer msgs) closeProducer
  tid <- forkNewProducer
  multiplexerMap <- M.newIO
  producerThreadId <- newMVar tid
  pure $ Multiplexer multiplexerMap msgs producerThreadId forkNewProducer

-- |  Given a multiplexer, a number of milliseconds and an IO computation that returns a boolean
--      Runs the IO computation at every interval of milliseconds interval and reopens the multiplexer producer
--      if the resulting boolean is true
--      When interval is 0 this is NOOP, so the minimum interval is 1ms
--      Call this in case you want to ensure the producer thread is killed and restarted under a certain condition
superviseMultiplexer :: Multiplexer -> Int -> IO Bool -> IO ()
superviseMultiplexer multi msInterval shouldRestart = do
  void $
    forkIO $
      forever $ do
        threadDelay $ msInterval * 1000
        sr <- shouldRestart
        when sr $ do
          snapBefore <- takeSnapshot multi
          void $ killThread <$> readMVar (producerThreadId multi)
          new <- reopenProducer multi
          void $ swapMVar (producerThreadId multi) new
          snapAfter <- takeSnapshot multi
          print $
            "Restarting producer. Multiplexer updated: "
              <> A.encode snapBefore
              <> " -> "
              <> A.encode snapAfter

openChannel :: Multiplexer -> Text -> STM Channel
openChannel multi chan = do
  c <- newBroadcastTChan
  let newChannel =
        Channel
          { broadcast = c,
            listeners = 0
          }
  M.insert newChannel chan (channels multi)
  return newChannel

-- |  Adds a listener to a certain multiplexer's channel.
--      The listener must be a function that takes a 'TChan Message' and perform any IO action.
--      All listeners run in their own thread.
--      The first listener will open the channel, when a listener dies it will check if there acquire
--      any others and close the channel when that's the case.
onMessage :: Multiplexer -> Text -> (Message -> IO ()) -> IO ()
onMessage multi chan action = do
  listener <- atomically $ openChannelWhenNotFound >>= addListener
  void $ forkFinally (forever (atomically (readTChan listener) >>= action)) disposeListener
  where
    disposeListener _ = atomically $ do
      mC <- M.lookup chan (channels multi)
      let c = fromMaybe (panic $ "trying to remove listener from non existing channel: " <> chan) mC
      M.delete chan (channels multi)
      when (listeners c - 1 > 0) $
        M.insert Channel {broadcast = broadcast c, listeners = listeners c - 1} chan (channels multi)
    openChannelWhenNotFound =
      M.lookup chan (channels multi) >>= \case
        Nothing -> openChannel multi chan
        Just ch -> return ch
    addListener ch = do
      M.delete chan (channels multi)
      let newChannel = Channel {broadcast = broadcast ch, listeners = listeners ch + 1}
      M.insert newChannel chan (channels multi)
      dupTChan $ broadcast newChannel
