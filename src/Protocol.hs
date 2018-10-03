{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}

module Protocol
  ( MsgConsumer(..)
  , MsgProducer(..)
  , ConsumerHandlers
  , ProducerHandlers
  , consumerSideProtocol1
  , producerSideProtocol1
  )where

import           Control.Monad

import           Block (HasHeader (..))
import           Chain (ChainUpdate (..), Point (..))
import qualified Chain
import           MonadClass
import           ProtocolInterfaces (ConsumerHandlers(..), ProducerHandlers(..))

{-# ANN module "HLint: ignore Use readTVarIO" #-}

--
-- IPC based protocol
--

-- | In this protocol the consumer always initiates things and the producer
-- replies. This is the type of messages that the consumer sends.
data MsgConsumer = MsgRequestNext
                 | MsgSetHead [Point]
    deriving (Show)

-- | This is the type of messages that the producer sends.
data MsgProducer block
  = MsgRollForward  block
  | MsgRollBackward Point
  | MsgAwaitReply
  | MsgIntersectImproved Point
  | MsgIntersectUnchanged
    deriving (Show)

consumerSideProtocol1
  :: forall block cid m.
     ( Show block
     , MonadSay m
     , Show cid
     )
  => ConsumerHandlers block m
  -> cid                     -- ^ consumer id
  -> (MsgConsumer -> m ())   -- ^ send
  -> (m (MsgProducer block)) -- ^ recv
  -> m ()
consumerSideProtocol1 ConsumerHandlers{..} cid send recv = do
    -- The consumer opens by sending a list of points on their chain.
    -- This typically includes the head block and recent points
    points <- getChainPoints
    unless (null points) $ do
      send (MsgSetHead points)
      _msg <- recv
      return ()
    requestNext
  where
    consumerId :: String
    consumerId = "consumer-" ++ show cid

    requestNext :: m ()
    requestNext = do
      send MsgRequestNext
      reply <- recv
      handleChainUpdate reply
      requestNext

    handleChainUpdate :: MsgProducer block -> m ()
    handleChainUpdate msg@MsgAwaitReply = do
      say (consumerId ++ ":handleChainUpdate: " ++ show msg)

    handleChainUpdate msg@(MsgRollForward  b) = do
      say (consumerId ++ ":handleChainUpdate: " ++ show msg)
      addBlock b

    handleChainUpdate msg@(MsgRollBackward p) = do
      say (consumerId ++ ":handleChainUpdate: " ++ show msg)
      rollbackTo p

    handleChainUpdate msg = do
        say (consumerId ++ ":handleChainUpdate: " ++ show msg)


-- |
--
producerSideProtocol1
  :: forall block pid m r.
     ( Show pid
     , HasHeader block
     , Show block
     , MonadSay m
     )
  => ProducerHandlers block m r
  -> pid                         -- ^ producer id
  -> (MsgProducer block -> m ()) -- ^ send
  -> (m MsgConsumer)             -- ^ recv
  -> m ()
producerSideProtocol1 ProducerHandlers{..} pid send recv =
    establishReaderState Chain.genesisPoint >>= awaitOngoing
  where
    producerId :: String
    producerId = show pid

    awaitOngoing r = forever $ do
      msg <- recv
      say $ producerId ++ ":awaitOngoing:recvMsg: " ++ show msg
      case msg of
        MsgRequestNext    -> handleNext r
        MsgSetHead points -> handleSetHead r points

    handleNext r = do
      mupdate <- tryReadChainUpdate r
      update  <- case mupdate of
        Just update -> return update

        -- Reader is at the head, have to wait for producer state changes.
        Nothing -> do
          let msg :: MsgProducer block
              msg = MsgAwaitReply
          say $ producerId ++ ":handleNext:sendMsg: " ++ show msg
          send msg
          readChainUpdate r
      let msg = updateMsg update
      say $ producerId ++ ":handleNext:sendMsg: " ++ show msg
      send msg

    handleSetHead r points = do
      -- TODO: guard number of points, points sorted
      -- Find the first point that is on our chain
      intersection <- findIntersectionRange points
      case intersection of
        Just pt -> do
          updateReaderState r pt
          let msg :: MsgProducer block
              msg = MsgIntersectImproved pt
          say $ producerId ++ ":handleSetHead:sendMsg: " ++ show msg
          send msg
        Nothing -> do
          let msg :: MsgProducer block
              msg = MsgIntersectUnchanged
          say $ producerId ++ ":handleSetHead:sendMsg: " ++ show msg
          send msg

    updateMsg (AddBlock b) = MsgRollForward b
    updateMsg (RollBack p) = MsgRollBackward p

