{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}

module Protocol
  ( MsgConsumer(..)
  , MsgProducer(..)
  , ConsumerHandlers
  , ProducerHandlers
  , consumerSideProtocol1
  , producerSideProtocol1
  , loggingSend
  , loggingRecv
  )where

import           Control.Monad

import           Block (HasHeader (..))
import           Chain (ChainUpdate (..), Point (..))
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
  :: forall block m.
     (Show block, Monad m)
  => ConsumerHandlers block m
  -> (MsgConsumer -> m ())   -- ^ send
  -> (m (MsgProducer block)) -- ^ recv
  -> m ()
consumerSideProtocol1 ConsumerHandlers{..} send recv = do
    -- The consumer opens by sending a list of points on their chain.
    -- This typically includes the head block and recent points
    points <- getChainPoints
    unless (null points) $ do
      send (MsgSetHead points)
      _msg <- recv
      return ()
    requestNext
  where
    requestNext :: m ()
    requestNext = do
      send MsgRequestNext
      reply <- recv
      handleChainUpdate reply
      requestNext

    handleChainUpdate :: MsgProducer block -> m ()
    handleChainUpdate MsgAwaitReply       = return ()
    handleChainUpdate (MsgRollForward  b) = addBlock b
    handleChainUpdate (MsgRollBackward p) = rollbackTo p
    handleChainUpdate msg = fail $ "protocol error " ++ show msg


-- |
--
producerSideProtocol1
  :: forall block m r.
     (HasHeader block, Monad m)
  => ProducerHandlers block m r
  -> (MsgProducer block -> m ()) -- ^ send
  -> (m MsgConsumer)             -- ^ recv
  -> m ()
producerSideProtocol1 ProducerHandlers{..} send recv =
    newReader >>= awaitOngoing
  where
    awaitOngoing r = forever $ do
      msg <- recv
      case msg of
        MsgRequestNext    -> handleNext r
        MsgSetHead points -> handleSetHead r points

    handleNext r = do
      mupdate <- tryReadChainUpdate r
      update  <- case mupdate of
        Just update -> return update

        -- Reader is at the head, have to wait for producer state changes.
        Nothing -> do
          send MsgAwaitReply
          readChainUpdate r
      send (updateMsg update)

    handleSetHead r points = do
      -- TODO: guard number of points, points sorted
      -- Find the first point that is on our chain
      changed <- improveReadPoint r points
      case changed of
        Just pt -> send (MsgIntersectImproved pt)
        Nothing -> send MsgIntersectUnchanged

    updateMsg (AddBlock b) = MsgRollForward b
    updateMsg (RollBack p) = MsgRollBackward p


-- | A wrapper for send that logs the messages
--
loggingSend :: (Show msg, MonadSay m) => String -> (msg -> m a) -> msg -> m a
loggingSend ident send msg = do
    say $ ident ++ ":send: " ++ show msg
    send msg

-- | A wrapper for recv that logs the messages
--
loggingRecv :: (Show msg, MonadSay m) => String -> m msg -> m msg
loggingRecv ident recv = do
    msg <- recv
    say $ ident ++ ":recv: " ++ show msg
    return msg

