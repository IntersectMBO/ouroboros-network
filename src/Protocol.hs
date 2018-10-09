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
data MsgConsumer
  = MsgRequestNext
  -- ^ Request next block from the producer
  | MsgSetHead [Point]
  -- ^
  -- Send set of points, it is up to the producer to find the intersection
  -- point on its chain and send it back to the consumer.
    deriving (Show)

-- | This is the type of messages that the producer sends.
data MsgProducer block
  = MsgRollForward  block
  -- ^ Ask the consumer to roll forward to a given block
  | MsgRollBackward Point
  -- ^
  -- Ask the consumer to roll back to a given Point on its chain
  | MsgAwaitReply
  -- ^
  -- Inform the consumer to await for next instructions; This means that the
  -- producer is synced with the consumer end and its awaiting for its chain to
  -- be changed.
  | MsgIntersectImproved Point
  -- ^
  -- Sends to consumer found intersection, but only if this is an improvement
  -- over previously established intersection point.  The consumer
  -- will decide weather to send more points.  They should all be newer than the
  -- received intersection.
  | MsgIntersectUnchanged
  -- ^
  -- After receiving intersection points from the consumer it maybe happen that
  -- none of the  points is on the producer chain; in this case
  -- @'MsgIntersectUnchanged'@ is send back.
    deriving (Show)

-- |
-- A simple version of a consumer which sends set of points, accepts any respond
-- and steps into the second phase of the protocol in which it sends @'MsgRequestNext'@ and expects one of:
--   - @'MsgAwaitReplay'@
--   - @'MsgRollForward'@
--   - @'MsgRollBackward'@
-- @'ConsumerHandlers'@ is a record which contains all the callbacks needed to
-- run the consumer side of the protocol.
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
loggingSend :: (Show msg, MonadSay m, Show id) => id -> (msg -> m a) -> msg -> m a
loggingSend ident send msg = do
    say $ (show ident) ++ ":send: " ++ show msg
    send msg

-- | A wrapper for recv that logs the messages
--
loggingRecv :: (Show msg, MonadSay m, Show id) => id -> m msg -> m msg
loggingRecv ident recv = do
    msg <- recv
    say $ (show ident) ++ ":recv: " ++ show msg
    return msg

