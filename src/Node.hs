{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Node where

import           Control.Exception (assert)
import           Control.Monad
import           Data.Functor (($>))
import           Data.List hiding (inits)
import           Data.Maybe (catMaybes)
import           Data.Semigroup (Semigroup (..))
import           Data.Tuple (swap)

import           Block
import           Block.Concrete hiding (fixupBlock)
import qualified Block.Concrete as Concrete
import           Chain (Chain (..), Point)
import qualified Chain
import           ChainProducerState (ChainProducerState (..), ReaderId,
                     initChainProducerState, producerChain, switchFork)
import           ConsumersAndProducers
import           MonadClass hiding (recvMsg, sendMsg)
import           Ouroboros
import           Protocol


-- |
-- State-full chain selection (@'ChainProducerState'@).
longestChainSelection :: forall block m stm.
                         ( HasHeader block
                         , MonadSTM m stm
                         )
                      => [TVar m (Maybe (Chain block))]
                      -> TVar m (ChainProducerState block)
                      -> m ()
longestChainSelection candidateChainVars cpsVar =
    forever (atomically updateCurrentChain)
  where
    updateCurrentChain :: stm ()
    updateCurrentChain = do
      candidateChains <- mapM readTVar candidateChainVars
      cps@ChainProducerState{chainState = chain} <- readTVar cpsVar
      let -- using foldl' since @Chain.selectChain@ is left biased
          chain' = foldl' Chain.selectChain chain (catMaybes candidateChains)
      if Chain.headPoint chain' == Chain.headPoint chain
        then retry
        else writeTVar cpsVar (switchFork chain' cps)


chainValidation :: forall block m stm. (HasHeader block, MonadSTM m stm)
                => TVar m (Chain block)
                -> TVar m (Maybe (Chain block))
                -> m ()
chainValidation peerChainVar candidateChainVar = do
    st <- atomically (newTVar Chain.genesisPoint)
    forever (atomically (update st))
  where
    update :: TVar m (Point block) -> stm ()
    update stateVar = do
      peerChain      <- readTVar peerChainVar
      candidatePoint <- readTVar stateVar
      check (Chain.headPoint peerChain /= candidatePoint)
      writeTVar stateVar (Chain.headPoint peerChain)
      let candidateChain | Chain.valid peerChain = Just peerChain
                         | otherwise             = Nothing
      writeTVar candidateChainVar candidateChain


chainTransferProtocol :: forall block m stm.
                         ( HasHeader block
                         , MonadSTM m stm
                         , MonadTimer m
                         )
                      => Duration (Time m)
                      -> TVar m (Chain block)
                      -> TVar m (Chain block)
                      -> m ()
chainTransferProtocol delay inputVar outputVar = do
    st <- atomically (newTVar Chain.genesisPoint)
    forever (update st)
  where
    update :: TVar m (Point block) -> m ()
    update stateVar = do
      input <- atomically $ do
                input    <- readTVar inputVar
                curPoint <- readTVar stateVar
                check (Chain.headPoint input /= curPoint)
                writeTVar stateVar (Chain.headPoint input)
                return input
      timer delay $ atomically $ writeTVar outputVar input


-- | One way transport channel.
data Chan m send recv = Chan { sendMsg :: send -> m ()
                             , recvMsg :: m recv
                             }


-- | Simulated transfer protocol.
--
transferProtocol :: forall send recv m stm.
                    ( MonadSTM m stm
                    , MonadTimer m
                    )
                 => Duration (Time m)
                 -> TVar m [send]
                 -> TVar m [recv]
                 -> Chan m send recv
transferProtocol delay sendVar recvVar
  = Chan sendMsg recvMsg
  where
    sendMsg a = timer delay $ atomically $ modifyTVar' sendVar (++[a])
    recvMsg =
      atomically $ do
        xs <- readTVar recvVar
        case xs of
          x : xs' -> writeTVar recvVar xs' $> x
          []      -> retry


-- | Create a symmetric pair of channels in opposite directions.
--
createCoupledChannels :: forall send recv m stm.
                         ( MonadSTM m stm
                         , MonadTimer m
                         )
                      => Duration (Time m)
                      -> Duration (Time m)
                      -> m (Chan m send recv, Chan m recv send)
createCoupledChannels delay1 delay2 = do
  sendVar <- atomically $ newTVar []
  recvVar <- atomically $ newTVar []
  let chan1 = transferProtocol delay1 sendVar recvVar
      chan2 = transferProtocol delay2 recvVar sendVar
  return (chan1, chan2)


-- | Simulated network channels for a given network node.
--
data NodeChannels m prodMsg consMsg = NodeChannels
  { consumerChans :: [Chan m consMsg prodMsg]
    -- ^ channels on which the node will play the consumer role:
    -- sending @consMsg@ and receiving @prodMsg@ messages.
  , producerChans :: [Chan m prodMsg consMsg]
    -- ^ channels on which the node will play the producer role:
    -- sending @prodMsg@ and receiving @consMsg@ messages.
  }

instance Semigroup (NodeChannels m prodMsg consMsg) where
  NodeChannels c1 p1 <> NodeChannels c2 p2 = NodeChannels (c1 ++ c2) (p1 ++ p2)

instance Monoid (NodeChannels m prodMsg consMsg) where
  mempty = NodeChannels [] []

  mappend = (<>)


-- | Create channels n1 → n2, where n1 is a producer and n2 is the consumer.
--
createOneWaySubscriptionChannels
  :: forall send recv m stm.
     ( MonadSTM m stm
     , MonadTimer m
     )
  => Duration (Time m)
  -> Duration (Time m)
  -> m (NodeChannels m send recv, NodeChannels m send recv)
createOneWaySubscriptionChannels trDelay1 trDelay2 = do
  (cr, rc) <- createCoupledChannels trDelay1 trDelay2
  return
    ( NodeChannels
        { consumerChans = []
        , producerChans = [cr]
        }
    , NodeChannels
        { consumerChans = [rc]
        , producerChans = []
        }
    )

-- | Create channels for n1 ↔ n2 where both nodes are a consumer and a producer
-- simultaneously.
--
createTwoWaySubscriptionChannels
  :: forall send recv m stm.
     ( MonadSTM m stm
     , MonadTimer m
     )
  => Duration (Time m)
  -> Duration (Time m)
  -> m (NodeChannels m send recv, NodeChannels m send recv)
createTwoWaySubscriptionChannels trDelay1 trDelay2 = do
  r12 <- createOneWaySubscriptionChannels trDelay1 trDelay2
  r21 <- createOneWaySubscriptionChannels trDelay2 trDelay1
  return $ r12 <> swap r21


-- | Generate a block from a given chain.  Each @block@ is produced at
-- @slotDuration * blockSlot block@ time.
--
-- TODO: invariant: generates blocks for current slots.
blockGenerator :: forall block m stm.
                  ( HasHeader block
                  , MonadSTM m stm
                  , MonadTimer m
                  )
               => Duration (Time m)
               -- ^ slot duration
               -> [block]
               -- ^ The list of blocks to generate. This allows for upstream
               -- users to generate \"half chains\" in case we want to simulate
               -- nodes having access to already part of the overall chain.
               -> m (TVar m (Maybe block))
               -- ^ returns an stm transaction which returns block
blockGenerator slotDuration chain = do
  outputVar <- atomically (newTVar Nothing)
  sequence_ [ timer (slotDuration * fromIntegral (getSlot $ blockSlot b)) (atomically (writeTVar outputVar (Just b)))
            | b <- chain]
  return outputVar

-- | Read one block from the @'blockGenerator'@.
--
getBlock :: forall block m stm.
            MonadSTM m stm
         => TVar m (Maybe block)
         -> stm block
getBlock v = do
  mb <- readTVar v
  case mb of
    Nothing -> retry
    Just b  -> writeTVar v Nothing $> b


-- | Observe @TVar ('ChainProducerState' block)@, and whenever the @TVar@
-- mutates, write the result to the supplied @'Probe'@.
--
observeChainProducerState
  :: forall m stm block.
     ( HasHeader block
     , MonadSTM m stm
     , MonadProbe m
     )
  => NodeId
  -> Probe m (NodeId, Chain block)
  -> TVar m (ChainProducerState block)
  -> m ()
observeChainProducerState nid p cpsVar = do
    st <- atomically (newTVar Chain.genesisPoint)
    forever (update st)
  where
    update :: TVar m (Point block) -> m ()
    update stateVar = do
      chain <- atomically $ do
                chain  <- producerChain <$> readTVar cpsVar
                curPoint <- readTVar stateVar
                check (Chain.headPoint chain /= curPoint)
                writeTVar stateVar (Chain.headPoint chain)
                return chain
      probeOutput p (nid, chain)

data ConsumerId = ConsumerId NodeId Int
  deriving (Eq, Ord, Show)

data ProducerId = ProducerId NodeId Int
  deriving (Eq, Ord, Show)

-- | Relay node, which takes @'NodeChannels'@ to communicate with its peers
-- (upstream and downstream).  If it is subscribed to n nodes and has
-- m subscriptions, it will run n consumer end protocols which listen for
-- updates; verify chains and select the longest one and feed it to the producer
-- side which sends updates to its m subscribers.
--
-- The main thread of the @'relayNode'@ is not blocking; it will return
-- @TVar ('ChainProducerState' block)@.  This allows to extend the relay node to
-- a core node.
forkRelayKernel :: forall block m stm.
                ( HasHeader block
                , MonadSTM m stm
                )
                => [TVar m (Chain block)]
                -- ^ These will track the upstream producers.
                -> TVar m (ChainProducerState block)
                -- ^ This is tracking the current node and the downstream.
                -> m ()
forkRelayKernel upstream cpsVar = do
  -- Mutable state
  -- 2. candidate chains
  candidateChainVars <- replicateM (length upstream) (atomically (newTVar Nothing))
  -- chain validation threads
  zipWithM_
    (\chain cchain -> fork $ chainValidation chain cchain)
    upstream
    candidateChainVars
  -- chain selection thread
  fork $ longestChainSelection candidateChainVars cpsVar

-- | Relay node, which takes @'NodeChannels'@ to communicate with its peers
-- (upstream and downstream).  If it is subscribed to n nodes and has
-- m subscriptions, it will run n consumer end protocols which listen for
-- updates; verify chains and select the longest one and feed it to the producer
-- side which sends updates to its m subscribers.
--
-- The main thread of the @'relayNode'@ is not blocking; it will return
-- @TVar ('ChainProducerState' block)@.  This allows to extend the relay node to
-- a core node.
relayNode :: forall m stm block.
             ( MonadSTM m stm
             , MonadSay m
             , HasHeader block
             , Show block
             )
          => NodeId
          -> Chain block
          -> NodeChannels m (MsgProducer block) (MsgConsumer block)
          -> m (TVar m (ChainProducerState block))
relayNode nid initChain chans = do
  -- Mutable state
  -- 1. input chains
  upstream <- zipWithM startConsumer [0..] (consumerChans chans)
  -- 2. ChainProducerState
  cpsVar <- atomically $ newTVar (initChainProducerState initChain)

  forkRelayKernel upstream cpsVar

  -- producers which share @'ChainProducerState'@
  let producer = exampleProducer cpsVar
  mapM_ (uncurry $ startProducer producer) (zip [0..] (producerChans chans))

  return cpsVar
  where
    startConsumer :: Int
                  -> Chan m (MsgConsumer block) (MsgProducer block)
                  -> m (TVar m (Chain block))
    startConsumer cid chan = do
      chainVar <- atomically $ newTVar Genesis
      let consumer = exampleConsumer chainVar
      fork $ consumerSideProtocol1 consumer
        (loggingSend (ConsumerId nid cid) (sendMsg chan))
        (loggingRecv (ConsumerId nid cid) (recvMsg chan))
      return chainVar

    startProducer :: ProducerHandlers block m ReaderId
                  -> Int
                  -> Chan m (MsgProducer block) (MsgConsumer block)
                  -> m ()
    startProducer producer pid chan = do
      fork $ producerSideProtocol1 producer
        (loggingSend (ProducerId nid pid) (sendMsg chan))
        (loggingRecv (ProducerId nid pid) (recvMsg chan))

-- | Core node simulation.  Given a chain it will generate a @block@ at its
-- slot time (i.e. @slotDuration * blockSlot block@).  When the node finds out
-- that the slot for which it was supposed to generate a block was already
-- occupied, it will replace it with its block.
--
-- TODO: This should not take a list of blocks, but rather a monadic action
-- to generate the blocks. At that point the 'fixup' argument can go also.
-- Alternatively, we should move this to the tests, and remove it from the
-- public network layer altogether.
--
forkCoreKernel :: forall block m stm.
                  ( HasHeader block
                  , MonadSTM m stm
                  , MonadTimer m
                  )
               => Duration (Time m)
               -- ^ slot duration
               -> [block]
               -- ^ Blocks to produce (in order they should be produced)
               -> (Chain block -> block -> block)
               -> TVar m (ChainProducerState block)
               -> m ()
forkCoreKernel slotDuration gchain fixupBlock cpsVar = do
  blockVar <- blockGenerator slotDuration gchain
  fork $ forever $ applyGeneratedBlock blockVar

  where
    applyGeneratedBlock
      :: TVar m (Maybe block)
      -> m ()
    applyGeneratedBlock blockVar = atomically $ do
      block <- getBlock blockVar
      cps@ChainProducerState{chainState = chain} <- readTVar cpsVar
      writeTVar cpsVar (switchFork (addBlock chain block) cps)

    addBlock :: Chain block -> block -> Chain block
    addBlock c b =
      case blockSlot b `compare` Chain.headSlot c of
        -- the block is OK
        GT -> let r = Chain.addBlock (fixupBlock c b) c in
              assert (Chain.valid r) r
        -- the slot @s@ is already taken; replace the previous block
        EQ -> let c' = Chain.drop 1 c
                  b' = fixupBlock c' b
                  r  = Chain.addBlock b' c'
              in assert (Chain.valid r) r
        LT -> error "blockGenerator invariant vaiolation: generated block is for slot in the past"

-- | Core node simulation.  Given a chain it will generate a @block@ at its
-- slot time (i.e. @slotDuration * blockSlot block@).  When the node finds out
-- that the slot for which it was supposed to generate a block was already
-- occupied, it will replace it with its block.
--
coreNode :: forall  m stm.
        ( MonadSTM m stm
        , MonadTimer m
        , MonadSay m
        )
     => NodeId
     -> Duration (Time m)
     -- ^ slot duration
     -> [Block]
     -> NodeChannels m (MsgProducer Block) (MsgConsumer Block)
     -> m (TVar m (ChainProducerState Block))
coreNode nid slotDuration gchain chans = do
  cpsVar <- relayNode nid Genesis chans
  forkCoreKernel slotDuration gchain Concrete.fixupBlock cpsVar
  return cpsVar
