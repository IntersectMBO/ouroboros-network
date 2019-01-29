{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE PolyKinds           #-}

module Ouroboros.Network.Node where

import           Control.Exception (assert)
import           Control.Monad
import           Data.Functor (($>))
import           Data.Hashable
import           Data.List hiding (inits)
import           Data.Maybe (catMaybes)
import           Data.Semigroup (Semigroup (..))
import           Data.Text (Text)
import           Data.Tuple (swap)
import           GHC.Generics (Generic)

import           Control.Monad.Class.MonadSay
import           Control.Monad.Class.MonadFork
import           Control.Monad.Class.MonadSTM
import           Control.Monad.Class.MonadTimer
import           Control.Monad.Class.MonadProbe

import           Protocol.Codec
import           Protocol.Core
import           Protocol.Channel
import           Protocol.Driver
import           Protocol.Transition (SomeTransition (..), withSomeTransition)

import           Ouroboros.Network.Block
import           Ouroboros.Network.Chain (Chain (..), Point)
import qualified Ouroboros.Network.Chain as Chain
import           Ouroboros.Network.ChainProducerState (ChainProducerState (..),
                                                       initChainProducerState,
                                                       producerChain,
                                                       switchFork)
import           Ouroboros.Network.Protocol.ChainSync.Codec.Id (codecChainSync)
import           Ouroboros.Network.Protocol.ChainSync.Client
import           Ouroboros.Network.Protocol.ChainSync.Server
import           Ouroboros.Network.Protocol.ChainSync.Type
import           Ouroboros.Network.Protocol.ChainSync.Examples
import           Ouroboros.Network.Protocol.Channel.Sim (delayChannel)
-- FIXME bad module name below. They're examples, sure, but they're also
-- legit useful.
import           Ouroboros.Network.Testing.ConcreteBlock hiding (fixupBlock)
import qualified Ouroboros.Network.Testing.ConcreteBlock as Concrete

data NodeId = CoreId Int
            | RelayId Int
  deriving (Eq, Ord, Show, Generic)

instance Hashable NodeId -- let generic instance do the job

-- |
-- State-full chain selection (@'ChainProducerState'@).
longestChainSelection :: forall block m.
                         ( HasHeader block
                         , MonadSTM m
                         )
                      => [TVar m (Maybe (Chain block))]
                      -> TVar m (ChainProducerState block)
                      -> m ()
longestChainSelection candidateChainVars cpsVar =
    forever (atomically updateCurrentChain)
  where
    updateCurrentChain :: Tr m ()
    updateCurrentChain = do
      candidateChains <- mapM readTVar candidateChainVars
      cps@ChainProducerState{chainState = chain} <- readTVar cpsVar
      let -- using foldl' since @Chain.selectChain@ is left biased
          chain' = foldl' Chain.selectChain chain (catMaybes candidateChains)
      if Chain.headPoint chain' == Chain.headPoint chain
        then retry
        else writeTVar cpsVar (switchFork chain' cps)


chainValidation :: forall block m. (HasHeader block, MonadSTM m)
                => TVar m (Chain block)
                -> TVar m (Maybe (Chain block))
                -> m ()
chainValidation peerChainVar candidateChainVar = do
    st <- atomically (newTVar Chain.genesisPoint)
    forever (atomically (update st))
  where
    update :: TVar m (Point block) -> Tr m ()
    update stateVar = do
      peerChain      <- readTVar peerChainVar
      candidatePoint <- readTVar stateVar
      check (Chain.headPoint peerChain /= candidatePoint)
      writeTVar stateVar (Chain.headPoint peerChain)
      let candidateChain | Chain.valid peerChain = Just peerChain
                         | otherwise             = Nothing
      writeTVar candidateChainVar candidateChain


chainTransferProtocol :: forall block m.
                         ( HasHeader block
                         , MonadSTM m
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
      fork $ threadDelay delay >> atomically (writeTVar outputVar input)

-- | Simulated transfer protocol.
--
transferProtocol :: forall send recv m.
                    ( MonadSTM m
                    , MonadTimer m
                    )
                 => Duration (Time m)
                 -> TVar m [send]
                 -> TVar m [recv]
                 -> Duplex m m send recv
transferProtocol delay sendVar recvVar
  = delayChannel delay $ uniformDuplex sendMsg recvMsg
  where
    sendMsg a = atomically (modifyTVar' sendVar (++[a]))
    recvMsg =
      atomically $ do
        xs <- readTVar recvVar
        case xs of
          x : xs' -> writeTVar recvVar xs' $> Just x
          []      -> retry


-- | Create a symmetric pair of channels in opposite directions.
--
createCoupledChannels :: forall send recv m.
                         ( MonadSTM m
                         , MonadTimer m
                         )
                      => Duration (Time m)
                      -> Duration (Time m)
                      -> m (Duplex m m send recv, Duplex m m recv send)
createCoupledChannels delay1 delay2 = do
  sendVar <- atomically $ newTVar []
  recvVar <- atomically $ newTVar []
  let chan1 = transferProtocol delay1 sendVar recvVar
      chan2 = transferProtocol delay2 recvVar sendVar
  return (chan1, chan2)


-- | Simulated network channels for a given network node.
--
data NodeChannels m block = NodeChannels
  { consumerChans :: [Channel m (SomeTransition (ChainSyncMessage block (Point block)))]
    -- ^ channels on which the node will play the consumer role:
    -- sending @consMsg@ and receiving @prodMsg@ messages.
  , producerChans :: [Channel m (SomeTransition (ChainSyncMessage block (Point block)))]
    -- ^ channels on which the node will play the producer role:
    -- sending @prodMsg@ and receiving @consMsg@ messages.
  }

instance Semigroup (NodeChannels m block) where
  NodeChannels c1 p1 <> NodeChannels c2 p2 = NodeChannels (c1 ++ c2) (p1 ++ p2)

instance Monoid (NodeChannels m block) where
  mempty = NodeChannels [] []

-- | Create channels n1 → n2, where n1 is a producer and n2 is the consumer.
--
createOneWaySubscriptionChannels
  :: forall block m.
     ( MonadSTM m
     , MonadTimer m
     )
  => Duration (Time m)
  -> Duration (Time m)
  -> m (NodeChannels m block, NodeChannels m block)
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
  :: forall block m.
     ( MonadSTM m
     , MonadTimer m
     )
  => Duration (Time m)
  -> Duration (Time m)
  -> m (NodeChannels m block, NodeChannels m block)
createTwoWaySubscriptionChannels trDelay1 trDelay2 = do
  r12 <- createOneWaySubscriptionChannels trDelay1 trDelay2
  r21 <- createOneWaySubscriptionChannels trDelay2 trDelay1
  return $ r12 <> swap r21

-- | Generate a block from a given chain.  Each @block@ is produced at
-- @slotDuration * blockSlot block@ time.
blockGenerator :: forall block m.
                  ( HasHeader block
                  , MonadSTM m
                  , MonadTimer m
                  )
               => Duration (Time m)
               -- ^ slot duration
               -> [block]
               -- ^ The list of blocks to generate in increasing slot order.
               -- This allows for upstream users to generate \"half chains\" in
               -- case we want to simulate nodes having access to already part
               -- of the overall chain.
               -> m (Tr m (Maybe block))
               -- ^ returns an stm transaction which returns block.  @Nothing@
               -- signifies that there will be no more blocks.  It is the caller
               -- responsibility to read this transaction untill @Nothing@ is
               -- returned.
blockGenerator slotDuration chain = do
  -- communicate through a @TBQueue@, it is enough to make it very shallow,
  -- since it is supposed to be written and read once a slot time.
  var <- atomically (newTBQueue 1)
  fork $ go var Nothing chain
  return (readTBQueue var)
 where
  go :: TBQueue m (Maybe block) -> Maybe Slot -> [block] -> m ()
  go var _ [] = do
    atomically (writeTBQueue var Nothing)
  go var mslot (b : bs) = do
    let slot  = blockSlot b
        delay = getSlot slot - maybe 0 getSlot mslot
    threadDelay (slotDuration * fromIntegral delay)
    atomically (writeTBQueue var (Just b))
    go var (Just slot) bs

-- | Observe @TVar ('ChainProducerState' block)@, and whenever the @TVar@
-- mutates, write the result to the supplied @'Probe'@.
--
observeChainProducerState
  :: forall m block.
     ( HasHeader block
     , MonadSTM m
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

-- | Channel which logs sent and received messages.
--
-- TODO: use a proper logger rather than @'MonadSay'@ constraint.
loggingChannel :: ( MonadSay sm
                  , MonadSay rm
                  , Show id
                  )
               => id
               -> (send -> String)
               -> (recv -> String)
               -> Duplex sm rm send recv
               -> Duplex sm rm send recv
loggingChannel ident showSend showRecv Duplex{send,recv} = Duplex {
    send = loggingSend,
    recv = loggingRecv
  }
 where
  loggingSend a = do
    say (show ident ++ ":send:" ++ showSend a)
    loggingChannel ident showSend showRecv <$> send a
  loggingRecv = do
    msg <- recv
    case msg of
      (Nothing, _) -> return msg
      (Just a, _)  -> say (show ident ++ ":recv:" ++ showRecv a) $> fmap (loggingChannel ident showSend showRecv) msg

-- | Relay node, which takes @'NodeChannels'@ to communicate with its peers
-- (upstream and downstream).  If it is subscribed to n nodes and has
-- m subscriptions, it will run n consumer end protocols which listen for
-- updates; verify chains and select the longest one and feed it to the producer
-- side which sends updates to its m subscribers.
--
-- The main thread of the @'relayNode'@ is not blocking; it will return
-- @TVar ('ChainProducerState' block)@.  This allows to extend the relay node to
-- a core node.
forkRelayKernel :: forall block m.
                ( HasHeader block
                , MonadSTM m
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
relayNode :: forall m block.
             ( MonadSTM m
             , MonadSay m
             , HasHeader block
             , Show block
             , MonadTimer m
             )
          => NodeId
          -> Chain block
          -> NodeChannels m block
          -> m (TVar m (ChainProducerState block))
relayNode nid initChain chans = do
  -- Mutable state
  -- 1. input chains
  upstream <- zipWithM startConsumer [0..] (consumerChans chans)
  -- 2. ChainProducerState
  cpsVar <- atomically $ newTVar (initChainProducerState initChain)

  forkRelayKernel upstream cpsVar

  -- producers which share @'ChainProducerState'@
  let producer = chainSyncServerPeer (chainSyncServerExample () cpsVar)
  mapM_ (uncurry $ startProducer producer) (zip [0..] (producerChans chans))

  return cpsVar
  where

    codec
      :: Codec m Text
                 (SomeTransition (ChainSyncMessage block (Point block)))
                 (SomeTransition (ChainSyncMessage block (Point block)))
                 (ChainSyncMessage block (Point block))
                 'StIdle
    codec = codecChainSync

    -- Note: there is asymmetry between producers and consumers: we run single
    -- @'ProducerHandlers'@ and multiple @'ConsumerHandlers'@.  An efficient
    -- implementation should run a as many producers as channels and not share
    -- state between producers than necessary (here are producers share chain
    -- state and all the reader states, while we could share just the chain).
    startConsumer :: Int
                  -> Channel m (SomeTransition (ChainSyncMessage block (Point block)))
                  -> m (TVar m (Chain block))
    startConsumer cid channel = do
      chainVar <- atomically $ newTVar Genesis
      let consumer = chainSyncClientPeer (chainSyncClientExample chainVar pureClient)
      fork $ void $ useCodecWithDuplex (loggingChannel (ConsumerId nid cid) (withSomeTransition show) (withSomeTransition show) channel) codec consumer
      return chainVar

    startProducer :: Peer ChainSyncProtocol (ChainSyncMessage block (Point block))
                          ('Awaiting 'StIdle) ('Finished 'StDone)
                          m ()
                  -> Int
                  -> Channel m (SomeTransition (ChainSyncMessage block (Point block)))
                  -> m ()
    startProducer producer pid channel =
      -- use 'void' because 'fork' only works with 'm ()'
      -- No sense throwing on Unexpected right? since fork will just squelch
      -- it. FIXME: use async...
      fork $ void $ useCodecWithDuplex (loggingChannel (ProducerId nid pid) (withSomeTransition show) (withSomeTransition show) channel) codec producer

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
forkCoreKernel :: forall block m.
                  ( HasHeader block
                  , MonadSTM m
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
  getBlock <- blockGenerator slotDuration gchain
  fork $ applyGeneratedBlock getBlock

  where
    applyGeneratedBlock
      :: Tr m (Maybe block)
      -> m ()
    applyGeneratedBlock getBlock = do
      cont <- atomically $ do
        mblock <- getBlock
        case mblock of
          Nothing    -> return False
          Just block -> do
            cps@ChainProducerState{chainState = chain} <- readTVar cpsVar
            writeTVar cpsVar (switchFork (addBlock chain block) cps)
            return True
      if cont
        then applyGeneratedBlock getBlock
        else return ()

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
coreNode :: forall m.
        ( MonadSTM m
        , MonadTimer m
        , MonadSay m
        )
     => NodeId
     -> Duration (Time m)
     -- ^ slot duration
     -> [Block]
     -> NodeChannels m Block
     -> m (TVar m (ChainProducerState Block))
coreNode nid slotDuration gchain chans = do
  cpsVar <- relayNode nid Genesis chans
  forkCoreKernel slotDuration gchain Concrete.fixupBlock cpsVar
  return cpsVar
