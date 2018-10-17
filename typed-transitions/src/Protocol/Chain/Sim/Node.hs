{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Protocol.Chain.Sim.Node where

import Control.Monad (forever, forM, forM_, mapM_, join)
import Control.Monad.Trans.Free (iterT)
import qualified Data.Foldable as Foldable (foldlM, toList)
import Data.List (foldl')
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Set (Set)
import qualified Data.Set as Set

-- Only for demonstration.
import Protocol.Chain.Direct

import Protocol.Chain.Sim.Producer
import Protocol.Chain.Sim.Consumer
import Protocol.Chain.StreamConsumer
import Protocol.Chain.StreamProducer
import Protocol.Chain.Type
import Protocol.Channel
import Protocol.Channel.Sim (simStmChannels)
import Protocol.Core
import Protocol.Transition (SomeTransition, withSomeTransition)

import Block
import Chain
import MonadClass.MonadSay
import MonadClass.MonadSTM
import MonadClass.MonadFork

mkConsumer
  :: ( MonadSTM m stm )
  => TVar m (Seq Block)
  -> Peer ChainExchange TrChainExchange ('Awaiting 'StInit) ('Yielding 'StIdle) m x
mkConsumer = streamConsumer . simpleConsumerStream

mkProducer
  :: ( MonadSTM m stm )
  => TVar m (Changing ChainSegment)
  -> Peer ChainExchange TrChainExchange ('Yielding 'StInit) ('Yielding ('StBusy 'Next)) m x
mkProducer = streamProducer . simpleBlockStream

-- We can use a set of pairs to describe the network. They give directed
-- edges (producer points to consumer)...
--
-- Well, no, we also need the initial chains!
--
-- Really, the succinct way is to give
--
--   [(Name, Seq Block, [Name])]
--
-- The name, the initial chain, and the peers from which to consume.
--
type NetworkDesc name = Map name (Seq Block, [name])

--type NetworkDesc name = Set (name, name)

{-
toNodeDescMap
  :: forall name m stm .
     ( MonadSTM m stm, Ord name )
  => NetworkDesc name
  -> stm (Map name (NodeDesc name m))
toNodeDescMap netDesc = undefined

  where

  mkConsumerChannels :: stm (Map name [

  {-Foldable.foldlM includePair mempty . Map.toList
  where
  includePair :: Map name (NodeDesc name m) -> (name, (Seq Block, [name])) -> stm (Map name (NodeDesc name m))
  includePair map (name, (initChain, consumeFrom)) = do
    let insertNewDesc = Map.insert (NodeDesc name initChain [] [])

    (chanA, chanB) <- simStmChannels
    let includeProducerChannel = Map.alter (alteration from (producerChannel chanA)) from
        includeConsumerChannel = Map.alter (alteration to (consumerChannel chanB)) to
        includeChannels = includeProducerChannel . includeConsumerChannel
    pure $ includeChannels map

  alteration :: name -> (NodeDesc name m -> NodeDesc name m) -> Maybe (NodeDesc name m) -> Maybe (NodeDesc name m)
  alteration name k Nothing = Just (k (NodeDesc name 
  -}

-- | Run a network, with each node in its own thread via 'runNodeSTM'.
runNetworkSTM
  :: forall name m stm .
     ( MonadSTM m stm, MonadFork m, Ord name, Show name )
  => NetworkDesc name
  -> m ()
runNetworkSTM networkDesc = do
  netMap <- atomically $ toNodeDescMap networkDesc
  mapM_ (fork . runNodeSTM) (Map.elems netMap)
-}

example
  :: forall m stm .
     ( MonadSTM m stm, MonadFork m, MonadSay m )
  => m ()
example = do

  (chanConsumeA, chanProduceB) <- atomically simStmChannels
  --(chanProduceA, chanConsumeB) <- simStmChannels

  let mkBlock p h s b =
        let header = BlockHeader
              (HeaderHash h)
              (HeaderHash p)
              (Slot s)
              (BlockNo b)
              (BlockSigner 0)
              (BodyHash 0)
        in  Block header (BlockBody "")
  
      block1 = mkBlock 0 100 1 1
      block2 = mkBlock 100 101 2 2
      block3 = mkBlock 101 102 3 3
      block4 = mkBlock 102 103 4 4
      block5 = mkBlock 103 104 5 5
      block6 = mkBlock 104 105 6 6
  
      chainA = block1 NE.:| [block2]
      chainB = block1 NE.:| [block2, block3, block4, block5, block6]
      nodeA = NodeDesc "A" chainA [channelSendEffect (say . withSomeTransition showTrChainExchange) chanConsumeA] []
      nodeB = NodeDesc "B" chainB [] [channelSendEffect (say . withSomeTransition showTrChainExchange) chanProduceB]

  fork $ runNodeSTM nodeB
  runNodeSTM nodeA

  {-
  -- In order to use 'direct', we need to come up with
  -- 'BlockStream m t' and 'ConsumerStream m t'
  producerVar <- atomically $ newTVar (Unchanged (chainSegmentFromChain chainB))
  consumerVar <- atomically $ newTVar (Seq.fromList (NE.toList chainA))

  let prod = simpleBlockStream producerVar
      cons = simpleConsumerStream consumerVar
      printNew :: Block -> m ()
      printNew cur = do
        (chain, new) <- atomically $ do
          chain <- readTVar consumerVar
          new <- case Seq.viewr chain of
            Seq.EmptyR -> retry
            _ Seq.:> r -> pure r
          if headerHash (blockHeader cur) == headerHash (blockHeader new)
          then retry
          else pure (chain, new)
        say $ "got new chain: " <> show chain
        printNew new

  fork $ printNew block1

  direct prod cons
  -}


-- | Description of a node: initial block chain, with consumer and producer
-- channels.
--
-- TODO include a description of chain building (minting new blocks).
data NodeDesc name m = NodeDesc
  { nodeName            :: name
    -- | If the genesis block were a real 'Block' and not implicit, it would be
    -- trivial to get a 'NonEmpty Block'. But in ouroboros-network it's not, so
    -- this may be annoying. We'll fix that problem later.
  , nodeDescBestChain   :: NonEmpty Block
    -- | Channels for the chains consumed here, each corresponding to one
    -- consumer thread.
  , nodeDescConsumeFrom :: [Channel m (SomeTransition TrChainExchange)]
    -- | The local producers (serving remote consumers), each corresponding to
    -- one producer thread.
  , nodeDescProduceTo   :: [Channel m (SomeTransition TrChainExchange)]
  }

-- | Run an STM-backed node by forking threads to run consumers, producers,
-- and chain selection.
runNodeSTM
  :: forall name m stm .
     ( MonadSTM m stm, MonadFork m, MonadSay m, Show name )
  => NodeDesc name m
  -> m ()
runNodeSTM nodeDesc = do

  let initChain = nodeDescBestChain nodeDesc
  bestChainVar <- atomically $ newTVar (Seq.fromList (NE.toList initChain))

  consumerVars <- forM (nodeDescConsumeFrom nodeDesc) $ \chan -> do
    consumerVar <- atomically $ newTVar mempty
    let peer = mkConsumer consumerVar
        proc = useChannel chan peer
    fork $ do
      outcome <- iterT join proc
      case outcome of 
        StreamDone anything -> pure anything
        StreamUnexpected someTransition -> error $ show name <> " unexpected consumer input"
        StreamEnd k -> error $ show name <> " consumer stream ended early"
    pure consumerVar

  producerVars <- forM (nodeDescProduceTo nodeDesc) $ \chan -> do
    producerVar <- atomically $ newTVar (Unchanged (chainSegmentFromChain initChain))
    let peer = mkProducer producerVar
        proc = useChannel chan peer
    fork $ do
      outcome <- iterT join proc
      case outcome of
        StreamDone anything -> pure anything
        -- FIXME what can we do with these? Can't give the value back, so I guess
        -- we just have to use effects and throw/print.
        StreamUnexpected someTransition -> error $ show name <> " unexpected producer input"
        StreamEnd k -> error $ show name <> " producer stream ended early"
    pure producerVar

  chainSelectionSTM bestChainVar consumerVars producerVars

  where

  name = nodeName nodeDesc

  chainSelectionSTM
    :: TVar m (Seq Block)               -- ^ Best chain.
    -> [TVar m (Seq Block)]             -- ^ Consumed chains
    -> [TVar m (Changing ChainSegment)] -- ^ Produced chains
    -> m ()
  chainSelectionSTM bestChainVar consumerVars producerVars = forever $ do
    newBest <- atomically $ do
      currentBest <- readTVar bestChainVar
      candidates  <- mapM readTVar consumerVars
      let newBest = foldl' longerChain currentBest candidates
      if betterChain newBest currentBest
      then do
        -- Found a strictly better chain. Update our best, and all of the
        -- producer vars.
        writeTVar bestChainVar newBest
        forM_ producerVars (flip modifyTVar' (carryChange (switchToChain_ newBest)))
        pure newBest
      else retry
    say $ show name <> " got better chain: " <> show (fmap (headerHash . blockHeader) newBest)

  switchToChain_ :: Seq Block -> ChainSegment -> Changing ChainSegment
  switchToChain_ newBest currentSegment = case Foldable.toList newBest of
    [] -> error $ show name <> " switched to an empty chain"
    (b : bs) -> case switchToChain (b NE.:| bs) currentSegment of
      Nothing -> error $ show name <> " new chain has nothing in common with current chain"
      Just it -> it

  -- Take the longer chain, favouring the left in case of a tie.
  longerChain :: Seq Block -> Seq Block -> Seq Block
  longerChain left right = if betterChain right left
                           then right
                           else left

  -- True if the left chain is better (strictly longer) than the right chain.
  betterChain :: Seq Block -> Seq Block -> Bool
  betterChain left right = case (Seq.viewr left, Seq.viewr right) of
    (Seq.EmptyR, Seq.EmptyR) -> False
    (Seq.EmptyR, _)          -> False
    (_, Seq.EmptyR)          -> True
    (_ Seq.:> l, _ Seq.:> r) -> headerBlockNo (blockHeader l) > headerBlockNo (blockHeader r)

{-
type NetDesc name m = Map Int (NodeDesc name m)

type NetBuilder name m = StateT (NetDesc name m, Int) m

-- No, all we need is a set of pairs: directed edges, from producer to consumer.
-- From this, along with a mapping from names to chains, we can produce the
-- network.
--
--   -- To make a node you give a name and then an initial chain along with
--   -- further evolution of that chain: a ListT m (Seq Block)... er, not
--   -- quite right. It must only use the best chain (at the given time).
--   do a <- node "A" (... :: ?)
--      b <- node "B" ...
--      a `consumeFrom` b
--      c <- node "C" ...
--      a `consumeFrom` c
--
-- Or maybe we want some sort of 'fix' like thing? With all of the names in
-- scope, ... nah
--
data EvolvingChain m = EvolvingChain
  { currentChain :: Seq Block
  , nextChain    :: m (Block -> (Block, ()))
  }

data NodeSTMConsumer m = NodeSTMConsumer
  { nodeConsumerState   :: TVar m (Seq Block)
  , nodeConsumerChannel :: Channel m (SomeTransition TrChainExchange)
  }

data NodeSTMProducer m = NodeSTMProducer
  { nodeProducerState   :: TVar m (Changing ChainSegment)
  , nodeProducerChannel :: Channel m (SomeTransition TrChainExchange)
  }

-- I think what we're after is a kind of "network builder".
-- It can construct nodes, and those nodes can be connected, and what we get
-- in the end is an STM.
--
--   node :: Seq Block -> NetBuilder m (NodeSTM m)
--   consumeFrom :: NodeSTM m -> NodeSTM m -> NetBuilder m ()
--
-- Each node needs an initial chain (given by 'node'), probably a label too,
-- and then a list of consumer channels and producer channels. The states for
-- these can be created at run time, and derived from the starting chain.
-- So the type we want?
--
--   stm [Node m]
--
-- Simple option is state with a map keyed on names... But is there are more
-- clever way?
--
-- Could our node builder just be  Endo (Map String (NodeDesc m))  ?
-- Er, we'd rather use StateT, so we can bind names. Or WriterT over the
-- Endo, over STM.
--
--
-- For the producers, we need that
--
--   TVar m (Changing ChainSegment)
--
-- but how do we get that? It should be brought up when the _run_ the node,
-- not when we construct it.
--
-- How to run it then?
--
--   run :: (Node m -> m ()) -> NetBuilder m -> m ()
--
--

-- | First consumes from the second. It's in 'm' because we need to create a
-- channel and state variables. The first component is the new consuming node,
-- second is producing. So the idomatic use is to shadow the old ones:
--
--   do a <- ...
--      b <- ...
--      (a, b) <- a `consumeFrom` b
--      ...
--
consumeFrom
  :: ( MonadSTM m stm )
  => NodeSTM m
  -> NodeSTM m
  -> m (NodeSTM m, NodeSTM m)
consumeFrom consumer producer = do
  (chanA, chanB) <- simStmChannels
  consumerState <- atomically $ newTVar mempty
  producerState <- atomically $ newTVar 
  let nodeConsumer = NodeSTMConsumer consumerState
      consumer' = consumer { nodeConsumers = nodeConsumer : nodeConsumers consumer }
      producer' = producer { nodeProducers = nodeProducer : nodeProducers producer }

-- | Run a bunch of nodes, forking a thread for each.
runNodesSTM
  :: forall m stm .
     ( MonadSTM m stm, MonadFork m )
  => [NodeSTM m]
  -> m ()
runNodesSTM ns = forM_ ns runNodeSTM

{-
type Process s m t = Channel m s -> m t

-- | Use an effectful channel to run a 'Peer'.
mkProcess
  :: ( Monad m )
  => Peer ChainExchange TrChainExchange (status from) to m x
  -> Process (SomeTransition TrChainExchange) m (Either String x)
mkProcess peer channel = do
  outcome <- iterT join (useChannel channel peer)
  case outcome of
    StreamDone x -> pure $ Right x
    -- Linked 2 incompatible protocols by way of an effectful channel, or
    -- used a bogus encoder/decoder for the transition type.
    StreamUnexpected someTransition -> pure $ Left "Unexpected transition."
    StreamEnd k -> pure $ Left "Stream ended early."

-- | Use STM-backed channels to link 2 processes. You're responsible for
-- ensuring that the processes run the same protocol from the same point.
-- TBD can't I help with that? Right, forget about processes here.
-- mkProducer and mkConsumer have compatible types when applied to the proper
-- TVars. WE can have an STM channel linker which requires compatible 'Peer'
-- types.
linkProcessesSTM
  :: ( MonadSTM m stm )
  => Process s m ()
  -> Process s m ()
  -> m ()
linkProcessesSTM p1 p2 = do
  (chan1, chan2) <- simStmChannels
  fork $ p1 chan1
  fork $ p2 chan2

mkConsumerProcess
  :: ( MonadSTM m stm )
  => TVar m (Seq Block)
  -> Process (SomeTransition TrChainExchange) m (Either String x)
-- GHC rejects the point-free version of this. Thinks it's impredicative
-- polymorphism...
mkConsumerProcess chainVar = mkProcess (mkConsumer chainVar)

mkProducerProcess
  :: ( MonadSTM m stm )
  => TVar m (Changing ChainSegment)
  -> Process (SomeTransition TrChainExchange) m (Either String x)
mkProducerProcess chainVar = mkProcess (mkProducer chainVar)

-}

-}
