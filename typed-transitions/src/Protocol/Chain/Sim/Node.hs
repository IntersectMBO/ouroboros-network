{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTSyntax #-}

module Protocol.Chain.Sim.Node where

import Control.Concurrent.Async (concurrently, forConcurrently)
import Control.Monad (forever, forM, forM_, mapM_, join)
import Control.Monad.Trans.Free (iterT)
import qualified Data.Array as Array (assocs)
import qualified Data.Foldable as Foldable (foldlM, toList)
import Data.List (foldl')
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import Data.Graph (Graph, Vertex, buildG, vertices)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (mapMaybe)
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Void (Void)

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

import qualified Debug.Trace as Debug (traceM)

-- | Description of a static network.
data StaticNetDesc p x = StaticNetDesc Graph (Map Vertex (StaticNodeDesc p x))

-- | Description of a static node (static meaning the in/out edges don't
-- change).
data StaticNodeDesc p e = StaticNodeDesc
  { nodeDescName         :: String
  , nodeDescInitialChain :: NonEmpty (Block p)
  , nodeDescOutEdges     :: [e]
  , nodeDescInEdges      :: [e]
  }

staticNodeDesc :: String -> NonEmpty (Block p) -> StaticNodeDesc p x
staticNodeDesc name initialChain = StaticNodeDesc
  { nodeDescName         = name
  , nodeDescInitialChain = initialChain
  , nodeDescOutEdges     = []
  , nodeDescInEdges      = []
  }

includeOutEdges :: [e] -> StaticNodeDesc p e -> StaticNodeDesc p e
includeOutEdges es nd = nd { nodeDescOutEdges = es ++ nodeDescOutEdges nd }

includeInEdge :: e -> StaticNodeDesc p e -> StaticNodeDesc p e
includeInEdge e nd = nd { nodeDescInEdges = e : nodeDescInEdges nd }

-- | Construct 'StaticNodeDesc's from a 'StaticNetDesc' using 'MonadSTM'
-- channels for in/out edges.
realiseStaticNetDescSTM
  :: forall p m stm t .
     ( MonadSTM m stm )
  => (forall x . StaticNetDesc p x)
  -> stm (Map Vertex (StaticNodeDesc p (Channel m t)))
realiseStaticNetDescSTM (StaticNetDesc gr descs) =
  Foldable.foldlM createChannels descs (Array.assocs gr)
  where

  -- | Creates a channel for each target vertex, includes them in the local
  -- vertex's out-edges, and in the target vertex's in-edges.
  createChannels
    :: Map Vertex (StaticNodeDesc p (Channel m t))
    -> (Vertex, [Vertex])
    -> stm (Map Vertex (StaticNodeDesc p (Channel m t)))
  createChannels descs (v, targets) = do
    -- Create all necessary channel pairs.
    channelPairs <- forM targets (\_ -> simStmChannels)
    -- Including the out-edges in the map is just an insertion at the current
    -- vertex.
    -- Including the in-edges is expressed as a fold.
    pure $ foldl (\m (v, c) -> Map.adjust (includeInEdge c) v m)
                 (Map.adjust (includeOutEdges (fmap fst channelPairs)) v descs)
                 (zip targets (fmap snd channelPairs))

data ChainSelection p m t where
  Continue :: (Seq (Block p) -> m (ChainSelection p m t)) -> ChainSelection p m t
  Finished :: t -> ChainSelection p m t

chainSelectionForever :: Applicative m => (Seq (Block p) -> m ()) -> ChainSelection p m x
chainSelectionForever act = Continue $ \blk -> act blk *> pure (chainSelectionForever act)

-- | Use STM-backed chain selection to run a 'StaticNodeDesc'.
--
-- Two functions must be given: one will run the consumer threads, one will
-- run the producer threads, both using relevant 'TVar' state and a 'Channel'.
--
-- These are applied uniformly to the out- and in-edges of the 'StaticNodeDesc'.
-- They are all run concurrently, according to the concurrency combinator
-- provided ('MonadFork' is inadequate).
--
runWithChainSelection
  :: forall p m stm x consumer producer t .
     ( MonadSTM m stm )
  => (forall s t . m s -> m t -> m (s, t))
  -> (TVar m (Seq (Block p)) -> Channel m x -> m consumer)
  -> (TVar m (Changing (ChainSegment p)) -> Channel m x -> m producer)
  -> ChainSelection p m t
  -> StaticNodeDesc p (Channel m x)
  -> m (t, ([consumer], [producer]))
runWithChainSelection concurrently mkConsumer mkProducer selection nodeDesc = do

  let initChain = nodeDescInitialChain nodeDesc
  bestChainVar <- atomically $ newTVar (Seq.fromList (NE.toList initChain))

  consumers <- forM (nodeDescOutEdges nodeDesc) $ \chan -> do
    consumerVar <- atomically $ newTVar (Seq.singleton (NE.head initChain))
    pure (consumerVar, mkConsumer consumerVar chan)

  producers <- forM (nodeDescInEdges nodeDesc) $ \chan -> do
    producerVar <- atomically $ newTVar (Unchanged (chainSegmentFromChain initChain))
    pure (producerVar, mkProducer producerVar chan)

  let consumerVars = fmap fst consumers
      producerVars = fmap fst producers

  concurrently (chainSelectionSTM bestChainVar consumerVars producerVars selection)
               (concurrently (concurrentList (fmap snd consumers)) (concurrentList (fmap snd producers)))

  where

  name = nodeDescName nodeDesc

  concurrentList :: forall t . [m t] -> m [t]
  concurrentList = foldr (\m ms -> uncurry (:) <$> concurrently m ms) (pure [])

  chainSelectionSTM
    :: TVar m (Seq (Block p))               -- ^ Best chain.
    -> [TVar m (Seq (Block p))]             -- ^ Consumed chains
    -> [TVar m (Changing (ChainSegment p))] -- ^ Produced chains
    -> ChainSelection p m t
    -> m t
  chainSelectionSTM bestChainVar consumerVars producerVars selection = case selection of
    Finished t -> pure t
    Continue k -> do
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
      selection' <- k newBest
      chainSelectionSTM bestChainVar consumerVars producerVars selection'

  switchToChain_ :: Seq (Block p) -> ChainSegment p -> Changing (ChainSegment p)
  switchToChain_ newBest currentSegment = case Foldable.toList newBest of
    [] -> error $ show name <> " switched to an empty chain"
    (b : bs) -> case switchToChain (b NE.:| bs) currentSegment of
      Nothing -> error $ show name <> " new chain has nothing in common with current chain\n\n" ++ show newBest ++ "\n\n" ++ show currentSegment
      Just it -> it

  -- Take the longer chain, favouring the left in case of a tie.
  longerChain :: Seq (Block p) -> Seq (Block p) -> Seq (Block p)
  longerChain left right = if betterChain right left
                           then right
                           else left

  -- True if the left chain is better (strictly longer) than the right chain.
  betterChain :: Seq (Block p) -> Seq (Block p) -> Bool
  betterChain left right = case (Seq.viewr left, Seq.viewr right) of
    (Seq.EmptyR, Seq.EmptyR) -> False
    (Seq.EmptyR, _)          -> False
    (_, Seq.EmptyR)          -> True
    (_ Seq.:> l, _ Seq.:> r) -> headerBlockNo (blockHeader l) > headerBlockNo (blockHeader r)

standardConsumer
  :: ( MonadSTM m stm )
  => TVar m (Seq (Block p))
  -> Channel m (SomeTransition (TrChainExchange p))
  -> m (FromStream (TrChainExchange p) m ())
standardConsumer var chan = useChannelHomogeneous chan (streamConsumer (simpleConsumerStream var))

standardProducer
  :: ( MonadSTM m stm )
  => TVar m (Changing (ChainSegment p))
  -> Channel m (SomeTransition (TrChainExchange p))
  -> m (FromStream (TrChainExchange p) m ())
standardProducer var chan = useChannelHomogeneous chan (streamProducer (simpleBlockStream var))

exampleNetDesc :: StaticNetDesc p x
exampleNetDesc = StaticNetDesc gr nodes
  where
  -- Very simple graph in which node 0 consumes from node 1, 1 from node 2.
  -- 0 --> 1 --> 2
  gr = buildG (0,2) [(0,1), (1,2)]
  nodes = Map.fromList
    [ (0, staticNodeDesc "0" chain0)
    , (1, staticNodeDesc "1" chain1)
    , (2, staticNodeDesc "2" chain2)
    ]

  mkBlock p h s b =
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

  chain0 = block1 NE.:| [block2]
  chain1 = block1 NE.:| [block2]
  chain2 = block1 NE.:| [block2, block3, block4, block5, block6]

runNetDescStandardIO
  :: forall p t producer consumer .
     (forall x . StaticNetDesc p x)
  -> IO [(t, ([FromStream (TrChainExchange p) IO ()], [FromStream (TrChainExchange p) IO ()]))]
runNetDescStandardIO netDesc = do
  nodes <- atomically $ realiseStaticNetDescSTM netDesc
  let runNode
        :: forall x .
           StaticNodeDesc p (Channel IO (SomeTransition (TrChainExchange p)))
        -> IO (x, ([FromStream (TrChainExchange p) IO ()], [FromStream (TrChainExchange p) IO ()]))
      runNode snd =
        let selectionAction = \chain -> Debug.traceM (nodeDescName snd ++ ": " ++ show chain)
        in  runWithChainSelection concurrently standardConsumer standardProducer (chainSelectionForever selectionAction) snd
  forConcurrently (Map.elems nodes) runNode
