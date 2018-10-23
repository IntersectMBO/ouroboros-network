{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTSyntax #-}

module Protocol.Chain.Sim.Node where

import Control.Concurrent.Async (concurrently, forConcurrently)
import Control.Monad (forM, forM_, join)
import qualified Data.Array as Array (assocs)
import qualified Data.Foldable as Foldable (foldlM, toList)
import Data.List (foldl')
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import Data.Graph (Graph, Vertex, buildG)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq

import Protocol.Chain.Sim.Producer
import Protocol.Chain.Sim.Consumer
import Protocol.Chain.StreamConsumer
import Protocol.Chain.StreamProducer
import Protocol.Chain.Type
import Protocol.Channel
import Protocol.Channel.Sim (simStmChannels)
import Protocol.Transition (SomeTransition)

import Block
import Chain
import MonadClass.MonadSTM

-- | Description of a static network.
data StaticNetDesc header x = StaticNetDesc Graph (Map Vertex (StaticNodeDesc header x))

-- | Description of a static node (static meaning the in/out edges don't
-- change).
data StaticNodeDesc header e = StaticNodeDesc
  { nodeDescName         :: String
  , nodeDescInitialChain :: NonEmpty header
  , nodeDescOutEdges     :: [e]
  , nodeDescInEdges      :: [e]
  }

staticNodeDesc :: String -> NonEmpty header -> StaticNodeDesc header x
staticNodeDesc name initialChain = StaticNodeDesc
  { nodeDescName         = name
  , nodeDescInitialChain = initialChain
  , nodeDescOutEdges     = []
  , nodeDescInEdges      = []
  }

includeOutEdges :: [e] -> StaticNodeDesc header e -> StaticNodeDesc header e
includeOutEdges es nd = nd { nodeDescOutEdges = es ++ nodeDescOutEdges nd }

includeInEdge :: e -> StaticNodeDesc header e -> StaticNodeDesc header e
includeInEdge e nd = nd { nodeDescInEdges = e : nodeDescInEdges nd }

-- | Construct 'StaticNodeDesc's from a 'StaticNetDesc' using 'MonadSTM'
-- channels for in/out edges.
realiseStaticNetDescSTM
  :: forall header m stm t .
     ( MonadSTM m stm )
  => (forall x . StaticNetDesc header x)
  -> stm (Map Vertex (StaticNodeDesc header (Channel m t)))
realiseStaticNetDescSTM (StaticNetDesc gr descs) =
  Foldable.foldlM createChannels descs (Array.assocs gr)
  where

  -- | Creates a channel for each target vertex, includes them in the local
  -- vertex's out-edges, and in the target vertex's in-edges.
  createChannels
    :: Map Vertex (StaticNodeDesc header (Channel m t))
    -> (Vertex, [Vertex])
    -> stm (Map Vertex (StaticNodeDesc header (Channel m t)))
  createChannels descs (v, targets) = do
    -- Create all necessary channel pairs.
    channelPairs <- forM targets (\_ -> simStmChannels)
    -- Including the out-edges in the map is just an insertion at the current
    -- vertex.
    -- Including the in-edges is expressed as a fold.
    pure $ foldl (\m (v, c) -> Map.adjust (includeInEdge c) v m)
                 (Map.adjust (includeOutEdges (fmap fst channelPairs)) v descs)
                 (zip targets (fmap snd channelPairs))

newtype ChainSelection header m t = ChainSelection
  { runChainSelection :: Seq header -> m (Either t (ChainSelection header m t))
  }

chainSelectionUntil
  :: ( Monad m )
  => (Seq header -> m (Maybe t))
  -> ChainSelection header m t
chainSelectionUntil k = ChainSelection $ \blks -> do
  next <- k blks
  case next of
    Just t  -> pure $ Left t
    Nothing -> pure $ Right $ chainSelectionUntil k

chainSelectionForever :: Monad m => (Seq header -> m ()) -> ChainSelection header m x
chainSelectionForever act = chainSelectionUntil (\blks -> Nothing <$ act blks)

-- | Use STM-backed chain selection to run a 'StaticNodeDesc'.
--
-- Two functions must be given: one will run the consumer threads, one will
-- run the producer threads, both using relevant 'TVar' state and a 'Channel'.
--
-- These are applied uniformly to the out- and in-edges of the 'StaticNodeDesc'.
-- They are all run concurrently, according to the concurrency combinator
-- provided ('MonadFork' is inadequate).
--
runWithChainSelectionSTM
  :: forall p m stm x consumer producer t .
     ( MonadSTM m stm )
  => (forall s t . m s -> m t -> m (s, t))
     -- ^ Concurrency
  -> (TVar m (Seq (BlockHeader p)) -> Channel m x -> m consumer)
     -- ^ Create a consumer
  -> (TVar m (Changing (ChainSegment (BlockHeader p)), Exhausted) -> Channel m x -> m producer)
     -- ^ Create a producer
  -> ChainSelection (BlockHeader p) stm t
     -- ^ Chain selection termination condition
  -> StaticNodeDesc (BlockHeader p) (Channel m x)
     -- ^ Description of the node (name, initial chain, and adjacency)
  -> m (t, ([consumer], [producer]))
runWithChainSelectionSTM concurrently mkConsumer mkProducer selection nodeDesc = do

  let initChain = nodeDescInitialChain nodeDesc
  bestChainVar <- atomically $ newTVar (Seq.fromList (NE.toList initChain))

  consumers <- forM (nodeDescOutEdges nodeDesc) $ \chan -> do
    consumerVar <- atomically $ newTVar (Seq.singleton (NE.head initChain))
    pure (consumerVar, mkConsumer consumerVar chan)

  producers <- forM (nodeDescInEdges nodeDesc) $ \chan -> do
    producerVar <- atomically $ newTVar (Unchanged (chainSegmentFromChain initChain), False)
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
    :: TVar m (Seq (BlockHeader p))               -- ^ Best chain.
    -> [TVar m (Seq (BlockHeader p))]             -- ^ Consumed chains
    -> [TVar m (Changing (ChainSegment (BlockHeader p)), Exhausted)] -- ^ Produced chains
    -> ChainSelection (BlockHeader p) stm t
    -> m t
  chainSelectionSTM bestChainVar consumerVars producerVars selection =
    join $ atomically $ do
      currentBest <- readTVar bestChainVar
      next <- runChainSelection selection currentBest
      case next of
        Left t -> do
          -- Mark the chain as exhausted.
          forM_ producerVars (flip modifyTVar' (\it -> (fst it, True)))
          pure $ pure t
        Right selection' -> do
          candidates  <- mapM readTVar consumerVars
          let newBest = foldl' longerChain currentBest candidates
          if betterChain newBest currentBest
          then do
            -- Found a strictly better chain. Update our best, and all of the
            -- producer vars.
            writeTVar bestChainVar newBest
            forM_ producerVars (flip modifyTVar' (\it -> (carryChange (switchToChain_ newBest) (fst it), snd it)))
            pure (chainSelectionSTM bestChainVar consumerVars producerVars selection')
          else retry

  switchToChain_ :: Seq (BlockHeader p) -> ChainSegment (BlockHeader p) -> Changing (ChainSegment (BlockHeader p))
  switchToChain_ newBest currentSegment = case Foldable.toList newBest of
    [] -> error $ show name <> " switched to an empty chain"
    (b : bs) -> case switchToChain eqBlockHeader (b NE.:| bs) currentSegment of
      Nothing -> error $ show name <> " new chain has nothing in common with current chain\n\n" ++ show newBest ++ "\n\n" ++ show currentSegment
      Just it -> it

  eqBlockHeader :: BlockHeader p -> BlockHeader p -> Bool
  eqBlockHeader a b = headerHash a == headerHash b

  -- Take the longer chain, favouring the left in case of a tie.
  longerChain :: Seq (BlockHeader p) -> Seq (BlockHeader p) -> Seq (BlockHeader p)
  longerChain left right = if betterChain right left
                           then right
                           else left

  -- True if the left chain is better (strictly longer) than the right chain.
  betterChain :: Seq (BlockHeader p) -> Seq (BlockHeader p) -> Bool
  betterChain left right = case (Seq.viewr left, Seq.viewr right) of
    (Seq.EmptyR, Seq.EmptyR) -> False
    (Seq.EmptyR, _)          -> False
    (_, Seq.EmptyR)          -> True
    (_ Seq.:> l, _ Seq.:> r) -> headerBlockNo l > headerBlockNo r

standardConsumer
  :: ( MonadSTM m stm )
  => String
  -> TVar m (Seq (BlockHeader p))
  -> Channel m (SomeTransition (TrChainExchange Point (BlockHeader p)))
  -> m (FromStream (TrChainExchange Point (BlockHeader p)) m ())
standardConsumer name var chan =
  useChannelHomogeneous chan (streamConsumer (simpleConsumerStream blockPoint (==) var))

standardProducer
  :: ( MonadSTM m stm )
  => String
  -> TVar m (Changing (ChainSegment (BlockHeader p)), Exhausted)
  -> Channel m (SomeTransition (TrChainExchange Point (BlockHeader p)))
  -> m (FromStream (TrChainExchange Point (BlockHeader p)) m ())
standardProducer name var chan =
  useChannelHomogeneous chan (streamProducer (simpleHeaderStream blockPoint var))
  -- NB blockPoint is a bad name, since in this case it specializes to type
  -- BlockHeader p -> Point

exampleNetDesc :: StaticNetDesc (BlockHeader p) x
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

  mkBlockHeader p h s b = BlockHeader
    (HeaderHash h)
    (HeaderHash p)
    (Slot s)
    (BlockNo b)
    (BlockSigner 0)
    (BodyHash 0)
  
  header1 = mkBlockHeader 0 100 1 1
  header2 = mkBlockHeader 100 101 2 2
  header3 = mkBlockHeader 101 102 3 3
  header4 = mkBlockHeader 102 103 4 4
  header5 = mkBlockHeader 103 104 5 5
  header6 = mkBlockHeader 104 105 6 6

  chain0 = header1 NE.:| [header2]
  chain1 = header1 NE.:| [header2]
  chain2 = header1 NE.:| [header2, header3, header4, header5, header6]

runNetDescStandardIO
  :: forall p producer consumer .
     (forall x . StaticNetDesc (BlockHeader p) x)
  -> IO [(Seq (BlockHeader p), ([FromStream (TrChainExchange Point (BlockHeader p)) IO ()], [FromStream (TrChainExchange Point (BlockHeader p)) IO ()]))]
runNetDescStandardIO netDesc = do
  nodes <- atomically $ realiseStaticNetDescSTM netDesc
  let runNode snd =
        let selectionP = \chain -> do
              case Seq.viewr chain of
                Seq.EmptyR -> pure Nothing
                _ Seq.:> newest -> pure $
                  if blockNo newest >= BlockNo 6
                  then Just chain
                  else Nothing
        in  runWithChainSelectionSTM concurrently
                                     (standardConsumer (nodeDescName snd))
                                     (standardProducer (nodeDescName snd))
                                     (chainSelectionUntil selectionP)
                                     snd
  forConcurrently (Map.elems nodes) runNode
