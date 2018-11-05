{-# LANGUAGE DataKinds #-}
{-# LANGUAGE CPP       #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTSyntax #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}

{-# OPTIONS_GHC "-fno-warn-name-shadowing" #-}

module Ouroboros.Network.Protocol.Chain.Node where

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
#if __GLASGOW_HASKELL__ < 804
import Data.Semigroup ((<>))
#endif
import Data.Void (Void, absurd)

import Protocol.Chain.ConsumerStream
import Protocol.Chain.ProducerStream
import Protocol.Chain.Type
import Protocol.Channel
import Protocol.Transition (SomeTransition)

import Ouroboros.Network.Block
import Ouroboros.Network.MonadClass.MonadSTM
import Ouroboros.Network.Protocol.Chain.Producer
import Ouroboros.Network.Protocol.Chain.Consumer
import Ouroboros.Network.Protocol.Channel.Sim (simStmChannels)

-- | Description of a static network.
data StaticNetDesc header = StaticNetDesc Graph (Map Vertex (StaticNodeDesc header Void))
  deriving (Show)

-- | Description of a static node (static meaning the in/out edges don't
-- change).
data StaticNodeDesc header e = StaticNodeDesc
  { nodeDescName         :: String
  , nodeDescInitialChain :: NonEmpty header
  , nodeDescOutEdges     :: [e]
  , nodeDescInEdges      :: [e]
  }
  deriving (Show)

instance Functor (StaticNodeDesc header) where
  fmap f desc = desc
    { nodeDescOutEdges = fmap f (nodeDescOutEdges desc)
    , nodeDescInEdges  = fmap f (nodeDescInEdges desc)
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
  => StaticNetDesc header
  -> stm (Map Vertex (StaticNodeDesc header (Channel m t)))
realiseStaticNetDescSTM (StaticNetDesc gr descs) =
  Foldable.foldlM createChannels ((fmap . fmap) absurd descs) (Array.assocs gr)
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
  :: forall blockHeader m stm x consumer producer t .
     ( MonadSTM m stm, Show blockHeader )
  => (blockHeader -> blockHeader -> Bool)
     -- ^ Block header equality
  -> (blockHeader -> BlockNo)
     -- ^ Block number from header
  -> (forall a b . m a -> m b -> m (a, b))
     -- ^ Concurrency
  -> (TVar m (Seq blockHeader) -> Channel m x -> m consumer)
     -- ^ Create a consumer
  -> (TVar m (Changing (ChainSegment blockHeader), Exhausted) -> Channel m x -> m producer)
     -- ^ Create a producer
  -> ChainSelection blockHeader stm t
     -- ^ Chain selection termination condition
  -> StaticNodeDesc blockHeader (Channel m x)
     -- ^ Description of the node (name, initial chain, and adjacency)
  -> m (t, ([consumer], [producer]))
runWithChainSelectionSTM eqBlockHeader headerBlockNo conc mkConsumer mkProducer selection nodeDesc = do

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

  conc (chainSelectionSTM bestChainVar consumerVars producerVars selection)
       (conc (concurrentList (fmap snd consumers)) (concurrentList (fmap snd producers)))

  where

  name = nodeDescName nodeDesc

  concurrentList :: forall t . [m t] -> m [t]
  concurrentList = foldr (\m ms -> uncurry (:) <$> conc m ms) (pure [])

  chainSelectionSTM
    :: TVar m (Seq blockHeader)               -- ^ Best chain.
    -> [TVar m (Seq blockHeader)]             -- ^ Consumed chains
    -> [TVar m (Changing (ChainSegment blockHeader), Exhausted)] -- ^ Produced chains
    -> ChainSelection blockHeader stm t
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

  switchToChain_ :: Seq blockHeader -> ChainSegment blockHeader -> Changing (ChainSegment blockHeader)
  switchToChain_ newBest currentSegment = case Foldable.toList newBest of
    [] -> error $ show name <> " switched to an empty chain"
    (b : bs) -> case switchToChain eqBlockHeader (b NE.:| bs) currentSegment of
      Nothing -> error $ show name <> " new chain has nothing in common with current chain\n\n" ++ show newBest ++ "\n\n" ++ show currentSegment
      Just it -> it

  -- Take the longer chain, favouring the left in case of a tie.
  longerChain :: Seq blockHeader -> Seq blockHeader -> Seq blockHeader
  longerChain left right = if betterChain right left
                           then right
                           else left

  -- True if the left chain is better (strictly longer) than the right chain.
  betterChain :: Seq blockHeader -> Seq blockHeader -> Bool
  betterChain left right = case (Seq.viewr left, Seq.viewr right) of
    (Seq.EmptyR, Seq.EmptyR) -> False
    (Seq.EmptyR, _)          -> False
    (_, Seq.EmptyR)          -> True
    (_ Seq.:> l, _ Seq.:> r) -> headerBlockNo l > headerBlockNo r

standardConsumer
  :: ( MonadSTM m stm, Ord point )
  => String
  -> (blockHeader -> point)
  -> TVar m (Seq blockHeader)
  -> Channel m (SomeTransition (TrChainExchange point blockHeader))
  -> m (FromStream (TrChainExchange point blockHeader) m ())
standardConsumer _ mkPoint var chan =
  useChannelHomogeneous chan (streamConsumer (simpleConsumerStream mkPoint (==) var))

standardProducer
  :: ( MonadSTM m stm, Ord point )
  => String
  -> (blockHeader -> point)
  -> TVar m (Changing (ChainSegment blockHeader), Exhausted)
  -> Channel m (SomeTransition (TrChainExchange point blockHeader))
  -> m (FromStream (TrChainExchange point blockHeader) m ())
standardProducer _ mkPoint var chan =
  useChannelHomogeneous chan (streamProducer (simpleProducerStream mkPoint var))
  -- NB blockPoint is a bad name, since in this case it specializes to type
  -- blockHeader p -> Point p

{-
-- Can't do this because we don't have a concrete block type anymore.
exampleNetDesc :: StaticNetDesc (blockHeader p)
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
-}

runNetDescStandardIO
  :: forall point blockHeader t .
     ( Show blockHeader, Ord point )
  => StaticNetDesc blockHeader
  -> (blockHeader -> blockHeader -> Bool) -- ^ header equality
  -> (blockHeader -> BlockNo)
  -> (blockHeader -> point)
  -> (forall m . Monad m => ChainSelection blockHeader m t)
  -> IO (Map Int (t, ([FromStream (TrChainExchange point blockHeader) IO ()], [FromStream (TrChainExchange point blockHeader) IO ()])))
runNetDescStandardIO netDesc eqBlockHeader headerBlockNo mkPoint chainSelection = do
  nodes <- atomically $ realiseStaticNetDescSTM netDesc
  let runNode desc = runWithChainSelectionSTM
        eqBlockHeader
        headerBlockNo
        concurrently
        (standardConsumer (nodeDescName desc) mkPoint)
        (standardProducer (nodeDescName desc) mkPoint)
        chainSelection
        desc
  Map.fromList <$> forConcurrently (Map.toList nodes) (\(v, desc) -> (,) v <$> runNode desc)
