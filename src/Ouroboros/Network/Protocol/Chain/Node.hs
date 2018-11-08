{-# LANGUAGE DataKinds #-}
{-# LANGUAGE CPP       #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTSyntax #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE BangPatterns #-}

{-# OPTIONS_GHC "-fno-warn-name-shadowing" #-}

module Ouroboros.Network.Protocol.Chain.Node where

import Control.Concurrent.Async (concurrently, forConcurrently)
import Control.Concurrent.MVar (newMVar, withMVar)
import Control.Concurrent (threadDelay)
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

-- For an example, we need concrete blocks.
import qualified Ouroboros.Network.Testing.ConcreteBlock as Testing
import Ouroboros.Network.Block (Hash (..))
import qualified Ouroboros.Network.Chain as Chain (blockPoint)

-- | A list of 'elem' which changes within some 'm'.
data EvolvingChain elem m t = EvolvingChain
  { ecInitial :: NonEmpty elem
  , ecChanges :: m (NextElem elem m t)
  }

data NextElem elem m t where
  Complete :: t -> NextElem elem m t
  -- | Rather than giving the next elem, give a function from the last
  -- elem to a new elem. This makes 'NextElem' appropriate for use in a
  -- concurrent setting, where the 'NonEmpty elem' given by 'ecInitial' from
  -- 'EvolvingChain' is just a seed for a mutable chain.
  NextElem :: (elem -> elem) -> m (NextElem elem m t) -> NextElem elem m t

staticChain :: Applicative m => NonEmpty elem -> EvolvingChain elem m ()
staticChain it = EvolvingChain
  { ecInitial = it
  , ecChanges = pure (Complete ())
  }

uniformEvolvingChain
  :: ( Functor m )
  => NonEmpty elem
  -> m (elem -> elem)
  -> EvolvingChain elem m ()
uniformEvolvingChain initial mk = EvolvingChain
  { ecInitial = initial
  , ecChanges = let loop = fmap (\k -> NextElem k loop) mk in loop
  }

-- | Description of a static network (static in topology).
data StaticNetDesc header m t = StaticNetDesc Graph (Map Vertex (StaticNodeDesc Void header m t))

deriving instance Show (StaticNetDesc header m t)

-- | Description of a static node (static meaning the in/out edges don't
-- change).
data StaticNodeDesc e header m t = StaticNodeDesc
  { nodeDescName         :: String
  , nodeDescChain        :: EvolvingChain header m t
  , nodeDescOutEdges     :: [e]
  , nodeDescInEdges      :: [e]
  }

instance Show (StaticNodeDesc e header m t) where
  show snd = mconcat ["StaticNodeDesc " ++ nodeDescName snd]

mapEdges :: (e -> e') -> StaticNodeDesc e header m t -> StaticNodeDesc e' header m t
mapEdges f desc = desc
  { nodeDescOutEdges = fmap f (nodeDescOutEdges desc)
  , nodeDescInEdges  = fmap f (nodeDescInEdges desc)
  }

staticNodeDesc
  :: ( Applicative m )
  => String
  -> EvolvingChain header m t
  -> StaticNodeDesc x header m t
staticNodeDesc name evolvingChain = StaticNodeDesc
  { nodeDescName         = name
  , nodeDescChain        = evolvingChain
  , nodeDescOutEdges     = []
  , nodeDescInEdges      = []
  }

includeOutEdges :: [e] -> StaticNodeDesc e header m t -> StaticNodeDesc e header m t
includeOutEdges es nd = nd { nodeDescOutEdges = es ++ nodeDescOutEdges nd }

includeInEdge :: e -> StaticNodeDesc e header m t -> StaticNodeDesc e header m t
includeInEdge e nd = nd { nodeDescInEdges = e : nodeDescInEdges nd }

-- | Construct 'StaticNodeDesc's from a 'StaticNetDesc' using 'MonadSTM'
-- channels for in/out edges.
realiseStaticNetDescSTM
  :: forall header n m stm t r .
     ( MonadSTM m stm )
  => StaticNetDesc header n r
  -> stm (Map Vertex (StaticNodeDesc (Channel m t) header n r))
realiseStaticNetDescSTM (StaticNetDesc gr descs) =
  Foldable.foldlM createChannels (fmap (mapEdges absurd) descs) (Array.assocs gr)
  where

  -- | Creates a channel for each target vertex, includes them in the local
  -- vertex's out-edges, and in the target vertex's in-edges.
  createChannels
    :: Map Vertex (StaticNodeDesc (Channel m t) header n r)
    -> (Vertex, [Vertex])
    -> stm (Map Vertex (StaticNodeDesc (Channel m t) header n r))
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
  :: forall header m stm x consumer producer r t .
     ( MonadSTM m stm, Show header )
  => (header -> header -> Bool)
     -- ^ Block header equality
  -> (header -> BlockNo)
     -- ^ Block number from header
  -> (forall a b . m a -> m b -> m (a, b))
     -- ^ Concurrency
  -> (TVar m (Seq header) -> Channel m x -> m consumer)
     -- ^ Create a consumer
  -> (TVar m (Changing (ChainSegment header), Exhausted) -> Channel m x -> m producer)
     -- ^ Create a producer
  -> ChainSelection header stm t
     -- ^ Chain selection termination condition.
  -> (Seq header -> m ())
     -- ^ Tracing: run this whenever a new best chain is selected.
     -- TODO use proper 'Trace' type. Unfortunately it is in cardano-sl-util.
     -- Should be in its own package.
  -> StaticNodeDesc (Channel m x) header m r
     -- ^ Description of the node (name, initial chain, and adjacency)
  -> m (t, (r, ([consumer], [producer])))
runWithChainSelectionSTM eqBlockHeader headerBlockNo conc mkConsumer mkProducer selection trace nodeDesc = do

  let initChain = ecInitial (nodeDescChain nodeDesc)
      changes   = ecChanges (nodeDescChain nodeDesc)
  bestChainVar <- atomically $ newTVar (Seq.fromList (NE.toList initChain))

  consumers <- forM (nodeDescOutEdges nodeDesc) $ \chan -> do
    consumerVar <- atomically $ newTVar (Seq.singleton (NE.head initChain))
    pure (consumerVar, mkConsumer consumerVar chan)

  producers <- forM (nodeDescInEdges nodeDesc) $ \chan -> do
    producerVar <- atomically $ newTVar (Unchanged (chainSegmentFromChain initChain), False)
    pure (producerVar, mkProducer producerVar chan)

  let consumerVars = fmap fst consumers
      producerVars = fmap fst producers

  conc (chainSelectionSTM bestChainVar consumerVars producerVars selection) $
    conc (localEvolution bestChainVar producerVars changes) $
      conc (concurrentList (fmap snd consumers)) (concurrentList (fmap snd producers))

  where

  name = nodeDescName nodeDesc

  concurrentList :: forall t . [m t] -> m [t]
  concurrentList = foldr (\m ms -> uncurry (:) <$> conc m ms) (pure [])

  -- Extends the local chain to simulate minting of new blocks.
  localEvolution
    :: TVar m (Seq header)
    -> [TVar m (Changing (ChainSegment header), Exhausted)] -- ^ Produced chains
    -> m (NextElem header m r)
    -> m r
  localEvolution bestChainVar producerVars term = do
    nextElem <- term
    case nextElem of
      Complete r -> pure r
      NextElem k term' -> do
        _ <- atomically $ do
          chain <- readTVar bestChainVar
          case Seq.viewr chain of
            Seq.EmptyR -> error "localEvolution: empty chain"
            -- Here we don't simply update the bestChainVar. We must, as in
            -- 'chainSelectionSTM' when a consumer chain becomes best, update
            -- all producerVars.
            _ Seq.:> h -> do
              let !chain' = chain Seq.|> k h
              writeTVar bestChainVar chain'
              forM_ producerVars (flip modifyTVar' (\it -> (carryChange (switchToChain_ chain') (fst it), False)))
        localEvolution bestChainVar producerVars term'

  chainSelectionSTM
    :: TVar m (Seq header)               -- ^ Best chain.
    -> [TVar m (Seq header)]             -- ^ Consumed chains
    -> [TVar m (Changing (ChainSegment header), Exhausted)] -- ^ Produced chains
    -> ChainSelection header stm t
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
            -- We also set the second component to False :: Exhausted.
            writeTVar bestChainVar newBest
            forM_ producerVars (flip modifyTVar' (\it -> (carryChange (switchToChain_ newBest) (fst it), False)))
            pure $ do
              trace newBest
              chainSelectionSTM bestChainVar consumerVars producerVars selection'
          else retry

  switchToChain_ :: Seq header -> ChainSegment header -> Changing (ChainSegment header)
  switchToChain_ newBest currentSegment = case Foldable.toList newBest of
    [] -> error $ show name <> " switched to an empty chain"
    (b : bs) -> case switchToChain eqBlockHeader (b NE.:| bs) currentSegment of
      Nothing -> error $ show name <> " new chain has nothing in common with current chain\n\n" ++ show newBest ++ "\n\n" ++ show currentSegment
      Just it -> it

  -- Take the longer chain, favouring the left in case of a tie.
  longerChain :: Seq header -> Seq header -> Seq header
  longerChain left right = if betterChain right left
                           then right
                           else left

  -- True if the left chain is better (strictly longer) than the right chain.
  betterChain :: Seq header -> Seq header -> Bool
  betterChain left right = case (Seq.viewr left, Seq.viewr right) of
    (Seq.EmptyR, Seq.EmptyR) -> False
    (Seq.EmptyR, _)          -> False
    (_, Seq.EmptyR)          -> True
    (_ Seq.:> l, _ Seq.:> r) -> headerBlockNo l > headerBlockNo r

standardConsumer
  :: ( MonadSTM m stm, Ord point )
  => String
  -> (header -> point)
  -> TVar m (Seq header)
  -> Channel m (SomeTransition (TrChainExchange point header))
  -> m (FromStream (TrChainExchange point header) m ())
standardConsumer _ mkPoint var chan =
  useChannelHomogeneous chan (streamConsumer (simpleConsumerStream mkPoint (==) var))

standardProducer
  :: ( MonadSTM m stm, Ord point )
  => String
  -> (header -> point)
  -> TVar m (Changing (ChainSegment header), Exhausted)
  -> Channel m (SomeTransition (TrChainExchange point header))
  -> m (FromStream (TrChainExchange point header) m ())
standardProducer _ mkPoint var chan =
  useChannelHomogeneous chan (streamProducer (simpleProducerStream mkPoint var))
  -- NB blockPoint is a bad name, since in this case it specializes to type
  -- header p -> Point p

runNetDescStandardIO
  :: forall point header r t .
     ( Show header, Ord point )
  => StaticNetDesc header IO r
  -> (header -> header -> Bool) -- ^ header equality
  -> (header -> BlockNo)
  -> (header -> point)
  -> (forall m . Monad m => ChainSelection header m t)
  -> IO (Map Int (t, (r, (([FromStream (TrChainExchange point header) IO ()], [FromStream (TrChainExchange point header) IO ()])))))
runNetDescStandardIO netDesc eqBlockHeader headerBlockNo mkPoint chainSelection = do
  nodes <- atomically $ realiseStaticNetDescSTM netDesc
  stdoutLock <- newMVar ()
  let echoBestChain name = \bestChain -> withMVar stdoutLock $ \_ -> case Seq.viewr bestChain of
        Seq.EmptyR -> putStrLn $ name ++ ": empty chain"
        _ Seq.:> h -> putStrLn $ name ++ ": tip is " ++ show h
  let runNode desc = runWithChainSelectionSTM
        eqBlockHeader
        headerBlockNo
        concurrently
        (standardConsumer (nodeDescName desc) mkPoint)
        (standardProducer (nodeDescName desc) mkPoint)
        chainSelection
        (echoBestChain (nodeDescName desc))
        desc
  Map.fromList <$> forConcurrently (Map.toList nodes) (\(v, desc) -> (,) v <$> runNode desc)

-- Can't do this because we don't have a concrete block type anymore.
exampleNetDesc :: StaticNetDesc Testing.BlockHeader IO ()
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

  mkBlockHeader p h s b = Testing.BlockHeader
    (Testing.HeaderHash h)
    (BlockHash (Testing.HeaderHash p))
    s
    b
    (Testing.BlockSigner 0)
    (Testing.BodyHash 0)
  
  header1  = mkBlockHeader 0 100 (Slot 1) (BlockNo 1)
  header2  = mkBlockHeader 100 101 (Slot 2) (BlockNo 2)
  header2' = mkBlockHeader 100 111 (Slot 2) (BlockNo 2)
  header3  = mkBlockHeader 101 102 (Slot 3) (BlockNo 3)

  initialChain0 = header1 NE.:| [header2]
  initialChain1 = header1 NE.:| [header2']
  initialChain2 = header1 NE.:| [header2, header3]

  chain0 = staticChain initialChain0
  chain1 = staticChain initialChain1
  chain2 = uniformEvolvingChain initialChain2 $ do
    threadDelay 1000000
    print "Minting new block"
    pure nextBlockHeader

  nextBlockHeader :: Testing.BlockHeader -> Testing.BlockHeader
  nextBlockHeader header = mkBlockHeader
    (let Testing.HeaderHash i = blockHash header in i)
    (let Testing.HeaderHash i = blockHash header in i+1)
    (succ (blockSlot header))
    (succ (blockNo header))

example :: IO ()
example = do
  _ <- runNetDescStandardIO
    exampleNetDesc
    (\bh1 bh2 -> Testing.headerHash bh1 == Testing.headerHash bh2)
    blockNo
    Chain.blockPoint
    -- Never stop selecting the best chain.
    (chainSelectionForever (const (pure ())))
  pure ()
