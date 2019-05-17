{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE TypeFamilies               #-}

module Ouroboros.Network.BlockFetch.Examples (
    blockFetchExample1,
    mockBlockFetchServer1,
    exampleFixedPeerGSVs,
  ) where

import qualified Data.Map.Strict as Map
import           Data.Map (Map)
import qualified Data.Set as Set
import           Data.Set (Set)
import qualified Data.ByteString.Lazy as LBS
import           Data.Functor.Contravariant (contramap)
import           Codec.Serialise (Serialise(..))

import           Control.Monad.Class.MonadSTM
import           Control.Monad.Class.MonadST
import           Control.Monad.Class.MonadThrow
import           Control.Monad.Class.MonadAsync
import           Control.Monad.Class.MonadTime
import           Control.Tracer (Tracer, nullTracer)
import           Control.Exception (assert)

import           Ouroboros.Network.Block
import           Ouroboros.Network.ChainFragment (ChainFragment(..))
import qualified Ouroboros.Network.ChainFragment as ChainFragment
import           Ouroboros.Network.AnchoredFragment (AnchoredFragment(..))
import qualified Ouroboros.Network.AnchoredFragment as AnchoredFragment

import           Ouroboros.Network.Protocol.BlockFetch.Type
import           Ouroboros.Network.Protocol.BlockFetch.Server
import           Ouroboros.Network.Protocol.BlockFetch.Codec
import           Network.TypedProtocol.Core
import           Network.TypedProtocol.Pipelined
import           Network.TypedProtocol.Driver
import           Ouroboros.Network.Channel

import           Ouroboros.Network.DeltaQ
import           Ouroboros.Network.BlockFetch.Client
import           Ouroboros.Network.BlockFetch.ClientRegistry
import           Ouroboros.Network.BlockFetch.State
import           Ouroboros.Network.BlockFetch

import           Ouroboros.Network.Testing.ConcreteBlock


--
-- Sample setups of block fetch logic with fetch clients and peers
--

-- | End to end test of block fetching with fixed chain and candidates.
--
-- The setup is the block fetch logic thread and a bunch of peers each with a
-- chain. The current chain and candidate chains are fixed and the peers never
-- fail or go slowly.
--
-- Run the block fetch until all the chains are downloaded. So this assumes
-- all the candidates do intersect the current chain, and are longer, so we
-- will be interested in downloading them all.
--
blockFetchExample1 :: forall m. (MonadSTM m, MonadST m, MonadAsync m,
                                 MonadCatch m, MonadTime m)
                   => Tracer m [TraceLabelPeer Int
                                 (FetchDecision [Point BlockHeader])]
                   -> Tracer m (TraceLabelPeer Int
                                 (TraceFetchClientState BlockHeader))
                   -> Tracer m (TraceLabelPeer Int
                                 (TraceSendRecv (BlockFetch BlockHeader Block)))
                   -> AnchoredFragment Block   -- ^ Fixed current chain
                   -> [AnchoredFragment Block] -- ^ Fixed candidate chains
                   -> m ()
blockFetchExample1 decisionTracer clientStateTracer clientMsgTracer
                   currentChain candidateChains = do

    registry    <- newFetchClientRegistry
    blockHeap   <- mkTestFetchedBlockHeap (anchoredChainPoints currentChain)

    peerAsyncs  <- sequence
                    [ runFetchClientAndServerAsync
                        clientMsgTracer'
                        serverMsgTracer
                        registry peerno
                        (mockedBlockFetchClient1 clientStateTracer' blockHeap)
                        (mockBlockFetchServer1 (unanchorFragment candidateChain))
                    | (peerno, candidateChain) <- zip [1..] candidateChains
                    , let clientMsgTracer'   = contramap (TraceLabelPeer peerno)
                                                         clientMsgTracer
                          clientStateTracer' = contramap (TraceLabelPeer peerno)
                                                         clientStateTracer
                    ]
    fetchAsync  <- async $ blockFetch registry blockHeap
    driverAsync <- async $ driver blockHeap

    -- Order of shutdown here is important for this example: must kill off the
    -- fetch thread before the peer threads.
    _ <- waitAnyCancel $ [ fetchAsync, driverAsync ]
                      ++ [ peerAsync
                         | (client, server) <- peerAsyncs
                         , peerAsync <- [client, server] ]
    return ()

  where
    serverMsgTracer = nullTracer

    currentChainHeaders =
      AnchoredFragment.mapAnchoredFragment blockHeader currentChain

    candidateChainHeaders =
      Map.fromList $ zip [1..] $
      map (AnchoredFragment.mapAnchoredFragment blockHeader) candidateChains

    anchoredChainPoints c = anchorPoint c
                          : map blockPoint (AnchoredFragment.toOldestFirst c)

    blockFetch :: FetchClientRegistry Int BlockHeader m
               -> TestFetchedBlockHeap m Block
               -> m ()
    blockFetch registry blockHeap =
        blockFetchLogic
          decisionTracer clientStateTracer
          (sampleBlockFetchPolicy1 blockHeap currentChainHeaders candidateChainHeaders)
          registry
        >> return ()

    driver :: TestFetchedBlockHeap m Block -> m ()
    driver blockHeap = do
      atomically $ do
        heap <- getTestFetchedBlocks blockHeap
        check $
          all (\c -> AnchoredFragment.headPoint c `Set.member` heap)
              candidateChains


--
-- Sample block fetch configurations
--

sampleBlockFetchPolicy1 :: (MonadSTM m, HasHeader header, HasHeader block)
                        => TestFetchedBlockHeap m block
                        -> AnchoredFragment header
                        -> Map peer (AnchoredFragment header)
                        -> BlockFetchConsensusInterface peer header block m
sampleBlockFetchPolicy1 blockHeap currentChain candidateChains =
    BlockFetchConsensusInterface {
      readCandidateChains    = return candidateChains,
      readCurrentChain       = return currentChain,
      readFetchMode          = return FetchModeBulkSync,
      readFetchedBlocks      = flip Set.member <$>
                                 getTestFetchedBlocks blockHeap,
      addFetchedBlock        = addTestFetchedBlock blockHeap,

      plausibleCandidateChain,
      compareCandidateChains,

      blockFetchSize         = \_ -> 2000,
      blockMatchesHeader     = \_ _ -> True
    }
  where
    plausibleCandidateChain cur candidate =
      AnchoredFragment.headBlockNo candidate > AnchoredFragment.headBlockNo cur

    compareCandidateChains c1 c2 =
      AnchoredFragment.headBlockNo c1 `compare` AnchoredFragment.headBlockNo c2



--
-- Utils to run fetch clients and servers
--

runFetchClient :: (MonadCatch m, MonadAsync m, MonadST m, Ord peerid,
                   Serialise header,
                   Serialise block,
                   Serialise (HeaderHash header))
                => Tracer m (TraceSendRecv (BlockFetch header block))
                -> FetchClientRegistry peerid header m
                -> peerid
                -> Channel m LBS.ByteString
                -> (  FetchClientStateVars m header
                   -> PeerPipelined (BlockFetch header block)
                                    AsClient BFIdle m a)
                -> m a
runFetchClient tracer registry peerid channel client =
    bracketFetchClient registry peerid $ \stateVars ->
      runPipelinedPeer 10 tracer (codecBlockFetch encode encode decode decode) channel (client stateVars)

runFetchServer :: (MonadThrow m, MonadST m,
                   Serialise header,
                   Serialise block,
                   Serialise (HeaderHash header))
                => Tracer m (TraceSendRecv (BlockFetch header block))
                -> Channel m LBS.ByteString
                -> BlockFetchServer header block m a
                -> m a
runFetchServer tracer channel server =
    runPeer tracer (codecBlockFetch encode encode decode decode) channel (blockFetchServerPeer server)

runFetchClientAndServerAsync
               :: (MonadCatch m, MonadAsync m, MonadST m, Ord peerid,
                   Serialise header,
                   Serialise block,
                   Serialise (HeaderHash header))
                => Tracer m (TraceSendRecv (BlockFetch header block))
                -> Tracer m (TraceSendRecv (BlockFetch header block))
                -> FetchClientRegistry peerid header m
                -> peerid
                -> (  FetchClientStateVars m header
                   -> PeerPipelined (BlockFetch header block)
                                    AsClient BFIdle m a)
                -> BlockFetchServer header block m b
                -> m (Async m a, Async m b)
runFetchClientAndServerAsync clientTracer serverTracer
                             registry peerid client server = do
    (clientChannel, serverChannel) <- createConnectedChannels

    clientAsync <- async $ runFetchClient
                             clientTracer
                             registry peerid
                             clientChannel client

    serverAsync <- async $ runFetchServer
                             serverTracer
                             serverChannel server

    return (clientAsync, serverAsync)


--
-- Block fetch clients with example policies and connected to mock block heaps
--

-- | An instantiation of the 'blockFetchClient' with a 'FetchClientPolicy'
-- using a 'TestFetchedBlockHeap' and simplistic example choices
-- for size and validation.
--
mockedBlockFetchClient1 :: (MonadSTM m, MonadTime m, MonadThrow m,
                            HasHeader header, HasHeader block,
                            HeaderHash header ~ HeaderHash block
                          , Show block)
                        => Tracer m (TraceFetchClientState header)
                        -> TestFetchedBlockHeap m block
                        -> FetchClientStateVars m header
                        -> PeerPipelined (BlockFetch header block)
                                         AsClient BFIdle m ()
mockedBlockFetchClient1 clientStateTracer blockHeap clientStateVars =
    blockFetchClient
      clientStateTracer
      (mockFetchClientPolicy1 blockHeap)
      clientStateVars

mockFetchClientPolicy1 :: TestFetchedBlockHeap m block
                       -> FetchClientPolicy header block m
mockFetchClientPolicy1 blockHeap =
    FetchClientPolicy {
      blockFetchSize     = \_   -> 2000,
      blockMatchesHeader = \_ _ -> True,
      addFetchedBlock    = addTestFetchedBlock blockHeap
    }

-- | Roughly 10ms ping time and 1MBit\/s bandwidth, leads to ~2200 bytes in
-- flight minimum.
--
exampleFixedPeerGSVs :: PeerGSV
exampleFixedPeerGSVs =
    PeerGSV{outboundGSV, inboundGSV}
  where
    inboundGSV  = ballisticGSV 10e-3 10e-6 (degenerateDistribution 0)
    outboundGSV = inboundGSV


--
-- Mock block fetch servers
--

-- | A demo server for the block fetch protocol.
--
-- It serves up ranges on a single given 'ChainFragment'. It does not simulate
-- any delays, so is not suitable for timing-accurate simulations.
--
mockBlockFetchServer1 :: forall header block m.
                        (MonadSTM m, HasHeader block,
                         HeaderHash header ~ HeaderHash block)
                      => ChainFragment block
                      -> BlockFetchServer header block m ()
mockBlockFetchServer1 chain =
    senderSide
  where
    senderSide :: BlockFetchServer header block m ()
    senderSide = BlockFetchServer receiveReq ()

    receiveReq :: ChainRange header
               -> m (BlockFetchBlockSender header block m ())
    receiveReq (ChainRange lpoint upoint) =
      -- We can only assert this for tests, not for the real thing.
      assert (pointSlot lpoint <= pointSlot upoint) $
      case ChainFragment.sliceRange chain
             (castPoint lpoint) (castPoint upoint) of
        Nothing     -> return $ SendMsgNoBlocks (return senderSide)
        Just chain' -> return $ SendMsgStartBatch (sendBlocks blocks)
          where blocks = ChainFragment.toOldestFirst chain'


    sendBlocks :: [block] -> m (BlockFetchSendBlocks header block m ())
    sendBlocks []     = return $ SendMsgBatchDone (return senderSide)
    sendBlocks (b:bs) = return $ SendMsgBlock b (sendBlocks bs)


--
-- Mock downloaded block heap
--

-- | This provides an interface to a collection of dowloaded blocks. This is
-- enough to implement the 'addFetchedBlock' and 'readFetchedBlocks' methods
-- in the 'BlockFetchConsensusInterface' and related interfaces.
--
-- The interface is enough to use in examples and tests.
--
data TestFetchedBlockHeap m block = TestFetchedBlockHeap {
       getTestFetchedBlocks  :: STM m (Set (Point block)),
       addTestFetchedBlock   :: Point block -> block -> m ()
     }

-- | Make a 'TestFetchedBlockHeap' using a simple in-memory 'Map', stored in an
-- 'STM' 'TVar'.
--
-- This is suitable for examples and tests.
--
mkTestFetchedBlockHeap :: (MonadSTM m, Ord (Point block))
                       => [Point block]
                       -> m (TestFetchedBlockHeap m block)
mkTestFetchedBlockHeap points = do
    v <- atomically (newTVar (Set.fromList points))
    return TestFetchedBlockHeap {
      getTestFetchedBlocks = readTVar v,
      addTestFetchedBlock  = \p _b -> atomically (modifyTVar' v (Set.insert p))
    }
