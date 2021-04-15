{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE Rank2Types          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

module Ouroboros.Network.BlockFetch.Examples (
    blockFetchExample0,
    blockFetchExample1,
    mockBlockFetchServer1,
    exampleFixedPeerGSVs,
  ) where

import           Codec.Serialise (Serialise (..))
import qualified Data.ByteString.Lazy as LBS
import           Data.List (foldl')
import           Data.Map (Map)
import qualified Data.Map.Strict as Map
import           Data.Maybe (fromMaybe)
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Typeable (Typeable)

import           Control.Exception (assert)
import           Control.Monad (forever)
import           Control.Monad.Class.MonadAsync
import           Control.Monad.Class.MonadFork
import           Control.Monad.Class.MonadST
import           Control.Monad.Class.MonadSTM.Strict
import           Control.Monad.Class.MonadThrow
import           Control.Monad.Class.MonadTime
import           Control.Monad.Class.MonadTimer
import           Control.Tracer (Tracer, contramap, nullTracer)

import           Ouroboros.Network.AnchoredFragment (AnchoredFragment,
                     anchorPoint)
import qualified Ouroboros.Network.AnchoredFragment as AnchoredFragment
import           Ouroboros.Network.Block

import           Network.TypedProtocol.Core
import           Network.TypedProtocol.Pipelined
import           Ouroboros.Network.Mux (ControlMessageSTM)

import           Ouroboros.Network.BlockFetch
import           Ouroboros.Network.BlockFetch.Client
import           Ouroboros.Network.Channel
import           Ouroboros.Network.DeltaQ
import           Ouroboros.Network.Driver
import           Ouroboros.Network.NodeToNode (NodeToNodeVersion (..))
import           Ouroboros.Network.Protocol.BlockFetch.Codec
import           Ouroboros.Network.Protocol.BlockFetch.Server
import           Ouroboros.Network.Protocol.BlockFetch.Type
import           Ouroboros.Network.Util.ShowProxy

import           Ouroboros.Network.Testing.ConcreteBlock


-- | Run a single block fetch protocol until the chain is downloaded.
--
blockFetchExample0 :: forall m.
                      (MonadSTM m, MonadST m, MonadAsync m, MonadFork m,
                       MonadTime m, MonadTimer m, MonadMask m, MonadThrow (STM m))
                   => Tracer m [TraceLabelPeer Int
                                 (FetchDecision [Point BlockHeader])]
                   -> Tracer m (TraceLabelPeer Int
                                 (TraceFetchClientState BlockHeader))
                   -> Tracer m (TraceLabelPeer Int
                                 (TraceSendRecv (BlockFetch Block (Point Block))))
                   -> Maybe DiffTime -- ^ client's channel delay
                   -> Maybe DiffTime -- ^ servers's channel delay
                   -> ControlMessageSTM m
                   -> AnchoredFragment Block -- ^ Fixed current chain
                   -> AnchoredFragment Block -- ^ Fixed candidate chain
                   -> m ()
blockFetchExample0 decisionTracer clientStateTracer clientMsgTracer
                   clientDelay serverDelay
                   controlMessageSTM
                   currentChain candidateChain = do

    registry    <- newFetchClientRegistry :: m (FetchClientRegistry Int BlockHeader Block m)
    blockHeap   <- mkTestFetchedBlockHeap (anchoredChainPoints currentChain)

    (clientAsync, serverAsync, syncClientAsync, keepAliveAsync)
                <- runFetchClientAndServerAsync
                    (contramap (TraceLabelPeer peerno) clientMsgTracer)
                    (contramap (TraceLabelPeer peerno) serverMsgTracer)
                    clientDelay serverDelay
                    registry peerno
                    (blockFetchClient NodeToNodeV_1 controlMessageSTM (\_ _ _ -> pure ()))
                    (mockBlockFetchServer1 candidateChain)

    fetchAsync  <- async $ do
      threadId <- myThreadId
      labelThread threadId "block-fetch-logic"
      blockFetch registry blockHeap
    driverAsync <- async $ do
      threadId <- myThreadId
      labelThread threadId "driver"
      driver blockHeap

    -- Order of shutdown here is important for this example: must kill off the
    -- fetch thread before the peer threads.
    _ <- waitAnyCancel $ [ fetchAsync, driverAsync,
                           clientAsync, serverAsync,
                           syncClientAsync, keepAliveAsync]
    return ()

  where
    peerno = 1 :: Int

    serverMsgTracer = nullTracer

    currentChainHeaders =
      AnchoredFragment.mapAnchoredFragment blockHeader currentChain

    candidateChainHeaders =
      Map.fromList $ zip [1..] $
      map (AnchoredFragment.mapAnchoredFragment blockHeader) [candidateChain]

    anchoredChainPoints c = anchorPoint c
                          : map blockPoint (AnchoredFragment.toOldestFirst c)

    blockFetch :: FetchClientRegistry Int BlockHeader Block m
               -> TestFetchedBlockHeap m Block
               -> m ()
    blockFetch registry blockHeap =
        blockFetchLogic
          decisionTracer clientStateTracer
          (sampleBlockFetchPolicy1 headerForgeUTCTime blockHeap currentChainHeaders candidateChainHeaders)
          registry
          (BlockFetchConfiguration {
            bfcMaxConcurrencyBulkSync = 1,
            bfcMaxConcurrencyDeadline = 2,
            bfcMaxRequestsInflight    = 10,
            bfcDecisionLoopInterval   = 0.01,
            bfcSalt                   = 0
          })
        >> return ()

    headerForgeUTCTime (FromConsensus x) =
        pure $ convertSlotToTimeForTestsAssumingNoHardFork (blockSlot x)

    driver :: TestFetchedBlockHeap m Block -> m ()
    driver blockHeap = do
      atomically $ do
        heap <- getTestFetchedBlocks blockHeap
        check $
          all (\c -> AnchoredFragment.headPoint c `Set.member` heap)
              [candidateChain]


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
blockFetchExample1 :: forall m.
                      (MonadSTM m, MonadST m, MonadAsync m, MonadFork m,
                       MonadTime m, MonadTimer m, MonadMask m, MonadThrow (STM m))
                   => Tracer m [TraceLabelPeer Int
                                 (FetchDecision [Point BlockHeader])]
                   -> Tracer m (TraceLabelPeer Int
                                 (TraceFetchClientState BlockHeader))
                   -> Tracer m (TraceLabelPeer Int
                                 (TraceSendRecv (BlockFetch Block (Point Block))))
                   -> Maybe DiffTime -- ^ client's channel delay
                   -> Maybe DiffTime -- ^ server's channel delay
                   -> ControlMessageSTM m
                   -> AnchoredFragment Block   -- ^ Fixed current chain
                   -> [AnchoredFragment Block] -- ^ Fixed candidate chains
                   -> m ()
blockFetchExample1 decisionTracer clientStateTracer clientMsgTracer
                   clientDelay serverDelay
                   controlMessageSTM
                   currentChain candidateChains = do

    registry    <- newFetchClientRegistry
    blockHeap   <- mkTestFetchedBlockHeap (anchoredChainPoints currentChain)

    peerAsyncs  <- sequence
                    [ runFetchClientAndServerAsync
                        (contramap (TraceLabelPeer peerno) clientMsgTracer)
                        (contramap (TraceLabelPeer peerno) serverMsgTracer)
                        clientDelay serverDelay
                        registry peerno
                        (blockFetchClient NodeToNodeV_1 controlMessageSTM (\_ _ _ -> pure ()))
                        (mockBlockFetchServer1 candidateChain)
                    | (peerno, candidateChain) <- zip [1..] candidateChains
                    ]
    fetchAsync  <- async $ do
      threadId <- myThreadId
      labelThread threadId "block-fetch-logic"
      blockFetch registry blockHeap
    driverAsync <- async $ do
      threadId <- myThreadId
      labelThread threadId "block-fetch-driver"
      driver blockHeap

    -- Order of shutdown here is important for this example: must kill off the
    -- fetch thread before the peer threads.
    _ <- waitAnyCancel $ [ fetchAsync, driverAsync ]
                      ++ [ peerAsync
                         | (client, server, sync, ks) <- peerAsyncs
                         , peerAsync <- [client, server, sync, ks] ]
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

    blockFetch :: FetchClientRegistry Int BlockHeader Block m
               -> TestFetchedBlockHeap m Block
               -> m ()
    blockFetch registry blockHeap =
        blockFetchLogic
          decisionTracer clientStateTracer
          (sampleBlockFetchPolicy1 headerForgeUTCTime blockHeap currentChainHeaders candidateChainHeaders)
          registry
          (BlockFetchConfiguration {
            bfcMaxConcurrencyBulkSync = 1,
            bfcMaxConcurrencyDeadline = 2,
            bfcMaxRequestsInflight    = 10,
            bfcDecisionLoopInterval   = 0.01,
            bfcSalt                   = 0
          })
        >> return ()

    headerForgeUTCTime (FromConsensus x) =
        pure $ convertSlotToTimeForTestsAssumingNoHardFork (blockSlot x)

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
                        => (forall x. HasHeader x => FromConsensus x -> STM m UTCTime)
                        -> TestFetchedBlockHeap m block
                        -> AnchoredFragment header
                        -> Map peer (AnchoredFragment header)
                        -> BlockFetchConsensusInterface peer header block m
sampleBlockFetchPolicy1 headerFieldsForgeUTCTime blockHeap currentChain candidateChains =
    BlockFetchConsensusInterface {
      readCandidateChains    = return candidateChains,
      readCurrentChain       = return currentChain,
      readFetchMode          = return FetchModeBulkSync,
      readFetchedBlocks      = flip Set.member <$>
                                 getTestFetchedBlocks blockHeap,
      readFetchedMaxSlotNo   = foldl' max NoMaxSlotNo .
                               map (maxSlotNoFromWithOrigin . pointSlot) .
                               Set.elems <$>
                               getTestFetchedBlocks blockHeap,
      addFetchedBlock        = addTestFetchedBlock blockHeap,

      plausibleCandidateChain,
      compareCandidateChains,

      blockFetchSize         = \_ -> 2000,
      blockMatchesHeader     = \_ _ -> True,

      headerForgeUTCTime     = headerFieldsForgeUTCTime,
      blockForgeUTCTime      = headerFieldsForgeUTCTime
      }
  where
    plausibleCandidateChain cur candidate =
      AnchoredFragment.headBlockNo candidate > AnchoredFragment.headBlockNo cur

    compareCandidateChains c1 c2 =
      AnchoredFragment.headBlockNo c1 `compare` AnchoredFragment.headBlockNo c2

-- | Roughly 10ms ping time and 1MBit\/s bandwidth, leads to ~2200 bytes in
-- flight minimum.
--
exampleFixedPeerGSVs :: PeerGSV
exampleFixedPeerGSVs =
    PeerGSV{sampleTime, outboundGSV, inboundGSV}
  where
    inboundGSV  = ballisticGSV 10e-3 10e-6 (degenerateDistribution 0)
    outboundGSV = inboundGSV
    sampleTime  = Time 0


--
-- Utils to run fetch clients and servers
--

runFetchClient :: (MonadAsync m, MonadFork m, MonadMask m, MonadThrow (STM m),
                   MonadST m, MonadTime m, MonadTimer m,
                   Ord peerid, Serialise block, Serialise point,
                   Typeable block, ShowProxy block)
                => Tracer m (TraceSendRecv (BlockFetch block point))
                -> FetchClientRegistry peerid header block m
                -> peerid
                -> Channel m LBS.ByteString
                -> (  FetchClientContext header block m
                   -> PeerPipelined (BlockFetch block point) AsClient BFIdle m a)
                -> m a
runFetchClient tracer registry peerid channel client =
    bracketFetchClient registry peerid $ \clientCtx ->
      fst <$>
        runPipelinedPeerWithLimits tracer codec (byteLimitsBlockFetch (fromIntegral . LBS.length))
          timeLimitsBlockFetch channel (client clientCtx)
  where
    codec = codecBlockFetch encode decode encode decode

runFetchServer :: (MonadAsync m, MonadFork m, MonadMask m, MonadThrow (STM m),
                   MonadST m, MonadTime m, MonadTimer m,
                   Serialise block, Serialise point,
                   Typeable block,
                   ShowProxy block)
                => Tracer m (TraceSendRecv (BlockFetch block point))
                -> Channel m LBS.ByteString
                -> BlockFetchServer block point m a
                -> m a
runFetchServer tracer channel server =
    fst <$>
      runPeerWithLimits tracer codec (byteLimitsBlockFetch (fromIntegral . LBS.length))
        timeLimitsBlockFetch channel (blockFetchServerPeer server)
  where
    codec = codecBlockFetch encode decode encode decode

runFetchClientAndServerAsync
               :: forall peerid block header m a b.
                  (MonadAsync m, MonadFork m, MonadMask m, MonadThrow (STM m),
                   MonadST m, MonadTime m, MonadTimer m,
                   Ord peerid, Show peerid,
                   Serialise header, Serialise block,
                   Serialise (HeaderHash block),
                   Typeable block,
                   ShowProxy block)
                => Tracer m (TraceSendRecv (BlockFetch block (Point block)))
                -> Tracer m (TraceSendRecv (BlockFetch block (Point block)))
                -> Maybe DiffTime -- ^ client's channel delay
                -> Maybe DiffTime -- ^ server's channel delay
                -> FetchClientRegistry peerid header block m
                -> peerid
                -> (  FetchClientContext header block m
                   -> PeerPipelined (BlockFetch block (Point block)) AsClient BFIdle m a)
                -> BlockFetchServer block (Point block) m b
                -> m (Async m a, Async m b, Async m (), Async m ())
runFetchClientAndServerAsync clientTracer serverTracer
                             clientDelay  serverDelay
                             registry peerid client server = do
    (clientChannel, serverChannel) <- createConnectedChannels

    clientAsync <- async $ do
      threadId <- myThreadId
      labelThread threadId ("block-fetch-client-" ++ show peerid)
      runFetchClient
        clientTracer
        registry peerid
        (fromMaybe id (delayChannel <$> clientDelay) clientChannel)
        client

    serverAsync <- async $ do
      threadId <- myThreadId
      labelThread threadId ("block-fetch-server-" ++ show peerid)
      runFetchServer
        serverTracer
        (fromMaybe id (delayChannel <$> serverDelay) serverChannel)
        server

    -- we are tagging messages with the current peerid, not the target
    -- one, this is different than what's intended but it's fine to do that in
    -- these examples;
    syncClientAsync <- async $ do
      threadId <- myThreadId
      labelThread threadId ("registry-" ++ show peerid)
      bracketSyncWithFetchClient
        registry peerid
        (forever (threadDelay 1000) >> return ())
    keepAliveAsync <- async $ do
      threadId <- myThreadId
      labelThread threadId ("keep-alive-" ++ show peerid)
      bracketKeepAliveClient
        registry peerid
        (\_ -> forever (threadDelay 1000) >> return ())

    return (clientAsync, serverAsync, syncClientAsync, keepAliveAsync)


--
-- Mock block fetch servers
--

-- | A demo server for the block fetch protocol.
--
-- It serves up ranges on a single given 'AnchoredFragment'. It does not
-- simulate any delays, so is not suitable for timing-accurate simulations.
--
mockBlockFetchServer1 :: forall block m.
                        (MonadSTM m, HasHeader block)
                      => AnchoredFragment block
                      -> BlockFetchServer block (Point block) m ()
mockBlockFetchServer1 chain =
    senderSide
  where
    senderSide :: BlockFetchServer block (Point block) m ()
    senderSide = BlockFetchServer receiveReq ()

    receiveReq :: ChainRange (Point block)
               -> m (BlockFetchBlockSender block (Point block) m ())
    receiveReq (ChainRange lpoint upoint) =
      -- We can only assert this for tests, not for the real thing.
      assert (pointSlot lpoint <= pointSlot upoint) $
      case AnchoredFragment.sliceRange chain lpoint upoint of
        Nothing     -> return $ SendMsgNoBlocks (return senderSide)
        Just chain' -> return $ SendMsgStartBatch (sendBlocks blocks)
          where blocks = AnchoredFragment.toOldestFirst chain'


    sendBlocks :: [block] -> m (BlockFetchSendBlocks block (Point block) m ())
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
       getTestFetchedBlocks :: STM m (Set (Point block)),
       addTestFetchedBlock  :: Point block -> block -> m ()
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
    v <- newTVarIO (Set.fromList points)
    return TestFetchedBlockHeap {
      getTestFetchedBlocks = readTVar v,
      addTestFetchedBlock  = \p _b -> atomically (modifyTVar v (Set.insert p))
    }
