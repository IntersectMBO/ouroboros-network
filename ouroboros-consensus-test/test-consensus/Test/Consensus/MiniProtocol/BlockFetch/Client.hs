{-# LANGUAGE BlockArguments             #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TupleSections              #-}

-- | A test for the consensus-specific parts of the BlockFetch client.
--
-- When adding a block to the ChainDB, we allocate potential punishments, which
-- are later invoked after block validation, crucially allowing us to kill the
-- BlockFetch client and hence disconnect from malicious peers.
--
-- This test spins up several BlockFetch clients, which download randomly
-- generated chains and add them to the ChainDB, which will enact these
-- punishments on validation. We check both for false positives and negatives in
-- the process.
module Test.Consensus.MiniProtocol.BlockFetch.Client (tests) where

import           Control.Monad (replicateM)
import           Control.Monad.Class.MonadTime
import           Control.Monad.IOSim (runSimOrThrow)
import           Control.Tracer (Tracer (..), nullTracer, traceWith)
import           Data.Bifunctor (first)
import           Data.Either (isLeft)
import           Data.Functor.Contravariant ((>$<))
import           Data.Hashable (Hashable)
import           Data.List (intercalate)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Traversable (for)

import           Network.TypedProtocol.Codec (AnyMessageAndAgency (..))

import           Test.QuickCheck
import           Test.Tasty
import           Test.Tasty.QuickCheck
import           Test.Util.ChainDB

import           Ouroboros.Network.AnchoredFragment (AnchoredFragment)
import qualified Ouroboros.Network.AnchoredFragment as AF
import           Ouroboros.Network.BlockFetch (BlockFetchConfiguration (..),
                     BlockFetchConsensusInterface, FetchMode (..),
                     blockFetchLogic, bracketFetchClient,
                     bracketKeepAliveClient, bracketSyncWithFetchClient,
                     newFetchClientRegistry)
import           Ouroboros.Network.BlockFetch.Client (blockFetchClient)
import           Ouroboros.Network.Channel (createConnectedChannels)
import qualified Ouroboros.Network.Driver.Simple as Driver
import           Ouroboros.Network.MockChain.Chain (Chain)
import qualified Ouroboros.Network.MockChain.Chain as Chain
import qualified Ouroboros.Network.MockChain.ProducerState as CPS
import qualified Ouroboros.Network.Mux as Mux
import           Ouroboros.Network.NodeToNode.Version (NodeToNodeVersion,
                     isPipeliningEnabled)
import           Ouroboros.Network.Protocol.BlockFetch.Codec (codecBlockFetchId)
import           Ouroboros.Network.Protocol.BlockFetch.Server
                     (BlockFetchBlockSender (SendMsgNoBlocks, SendMsgStartBatch),
                     BlockFetchSendBlocks (SendMsgBatchDone, SendMsgBlock),
                     BlockFetchServer (..), blockFetchServerPeer)
import           Ouroboros.Network.Protocol.BlockFetch.Type (ChainRange (..),
                     Message (MsgBlock))
import           Ouroboros.Network.Protocol.ChainSync.ClientPipelined
                     (chainSyncClientPeerPipelined)
import           Ouroboros.Network.Protocol.ChainSync.Codec (codecChainSyncId)
import           Ouroboros.Network.Protocol.ChainSync.Examples
                     (chainSyncServerExample)
import           Ouroboros.Network.Protocol.ChainSync.PipelineDecision
                     (pipelineDecisionLowHighMark)
import           Ouroboros.Network.Protocol.ChainSync.Server
                     (chainSyncServerPeer)

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Config
import qualified Ouroboros.Consensus.MiniProtocol.BlockFetch.ClientInterface as BlockFetchClientInterface
import           Ouroboros.Consensus.MiniProtocol.ChainSync.Client
                     (ChainSyncClientException, bracketChainSyncClient,
                     chainSyncClient)
import qualified Ouroboros.Consensus.MiniProtocol.ChainSync.Client as ChainSyncClient
import           Ouroboros.Consensus.Node.ProtocolInfo (NumCoreNodes (..))
import qualified Ouroboros.Consensus.Storage.ChainDB.API as ChainDB
import           Ouroboros.Consensus.Storage.ChainDB.Impl (ChainDbArgs (..))
import qualified Ouroboros.Consensus.Storage.ChainDB.Impl as ChainDBImpl
import           Ouroboros.Consensus.Util.Condense (Condense (..))
import           Ouroboros.Consensus.Util.Exception (catchAlsoLinked)
import           Ouroboros.Consensus.Util.IOLike
import           Ouroboros.Consensus.Util.ResourceRegistry
import           Ouroboros.Consensus.Util.STM (forkLinkedWatcher)

import           Test.Util.ChainUpdates
import qualified Test.Util.LogicalClock as LogicalClock
import           Test.Util.LogicalClock (Tick (..))
import           Test.Util.Orphans.IOLike ()
import           Test.Util.Schedule
import           Test.Util.TestBlock
import           Test.Util.Time (dawnOfTime)
import           Test.Util.Tracer (recordingTracerTVar)

tests :: TestTree
tests = testGroup "BlockFetchClient"
    [ testProperty "blockFetch" prop_blockFetch
    ]

prop_blockFetch :: BlockFetchClientTestSetup -> Property
prop_blockFetch bfcts@BlockFetchClientTestSetup{..} =
    counterexample ("Trace:\n" <> unlines (ppTrace <$> bfcoTrace)) $
    counterexample (condense bfcts) $
    conjoin $
      [ counterexample ("Classified behavior: " <> show classifiedBehavior) $
        case behaviorValidity classifiedBehavior of
          Valid -> conjoin
            [ noException ("BlockFetch client " <> condense peerId) blockFetchRes
            , noException ("ChainSync client " <> condense peerId) chainSyncRes
            ]
          Invalid ->
            counterexample "Invalid behavior not caught" $
            tabulateFailureMode $
            property (isLeft blockFetchRes || isLeft chainSyncRes)
      | (peerId, PeerOutcome{..}) <- Map.toList bfcoPeerOutcomes
      , let classifiedBehavior =
              classifyBehavior (joinSchedule $ peerUpdates Map.! peerId)
            tabulateFailureMode = tabulate "Expected failure due to" $
              pure . intercalate ","  $
                   ["BlockFetch" | isLeft blockFetchRes]
                <> ["ChainSync"  | isLeft chainSyncRes]
      ] <>
      [ Map.keysSet bfcoPeerOutcomes === Map.keysSet peerUpdates
      , counterexample ("Fetched blocks per peer: " <> condense bfcoFetchedBlocks) $
        property $ all (> 0) bfcoFetchedBlocks
      ]
  where
    BlockFetchClientOutcome{..} = runSimOrThrow $ runBlockFetchTest bfcts

    noException msg = \case
      Right () -> property ()
      Left ex  ->
        counterexample (msg <> ": exception: " <> displayException ex) False

    ppTrace (Tick tick, ev) = show tick <> ": " <> ev

{-------------------------------------------------------------------------------
  Running a test involving the consensus BlockFetch interface
-------------------------------------------------------------------------------}

data BlockFetchClientOutcome = BlockFetchClientOutcome {
    bfcoPeerOutcomes  :: Map PeerId PeerOutcome
  , bfcoFetchedBlocks :: Map PeerId Word
  , -- | Trace messages, only used for debugging.
    bfcoTrace         :: [(Tick, String)]
  }

data PeerOutcome = PeerOutcome {
    blockFetchRes :: Either SomeException ()
  , chainSyncRes  :: Either ChainSyncClientException ()
  }

runBlockFetchTest ::
     forall m.
     (IOLike m, MonadTime m)
  => BlockFetchClientTestSetup
  -> m BlockFetchClientOutcome
runBlockFetchTest BlockFetchClientTestSetup{..} = withRegistry \registry -> do
    varCandidates     <- uncheckedNewTVarM Map.empty
    varControlMessage <- uncheckedNewTVarM Mux.Continue
    varFetchedBlocks  <- uncheckedNewTVarM (0 <$ peerUpdates)
    varChains         <- uncheckedNewTVarM (Chain.Genesis <$ peerUpdates)
    varChainSyncRes   <- uncheckedNewTVarM (Right () <$ peerUpdates)
    producerStateVars <- for peerUpdates $ const $
      uncheckedNewTVarM $ CPS.initChainProducerState Chain.Genesis

    fetchClientRegistry <- newFetchClientRegistry
    clock               <- LogicalClock.new registry $
      LogicalClock.sufficientTimeFor $ lastTick <$> Map.elems peerUpdates
    (tracer, getTrace)  <-
      first (LogicalClock.tickTracer clock) <$> recordingTracerTVar
    chainDB             <- mkChainDB registry tracer

    let blockFetchConsensusInterface =
          mkTestBlockFetchConsensusInterface
            (readTVar varCandidates >>= traverse readTVar)
            chainDB

    _ <- forkLinkedThread registry "BlockFetchLogic" $
      blockFetchLogic
        nullTracer
        nullTracer
        blockFetchConsensusInterface
        fetchClientRegistry
        blockFetchCfg

    let runBlockFetchClient peerId =
          bracketFetchClient fetchClientRegistry ntnVersion isPipeliningEnabled peerId \clientCtx -> do
            let bfClient = blockFetchClient
                    ntnVersion
                    (readTVar varControlMessage)
                    nullTracer
                    clientCtx
                bfServer =
                    blockFetchServerPeer $ mockBlockFetchServer getCurrentChain
                  where
                    getCurrentChain =
                          chainToAnchoredFragment . (Map.! peerId)
                      <$> readTVarIO varChains

                blockFetchTracer = Tracer \case
                    (Driver.Client, ev) -> do
                      atomically case ev of
                        Driver.TraceRecvMsg (AnyMessageAndAgency _ (MsgBlock _)) ->
                           modifyTVar varFetchedBlocks $ Map.adjust (+ 1) peerId
                        _ -> pure ()
                      traceWith tracer $
                        show peerId <> ": BlockFetchClient: " <> show ev
                    _ -> pure ()
            fst <$> Driver.runConnectedPeersPipelined
              createConnectedChannels
              blockFetchTracer
              codecBlockFetchId
              bfClient
              bfServer

        forkTicking peerId =
            forkLinkedWatcher registry ("TickWatcher " <> condense peerId) $
              LogicalClock.tickWatcher clock \tick -> atomically do
                let updates = toChainUpdates $
                      Map.findWithDefault [] tick $
                        getSchedule $ peerUpdates Map.! peerId
                    updateChain chain =
                      case Chain.applyChainUpdates updates chain of
                        Just chain' -> chain'
                        Nothing     -> error "Chain update failed"
                    updateCPS cps =
                      case CPS.applyChainUpdates (fmap TestHeader <$> updates) cps of
                        Just cps' -> cps'
                        Nothing   -> error "Chain update failed"
                modifyTVar varChains $ Map.adjust updateChain peerId
                modifyTVar (producerStateVars Map.! peerId) updateCPS

        forkChainSync peerId =
            forkThread registry ("BracketSync" <> condense peerId) $
            recordChainSyncExceptions $
            bracketSyncWithFetchClient fetchClientRegistry peerId $ do
              let chainSyncTracer =
                        (\ev -> show peerId <> ": ChainSyncClient: " <> show ev)
                    >$< tracer
                  chainDbView = (ChainSyncClient.defaultChainDbView chainDB) {
                      ChainSyncClient.getCurrentChain =
                        pure $ AF.Empty AF.AnchorGenesis
                    }
                  client =
                    chainSyncClient
                      (pipelineDecisionLowHighMark 10 20)
                      chainSyncTracer
                      topLevelConfig
                      chainDbView
                      ntnVersion
                      (pure Mux.Continue)
                      nullTracer
                  server =
                    chainSyncServerExample () (producerStateVars Map.! peerId)
              bracketChainSyncClient
                chainSyncTracer
                chainDbView
                varCandidates
                peerId
                ntnVersion $ \varCandidate ->
                  Driver.runConnectedPeersPipelined
                    createConnectedChannels
                    nullTracer
                    codecChainSyncId
                    (chainSyncClientPeerPipelined $ client varCandidate)
                    (chainSyncServerPeer server)
          where
            recordChainSyncExceptions = flip catchAlsoLinked $ \ex -> do
              atomically $ modifyTVar varChainSyncRes $
                Map.insert peerId (Left ex)
              throwIO ex

        -- The BlockFetch logic requires initializing the KeepAlive
        -- miniprotocol, even if it does not do anything.
        forkKeepAlive peerId =
          forkLinkedThread registry "KeepAlive" $
            bracketKeepAliveClient fetchClientRegistry peerId \_ ->
              infiniteDelay

    blockFetchThreads <- Map.fromList <$> for peerIds \peerId -> do
      _ <- forkTicking   peerId
      _ <- forkChainSync peerId
      _ <- forkKeepAlive peerId
      fmap (peerId,) $
        forkThread registry ("BlockFetch " <> condense peerId) $
          try $ runBlockFetchClient peerId

    LogicalClock.waitUntilDone clock
    atomically $ writeTVar varControlMessage Mux.Terminate

    bfcoPeerOutcomes  <-
      flip Map.traverseWithKey blockFetchThreads \peerId threadId -> do
        blockFetchRes <- waitThread threadId
        chainSyncRes  <- (Map.! peerId) <$> readTVarIO varChainSyncRes
        pure PeerOutcome {..}
    bfcoFetchedBlocks <- readTVarIO varFetchedBlocks
    bfcoTrace         <- getTrace
    pure BlockFetchClientOutcome {..}
  where
    peerIds = Map.keys peerUpdates

    numCoreNodes = NumCoreNodes $ fromIntegral $ Map.size peerUpdates + 1

    -- Needs to be larger than any chain length in this test, to ensure that
    -- switching to any chain is never too deep.
    securityParam = SecurityParam 1000
    topLevelConfig = singleNodeTestConfigWithK securityParam

    mkChainDB ::
         ResourceRegistry m
      -> Tracer m String
      -> m (ChainDB.ChainDB m TestBlock)
    mkChainDB registry tracer = do
        chainDbArgs <- do
          nodeDBs <- emptyNodeDBs
          let args = fromMinimalChainDbArgs $ MinimalChainDbArgs {
                  mcdbTopLevelConfig = topLevelConfig
                , mcdbChunkInfo = mkTestChunkInfo topLevelConfig
                , mcdbInitLedger = testInitExtLedger
                , mcdbRegistry = registry
                , mcdbNodeDBs = nodeDBs
                }
          -- TODO: Test with more interesting behaviour for cdbCheckInFuture
          pure $ args { cdbTracer = cdbTracer }
        (_, (chainDB, ChainDBImpl.Internal{intAddBlockRunner})) <-
          allocate
            registry
            (\_ -> ChainDBImpl.openDBInternal chainDbArgs False)
            (ChainDB.closeDB . fst)
        _ <- forkLinkedThread registry "AddBlockRunner" intAddBlockRunner
        pure chainDB
      where
        cdbTracer = Tracer \case
            ChainDBImpl.TraceAddBlockEvent ev ->
              traceWith tracer $ "ChainDB: " <> show ev
            _ -> pure ()


    mkTestBlockFetchConsensusInterface ::
         STM m (Map PeerId (AnchoredFragment (Header TestBlock)))
      -> ChainDB.ChainDB m TestBlock
      -> BlockFetchConsensusInterface PeerId (Header TestBlock) TestBlock m
    mkTestBlockFetchConsensusInterface getCandidates chainDB =
        BlockFetchClientInterface.mkBlockFetchConsensusInterface
          (TestBlockConfig numCoreNodes)
          chainDbView
          getCandidates
          (\_hdr -> 1000) -- header size, only used for peer prioritization
          slotForgeTime
          (pure blockFetchMode)
      where
        -- Bogus implementation; this is fine as this is only used for
        -- enriching tracing information ATM.
        slotForgeTime :: BlockFetchClientInterface.SlotForgeTimeOracle m blk
        slotForgeTime _ = pure dawnOfTime

        chainDbView = BlockFetchClientInterface.ChainDbView {..}
          where
            -- Always return the empty chain such that the BlockFetch logic
            -- downloads all chains.
            getCurrentChain           = pure $ AF.Empty AF.AnchorGenesis
            getIsFetched              = ChainDB.getIsFetched chainDB
            getMaxSlotNo              = ChainDB.getMaxSlotNo chainDB
            addBlockWaitWrittenToDisk = ChainDB.addBlockWaitWrittenToDisk chainDB

mockBlockFetchServer ::
     forall m blk.
     (Monad m, HasHeader blk)
  => m (AnchoredFragment blk)
  -> BlockFetchServer blk (Point blk) m ()
mockBlockFetchServer getCurrentChain = idle
  where
    idle :: BlockFetchServer blk (Point blk) m ()
    idle = flip BlockFetchServer () \(ChainRange from to) -> do
        curChain <- getCurrentChain
        pure case AF.sliceRange curChain from to of
          Nothing    -> SendMsgNoBlocks (pure idle)
          Just slice -> SendMsgStartBatch $ sendBlocks (AF.toOldestFirst slice)

    sendBlocks :: [blk] -> m (BlockFetchSendBlocks blk (Point blk) m ())
    sendBlocks = pure . \case
      []         -> SendMsgBatchDone (pure idle)
      blk : blks -> SendMsgBlock blk (sendBlocks blks)

ntnVersion :: NodeToNodeVersion
ntnVersion = maxBound

{-------------------------------------------------------------------------------
  BlockFetchClientTestSetup
-------------------------------------------------------------------------------}

data BlockFetchClientTestSetup = BlockFetchClientTestSetup {
    -- | A 'Schedule' of 'ChainUpdate's for every peer. This emulates
    -- the candidate fragments provided by the ChainSync client.
    peerUpdates    :: Map PeerId (Schedule ChainUpdate)
    -- | BlockFetch 'FetchMode'
  , blockFetchMode :: FetchMode
  , blockFetchCfg  :: BlockFetchConfiguration
  }
  deriving stock (Show)

instance Condense BlockFetchClientTestSetup where
  condense BlockFetchClientTestSetup{..} = unlines
      [ "Number of peers: "
          <> show (Map.size peerUpdates)
      , "Chain updates:\n"
          <> ppPerPeer peerUpdates
      , "BlockFetch mode: " <> show blockFetchMode
      , "BlockFetch cfg: " <> show blockFetchCfg
      ]
    where
      ppPerPeer peerMap = unlines
        [ "  " <> show peerId <> ": " <> valLine
        | (peerId, val) <- Map.toAscList peerMap
        , valLine       <- lines $ condense val
        ]

instance Arbitrary BlockFetchClientTestSetup where
  arbitrary = do
      behavior <- elements
        [ SelectedChainBehavior
        , TentativeChainBehavior
        , InvalidChainBehavior
        ]
      numPeers <- case behaviorValidity behavior of
        -- Interaction of multiple clients can hide invalid behavior.
        Invalid -> pure 1
        Valid   -> chooseInt (1, 3)
      let peerIds = PeerId <$> [1 .. numPeers]
      peerUpdates <-
            Map.fromList . zip peerIds
        <$> replicateM numPeers (genUpdateSchedule behavior)
      blockFetchMode <- elements [FetchModeBulkSync, FetchModeDeadline]
      blockFetchCfg  <- do
        let -- ensure that we can download blocks from all peers
            bfcMaxConcurrencyBulkSync = fromIntegral numPeers
            bfcMaxConcurrencyDeadline = fromIntegral numPeers
            -- This is used to introduce a minimal delay between BlockFetch
            -- logic iterations in case the monitored state vars change too
            -- fast, which we don't have to worry about in this test.
            bfcDecisionLoopInterval   = 0
        bfcMaxRequestsInflight <- chooseEnum (2, 10)
        bfcSalt                <- arbitrary
        pure BlockFetchConfiguration {..}
      pure BlockFetchClientTestSetup {..}
    where
      genUpdateSchedule behavior =
          genChainUpdates behavior maxRollback 20 >>= genSchedule strat
        where
          strat = case behaviorValidity behavior of
            -- Multiple updates per tick can hide invalid behavior.
            Invalid -> SingleItemPerTickStrategy
            Valid   -> DefaultSchedulingStrategy

      -- Only use a small k to avoid rolling forward by a big chain.
      maxRollback = SecurityParam 5

  shrink BlockFetchClientTestSetup{..} =
      -- If we have multiple peers, check if removing the peer still
      -- yields an error
      [ BlockFetchClientTestSetup {
            peerUpdates = Map.delete peerId peerUpdates
          , ..
          }
      | length peerIds > 1
      , peerId <- peerIds
      ] <>
      -- Shrink the schedules for all peers simultaneously
      [ BlockFetchClientTestSetup {
            peerUpdates = Map.insert peerId updates peerUpdates
          , ..
          }
      | peerId <- peerIds
      , updates <-
          filter (not . null . joinSchedule) $
            shrinkSchedule (peerUpdates Map.! peerId)
      ]
    where
      peerIds = Map.keys peerUpdates

newtype PeerId = PeerId Int
  deriving stock (Show, Eq, Ord)
  deriving newtype (Condense, Hashable, Enum, Bounded)

{-------------------------------------------------------------------------------
  Utilities
-------------------------------------------------------------------------------}

infiniteDelay :: MonadSTM m => m a
infiniteDelay = atomically retry

chainToAnchoredFragment :: HasHeader blk => Chain blk -> AnchoredFragment blk
chainToAnchoredFragment =
    AF.fromOldestFirst AF.AnchorGenesis . Chain.toOldestFirst
