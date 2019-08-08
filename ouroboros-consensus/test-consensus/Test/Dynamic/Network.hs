{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE NamedFieldPuns       #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TupleSections        #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Setup network
module Test.Dynamic.Network (
    broadcastNetwork
  , TracingConstraints
    -- * Test Output
  , TestOutput (..)
  ) where

import           Control.Monad
import           Control.Tracer
import           Crypto.Random (ChaChaDRG, drgNew)
import           Data.Foldable (traverse_)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Proxy (Proxy (..))

import           Control.Monad.Class.MonadAsync
import           Control.Monad.Class.MonadFork (MonadFork)
import           Control.Monad.Class.MonadST
import           Control.Monad.Class.MonadSTM
import           Control.Monad.Class.MonadThrow
import           Control.Monad.Class.MonadTime
import           Control.Monad.Class.MonadTimer

import           Network.TypedProtocol.Channel
import           Network.TypedProtocol.Codec (AnyMessage (..))

import           Ouroboros.Network.Block
import           Ouroboros.Network.MockChain.Chain

import           Ouroboros.Network.Protocol.BlockFetch.Type
import           Ouroboros.Network.Protocol.ChainSync.Type
import           Ouroboros.Network.Protocol.TxSubmission.Type

import           Ouroboros.Consensus.BlockchainTime
import           Ouroboros.Consensus.ChainSyncClient (ClockSkew (..))
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Ledger.Extended
import           Ouroboros.Consensus.Ledger.Mock
import           Ouroboros.Consensus.Mempool
import           Ouroboros.Consensus.Node
import           Ouroboros.Consensus.Node.ProtocolInfo
import           Ouroboros.Consensus.Node.Run
import           Ouroboros.Consensus.Node.Tracers
import           Ouroboros.Consensus.NodeId
import           Ouroboros.Consensus.NodeNetwork
import           Ouroboros.Consensus.Protocol.Abstract
import           Ouroboros.Consensus.Util.Orphans ()
import           Ouroboros.Consensus.Util.STM
import           Ouroboros.Consensus.Util.ThreadRegistry

import qualified Ouroboros.Storage.ChainDB as ChainDB
import           Ouroboros.Storage.ChainDB.Impl (ChainDbArgs (..))
import           Ouroboros.Storage.EpochInfo (newEpochInfo)
import qualified Ouroboros.Storage.FS.Sim.MockFS as Mock
import           Ouroboros.Storage.FS.Sim.STM (simHasFS)
import qualified Ouroboros.Storage.ImmutableDB as ImmDB
import qualified Ouroboros.Storage.LedgerDB.DiskPolicy as LgrDB
import qualified Ouroboros.Storage.LedgerDB.MemPolicy as LgrDB
import qualified Ouroboros.Storage.Util.ErrorHandling as EH

import           Test.Dynamic.TxGen

-- | Interface provided by 'ouroboros-network'.  At the moment
-- 'ouroboros-network' only provides this interface in 'IO' backed by sockets,
-- we cook up here one using 'NodeChans'.
--
data NetworkInterface m peer = NetworkInterface {
      -- | Like 'Ouroboros.Network.NodeToNode.connectTo'
      --
      niConnectTo      :: peer -> m ()

      -- | Like 'Ouroboros.Network.NodeToNode.withServer'
      --
    , niWithServerNode :: forall t.  (Async m () -> m t) -> m t
    }

-- | Create 'NetworkInterface' from a map of channels between nodes.
--
-- TODO: move this function to 'ouroboros-network'.
--
createNetworkInterface
    :: forall m peer blk unused1 unused2.
       ( MonadAsync m
       , MonadMask  m
       , Ord peer
       )
    => NodeChans m peer blk -- ^ map of channels between nodes
    -> [peer]               -- ^ list of nodes which we want to serve
    -> peer                 -- ^ our peer
    -> NetworkApplication m peer
        (AnyMessage (ChainSync (Header blk) (Point blk)))
        (AnyMessage (BlockFetch blk))
        (AnyMessage (TxSubmission (GenTxId blk) (GenTx blk)))
        unused1 -- the local node-to-client channel types
        unused2
        ()
    -> NetworkInterface m peer
createNetworkInterface chans nodeIds us
                       NetworkApplication {
                         naChainSyncClient,
                         naChainSyncServer,
                         naBlockFetchClient,
                         naBlockFetchServer,
                         naTxSubmissionClient,
                         naTxSubmissionServer
                         -- Note that this test is not intended to cover the
                         -- mini-protocols in the node-to-client bundle, so
                         -- we don't pull those handlers out here.
                       } =
  NetworkInterface
    { niConnectTo = \them -> do
        let nodeChan = chans Map.! them Map.! us

        -- 'withAsync' guarantees that when 'waitAny' below receives an
        -- exception the threads will be killed.  If one of the threads will
        -- error, 'waitAny' will terminate and both threads will be killed (thus
        -- there's no need to use 'waitAnyCancel').
        withAsync (void $ naChainSyncClient them
                        $ chainSyncProducer nodeChan)
                  $ \aCS ->
          withAsync (naBlockFetchClient them
                        $ blockFetchProducer nodeChan)
                  $ \aBF ->
            withAsync (naTxSubmissionClient them
                        $ txSubmissionProducer nodeChan)
                  $ \aTX ->
                    -- wait for all the threads, if any throws an exception, cancel all
                    -- of them; this is consistent with
                    -- 'Ouroboros.Network.Socket.connectTo'.
                    void $ waitAny [aCS, aBF, aTX]

      , niWithServerNode = \k -> mask $ \unmask -> do
        ts :: [Async m ()] <- fmap concat $ forM (filter (/= us) nodeIds) $ \them -> do
              let nodeChan = chans Map.! us Map.! them

              aCS <- async $ unmask
                           $ void $ naChainSyncServer
                             them
                             (chainSyncConsumer nodeChan)
              aBF <- async $ unmask
                           $ void $ naBlockFetchServer
                             them
                             (blockFetchConsumer nodeChan)
              aTX <- async $ unmask
                           $ void $ naTxSubmissionServer
                             them
                             (txSubmissionConsumer nodeChan)

              return [aCS, aBF, aTX]

        -- if any thread raises an exception, kill all of them;
        -- if an exception is thrown to this thread, cancel all threads;
        (waitAnyCancel ts `onException` traverse_ cancel ts) >>= k . fst
    }

-- | Setup fully-connected topology, where every node is both a producer
-- and a consumer
--
-- We run for the specified number of blocks, then return the final state of
-- each node.
broadcastNetwork :: forall m blk.
                    ( MonadAsync m
                    , MonadFork  m
                    , MonadMask  m
                    , MonadST    m
                    , MonadTime  m
                    , MonadTimer m
                    , MonadThrow (STM m)
                    , RunNode blk
                    , TxGen blk
                    , TracingConstraints blk
                    )
                 => ThreadRegistry m
                 -> TestBlockchainTime m
                 -> NumCoreNodes
                 -> (CoreNodeId -> ProtocolInfo blk)
                 -> ChaChaDRG
                 -> DiffTime
                 -> m (TestOutput blk)
broadcastNetwork registry testBtime numCoreNodes pInfo initRNG slotLen = do
    chans :: NodeChans m NodeId blk <- createCommunicationChannels

    varRNG <- atomically $ newTVar initRNG

    nodes <- forM coreNodeIds $ \coreNodeId -> do
      node <- createAndConnectNode chans varRNG coreNodeId
      return (coreNodeId, pInfoConfig (pInfo coreNodeId), node)

    -- Wait a random amount of time after the final slot for the block fetch
    -- and chain sync to finish
    testBlockchainTimeDone testBtime
    threadDelay 2000

    getTestOutput nodes
  where
    btime = testBlockchainTime testBtime

    nodeIds :: [NodeId]
    nodeIds = map fromCoreNodeId coreNodeIds

    coreNodeIds :: [CoreNodeId]
    coreNodeIds = enumCoreNodes numCoreNodes

    -- | Produce transactions every time the slot changes and submit them to
    -- the mempool.
    txProducer :: NodeConfig (BlockProtocol blk)
               -> m ChaChaDRG
                  -- ^ How to get a DRG
               -> STM m (ExtLedgerState blk)
                  -- ^ How to get the current ledger state
               -> Mempool m blk TicketNo
               -> m ()
    txProducer cfg produceDRG getExtLedger mempool =
      onSlotChange btime $ \_curSlotNo -> do
        drg <- produceDRG
        txs <- atomically $ do
          ledger <- ledgerState <$> getExtLedger
          varDRG <- newTVar drg
          simChaChaT varDRG id $ testGenTxs numCoreNodes cfg ledger
        void $ addTxs mempool txs

    createCommunicationChannels :: m (NodeChans m NodeId blk)
    createCommunicationChannels = fmap Map.fromList $ forM nodeIds $ \us ->
      fmap ((us, ) . Map.fromList) $ forM (filter (/= us) nodeIds) $ \them -> do
        (chainSyncConsumer,    chainSyncProducer)    <- createConnectedChannels
        (blockFetchConsumer,   blockFetchProducer)   <- createConnectedChannels
        (txSubmissionConsumer, txSubmissionProducer) <- createConnectedChannels
        return (them, NodeChan {..})

    mkArgs :: NodeConfig (BlockProtocol blk)
           -> ExtLedgerState blk
           -> m (ChainDbArgs m blk)
    mkArgs cfg initLedger = do
      (immDbFsVar, volDbFsVar, lgrDbFsVar) <- atomically $
        (,,) <$> newTVar Mock.empty
             <*> newTVar Mock.empty
             <*> newTVar Mock.empty
      epochInfo <- newEpochInfo $ nodeEpochSize (Proxy @blk)
      return ChainDbArgs
        { -- Decoders
          cdbDecodeHash       = nodeDecodeHeaderHash (Proxy @blk)
        , cdbDecodeBlock      = nodeDecodeBlock cfg
        , cdbDecodeLedger     = nodeDecodeLedgerState cfg
        , cdbDecodeChainState = nodeDecodeChainState (Proxy @blk)
          -- Encoders
        , cdbEncodeBlock      = nodeEncodeBlock cfg
        , cdbEncodeHash       = nodeEncodeHeaderHash (Proxy @blk)
        , cdbEncodeLedger     = nodeEncodeLedgerState cfg
        , cdbEncodeChainState = nodeEncodeChainState (Proxy @blk)
          -- Error handling
        , cdbErrImmDb         = EH.monadCatch
        , cdbErrVolDb         = EH.monadCatch
        , cdbErrVolDbSTM      = EH.throwSTM
          -- HasFS instances
        , cdbHasFSImmDb       = simHasFS EH.monadCatch immDbFsVar
        , cdbHasFSVolDb       = simHasFS EH.monadCatch volDbFsVar
        , cdbHasFSLgrDB       = simHasFS EH.monadCatch lgrDbFsVar
          -- Policy
        , cdbValidation       = ImmDB.ValidateAllEpochs
        , cdbBlocksPerFile    = 4
        , cdbMemPolicy        = LgrDB.defaultMemPolicy  (protocolSecurityParam cfg)
        , cdbDiskPolicy       = LgrDB.defaultDiskPolicy (protocolSecurityParam cfg) slotLen
          -- Integration
        , cdbNodeConfig       = cfg
        , cdbEpochInfo        = epochInfo
        , cdbIsEBB            = \blk -> if nodeIsEBB blk
                                        then Just (blockHash blk)
                                        else Nothing
        , cdbGenesis          = return initLedger
        -- Misc
        , cdbTracer           = nullTracer
        , cdbThreadRegistry   = registry
        , cdbGcDelay          = 0
        }

    createAndConnectNode
      :: NodeChans m NodeId blk
      -> TVar m ChaChaDRG
      -> CoreNodeId
      -> m (NodeKernel m NodeId blk)
    createAndConnectNode chans varRNG coreNodeId = do
      let us               = fromCoreNodeId coreNodeId
          ProtocolInfo{..} = pInfo coreNodeId

      let callbacks :: NodeCallbacks m blk
          callbacks = NodeCallbacks {
              produceBlock = \proof _l slot prevPoint prevNo txs -> do
                let curNo :: BlockNo
                    curNo = succ prevNo

                let prevHash :: ChainHash blk
                    prevHash = castHash (pointHash prevPoint)

                nodeForgeBlock pInfoConfig
                               slot
                               curNo
                               prevHash
                               txs
                               proof

            , produceDRG      = atomically $ simChaChaT varRNG id $ drgNew
            }

      args    <- mkArgs pInfoConfig pInfoInitLedger
      chainDB <- ChainDB.openDB args

      let nodeParams = NodeParams
            { tracers            = nullTracers
            , threadRegistry     = registry
            , maxClockSkew       = ClockSkew 1
            , cfg                = pInfoConfig
            , initState          = pInfoInitState
            , btime
            , chainDB
            , callbacks
            , blockFetchSize     = nodeBlockFetchSize
            , blockMatchesHeader = nodeBlockMatchesHeader
            , maxUnackTxs        = 1000 -- TODO
            }

      node <- nodeKernel nodeParams
      let app = consensusNetworkApps
                  node
                  nullProtocolTracers
                  protocolCodecsId
                  (protocolHandlers nodeParams node)

          ni :: NetworkInterface m NodeId
          ni = createNetworkInterface chans nodeIds us app

      void $ forkLinked registry (niWithServerNode ni wait)
      void $ forkLinked registry $ txProducer
        pInfoConfig
        (produceDRG callbacks)
        (ChainDB.getCurrentLedger chainDB)
        (getMempool node)

      forM_ (filter (/= us) nodeIds) $ \them ->
        forkLinked registry (niConnectTo ni them)

      return node

{-------------------------------------------------------------------------------
  Test Output - records of how each node's chain changed
-------------------------------------------------------------------------------}

newtype TestOutput blk = TestOutput
    { testOutputNodes :: Map NodeId
          ( NodeConfig (BlockProtocol blk)
          , Chain blk
          )
      -- ^ Each node's config and final chain.
    }

-- | Gather the test output from the nodes
getTestOutput ::
    forall m blk.
       ( MonadSTM m
       , MonadThrow m
       , HasHeader blk
       )
    => [( CoreNodeId
        , NodeConfig (BlockProtocol blk)
        , NodeKernel m NodeId blk
        )]
    -> m (TestOutput blk)
getTestOutput nodes = do
    nodes' <- fmap Map.fromList $ forM nodes $
        \(cid, cfg, node) ->
                (\ch -> (fromCoreNodeId cid, (cfg, ch)))
            <$> ChainDB.toChain (getChainDB node)

    pure $ TestOutput
        { testOutputNodes = nodes'
        }

{-------------------------------------------------------------------------------
  Internal auxiliary
-------------------------------------------------------------------------------}

-- | Communication channel used for the Chain Sync protocol
type ChainSyncChannel m blk = Channel m (AnyMessage (ChainSync (Header blk) (Point blk)))

-- | Communication channel used for the Block Fetch protocol
type BlockFetchChannel m blk = Channel m (AnyMessage (BlockFetch blk))

-- | Communication channel used for the Tx Submission protocol
type TxSubmissionChannel m blk = Channel m (AnyMessage (TxSubmission (GenTxId blk) (GenTx blk)))

-- | The communication channels from and to each node
data NodeChan m blk = NodeChan
  { chainSyncConsumer    :: ChainSyncChannel    m blk
  , chainSyncProducer    :: ChainSyncChannel    m blk
  , blockFetchConsumer   :: BlockFetchChannel   m blk
  , blockFetchProducer   :: BlockFetchChannel   m blk
  , txSubmissionConsumer :: TxSubmissionChannel m blk
  , txSubmissionProducer :: TxSubmissionChannel m blk
  }

-- | All connections between all nodes
type NodeChans m peer blk = Map peer (Map peer (NodeChan m blk))


{-------------------------------------------------------------------------------
  Constraints needed for verbose tracing
-------------------------------------------------------------------------------}

-- These constraints are when using @showTracer(s) debugTracer@ instead of
-- @nullTracer(s)@.
type TracingConstraints blk =
  ( Show blk
  , Show (Header blk)
  , Show (GenTx blk)
  )
