{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE NumericUnderscores  #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -Wno-simplifiable-class-constraints #-}

module Run (
      runNode
    ) where

import           Codec.CBOR.Decoding (Decoder)
import           Codec.CBOR.Encoding (Encoding)
import           Control.Exception
import           Control.Concurrent (threadDelay)
import qualified Control.Concurrent.Async as Async
import           Control.Monad
import           Control.Tracer
import           Crypto.Random
import           Data.Functor.Contravariant (contramap)
import qualified Data.Map.Strict as M
import           Data.Maybe
import           Data.Semigroup ((<>))
import qualified Data.Text as T

import           Control.Monad.Class.MonadAsync

import           Network.Socket

import qualified Ouroboros.Network.AnchoredFragment as AF
import           Ouroboros.Network.Block
import qualified Ouroboros.Network.Block as Block
import           Ouroboros.Network.Chain (genesisPoint, pointHash)
import qualified Ouroboros.Network.Chain as Chain
import           Ouroboros.Network.NodeToNode
import           Ouroboros.Network.Mux.Interface

import           Ouroboros.Network.Protocol.BlockFetch.Codec
import           Ouroboros.Network.Protocol.ChainSync.Codec

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.BlockchainTime
import           Ouroboros.Consensus.ChainSyncClient (ClockSkew (..))
import           Ouroboros.Consensus.Demo
import           Ouroboros.Consensus.Demo.Run
import           Ouroboros.Consensus.NodeId
import           Ouroboros.Consensus.Node
import           Ouroboros.Consensus.NodeNetwork
import           Ouroboros.Consensus.Util.Condense
import           Ouroboros.Consensus.Util.Orphans ()
import           Ouroboros.Consensus.Util.STM
import           Ouroboros.Consensus.Util.ThreadRegistry

import           Ouroboros.Storage.ChainDB (ChainDB)
import qualified Ouroboros.Storage.ChainDB as ChainDB
import qualified Ouroboros.Storage.ChainDB.Mock as ChainDB

import qualified Cardano.BM.Configuration.Model as Monitoring (setupFromRepresentation)
import qualified Cardano.BM.Data.BackendKind as Monitoring
import qualified Cardano.BM.Data.Configuration as Monitoring (Representation (..), parseRepresentation)
import qualified Cardano.BM.Data.Output as Monitoring
import qualified Cardano.BM.Data.Severity as Monitoring
import qualified Cardano.BM.Data.Tracer as Monitoring (toLogObject)
import qualified Cardano.BM.Setup as Monitoring (withTrace)

import           CLI
import           Mock.TxSubmission
import           Topology

runNode :: CLI -> IO ()
runNode cli@CLI{..} = do
    -- If the user asked to submit a transaction, we don't have to spin up a
    -- full node, we simply transmit it and exit.
    case command of
      SimpleNode topology myNodeAddress protocol -> do
        SomeProtocol p <- fromProtocol protocol
        handleSimpleNode p cli myNodeAddress topology
      TxSubmitter topology tx ->
        handleTxSubmission   topology tx
      ProtoProposer topology stim ->
        handleUSSASubmission topology stim
      SoftProposer  topology stim ->
        handleUSSASubmission topology stim
      Voter         topology stim ->
        handleUSSASubmission topology stim

-- Inlined byron-proxy/src/exec/Logging.hs:defaultLoggerConfig, so as to avoid
-- introducing merge conflicts due to refactoring.  Should be factored after merges.
-- | It's called `Representation` but is closely related to the `Configuration`
-- from iohk-monitoring. The latter has to do with `MVar`s. It's all very
-- weird.
defaultLoggerConfig :: Monitoring.Representation
defaultLoggerConfig = Monitoring.Representation
  { Monitoring.minSeverity     = Monitoring.Debug
  , Monitoring.rotation        = Nothing
  , Monitoring.setupScribes    = [stdoutScribe]
  , Monitoring.defaultScribes  = [(Monitoring.StdoutSK, "stdout")]
  , Monitoring.setupBackends   = [Monitoring.KatipBK]
  , Monitoring.defaultBackends = [Monitoring.KatipBK]
  , Monitoring.hasEKG          = Nothing
  , Monitoring.hasPrometheus   = Nothing
  , Monitoring.hasGUI          = Nothing
  , Monitoring.options         = mempty
  }
  where
  stdoutScribe = Monitoring.ScribeDefinition
    { Monitoring.scKind     = Monitoring.StdoutSK
    , Monitoring.scFormat   = Monitoring.ScText
    , Monitoring.scName     = "stdout"
    , Monitoring.scPrivacy  = Monitoring.ScPublic
    , Monitoring.scRotation = Nothing
    }

-- | Sets up a simple node, which will run the chain sync protocol and block
-- fetch protocol, and, if core, will also look at the mempool when trying to
-- create a new block.
handleSimpleNode :: forall blk.  RunDemo blk
                 => DemoProtocol blk -> CLI -> NodeAddress -> TopologyInfo -> IO ()
handleSimpleNode p CLI{..} myNodeAddress (TopologyInfo myNodeId topologyFile) = do
    putStrLn $ "System started at " <> show systemStart
    t@(NetworkTopology nodeSetups) <-
      either error id <$> readTopologyFile topologyFile
    let topology  = toNetworkMap t
        nodeSetup = fromMaybe (error "node not found.") $
                          M.lookup myNodeId topology

    putStrLn $ "**************************************"
    putStrLn $ "I am Node = " <> show myNodeId
    putStrLn $ "My producers are " <> show (producers nodeSetup)
    putStrLn $ "**************************************"

    let pInfo@ProtocolInfo{..} =
          protocolInfo (NumCoreNodes (length nodeSetups)) (CoreNodeId nid) p

    withThreadRegistry $ \registry -> do

      let callbacks :: NodeCallbacks IO blk
          callbacks = NodeCallbacks {
              produceDRG   = drgNew
            , produceBlock = \proof els slot prevPoint prevBlockNo txs ussArgs -> do
                let curNo :: BlockNo
                    curNo = succ prevBlockNo

                    prevHash :: ChainHash blk
                    prevHash = castHash (pointHash prevPoint)

                 -- The transactions we get are consistent; the only reason not
                 -- to include all of them would be maximum block size, which
                 -- we ignore for now.
                demoForgeBlock pInfoConfig
                               els
                               slot
                               curNo
                               prevHash
                               txs
                               ussArgs
                               proof
          }

      chainDB :: ChainDB IO blk (Header blk) <-
        ChainDB.openDB
          pInfoConfig
          pInfoInitLedger
          getHeader

      -- Inlined (almost) byron-proxy/src/exec/Logging.hs:withLogging, so as to avoid
      -- introducing merge conflicts due to refactoring.Should be factored after merges.
      logConf <- case loggerConfig of
        Nothing -> pure defaultLoggerConfig
        Just fp -> Monitoring.parseRepresentation fp
      loggerConfig' <- Monitoring.setupFromRepresentation $
        logConf { Monitoring.minSeverity = loggerMinSev }
      Monitoring.withTrace loggerConfig' (T.pack $ show myNodeId) $ \baseTracer -> do

        btime  <- realBlockchainTime registry slotDuration systemStart
        let tracer = contramap ((show myNodeId <> " | ") <>)
                               (Monitoring.toLogObject baseTracer)
            nodeParams :: NodeParams IO NodeAddress blk
            nodeParams = NodeParams
              { tracer             = tracer
              , threadRegistry     = registry
              , maxClockSkew       = ClockSkew 1
              , cfg                = pInfoConfig
              , initState          = pInfoInitState
              , btime
              , chainDB
              , callbacks
              , blockFetchSize     = demoBlockFetchSize
              , blockMatchesHeader = demoBlockMatchesHeader
              }

        kernel <- nodeKernel nodeParams

        let networkApp =
              simpleSingletonVersions
                  NodeToNodeV_1
                  (NodeToNodeVersionData { networkMagic = 0 })
                  (DictVersion nodeToNodeCodecCBORTerm)
              <$> consensusNetworkApps
                    nullTracer
                    nullTracer
                    (codecChainSync
                      (demoEncodeHeader pInfoConfig)
                      (demoDecodeHeader pInfoConfig)
                      (encodePoint'         pInfo)
                      (decodePoint'         pInfo))
                    (codecBlockFetch
                      (demoEncodeBlock pInfoConfig)
                      demoEncodeHeaderHash
                      (demoDecodeBlock pInfoConfig)
                      demoDecodeHeaderHash)
                    nodeParams
                    kernel
        
        watchChain registry tracer chainDB

        -- Spawn the thread which listens to the mempool.
        mempoolThread <- spawnMempoolListener tracer myNodeId kernel

        -- Spawn the update system stimulus listener
        usThread      <- spawnUSSAListener    tracer myNodeId kernel

        myAddr:_ <- case myNodeAddress of
          NodeAddress host port -> getAddrInfo Nothing (Just host) (Just port)
        
        -- TODO: cheap subscription managment, a proper one is on the way.  The
        -- point is that it only requires 'NetworkApplications' which is a thin
        -- layer around 'MuxApplication'.
        
        -- serve downstream nodes
        _ <- forkLinked registry $ do
          versions <- runSharedState networkApp
          (withServer myAddr (\(DictVersion _) -> acceptEq) (muxResponderNetworkApplication <$> versions) wait)
        
        -- connect to upstream nodes
        forM_ (producers nodeSetup) $ \na@(NodeAddress host port) ->
          forkLinked registry $ do
        
            let io = do
                  addr:_ <- getAddrInfo Nothing (Just host) (Just port)
                  versions <- runSharedState networkApp
                  connectTo
                        (muxInitiatorNetworkApplication na <$> versions)
                        -- Do not bind to a local port, use ephemeral
                        -- one.  We cannot bind to port on which the server is
                        -- already accepting connections.
                        Nothing
                        addr
                    `catch` \(_ :: IOException) -> threadDelay 250_000 >> io
        
            io

        Async.wait mempoolThread
  where
      nid :: Int
      nid = case myNodeId of
              CoreId  n -> n
              RelayId _ -> error "Non-core nodes currently not supported"

      watchChain :: ThreadRegistry IO
                 -> Tracer IO String
                 -> ChainDB IO blk (Header blk)
                 -> IO ()
      watchChain registry tracer chainDB = onEachChange
          registry fingerprint initFingerprint
          (ChainDB.getCurrentChain chainDB) (const logFullChain)
        where
          initFingerprint  = (genesisPoint, genesisPoint)
          fingerprint frag = (AF.headPoint frag, AF.anchorPoint frag)
          logFullChain = do
            chain <- ChainDB.toChain chainDB
            traceWith tracer $
              "Updated chain: " <> condense (Chain.toOldestFirst chain)

      encodePoint' :: ProtocolInfo blk -> Point blk -> Encoding
      encodePoint' ProtocolInfo{..} =
          Block.encodePoint $ Block.encodeChainHash demoEncodeHeaderHash

      decodePoint' :: forall s. ProtocolInfo blk -> Decoder s (Point blk)
      decodePoint' ProtocolInfo{..} =
          Block.decodePoint $ Block.decodeChainHash demoDecodeHeaderHash
