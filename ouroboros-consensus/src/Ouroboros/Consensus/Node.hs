{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
-- | Run the whole Node
--
-- Intended for qualified import.
--
-- > import qualified Ouroboros.Consensus.Node as Node
-- > import Ouroboros.Consensus.Node (RunNetworkArgs (..))
-- > ..
-- > Node.run ..
module Ouroboros.Consensus.Node
  ( run
  , RunNetworkArgs (..)
    -- * Exposed by 'run'
  , RunNode (..)
  , Tracers
  , Tracers' (..)
  , ChainDB.TraceEvent (..)
  , ProtocolInfo (..)
  , ChainDbArgs (..)
  , NodeArgs (..)
  , NodeKernel (..)
    -- * Internal helpers
  , initChainDB
  , mkNodeArgs
  , initNetwork
  ) where

import           Codec.SerialiseTerm
import           Control.Monad (forM, void)
import           Control.Tracer
import           Crypto.Random
import           Data.ByteString.Lazy (ByteString)
import           Data.Proxy (Proxy (..))
import           Data.Time.Clock (secondsToDiffTime)
import           Network.Socket as Socket

import           Control.Monad.Class.MonadAsync
import           Control.Monad.Class.MonadSTM

import           Ouroboros.Network.Block
import qualified Ouroboros.Network.Block as Block
import           Ouroboros.Network.NodeToClient as NodeToClient
import           Ouroboros.Network.NodeToNode as NodeToNode
import           Ouroboros.Network.Socket
import           Ouroboros.Network.Subscription.Dns
import           Ouroboros.Network.Subscription.Ip

import           Ouroboros.Network.Protocol.Handshake.Type
import           Ouroboros.Network.Protocol.Handshake.Version

import           Ouroboros.Consensus.Block (BlockProtocol)
import           Ouroboros.Consensus.BlockchainTime
import           Ouroboros.Consensus.ChainSyncClient (ClockSkew (..))
import           Ouroboros.Consensus.Ledger.Extended (ExtLedgerState)
import           Ouroboros.Consensus.Mempool.API (GenTx)
import           Ouroboros.Consensus.Node.ProtocolInfo
import           Ouroboros.Consensus.Node.Run
import           Ouroboros.Consensus.Node.Tracers
import           Ouroboros.Consensus.NodeKernel
import           Ouroboros.Consensus.NodeNetwork
import           Ouroboros.Consensus.Protocol hiding (Protocol)
import           Ouroboros.Consensus.Util.Orphans ()
import           Ouroboros.Consensus.Util.ResourceRegistry
import qualified Ouroboros.Consensus.Util.ResourceRegistry as ResourceRegistry

import           Ouroboros.Storage.ChainDB (ChainDB, ChainDbArgs)
import qualified Ouroboros.Storage.ChainDB as ChainDB
import           Ouroboros.Storage.EpochInfo (newEpochInfo)
import           Ouroboros.Storage.ImmutableDB (ValidationPolicy (..))
import           Ouroboros.Storage.LedgerDB.DiskPolicy (defaultDiskPolicy)
import           Ouroboros.Storage.LedgerDB.MemPolicy (defaultMemPolicy)

-- | Start a node.
--
-- This opens the 'ChainDB', sets up the 'NodeKernel' and initialises the
-- network layer.
--
-- This function runs forever unless an exception is thrown.
run
  :: forall blk peer. (RunNode blk, Ord peer)
  => Tracers IO peer blk                  -- ^ Consensus tracers
  -> Tracer  IO (ChainDB.TraceEvent blk)  -- ^ ChainDB tracer
  -> RunNetworkArgs peer blk              -- ^ Network args
  -> FilePath                             -- ^ Database path
  -> ProtocolInfo blk
  -> (ChainDbArgs IO blk -> ChainDbArgs IO blk)
      -- ^ Customise the 'ChainDbArgs'
  -> (NodeArgs IO peer blk -> NodeArgs IO peer blk)
      -- ^ Customise the 'NodeArgs'
  -> (ResourceRegistry IO -> NodeKernel IO peer blk -> IO ())
     -- ^ Called on the 'NodeKernel' after creating it, but before the network
     -- layer is initialised.
  -> IO ()
run tracers chainDbTracer rna dbPath pInfo
    customiseChainDbArgs customiseNodeArgs onNodeKernel =
    ResourceRegistry.with $ \registry -> do

      chainDB <- initChainDB
        chainDbTracer
        registry
        dbPath
        cfg
        initLedger
        slotLength
        customiseChainDbArgs

      btime <- realBlockchainTime
        registry
        slotLength
        (nodeStartTime (Proxy @blk) cfg)

      let nodeArgs = customiseNodeArgs $ mkNodeArgs
            registry
            cfg
            initState
            tracers
            btime
            chainDB

      nodeKernel <- initNodeKernel nodeArgs

      onNodeKernel registry nodeKernel

      initNetwork
        registry
        nodeArgs
        nodeKernel
        rna
  where
    ProtocolInfo
      { pInfoConfig     = cfg
      , pInfoInitLedger = initLedger
      , pInfoInitState  = initState
      } = pInfo

    -- TODO this will depend on the protocol and can change when a hard-fork
    -- happens, see #921 and #282.
    --
    -- For now, we hard-code it to Byron's 20 seconds.
    slotLength = slotLengthFromMillisec (20 * 1000)

initChainDB
  :: forall blk. RunNode blk
  => Tracer IO (ChainDB.TraceEvent blk)
  -> ResourceRegistry IO
  -> FilePath
     -- ^ Database path
  -> NodeConfig (BlockProtocol blk)
  -> ExtLedgerState blk
     -- ^ Initial ledger
  -> SlotLength
  -> (ChainDbArgs IO blk -> ChainDbArgs IO blk)
      -- ^ Customise the 'ChainDbArgs'
  -> IO (ChainDB IO blk)
initChainDB tracer registry dbPath cfg initLedger slotLength
            customiseArgs = do
    epochInfo <- newEpochInfo $ nodeEpochSize (Proxy @blk) cfg
    ChainDB.openDB $ mkArgs epochInfo
  where
    secParam = protocolSecurityParam cfg

    -- TODO cleaner way with subsecond precision
    slotDiffTime = secondsToDiffTime
      (slotLengthToMillisec slotLength `div` 1000)

    mkArgs epochInfo = customiseArgs $ (ChainDB.defaultArgs dbPath)
      { ChainDB.cdbBlocksPerFile    = 1000
      , ChainDB.cdbDecodeBlock      = nodeDecodeBlock       cfg
      , ChainDB.cdbDecodeChainState = nodeDecodeChainState  (Proxy @blk)
      , ChainDB.cdbDecodeHash       = nodeDecodeHeaderHash  (Proxy @blk)
      , ChainDB.cdbDecodeLedger     = nodeDecodeLedgerState cfg
      , ChainDB.cdbEncodeBlock      = nodeEncodeBlock       cfg
      , ChainDB.cdbEncodeChainState = nodeEncodeChainState  (Proxy @blk)
      , ChainDB.cdbEncodeHash       = nodeEncodeHeaderHash  (Proxy @blk)
      , ChainDB.cdbEncodeLedger     = nodeEncodeLedgerState cfg
      , ChainDB.cdbEpochInfo        = epochInfo
      , ChainDB.cdbGenesis          = return initLedger
      , ChainDB.cdbDiskPolicy       = defaultDiskPolicy secParam slotDiffTime
      , ChainDB.cdbIsEBB            = \blk -> if nodeIsEBB blk
                                              then Just (blockHash blk)
                                              else Nothing
      , ChainDB.cdbMemPolicy        = defaultMemPolicy secParam
      , ChainDB.cdbNodeConfig       = cfg
      , ChainDB.cdbRegistry         = registry
      , ChainDB.cdbTracer           = tracer
      , ChainDB.cdbValidation       = ValidateMostRecentEpoch
      , ChainDB.cdbGcDelay          = secondsToDiffTime 10
      }

mkNodeArgs
  :: forall blk peer. RunNode blk
  => ResourceRegistry IO
  -> NodeConfig (BlockProtocol blk)
  -> NodeState  (BlockProtocol blk)
  -> Tracers IO peer blk
  -> BlockchainTime IO
  -> ChainDB IO blk
  -> NodeArgs IO peer blk
mkNodeArgs registry cfg initState tracers btime chainDB = NodeArgs
    { tracers
    , registry
    , maxClockSkew       = ClockSkew 1
    , cfg
    , initState
    , btime
    , chainDB
    , callbacks
    , blockFetchSize     = nodeBlockFetchSize
    , blockMatchesHeader = nodeBlockMatchesHeader
    , maxUnackTxs        = 100 -- TODO
    }
  where
    callbacks = NodeCallbacks
      { produceDRG   = drgNew
      , produceBlock = produceBlock
      }

    produceBlock
      :: IsLeader (BlockProtocol blk)  -- ^ Proof we are leader
      -> ExtLedgerState blk            -- ^ Current ledger state
      -> SlotNo                        -- ^ Current slot
      -> Point blk                     -- ^ Previous point
      -> BlockNo                       -- ^ Previous block number
      -> [GenTx blk]                   -- ^ Contents of the mempool
      -> ProtocolM blk IO blk
    produceBlock proof _l slot prevPoint prevBlockNo txs =
        -- The transactions we get are consistent; the only reason not to
        -- include all of them would be maximum block size, which we ignore
        -- for now.
        nodeForgeBlock cfg slot curNo prevHash txs proof
      where
        curNo :: BlockNo
        curNo = succ prevBlockNo

        prevHash :: ChainHash blk
        prevHash = castHash (Block.pointHash prevPoint)

-- | Short-hand for the instantiated 'NetworkApplication'
type NetworkApps peer =
  NetworkApplication IO peer
    ByteString ByteString ByteString ByteString ByteString ()

-- | Arguments specific to the network stack
data RunNetworkArgs peer blk = RunNetworkArgs
  { rnaIpSubscriptionTracer  :: Tracer IO (WithIPList (SubscriptionTrace Socket.SockAddr))
    -- ^ IP subscription tracer
  , rnaDnsSubscriptionTracer :: Tracer IO (WithDomainName (SubscriptionTrace Socket.SockAddr))
    -- ^ DNS subscription tracer
  , rnaDnsResolverTracer     :: Tracer IO (WithDomainName DnsTrace)
    -- ^ DNS resolver tracer
  , rnaMkPeer                :: SockAddr -> SockAddr -> peer
    -- ^ How to create a peer
  , rnaMyAddr                :: AddrInfo
    -- ^ The node's own address
  , rnaMyLocalAddr           :: AddrInfo
    -- ^ The node's own local address
  , rnaIpProducers           :: [SockAddr]
    -- ^ IP producers
  , rnaDnsProducers          :: [DnsSubscriptionTarget]
    -- ^ DNS producers
  }

initNetwork
  :: forall blk peer.
     (RunNode blk, Ord peer)
  => ResourceRegistry IO
  -> NodeArgs    IO peer blk
  -> NodeKernel  IO peer blk
  -> RunNetworkArgs peer blk
  -> IO ()
initNetwork registry nodeArgs kernel RunNetworkArgs{..} = do
    -- serve local clients (including tx submission)
    localServer <- forkLinked registry runLocalServer

    -- serve downstream nodes
    connTable  <- newConnectionTable
    peerServer <- forkLinked registry $
      runPeerServer connTable

    ipSubscriptions <- forkLinked registry $ do
      runIpSubscriptionWorker connTable

    -- dns subscription managers
    dnsSubscriptions <- forM rnaDnsProducers $ \dnsProducer ->
      forkLinked registry $ runDnsSubscriptionWorker connTable dnsProducer

    let asyncs = localServer : peerServer : ipSubscriptions : dnsSubscriptions
    void $ waitAny asyncs
  where
    networkApps :: NetworkApps peer
    networkApps = consensusNetworkApps
      kernel
      nullProtocolTracers
      (protocolCodecs (getNodeConfig kernel))
      (protocolHandlers nodeArgs kernel)

    networkAppNodeToNode :: Versions
                              NodeToNodeVersion
                              DictVersion
                              (NetworkApps peer)
    networkAppNodeToNode =
      simpleSingletonVersions
        NodeToNodeV_1
        (NodeToNodeVersionData { networkMagic = 0 })
        (DictVersion nodeToNodeCodecCBORTerm)
        networkApps

    networkAppNodeToClient :: Versions
                                NodeToClientVersion
                                DictVersion
                                (NetworkApps peer)
    networkAppNodeToClient =
      simpleSingletonVersions
        NodeToClientV_1
        (NodeToClientVersionData { networkMagic = 0 })
        (DictVersion nodeToClientCodecCBORTerm)
        networkApps

    runLocalServer :: IO ()
    runLocalServer = do
      connTable <- newConnectionTable
      NodeToClient.withServer
        connTable
        rnaMyLocalAddr
        rnaMkPeer
        (\(DictVersion _) -> acceptEq)
        (localResponderNetworkApplication <$> networkAppNodeToClient)
        wait

    runPeerServer :: ConnectionTable IO Socket.SockAddr -> IO ()
    runPeerServer connTable = NodeToNode.withServer
      connTable
      rnaMyAddr
      rnaMkPeer
      (\(DictVersion _) -> acceptEq)
      (responderNetworkApplication <$> networkAppNodeToNode)
      wait

    runIpSubscriptionWorker :: ConnectionTable IO Socket.SockAddr -> IO ()
    runIpSubscriptionWorker connTable = ipSubscriptionWorker
      rnaIpSubscriptionTracer
      connTable
      -- the comments in dnsSbuscriptionWorker call apply
      (Just (Socket.SockAddrInet 0 0))
      (Just (Socket.SockAddrInet6 0 0 (0, 0, 0, 1) 0))
      (const Nothing)
      IPSubscriptionTarget
        { ispIps     = rnaIpProducers
        , ispValency = length rnaIpProducers
        }
      (\_ -> retry)
      (connectToNode'
        (\(DictVersion codec) -> encodeTerm codec)
        (\(DictVersion codec) -> decodeTerm codec)
        nullTracer
        rnaMkPeer
        (initiatorNetworkApplication <$> networkAppNodeToNode))

    runDnsSubscriptionWorker :: ConnectionTable IO Socket.SockAddr
                             -> DnsSubscriptionTarget
                             -> IO ()
    runDnsSubscriptionWorker connTable dnsProducer = dnsSubscriptionWorker
      rnaDnsSubscriptionTracer
      rnaDnsResolverTracer
      connTable
      -- IPv4 address
      --
      -- We can't share portnumber with our server since we run separate
      -- 'MuxInitiatorApplication' and 'MuxResponderApplication'
      -- applications instead of a 'MuxInitiatorAndResponderApplication'.
      -- This means we don't utilise full duplex connection.
      (Just (Socket.SockAddrInet 0 0))
      -- IPv6 address
      (Just (Socket.SockAddrInet6 0 0 (0, 0, 0, 1) 0))
      (const Nothing)
      dnsProducer
      (\_ -> retry)
      (connectToNode'
        (\(DictVersion codec) -> encodeTerm codec)
        (\(DictVersion codec) -> decodeTerm codec)
        nullTracer
        rnaMkPeer
        (initiatorNetworkApplication <$> networkAppNodeToNode))
