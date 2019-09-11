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

import           Control.Monad (forM, void)
import           Control.Tracer
import           Crypto.Random
import           Data.ByteString.Lazy (ByteString)
import           Data.Proxy (Proxy (..))
import           Data.Time.Clock (secondsToDiffTime)
import           Network.Socket as Socket
import qualified Codec.CBOR.Term as CBOR
import qualified Codec.CBOR.Read as CBOR

import           Control.Monad.Class.MonadAsync

import           Ouroboros.Network.Block
import qualified Ouroboros.Network.Block as Block
import           Ouroboros.Network.NodeToClient as NodeToClient
import           Ouroboros.Network.NodeToNode as NodeToNode

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
    withRegistry $ \registry -> do

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
  , rnaHandshakeTracer       :: Tracer IO (TraceSendRecv
                                            (Handshake NodeToNodeVersion CBOR.Term)
                                            peer
                                            (DecoderFailureOrTooMuchInput CBOR.DeserialiseFailure))
    -- ^ Handshake protocol tracer
  , rnaHandshakeLocalTracer  :: Tracer IO (TraceSendRecv
                                            (Handshake NodeToClientVersion CBOR.Term)
                                            peer
                                            (DecoderFailureOrTooMuchInput CBOR.DeserialiseFailure))
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
    localServer <- forkLinkedThread registry runLocalServer

    -- serve downstream nodes
    connTable  <- newConnectionTable
    peerServer <- forkLinkedThread registry $ runPeerServer connTable

    ipSubscriptions <- forkLinkedThread registry $
                         runIpSubscriptionWorker connTable

    -- dns subscription managers
    dnsSubscriptions <- forM rnaDnsProducers $ \dnsProducer -> do
       forkLinkedThread registry $
         runDnsSubscriptionWorker connTable dnsProducer

    let threads = localServer : peerServer : ipSubscriptions : dnsSubscriptions
    void $ waitAnyThread threads
  where
    networkApps :: NetworkApps peer
    networkApps = consensusNetworkApps
      kernel
      nullProtocolTracers
      (protocolCodecs (getNodeConfig kernel))
      (protocolHandlers nodeArgs kernel)

    -- TODO: network magics should be configurable, this gives an early fail if
    -- one tries to run testnet vs mainnet.
    nodeToNodeVersionData   = NodeToNodeVersionData { networkMagic   = 0 }
    nodeToClientVersionData = NodeToClientVersionData { networkMagic = 0 }

    runLocalServer :: IO ()
    runLocalServer = do
      (connTable :: ConnectionTable IO Socket.SockAddr) <- newConnectionTable
      NodeToClient.withServer_V1
        rnaHandshakeLocalTracer
        connTable
        rnaMyLocalAddr
        rnaMkPeer
        nodeToClientVersionData
        (localResponderNetworkApplication networkApps)
        wait

    runPeerServer :: ConnectionTable IO Socket.SockAddr -> IO ()
    runPeerServer connTable =
      NodeToNode.withServer_V1
        rnaHandshakeTracer
        connTable
        rnaMyAddr
        rnaMkPeer
        nodeToNodeVersionData
        (responderNetworkApplication networkApps)
        wait

    runIpSubscriptionWorker :: ConnectionTable IO Socket.SockAddr -> IO ()
    runIpSubscriptionWorker connTable = ipSubscriptionWorker_V1
      rnaIpSubscriptionTracer
      rnaHandshakeTracer
      rnaMkPeer
      connTable
      -- the comments in dnsSbuscriptionWorker call apply
      (Just (Socket.SockAddrInet 0 0))
      (Just (Socket.SockAddrInet6 0 0 (0, 0, 0, 0) 0))
      (const Nothing)
      IPSubscriptionTarget
        { ispIps     = rnaIpProducers
        , ispValency = length rnaIpProducers
        }
      nodeToNodeVersionData
      (initiatorNetworkApplication networkApps)

    runDnsSubscriptionWorker :: ConnectionTable IO Socket.SockAddr
                             -> DnsSubscriptionTarget
                             -> IO ()
    runDnsSubscriptionWorker connTable dnsProducer = dnsSubscriptionWorker_V1
      rnaDnsSubscriptionTracer
      rnaDnsResolverTracer
      rnaHandshakeTracer
      rnaMkPeer
      connTable
      -- IPv4 address
      --
      -- We can't share portnumber with our server since we run separate
      -- 'MuxInitiatorApplication' and 'MuxResponderApplication'
      -- applications instead of a 'MuxInitiatorAndResponderApplication'.
      -- This means we don't utilise full duplex connection.
      (Just (Socket.SockAddrInet 0 0))
      -- IPv6 address
      (Just (Socket.SockAddrInet6 0 0 (0, 0, 0, 0) 0))
      (const Nothing)
      dnsProducer
      nodeToNodeVersionData
      (initiatorNetworkApplication networkApps)
