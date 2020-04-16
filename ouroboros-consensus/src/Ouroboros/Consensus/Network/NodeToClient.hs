{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}

-- | Intended for qualified import
module Ouroboros.Consensus.Network.NodeToClient (
    -- * Handlers
    Handlers (..)
  , mkHandlers
    -- * Codecs
  , Codecs' (..)
  , Codecs
  , DefaultCodecs
  , ClientCodecs
  , defaultCodecs
  , clientCodecs
  , identityCodecs
    -- * ClientCodecs
    -- * Tracers
  , Tracers
  , Tracers' (..)
  , nullTracers
  , showTracers
    -- * Applications
  , Apps (..)
  , mkApps
    -- ** Projections
  , responder
  ) where

import           Control.Tracer
import           Data.ByteString.Lazy (ByteString)
import           Data.Proxy (Proxy (..))
import           Data.Void (Void)

import           Ouroboros.Network.Block
import           Ouroboros.Network.BlockFetch
import           Ouroboros.Network.Channel
import           Ouroboros.Network.Codec
import           Ouroboros.Network.Driver
import           Ouroboros.Network.Mux
import           Ouroboros.Network.NodeToClient hiding (NodeToClientVersion(..))
import qualified Ouroboros.Network.NodeToClient as N (NodeToClientVersion(..))
import           Ouroboros.Network.Protocol.ChainSync.Codec
import           Ouroboros.Network.Protocol.ChainSync.Server
import           Ouroboros.Network.Protocol.ChainSync.Type
import           Ouroboros.Network.Protocol.LocalStateQuery.Codec
import           Ouroboros.Network.Protocol.LocalStateQuery.Server
import           Ouroboros.Network.Protocol.LocalStateQuery.Type
import           Ouroboros.Network.Protocol.LocalTxSubmission.Codec
import           Ouroboros.Network.Protocol.LocalTxSubmission.Server
import           Ouroboros.Network.Protocol.LocalTxSubmission.Type

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Config
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Mempool.API
import           Ouroboros.Consensus.MiniProtocol.ChainSync.Server
import           Ouroboros.Consensus.MiniProtocol.LocalStateQuery.Server
import           Ouroboros.Consensus.MiniProtocol.LocalTxSubmission.Server
import           Ouroboros.Consensus.Node.NetworkProtocolVersion
import           Ouroboros.Consensus.Node.Run
import qualified Ouroboros.Consensus.Node.Tracers as Node
import           Ouroboros.Consensus.NodeKernel
import           Ouroboros.Consensus.Util.IOLike
import           Ouroboros.Consensus.Util.Orphans ()
import           Ouroboros.Consensus.Util.ResourceRegistry

import qualified Ouroboros.Consensus.Storage.ChainDB.API as ChainDB

{-------------------------------------------------------------------------------
  Handlers
-------------------------------------------------------------------------------}

-- | Protocol handlers for node-to-client (local) communication
data Handlers m peer blk = Handlers {
      hChainSyncServer
        :: ResourceRegistry m
        -> ChainSyncServer (Serialised blk) (Tip blk) m ()

    , hTxSubmissionServer
        :: LocalTxSubmissionServer (GenTx blk) (ApplyTxErr blk) m ()

    , hStateQueryServer
        :: LocalStateQueryServer blk (Query blk) m ()
    }

mkHandlers
  :: forall m blk remotePeer localPeer. (IOLike m, ApplyTx blk, QueryLedger blk)
  => NodeArgs   m remotePeer localPeer blk
  -> NodeKernel m remotePeer localPeer blk
  -> Handlers   m            localPeer blk
mkHandlers NodeArgs {cfg, tracers} NodeKernel {getChainDB, getMempool} =
    Handlers {
        hChainSyncServer =
          chainSyncBlocksServer
            (Node.chainSyncServerBlockTracer tracers)
            getChainDB
      , hTxSubmissionServer =
          localTxSubmissionServer
            (Node.localTxSubmissionServerTracer tracers)
            getMempool
      , hStateQueryServer =
          localStateQueryServer
            (configLedger cfg)
            (ChainDB.newLedgerCursor getChainDB)
      }

{-------------------------------------------------------------------------------
  Codecs
-------------------------------------------------------------------------------}

-- | Node-to-client protocol codecs needed to run 'Handlers'.
data Codecs' blk serialisedBlk e m bCS bTX bSQ = Codecs {
      cChainSyncCodec    :: Codec (ChainSync serialisedBlk (Tip blk))              e m bCS
    , cTxSubmissionCodec :: Codec (LocalTxSubmission (GenTx blk) (ApplyTxErr blk)) e m bTX
    , cStateQueryCodec   :: Codec (LocalStateQuery blk (Query blk))                e m bSQ
    }

type Codecs blk e m bCS bTX bSQ =
    Codecs' blk (Serialised blk) e m bCS bTX bSQ
type DefaultCodecs blk m =
    Codecs' blk (Serialised blk) DeserialiseFailure m ByteString ByteString ByteString
type ClientCodecs blk  m =
    Codecs' blk blk DeserialiseFailure m ByteString ByteString ByteString

-- | Protocol codecs for the node-to-client protocols
--
-- We pass the 'BlockConfig' here, even though it is currently unused. If at any
-- point we want to introduce local protocols that for example send Byron blocks
-- or headers across, we will need to have the epoch size, which comes from the
-- Byron config. Unlike the full 'TopLevelConfig', it should not be difficult
-- for a wallet to construct the 'BlockConfig'.
--
-- NOTE: Somewhat confusingly, 'pcChainSyncCodec' currently /does/ send Byron
-- blocks across, but it does not deserialize them (the user of the codec is
-- itself responsible for doing that), which is why it currently does not need
-- the config.
--
-- Implementation mode: currently none of the consensus encoders/decoders do
-- anything different based on the version, so @_version@ is unused; it's just
-- that not all codecs are used, depending on the version number.
defaultCodecs :: forall m blk. (RunNode blk, MonadST m)
              => BlockConfig         blk
              -> NodeToClientVersion blk
              -> DefaultCodecs blk m
defaultCodecs _cfg _version = Codecs {
      cChainSyncCodec =
        codecChainSyncSerialised
          (encodePoint (nodeEncodeHeaderHash (Proxy @blk)))
          (decodePoint (nodeDecodeHeaderHash (Proxy @blk)))
          (encodeTip   (nodeEncodeHeaderHash (Proxy @blk)))
          (decodeTip   (nodeDecodeHeaderHash (Proxy @blk)))

    , cTxSubmissionCodec =
        codecLocalTxSubmission
          nodeEncodeGenTx
          nodeDecodeGenTx
          (nodeEncodeApplyTxError (Proxy @blk))
          (nodeDecodeApplyTxError (Proxy @blk))

    , cStateQueryCodec =
        codecLocalStateQuery
          (encodePoint (nodeEncodeHeaderHash (Proxy @blk)))
          (decodePoint (nodeDecodeHeaderHash (Proxy @blk)))
          nodeEncodeQuery
          nodeDecodeQuery
          nodeEncodeResult
          nodeDecodeResult
    }


-- | Protocol codecs for the node-to-client protocols which serialise
-- / deserialise blocks in /chain-sync/ protocol.
--
clientCodecs :: forall m blk. (RunNode blk, MonadST m)
             => BlockConfig         blk
             -> NodeToClientVersion blk
             -> ClientCodecs blk m
clientCodecs cfg _version = Codecs {
      cChainSyncCodec =
        codecChainSync
          (wrapCBORinCBOR   (nodeEncodeBlock cfg))
          (unwrapCBORinCBOR (nodeDecodeBlock cfg))
          (encodePoint (nodeEncodeHeaderHash (Proxy @blk)))
          (decodePoint (nodeDecodeHeaderHash (Proxy @blk)))
          (encodeTip   (nodeEncodeHeaderHash (Proxy @blk)))
          (decodeTip   (nodeDecodeHeaderHash (Proxy @blk)))

    , cTxSubmissionCodec =
        codecLocalTxSubmission
          nodeEncodeGenTx
          nodeDecodeGenTx
          (nodeEncodeApplyTxError (Proxy @blk))
          (nodeDecodeApplyTxError (Proxy @blk))

    , cStateQueryCodec =
        codecLocalStateQuery
          (encodePoint (nodeEncodeHeaderHash (Proxy @blk)))
          (decodePoint (nodeDecodeHeaderHash (Proxy @blk)))
          nodeEncodeQuery
          nodeDecodeQuery
          nodeEncodeResult
          nodeDecodeResult
    }


-- | Identity codecs used in tests.
identityCodecs :: (Monad m, QueryLedger blk)
               => Codecs blk CodecFailure m
                    (AnyMessage (ChainSync (Serialised blk) (Tip blk)))
                    (AnyMessage (LocalTxSubmission (GenTx blk) (ApplyTxErr blk)))
                    (AnyMessage (LocalStateQuery blk (Query blk)))
identityCodecs = Codecs {
      cChainSyncCodec    = codecChainSyncId
    , cTxSubmissionCodec = codecLocalTxSubmissionId
    , cStateQueryCodec   = codecLocalStateQueryId eqQuery
    }

{-------------------------------------------------------------------------------
  Tracers
-------------------------------------------------------------------------------}

-- | A record of 'Tracer's for the different protocols.
type Tracers m peer blk e =
     Tracers'  peer blk e (Tracer m)

data Tracers' peer blk e f = Tracers {
      tChainSyncTracer    :: f (TraceLabelPeer peer (TraceSendRecv (ChainSync (Serialised blk) (Tip blk))))
    , tTxSubmissionTracer :: f (TraceLabelPeer peer (TraceSendRecv (LocalTxSubmission (GenTx blk) (ApplyTxErr blk))))
    , tStateQueryTracer   :: f (TraceLabelPeer peer (TraceSendRecv (LocalStateQuery blk (Query blk))))
    }

instance (forall a. Semigroup (f a)) => Semigroup (Tracers' peer blk e f) where
  l <> r = Tracers {
        tChainSyncTracer    = f tChainSyncTracer
      , tTxSubmissionTracer = f tTxSubmissionTracer
      , tStateQueryTracer   = f tStateQueryTracer
      }
    where
      f :: forall a. Semigroup a
        => (Tracers' peer blk e f -> a)
        -> a
      f prj = prj l <> prj r

-- | Use a 'nullTracer' for each protocol.
nullTracers :: Monad m => Tracers m peer blk e
nullTracers = Tracers {
      tChainSyncTracer    = nullTracer
    , tTxSubmissionTracer = nullTracer
    , tStateQueryTracer   = nullTracer
    }

showTracers :: ( Show peer
               , Show (GenTx blk)
               , Show (ApplyTxErr blk)
               , ShowQuery (Query blk)
               , HasHeader blk
               )
            => Tracer m String -> Tracers m peer blk e
showTracers tr = Tracers {
      tChainSyncTracer    = showTracing tr
    , tTxSubmissionTracer = showTracing tr
    , tStateQueryTracer   = showTracing tr
    }

{-------------------------------------------------------------------------------
  Applications
-------------------------------------------------------------------------------}

-- | Applications for the node-to-client (i.e., local) protocols
--
-- See 'Network.Mux.Types.MuxApplication'
data Apps m peer bCS bTX bSQ a = Apps {
      -- | Start a local chain sync server.
      aChainSyncServer    :: peer -> Channel m bCS -> m a

      -- | Start a local transaction submission server.
    , aTxSubmissionServer :: peer -> Channel m bTX -> m a

      -- | Start a local state query server.
    , aStateQueryServer   :: peer -> Channel m bSQ -> m a
    }

-- | Construct the 'NetworkApplication' for the node-to-client protocols
mkApps
  :: forall m peer blk e bCS bTX bSQ. (IOLike m, Exception e)
  => Tracers m peer blk e
  -> Codecs blk e m bCS bTX bSQ
  -> Handlers m peer blk
  -> Apps m peer bCS bTX bSQ ()
mkApps Tracers {..} Codecs {..} Handlers {..} =
    Apps {..}
  where
    aChainSyncServer
      :: peer
      -> Channel m bCS
      -> m ()
    aChainSyncServer them channel = do
      labelThisThread "LocalChainSyncServer"
      withRegistry $ \registry ->
        runPeer
          (contramap (TraceLabelPeer them) tChainSyncTracer)
          cChainSyncCodec
          channel
          $ chainSyncServerPeer
          $ hChainSyncServer registry

    aTxSubmissionServer
      :: peer
      -> Channel m bTX
      -> m ()
    aTxSubmissionServer them channel = do
      labelThisThread "LocalTxSubmissionServer"
      runPeer
        (contramap (TraceLabelPeer them) tTxSubmissionTracer)
        cTxSubmissionCodec
        channel
        (localTxSubmissionServerPeer (pure hTxSubmissionServer))

    aStateQueryServer
      :: peer
      -> Channel m bSQ
      -> m ()
    aStateQueryServer them channel = do
      labelThisThread "LocalStateQueryServer"
      runPeer
        (contramap (TraceLabelPeer them) tStateQueryTracer)
        cStateQueryCodec
        channel
        (localStateQueryServerPeer hStateQueryServer)

{-------------------------------------------------------------------------------
  Projections from 'Apps'
-------------------------------------------------------------------------------}

-- | A projection from 'NetworkApplication' to a server-side
-- 'OuroborosApplication' for the node-to-client protocols.
responder
  :: N.NodeToClientVersion
  -> Apps m peer b b b a
  -> peer
  -> OuroborosApplication 'ResponderApp b m Void a
responder version Apps {..} peer =
    nodeToClientProtocols
      NodeToClientProtocols {
          localChainSyncProtocol =
            (ResponderProtocolOnly (MuxPeerRaw (aChainSyncServer peer))),
          localTxSubmissionProtocol =
            (ResponderProtocolOnly (MuxPeerRaw (aTxSubmissionServer peer))),
          localStateQueryProtocol =
            (ResponderProtocolOnly (MuxPeerRaw (aStateQueryServer peer)))
        }
      version
