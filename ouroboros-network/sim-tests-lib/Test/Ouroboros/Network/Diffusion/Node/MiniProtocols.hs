{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeOperators       #-}

-- orphaned 'ShowProxy PingPong' instance.
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Ouroboros.Network.Diffusion.Node.MiniProtocols
  ( Codecs
  , cborCodecs
  , LimitsAndTimeouts (..)
  , AppArgs (..)
  , applications
  ) where

import Control.Applicative (Alternative)
import Control.Concurrent.Class.MonadMVar (MonadMVar)
import Control.Concurrent.Class.MonadSTM qualified as LazySTM
import Control.Concurrent.Class.MonadSTM.Strict
import Control.Monad.Class.MonadAsync
import Control.Monad.Class.MonadFork
import Control.Monad.Class.MonadSay
import Control.Monad.Class.MonadST
import Control.Monad.Class.MonadThrow
import Control.Monad.Class.MonadTime.SI
import Control.Monad.Class.MonadTimer.SI
import Control.Tracer (Tracer (..), contramap, nullTracer)
import Data.ByteString.Lazy (ByteString)
import Data.Functor (($>))
import Data.Maybe (fromMaybe)
import Data.Void (Void)
import System.Random (RandomGen, StdGen)

import Codec.CBOR.Read qualified as CBOR
import Codec.Serialise qualified as Serialise

import Network.TypedProtocol.Codec
import Network.TypedProtocol.PingPong.Client as PingPong
import Network.TypedProtocol.PingPong.Codec.CBOR
import Network.TypedProtocol.PingPong.Examples
import Network.TypedProtocol.PingPong.Server
import Network.TypedProtocol.PingPong.Type
import Ouroboros.Network.BlockFetch
import Ouroboros.Network.BlockFetch.Client
import Ouroboros.Network.Protocol.BlockFetch.Codec
import Ouroboros.Network.Protocol.BlockFetch.Examples
import Ouroboros.Network.Protocol.BlockFetch.Server
import Ouroboros.Network.Protocol.BlockFetch.Type
import Ouroboros.Network.Protocol.ChainSync.Client
import Ouroboros.Network.Protocol.ChainSync.Codec
import Ouroboros.Network.Protocol.ChainSync.Examples
import Ouroboros.Network.Protocol.ChainSync.Server
import Ouroboros.Network.Protocol.ChainSync.Type
import Ouroboros.Network.Protocol.Handshake.Type
import Ouroboros.Network.Protocol.Handshake.Unversioned
import Ouroboros.Network.Protocol.Handshake.Version (simpleSingletonVersions)
import Ouroboros.Network.Protocol.KeepAlive.Client
import Ouroboros.Network.Protocol.KeepAlive.Codec
import Ouroboros.Network.Protocol.KeepAlive.Server
import Ouroboros.Network.Protocol.KeepAlive.Type

import Data.Monoid.Synchronisation

import Ouroboros.Network.Block (HasHeader, HeaderHash, Point)
import Ouroboros.Network.Block qualified as Block
import Ouroboros.Network.Context
import Ouroboros.Network.ControlMessage (ControlMessage (..))
import Ouroboros.Network.Diffusion qualified as Diff (Applications (..))
import Ouroboros.Network.Driver.Limits
import Ouroboros.Network.KeepAlive
import Ouroboros.Network.Mock.Chain qualified as Chain
import Ouroboros.Network.Mock.ProducerState
import Ouroboros.Network.Mux
import Ouroboros.Network.NodeToNode.Version (DiffusionMode (..))
import Ouroboros.Network.Util.ShowProxy

import Ouroboros.Network.Mock.ConcreteBlock

import Network.TypedProtocol

import Pipes qualified

import Ouroboros.Network.NodeToNode (blockFetchMiniProtocolNum,
           chainSyncMiniProtocolNum, keepAliveMiniProtocolNum,
           peerSharingMiniProtocolNum, txSubmissionMiniProtocolNum)
import Ouroboros.Network.PeerSelection.LedgerPeers
import Ouroboros.Network.PeerSelection.LocalRootPeers (OutboundConnectionsState)
import Ouroboros.Network.PeerSelection.PeerSharing qualified as PSTypes
import Ouroboros.Network.PeerSharing (PeerSharingAPI, bracketPeerSharingClient,
           peerSharingClient, peerSharingServer)
import Ouroboros.Network.Protocol.PeerSharing.Client (peerSharingClientPeer)
import Ouroboros.Network.Protocol.PeerSharing.Codec (codecPeerSharing)
import Ouroboros.Network.Protocol.PeerSharing.Server (peerSharingServerPeer)
import Ouroboros.Network.Protocol.PeerSharing.Type (PeerSharing)
import Ouroboros.Network.Protocol.TxSubmission2.Client (txSubmissionClientPeer)
import Ouroboros.Network.Protocol.TxSubmission2.Server
           (txSubmissionServerPeerPipelined)
import Ouroboros.Network.Protocol.TxSubmission2.Type (NumTxIdsToAck (..),
           NumTxIdsToReq (..), TxSubmission2)
import Ouroboros.Network.TxSubmission.Inbound.Policy (TxDecisionPolicy (..))
import Ouroboros.Network.TxSubmission.Inbound.Registry (SharedTxStateVar,
           TxChannelsVar, withPeer)
import Ouroboros.Network.TxSubmission.Inbound.Server (txSubmissionInboundV2)
import Ouroboros.Network.TxSubmission.Inbound.Types (TraceTxLogic,
           TraceTxSubmissionInbound)
import Ouroboros.Network.TxSubmission.Outbound (txSubmissionOutbound)
import Test.Ouroboros.Network.Diffusion.Node.NodeKernel
import Test.Ouroboros.Network.TxSubmission.Types (Mempool, Tx, getMempoolReader,
           getMempoolWriter, txSubmissionCodec2)


-- | Protocol codecs.
--
data Codecs addr header block m = Codecs
  { chainSyncCodec   :: Codec (ChainSync header (Point block) (Tip block))
                          CBOR.DeserialiseFailure m ByteString
  , blockFetchCodec  :: Codec (BlockFetch block (Point block))
                          CBOR.DeserialiseFailure m ByteString
  , keepAliveCodec   :: Codec KeepAlive
                          CBOR.DeserialiseFailure m ByteString
  , pingPongCodec    :: Codec PingPong
                          CBOR.DeserialiseFailure m ByteString
  , peerSharingCodec :: Codec (PeerSharing addr)
                         CBOR.DeserialiseFailure m ByteString
  , txSubmissionCodec :: Codec (TxSubmission2 Int (Tx Int))
                            CBOR.DeserialiseFailure m ByteString
  }

cborCodecs :: MonadST m => Codecs NtNAddr BlockHeader Block m
cborCodecs = Codecs
  { chainSyncCodec = codecChainSync Serialise.encode Serialise.decode
                                    Serialise.encode Serialise.decode
                                    (Block.encodeTip Serialise.encode)
                                    (Block.decodeTip Serialise.decode)
  , blockFetchCodec = codecBlockFetch Serialise.encode Serialise.decode
                                      Serialise.encode Serialise.decode
  , keepAliveCodec = codecKeepAlive_v2
  , pingPongCodec  = codecPingPong
  , peerSharingCodec  = codecPeerSharing encodeNtNAddr decodeNtNAddr
  , txSubmissionCodec = txSubmissionCodec2
  }


-- | Limits and protocol timeouts
data LimitsAndTimeouts header block = LimitsAndTimeouts
  { -- chain-sync
    chainSyncLimits
      :: MiniProtocolLimits
  , chainSyncSizeLimits
      :: ProtocolSizeLimits (ChainSync header (Point block) (Tip block))
                            ByteString
  , chainSyncTimeLimits
      :: ProtocolTimeLimits (ChainSync header (Point block) (Tip block))

    -- block-fetch
  , blockFetchLimits
      :: MiniProtocolLimits
  , blockFetchSizeLimits
      :: ProtocolSizeLimits (BlockFetch block (Point block)) ByteString
  , blockFetchTimeLimits
      :: ProtocolTimeLimits (BlockFetch block (Point block))

    -- keep-alive
  , keepAliveLimits
      :: MiniProtocolLimits
  , keepAliveSizeLimits
      :: ProtocolSizeLimits KeepAlive ByteString
  , keepAliveTimeLimits
      :: ProtocolTimeLimits KeepAlive

    -- ping-pong
  , pingPongLimits
      :: MiniProtocolLimits
  , pingPongSizeLimits
      :: ProtocolSizeLimits PingPong ByteString
  , pingPongTimeLimits
      :: ProtocolTimeLimits PingPong

    -- handshake
  , handshakeLimits
      :: MiniProtocolLimits
  , handshakeTimeLimits
      :: ProtocolTimeLimits (Handshake NtNVersion NtNVersionData)
  , handhsakeSizeLimits
      :: ProtocolSizeLimits (Handshake NtNVersion NtNVersionData) ByteString

    -- peer sharing
  , peerSharingLimits
      :: MiniProtocolLimits
  , peerSharingTimeLimits
      :: ProtocolTimeLimits (PeerSharing NtNAddr)
  , peerSharingSizeLimits
      :: ProtocolSizeLimits (PeerSharing NtNAddr) ByteString

    -- tx submission
  , txSubmissionLimits
      :: MiniProtocolLimits
  , txSubmissionTimeLimits
      :: ProtocolTimeLimits (TxSubmission2 Int (Tx Int))
  , txSubmissionSizeLimits
      :: ProtocolSizeLimits (TxSubmission2 Int (Tx Int)) ByteString
  }


-- | Arguments for protocol handlers required by 'nodeApplications'.
--
data AppArgs header block m = AppArgs
  { aaLedgerPeersConsensusInterface
     :: LedgerPeersConsensusInterface m
  , aaKeepAliveStdGen
     :: StdGen
  , aaDiffusionMode
     :: DiffusionMode
  , aaKeepAliveInterval
     :: DiffTime
  , aaPingPongInterval
     :: DiffTime

    -- | if returns true, `chain-sync` client will exit as soon as it will see
    -- that block.
    --
  , aaShouldChainSyncExit :: header -> m Bool

    -- | if true, `chain-sync` will never go pass the query tip phase.  This
    -- simulates too far behind the chain in a crude way.
    --
  , aaChainSyncEarlyExit  :: Bool
  , aaOwnPeerSharing
     :: PSTypes.PeerSharing
  , aaUpdateOutboundConnectionsState
     :: OutboundConnectionsState -> STM m ()

  , aaTxDecisionPolicy :: TxDecisionPolicy
  }


-- | Protocol handlers.
--
applications :: forall block header s m.
                ( Alternative (STM m)
                , MonadAsync m
                , MonadFork  m
                , MonadMask  m
                , MonadMVar  m
                , MonadSay   m
                , MonadThrow m
                , MonadTime  m
                , MonadTimer m
                , MonadThrow (STM m)
                , HasHeader header
                , HasHeader block
                , HeaderHash header ~ HeaderHash block
                , Show block
                , ShowProxy block
                , ShowProxy header
                , RandomGen s
                )
             => Tracer m String
             -> Tracer m (TraceTxSubmissionInbound Int (Tx Int))
             -> Tracer m (TraceTxLogic NtNAddr Int (Tx Int))
             -> NodeKernel header block s Int m
             -> Codecs NtNAddr header block m
             -> LimitsAndTimeouts header block
             -> AppArgs header block m
             -> (block -> header)
             -> Diff.Applications NtNAddr NtNVersion NtNVersionData
                                  NtCAddr NtCVersion NtCVersionData
                                  m ()
applications debugTracer txSubmissionInboundTracer txSubmissionInboundDebug nodeKernel
             Codecs { chainSyncCodec, blockFetchCodec
                    , keepAliveCodec, pingPongCodec
                    , peerSharingCodec
                    , txSubmissionCodec
                    }
             limits
             AppArgs
               { aaLedgerPeersConsensusInterface
               , aaDiffusionMode
               , aaKeepAliveStdGen
               , aaKeepAliveInterval
               , aaPingPongInterval
               , aaShouldChainSyncExit
               , aaChainSyncEarlyExit
               , aaOwnPeerSharing
               , aaUpdateOutboundConnectionsState
               , aaTxDecisionPolicy
               }
             toHeader =
    Diff.Applications
      { Diff.daApplicationInitiatorMode =
          simpleSingletonVersions UnversionedProtocol
                                  (NtNVersionData InitiatorOnlyDiffusionMode aaOwnPeerSharing)
                                  initiatorApp
      , Diff.daApplicationInitiatorResponderMode =
          simpleSingletonVersions UnversionedProtocol
                                  (NtNVersionData aaDiffusionMode aaOwnPeerSharing)
                                  initiatorAndResponderApp
      , Diff.daLocalResponderApplication =
          simpleSingletonVersions UnversionedProtocol
                                  UnversionedProtocolData
                                  localResponderApp
      , Diff.daLedgerPeersCtx =
          aaLedgerPeersConsensusInterface
      , Diff.daUpdateOutboundConnectionsState =
          aaUpdateOutboundConnectionsState
      }
  where
    initiatorApp
      :: OuroborosBundleWithExpandedCtx InitiatorMode NtNAddr ByteString m () Void
    -- initiator mode will never run a peer sharing responder side
    initiatorApp = fmap f <$> initiatorAndResponderApp
      where
        f :: MiniProtocolWithExpandedCtx InitiatorResponderMode NtNAddr ByteString m () ()
          -> MiniProtocolWithExpandedCtx InitiatorMode          NtNAddr ByteString m () Void
        f MiniProtocol { miniProtocolNum
                       , miniProtocolLimits
                       , miniProtocolRun } =
          MiniProtocol { miniProtocolNum
                       , miniProtocolLimits
                       , miniProtocolRun =
                          case miniProtocolRun of
                            InitiatorAndResponderProtocol initiator _respnder ->
                              InitiatorProtocolOnly initiator
                       }

    initiatorAndResponderApp
      :: OuroborosBundleWithExpandedCtx InitiatorResponderMode NtNAddr ByteString m () ()
    initiatorAndResponderApp = TemperatureBundle
      { withHot = WithHot
          [ MiniProtocol
              { miniProtocolNum    = chainSyncMiniProtocolNum
              , miniProtocolLimits = chainSyncLimits limits
              , miniProtocolRun    =
                  InitiatorAndResponderProtocol
                    chainSyncInitiator
                    chainSyncResponder
              }
          , MiniProtocol
              { miniProtocolNum    = blockFetchMiniProtocolNum
              , miniProtocolLimits = blockFetchLimits limits
              , miniProtocolRun    =
                  InitiatorAndResponderProtocol
                    blockFetchInitiator
                    blockFetchResponder
              }

          , MiniProtocol {
              miniProtocolNum    = txSubmissionMiniProtocolNum,
              miniProtocolLimits = txSubmissionLimits limits,
              miniProtocolRun    =
                  InitiatorAndResponderProtocol
                    (txSubmissionInitiator aaTxDecisionPolicy (nkMempool nodeKernel))
                    (txSubmissionResponder (nkMempool nodeKernel)
                                          (nkTxChannelsVar nodeKernel)
                                          (nkSharedTxStateVar nodeKernel))
            }
          ]
      , withWarm = WithWarm
          [ MiniProtocol
              { miniProtocolNum    = MiniProtocolNum 9
              , miniProtocolLimits = pingPongLimits limits
              , miniProtocolRun    =
                  InitiatorAndResponderProtocol
                    pingPongInitiator
                    pingPongResponder
              }
          ]
      , withEstablished = WithEstablished $
          [ MiniProtocol
              { miniProtocolNum    = keepAliveMiniProtocolNum
              , miniProtocolLimits = keepAliveLimits limits
              , miniProtocolRun    =
                  InitiatorAndResponderProtocol
                    keepAliveInitiator
                    keepAliveResponder
              }
          ] ++ if aaOwnPeerSharing /= PSTypes.PeerSharingDisabled
                  then [ MiniProtocol
                          { miniProtocolNum    = peerSharingMiniProtocolNum
                          , miniProtocolLimits = peerSharingLimits limits
                          , miniProtocolRun    =
                              InitiatorAndResponderProtocol
                                peerSharingInitiator
                                (peerSharingResponder (nkPeerSharingAPI nodeKernel))
                          }
                       ]
                  else []
      }

    localResponderApp
      :: OuroborosApplicationWithMinimalCtx
           ResponderMode NtCAddr ByteString m Void ()
    localResponderApp = OuroborosApplication []

    chainSyncInitiator
      :: MiniProtocolCb (ExpandedInitiatorContext NtNAddr m) ByteString m ()
    chainSyncInitiator =
      MiniProtocolCb $
        \  ExpandedInitiatorContext {
             eicConnectionId   = connId,
             eicControlMessage = controlMessageSTM
           }
           channel
        ->
          let client :: Client header point tip m ()
              client = go
                where
                  go = Client
                    { rollbackward = \_ _ -> do
                        ctrl <- atomically controlMessageSTM
                        case ctrl of
                          Continue  -> pure (Right go)
                          Quiesce   -> error "Ouroboros.Network.Protocol.ChainSync.Examples.controlledClient: unexpected Quiesce"
                          Terminate -> pure (Left ())
                    , rollforward = \header -> do
                        exit <- aaShouldChainSyncExit header
                        if exit
                          then pure (Left ())
                          else do ctrl <- atomically controlMessageSTM
                                  case ctrl of
                                    Continue  -> pure (Right go)
                                    Quiesce   -> error "Ouroboros.Network.Protocol.ChainSync.Examples.controlledClient: unexpected Quiesce"
                                    Terminate -> pure (Left ())
                    , points = \_ -> pure $
                                       if aaChainSyncEarlyExit
                                       then Left ()
                                       else Right go
                    }
          in do labelThisThread "ChainSyncClient"
                bracketSyncWithFetchClient (nkFetchClientRegistry nodeKernel)
                                           (remoteAddress connId) $
                  bracket (registerClientChains nodeKernel (remoteAddress connId))
                          (\_ -> unregisterClientChains nodeKernel (remoteAddress connId))
                          (\chainVar ->
                            runPeerWithLimits
                              nullTracer
                              chainSyncCodec
                              (chainSyncSizeLimits limits)
                              (chainSyncTimeLimits limits)
                              channel
                              (chainSyncClientPeer $
                                 chainSyncClientExample
                                   chainVar
                                   client)
                          )

    chainSyncResponder
      :: MiniProtocolCb (ResponderContext NtNAddr) ByteString m ()
    chainSyncResponder = MiniProtocolCb $ \_ctx channel -> do
      labelThisThread "ChainSyncServer"
      runPeerWithLimits
        nullTracer
        chainSyncCodec
        (chainSyncSizeLimits limits)
        (chainSyncTimeLimits limits)
        channel
        (chainSyncServerPeer
          (chainSyncServerExample
            () (nkChainProducerState nodeKernel) toHeader))

    blockFetchInitiator
      :: MiniProtocolCb (ExpandedInitiatorContext NtNAddr m) ByteString m ()
    blockFetchInitiator  =
      MiniProtocolCb $
      \  ExpandedInitiatorContext {
           eicConnectionId   = ConnectionId { remoteAddress },
           eicControlMessage = controlMessageSTM
         }
         channel
      -> do labelThisThread "BlockFetchClient"
            bracketFetchClient (nkFetchClientRegistry nodeKernel)
                               UnversionedProtocol
                               (const NotReceivingTentativeBlocks)
                               remoteAddress
                               $ \clientCtx ->
              runPeerWithLimits
                nullTracer
                blockFetchCodec
                (blockFetchSizeLimits limits)
                (blockFetchTimeLimits limits)
                channel
                (forgetPipelined
                  $ blockFetchClient UnversionedProtocol controlMessageSTM
                                     nullTracer clientCtx)

    blockFetchResponder
      :: MiniProtocolCb (ResponderContext NtNAddr) ByteString m ()
    blockFetchResponder =
      MiniProtocolCb $ \_ctx channel -> do
        labelThisThread "BlockFetchServer"
        runPeerWithLimits
          nullTracer
          blockFetchCodec
          (blockFetchSizeLimits limits)
          (blockFetchTimeLimits limits)
          channel
          (blockFetchServerPeer $
            blockFetchServer
            (constantRangeRequests $ \(ChainRange from to) -> do
              nkChainProducer <- Pipes.lift
                               $ readTVarIO (nkChainProducerState nodeKernel)
              Pipes.each $ fromMaybe []
                         $ Chain.selectBlockRange (chainState nkChainProducer)
                                                  from
                                                  to
            )
          )

    keepAliveInitiator
      :: MiniProtocolCb (ExpandedInitiatorContext NtNAddr m) ByteString m ()
    keepAliveInitiator  =
      MiniProtocolCb $
      \  ExpandedInitiatorContext {
           eicConnectionId   = connId@ConnectionId { remoteAddress },
           eicControlMessage = controlMessageSTM
         }
         channel
      -> do labelThisThread "KeepAliveClient"
            let kacApp =
                  \ctxVar -> runPeerWithLimits
                               ((show . (connId,)) `contramap` debugTracer)
                               keepAliveCodec
                               (keepAliveSizeLimits limits)
                               (keepAliveTimeLimits limits)
                               channel
                               (keepAliveClientPeer $
                                  keepAliveClient
                                    nullTracer
                                    aaKeepAliveStdGen
                                    controlMessageSTM
                                    remoteAddress
                                    ctxVar
                                    (KeepAliveInterval aaKeepAliveInterval))
            bracketKeepAliveClient (nkFetchClientRegistry nodeKernel)
                                   remoteAddress
                                   kacApp

    keepAliveResponder
      :: MiniProtocolCb (ResponderContext NtNAddr) ByteString m ()
    keepAliveResponder = MiniProtocolCb $ \_ctx channel -> do
      labelThisThread "KeepAliveServer"
      runPeerWithLimits
        nullTracer
        keepAliveCodec
        (keepAliveSizeLimits limits)
        (keepAliveTimeLimits limits)
        channel
        (keepAliveServerPeer keepAliveServer)

    pingPongInitiator
      :: MiniProtocolCb (ExpandedInitiatorContext NtNAddr m) ByteString m ()
    pingPongInitiator  =
        MiniProtocolCb $
        \  ExpandedInitiatorContext {
             eicConnectionId   = connId,
             eicControlMessage = controlMessageSTM
           }
           channel
        -> let continueSTM :: STM m Bool
               continueSTM = do
                 ctrl <- controlMessageSTM
                 case ctrl of
                   Continue  -> return True
                   Quiesce   -> retry
                   Terminate -> return False

               pingPongClient :: PingPongClient m ()
               pingPongClient = SendMsgPing $ do
                 v <- registerDelay aaPingPongInterval
                 -- block on the timer, but terminate as soon
                 -- as 'ctroContinue' returns 'False'.
                 --
                 -- Note that if both branches of '<>' return they will return the same
                 -- value (which must be 'False') so it does not matter which branch is
                 -- picked.
                 continue <- atomically $ runFirstToFinish $
                      ( FirstToFinish $ do
                          LazySTM.readTVar v >>= check
                          continueSTM )
                   <> ( FirstToFinish $ do
                          continueSTM >>= \b -> check (not b) $> b )
                 if continue
                   then return   pingPongClient
                   else return $ PingPong.SendMsgDone ()
           in runPeerWithLimits
               ((show . (connId,)) `contramap` debugTracer)
               pingPongCodec
               (pingPongSizeLimits limits)
               (pingPongTimeLimits limits)
               channel
               (pingPongClientPeer pingPongClient)

    pingPongResponder
      :: MiniProtocolCb (ResponderContext NtNAddr) ByteString m ()
    pingPongResponder  = MiniProtocolCb $
      \ResponderContext { rcConnectionId = connId } channel ->
      runPeerWithLimits
        ((show . (connId,)) `contramap` debugTracer)
        pingPongCodec
        (pingPongSizeLimits limits)
        (pingPongTimeLimits limits)
        channel
        (pingPongServerPeer pingPongServerStandard)


    peerSharingInitiator
      :: MiniProtocolCb (ExpandedInitiatorContext NtNAddr m) ByteString m ()
    peerSharingInitiator  =
      MiniProtocolCb $
       \  ExpandedInitiatorContext {
            eicConnectionId   = connId@ConnectionId { remoteAddress = them },
            eicControlMessage = controlMessageSTM
          }
          channel
       -> do labelThisThread "PeerSharingClient"
             bracketPeerSharingClient (nkPeerSharingRegistry nodeKernel) them
               $ \controller -> do
                 psClient <- peerSharingClient controlMessageSTM controller
                 runPeerWithLimits
                   ((show . (connId,)) `contramap` debugTracer)
                   peerSharingCodec
                   (peerSharingSizeLimits limits)
                   (peerSharingTimeLimits limits)
                   channel
                   (peerSharingClientPeer psClient)

    peerSharingResponder
      :: PeerSharingAPI NtNAddr s m
      -> MiniProtocolCb (ResponderContext NtNAddr) ByteString m ()
    peerSharingResponder psAPI = MiniProtocolCb $ \ResponderContext { rcConnectionId = connId } channel -> do
      labelThisThread "PeerSharingServer"
      runPeerWithLimits
        ((show . (connId,)) `contramap` debugTracer)
        peerSharingCodec
        (peerSharingSizeLimits limits)
        (peerSharingTimeLimits limits)
        channel
        $ peerSharingServerPeer
        $ peerSharingServer psAPI

    txSubmissionInitiator
      :: TxDecisionPolicy
      -> Mempool m Int
      -> MiniProtocolCb (ExpandedInitiatorContext NtNAddr m) ByteString m ()
    txSubmissionInitiator txDecisionPolicy mempool =
      MiniProtocolCb $
        \ ExpandedInitiatorContext {
            eicConnectionId   = connId,
            eicControlMessage = controlMessageSTM
          }
          channel
        -> do
          let client = txSubmissionOutbound
                         ((show . (connId,)) `contramap` debugTracer)
                         (NumTxIdsToAck $ getNumTxIdsToReq
                                        $ maxUnacknowledgedTxIds
                                        $ txDecisionPolicy)
                         (getMempoolReader mempool)
                         maxBound
                         controlMessageSTM
          labelThisThread "TxSubmissionClient"
          runPeerWithLimits
            ((show . (connId,)) `contramap` debugTracer)
            txSubmissionCodec
            (txSubmissionSizeLimits limits)
            (txSubmissionTimeLimits limits)
            channel
            (txSubmissionClientPeer client)

    txSubmissionResponder
      :: Mempool m Int
      -> TxChannelsVar m NtNAddr Int (Tx Int)
      -> SharedTxStateVar m NtNAddr Int (Tx Int)
      -> MiniProtocolCb (ResponderContext NtNAddr) ByteString m ()
    txSubmissionResponder mempool txChannelsVar sharedTxStateVar =
      MiniProtocolCb $
        \ ResponderContext { rcConnectionId = connId@ConnectionId { remoteAddress = them }} channel
        -> do
          withPeer txSubmissionInboundDebug
                   txChannelsVar
                   sharedTxStateVar
                   (getMempoolReader mempool)
                   them $ \api -> do
            let server = txSubmissionInboundV2
                           txSubmissionInboundTracer
                           (getMempoolReader mempool)
                           (getMempoolWriter mempool)
                           api
            labelThisThread "TxSubmissionServer"
            runPipelinedPeerWithLimits
              ((show . (connId,)) `contramap` debugTracer)
              txSubmissionCodec
              (txSubmissionSizeLimits limits)
              (txSubmissionTimeLimits limits)
              channel
              (txSubmissionServerPeerPipelined server)

--
-- Orphaned Instances
--

instance ShowProxy PingPong where
    showProxy Proxy = "PingPong"
