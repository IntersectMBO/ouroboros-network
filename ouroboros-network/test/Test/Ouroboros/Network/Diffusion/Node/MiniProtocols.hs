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

import           Control.Applicative (Alternative)
import           Control.Concurrent.Class.MonadMVar (MonadMVar)
import qualified Control.Concurrent.Class.MonadSTM as LazySTM
import           Control.Concurrent.Class.MonadSTM.Strict
import           Control.Monad.Class.MonadAsync
import           Control.Monad.Class.MonadFork
import           Control.Monad.Class.MonadSay
import           Control.Monad.Class.MonadST
import           Control.Monad.Class.MonadThrow
import           Control.Monad.Class.MonadTime.SI
import           Control.Monad.Class.MonadTimer.SI
import           Control.Tracer (Tracer (..), contramap, nullTracer)
import           Data.ByteString.Lazy (ByteString)
import           Data.Functor (($>))
import           Data.Maybe (fromMaybe)
import           Data.Void (Void)
import           System.Random (StdGen)

import qualified Codec.CBOR.Read as CBOR
import qualified Codec.Serialise as Serialise

import           Network.TypedProtocol.Codec
import           Network.TypedProtocol.PingPong.Client as PingPong
import           Network.TypedProtocol.PingPong.Codec.CBOR
import           Network.TypedProtocol.PingPong.Examples
import           Network.TypedProtocol.PingPong.Server
import           Network.TypedProtocol.PingPong.Type
import           Ouroboros.Network.BlockFetch
import           Ouroboros.Network.BlockFetch.Client
import           Ouroboros.Network.Protocol.BlockFetch.Codec
import           Ouroboros.Network.Protocol.BlockFetch.Examples
import           Ouroboros.Network.Protocol.BlockFetch.Server
import           Ouroboros.Network.Protocol.BlockFetch.Type
import           Ouroboros.Network.Protocol.ChainSync.Client
import           Ouroboros.Network.Protocol.ChainSync.Codec
import           Ouroboros.Network.Protocol.ChainSync.Examples
import           Ouroboros.Network.Protocol.ChainSync.Server
import           Ouroboros.Network.Protocol.ChainSync.Type
import           Ouroboros.Network.Protocol.Handshake.Type
import           Ouroboros.Network.Protocol.Handshake.Unversioned
import           Ouroboros.Network.Protocol.Handshake.Version
                     (simpleSingletonVersions)
import           Ouroboros.Network.Protocol.KeepAlive.Client
import           Ouroboros.Network.Protocol.KeepAlive.Codec
import           Ouroboros.Network.Protocol.KeepAlive.Server
import           Ouroboros.Network.Protocol.KeepAlive.Type

import           Data.Monoid.Synchronisation

import           Ouroboros.Network.Block (HasHeader, HeaderHash, Point)
import qualified Ouroboros.Network.Block as Block
import           Ouroboros.Network.ConnectionId
import           Ouroboros.Network.ControlMessage (ControlMessage (..),
                     ControlMessageSTM)
import qualified Ouroboros.Network.Diffusion as Diff (Applications (..))
import           Ouroboros.Network.Driver.Limits
import           Ouroboros.Network.KeepAlive
import qualified Ouroboros.Network.Mock.Chain as Chain
import           Ouroboros.Network.Mock.ProducerState
import           Ouroboros.Network.Mux
import           Ouroboros.Network.NodeToNode.Version (DiffusionMode (..))
import           Ouroboros.Network.PeerSelection.LedgerPeers (IsBigLedgerPeer,
                     LedgerPeersConsensusInterface)
import           Ouroboros.Network.Util.ShowProxy

import           Ouroboros.Network.Mock.ConcreteBlock

import           Network.TypedProtocol

import qualified Pipes

import           Ouroboros.Network.NodeToNode (blockFetchMiniProtocolNum,
                     chainSyncMiniProtocolNum, keepAliveMiniProtocolNum,
                     peerSharingMiniProtocolNum)
import qualified Ouroboros.Network.PeerSelection.PeerSharing as PSTypes
import           Ouroboros.Network.PeerSharing (bracketPeerSharingClient,
                     peerSharingClient, peerSharingServer)
import           Ouroboros.Network.Protocol.PeerSharing.Client
                     (peerSharingClientPeer)
import           Ouroboros.Network.Protocol.PeerSharing.Codec (codecPeerSharing)
import           Ouroboros.Network.Protocol.PeerSharing.Server
                     (peerSharingServerPeer)
import           Ouroboros.Network.Protocol.PeerSharing.Type (PeerSharing,
                     PeerSharingAmount (..))
import           Test.Ouroboros.Network.Diffusion.Node.NodeKernel


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
  }


-- | Protocol handlers.
--
applications :: forall block header m.
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
                )
             => Tracer m String
             -> NodeKernel header block m
             -> Codecs NtNAddr header block m
             -> LimitsAndTimeouts header block
             -> AppArgs header block m
             -> (block -> header)
             -> Diff.Applications NtNAddr NtNVersion NtNVersionData
                                  NtCAddr NtCVersion NtCVersionData
                                  m ()
applications debugTracer nodeKernel
             Codecs { chainSyncCodec, blockFetchCodec
                    , keepAliveCodec, pingPongCodec
                    , peerSharingCodec
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
               }
             toHeader =
    Diff.Applications
      { Diff.daApplicationInitiatorMode =
          simpleSingletonVersions UnversionedProtocol
                                  (NtNVersionData InitiatorOnlyDiffusionMode aaOwnPeerSharing)
                                  initiatorApp
      , Diff.daApplicationInitiatorResponderMode = \computePeers ->
          simpleSingletonVersions UnversionedProtocol
                                  (NtNVersionData aaDiffusionMode aaOwnPeerSharing)
                                  (initiatorAndResponderApp computePeers)
      , Diff.daLocalResponderApplication =
          simpleSingletonVersions UnversionedProtocol
                                  UnversionedProtocolData
                                  localResponderApp
      , Diff.daLedgerPeersCtx =
          aaLedgerPeersConsensusInterface
      }
  where
    initiatorApp
      :: OuroborosBundle InitiatorMode NtNAddr ByteString m () Void
    -- initiator mode will never run a peer sharing responder side
    initiatorApp = (fmap (fmap (fmap f))) <$> initiatorAndResponderApp (error "impossible happened!")
      where
        f :: MiniProtocol InitiatorResponderMode ByteString m () ()
          -> MiniProtocol InitiatorMode          ByteString m () Void
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
      :: (PeerSharingAmount -> m [NtNAddr])
      -- ^ Peer Sharing result computation callback
      -> OuroborosBundle InitiatorResponderMode NtNAddr ByteString m () ()
    initiatorAndResponderApp computePeers = TemperatureBundle
      { withHot = WithHot $ \ connId controlMessageSTM ->
          [ MiniProtocol
              { miniProtocolNum    = chainSyncMiniProtocolNum
              , miniProtocolLimits = chainSyncLimits limits
              , miniProtocolRun    =
                  InitiatorAndResponderProtocol
                    (chainSyncInitiator connId controlMessageSTM)
                    chainSyncResponder
              }
          , MiniProtocol
              { miniProtocolNum    = blockFetchMiniProtocolNum
              , miniProtocolLimits = blockFetchLimits limits
              , miniProtocolRun    =
                  InitiatorAndResponderProtocol
                    (blockFetchInitiator connId controlMessageSTM)
                    blockFetchResponder
              }
          ]
      , withWarm = WithWarm $ \ connId controlMessageSTM ->
          [ MiniProtocol
              { miniProtocolNum    = MiniProtocolNum 9
              , miniProtocolLimits = pingPongLimits limits
              , miniProtocolRun    =
                  InitiatorAndResponderProtocol
                    (pingPongInitiator connId controlMessageSTM)
                    (pingPongResponder connId)
              }
          ]
      , withEstablished = WithEstablished $ \ connId controlMessageSTM ->
          [ MiniProtocol
              { miniProtocolNum    = keepAliveMiniProtocolNum
              , miniProtocolLimits = keepAliveLimits limits
              , miniProtocolRun    =
                  InitiatorAndResponderProtocol
                    (const $ keepAliveInitiator connId controlMessageSTM)
                    keepAliveResponder
              }
          ] ++ if aaOwnPeerSharing /= PSTypes.NoPeerSharing
                  then [ MiniProtocol
                          { miniProtocolNum    = peerSharingMiniProtocolNum
                          , miniProtocolLimits = peerSharingLimits limits
                          , miniProtocolRun    =
                              InitiatorAndResponderProtocol
                                (peerSharingInitiator controlMessageSTM (remoteAddress connId))
                                (peerSharingResponder computePeers)
                          }
                       ]
                  else []
      }

    localResponderApp
      :: OuroborosApplication ResponderMode NtCAddr ByteString m Void ()
    localResponderApp = OuroborosApplication (\_ _ -> [])

    chainSyncInitiator
      :: ConnectionId NtNAddr
      -> ControlMessageSTM m
      -> IsBigLedgerPeer
      -> MuxPeer ByteString m ()
    chainSyncInitiator ConnectionId { remoteAddress }
                       controlMessageSTM
                       _isBigLedgerPeer =
        MuxPeerRaw $ \channel -> do
          bracketSyncWithFetchClient (nkFetchClientRegistry nodeKernel)
                                     remoteAddress $
            bracket (registerClientChains nodeKernel remoteAddress)
                    (\_ -> unregisterClientChains nodeKernel remoteAddress)
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
      where
        client :: Client header point tip m ()
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

    chainSyncResponder
      :: MuxPeer ByteString m ()
    chainSyncResponder = MuxPeerRaw $ \channel -> do
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
      :: ConnectionId NtNAddr
      -> ControlMessageSTM m
      -> IsBigLedgerPeer
      -> MuxPeer ByteString m ()
    blockFetchInitiator ConnectionId { remoteAddress }
                        controlMessageSTM
                        _isBigLedgerPeer =
      MuxPeerRaw $ \channel -> do
        labelThisThread "BlockFetchClient"
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
      :: MuxPeer ByteString m ()
    blockFetchResponder =
      MuxPeerRaw $ \channel -> do
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
      :: ConnectionId NtNAddr
      -> ControlMessageSTM m
      -> MuxPeer ByteString m ()
    keepAliveInitiator connId@ConnectionId { remoteAddress }
                       controlMessageSTM =
      MuxPeerRaw $ \channel -> do
        labelThisThread "KeepAliveClient"
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
      :: MuxPeer ByteString m ()
    keepAliveResponder = MuxPeerRaw $ \channel -> do
      labelThisThread "KeepAliveServer"
      runPeerWithLimits
        nullTracer
        keepAliveCodec
        (keepAliveSizeLimits limits)
        (keepAliveTimeLimits limits)
        channel
        (keepAliveServerPeer keepAliveServer)

    pingPongInitiator
      :: ConnectionId NtNAddr
      -> ControlMessageSTM m
      -> IsBigLedgerPeer
      -> MuxPeer ByteString m ()
    pingPongInitiator connId
                      controlMessageSTM
                      _isBigLedgerPeer =
        MuxPeerRaw $ \channel ->
          runPeerWithLimits
            ((show . (connId,)) `contramap` debugTracer)
            pingPongCodec
            (pingPongSizeLimits limits)
            (pingPongTimeLimits limits)
            channel
            (pingPongClientPeer pingPongClient)
      where
        continueSTM :: STM m Bool
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

    pingPongResponder
      :: ConnectionId NtNAddr
      -> MuxPeer ByteString m ()
    pingPongResponder connId = MuxPeerRaw $ \channel ->
      runPeerWithLimits
        ((show . (connId,)) `contramap` debugTracer)
        pingPongCodec
        (pingPongSizeLimits limits)
        (pingPongTimeLimits limits)
        channel
        (pingPongServerPeer pingPongServerStandard)


    peerSharingInitiator
      :: ControlMessageSTM m
      -> NtNAddr
      -> IsBigLedgerPeer
      -> MuxPeer ByteString m ()
    peerSharingInitiator controlMessageSTM them _isBigLedgerPee4r =
      MuxPeerRaw $ \channel -> do
        labelThisThread "PeerSharingClient"
        bracketPeerSharingClient (nkPeerSharingRegistry nodeKernel) them
          $ \controller -> do
            psClient <- peerSharingClient controlMessageSTM controller
            runPeerWithLimits
              nullTracer
              peerSharingCodec
              (peerSharingSizeLimits limits)
              (peerSharingTimeLimits limits)
              channel
              (peerSharingClientPeer psClient)

    peerSharingResponder
      :: (PeerSharingAmount -> m [NtNAddr])
      -> MuxPeer ByteString m ()
    peerSharingResponder f = MuxPeerRaw $ \channel -> do
      labelThisThread "PeerSharingServer"
      runPeerWithLimits
        nullTracer
        peerSharingCodec
        (peerSharingSizeLimits limits)
        (peerSharingTimeLimits limits)
        channel
        $ peerSharingServerPeer
        $ peerSharingServer f


--
-- Orphaned Instances
--

instance ShowProxy PingPong where
    showProxy Proxy = "PingPong"
