{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- orphaned 'ShowProxy PingPong' instance.
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Ouroboros.Network.Diffusion.Node.MiniProtocols
  ( Codecs
  , cborCodecs
  , LimitsAndTimeouts (..)
  , AppArgs (..)
  , applications
  ) where

import           Control.Monad.Class.MonadAsync
import           Control.Monad.Class.MonadFork
import           Control.Monad.Class.MonadST
import qualified Control.Monad.Class.MonadSTM as LazySTM
import           Control.Monad.Class.MonadSTM.Strict
import           Control.Monad.Class.MonadThrow
import           Control.Monad.Class.MonadTime
import           Control.Monad.Class.MonadTimer
import           Control.Tracer (nullTracer)
import           Data.ByteString.Lazy (ByteString)
import           Data.Functor (($>))
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

import           Ouroboros.Network.Block (HasHeader, Point)
import qualified Ouroboros.Network.Block as Block
import           Ouroboros.Network.ConnectionId
import qualified Ouroboros.Network.Diffusion as Diff (Applications (..))
import           Ouroboros.Network.Driver.Limits
import           Ouroboros.Network.KeepAlive
import           Ouroboros.Network.Mux
import           Ouroboros.Network.NodeToNode.Version (DiffusionMode (..))
import           Ouroboros.Network.PeerSelection.LedgerPeers
                     (LedgerPeersConsensusInterface)
import           Ouroboros.Network.Util.ShowProxy

import           Ouroboros.Network.Testing.ConcreteBlock

import           Test.Ouroboros.Network.Diffusion.Node.NodeKernel


-- | Protocol codecs.
--
data Codecs block m = Codecs
  { chainSyncCodec :: Codec (ChainSync block (Point block) (Tip block))
                         CBOR.DeserialiseFailure m ByteString
  , keepAliveCodec :: Codec KeepAlive
                         CBOR.DeserialiseFailure m ByteString
  , pingPongCodec  :: Codec PingPong
                         CBOR.DeserialiseFailure m ByteString
  }

cborCodecs :: MonadST m => Codecs Block m
cborCodecs = Codecs
  { chainSyncCodec = codecChainSync Serialise.encode Serialise.decode
                                    Serialise.encode Serialise.decode
                                    (Block.encodeTip Serialise.encode)
                                    (Block.decodeTip Serialise.decode)
  , keepAliveCodec = codecKeepAlive
  , pingPongCodec  = codecPingPong
  }


-- | Limits and protocol timeouts
data LimitsAndTimeouts block = LimitsAndTimeouts
  { -- chain-sync
    chainSyncLimits
      :: MiniProtocolLimits
  , chainSyncSizeLimits
      :: ProtocolSizeLimits (ChainSync block (Point block) (Tip block)) ByteString
  , chainSyncTimeLimits
      :: ProtocolTimeLimits (ChainSync block (Point block) (Tip block))

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
      :: ProtocolSizeLimits (Handshake NtNVersion NtNVersionData) ByteString
  , handhsakeSizeLimits
      :: ProtocolTimeLimits (Handshake NtNVersion NtNVersionData)
  }


-- | Arguments for protocol handlers required by 'nodeApplications'.
--
data AppArgs m = AppArgs
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
  }


-- | Protocol handlers.
--
applications :: forall block m.
                ( MonadAsync m
                , MonadFork  m
                , MonadMask  m
                , MonadThrow m
                , MonadTime  m
                , MonadTimer m
                , MonadThrow (STM m)
                , HasHeader block
                , ShowProxy block
                )
             => NodeKernel block m
             -> Codecs block m
             -> LimitsAndTimeouts block
             -> AppArgs m
             -> m (Diff.Applications NtNAddr NtNVersion NtNVersionData
                                     NtCAddr NtCVersion NtCVersionData
                                     m)
applications nodeKernel
             Codecs { chainSyncCodec, keepAliveCodec, pingPongCodec }
             limits
             AppArgs
               { aaLedgerPeersConsensusInterface
               , aaDiffusionMode
               , aaKeepAliveStdGen
               , aaKeepAliveInterval
               , aaPingPongInterval
               }
             = do
    return $ Diff.Applications
      { Diff.daApplicationInitiatorMode =
          simpleSingletonVersions UnversionedProtocol
                                  (NtNVersionData InitiatorOnlyDiffusionMode)
                                  initiatorApp
      , Diff.daApplicationInitiatorResponderMode =
          simpleSingletonVersions UnversionedProtocol
                                  (NtNVersionData aaDiffusionMode)
                                  initiatorAndResponderApp
      , Diff.daLocalResponderApplication =
          simpleSingletonVersions UnversionedProtocol
                                  UnversionedProtocolData
                                  localResponderApp
      , Diff.daLedgerPeersCtx =
          aaLedgerPeersConsensusInterface
      }
  where
    -- TODO: initiator app can be derived from 'initiatorAndResponderApp'
    initiatorApp
      :: OuroborosBundle InitiatorMode NtNAddr ByteString m () Void
    initiatorApp = (fmap (fmap (fmap f))) <$> initiatorAndResponderApp
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
      :: OuroborosBundle InitiatorResponderMode NtNAddr ByteString m () ()
    initiatorAndResponderApp = Bundle
      { withHot = WithHot $ \ connId controlMessageSTM ->
          [ MiniProtocol
              { miniProtocolNum    = MiniProtocolNum 1
              , miniProtocolLimits = chainSyncLimits limits
              , miniProtocolRun    =
                  InitiatorAndResponderProtocol
                    (chainSyncInitiator connId controlMessageSTM)
                    chainSyncResponder
              }
          ]
      , withWarm = WithWarm $ \ _connId controlMessageSTM ->
          [ MiniProtocol
              { miniProtocolNum    = MiniProtocolNum 9
              , miniProtocolLimits = pingPongLimits limits
              , miniProtocolRun    =
                  InitiatorAndResponderProtocol
                    (pingPongInitiator controlMessageSTM)
                    pingPongResponder
              }
          ]
      , withEstablished = WithEstablished $ \ connId controlMessageSTM ->
          [ MiniProtocol
              { miniProtocolNum    = MiniProtocolNum 8
              , miniProtocolLimits = keepAliveLimits limits
              , miniProtocolRun    =
                  InitiatorAndResponderProtocol
                    (keepAliveInitiator connId controlMessageSTM)
                    keepAliveResponder
              }
          ]
      }

    localResponderApp
      :: OuroborosApplication ResponderMode NtCAddr ByteString m Void ()
    localResponderApp = OuroborosApplication (\_ _ -> [])

    chainSyncInitiator
      :: ConnectionId NtNAddr
      -> ControlMessageSTM m
      -> MuxPeer ByteString m ()
    chainSyncInitiator ConnectionId { remoteAddress }
                       controlMessageSTM =
      MuxPeerRaw $ \channel ->
        bracket (registerClient nodeKernel remoteAddress)
                (\_ -> unregisterClient nodeKernel remoteAddress)
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
                         (controlledClient controlMessageSTM))
                )

    chainSyncResponder
      :: MuxPeer ByteString m ()
    chainSyncResponder = MuxPeerRaw $ \channel ->
      runPeerWithLimits
        nullTracer
        chainSyncCodec
        (chainSyncSizeLimits limits)
        (chainSyncTimeLimits limits)
        channel
        (chainSyncServerPeer
          (chainSyncServerExample
            () (nkChainProducerState nodeKernel)))

    keepAliveInitiator
      :: ConnectionId NtNAddr
      -> ControlMessageSTM m
      -> MuxPeer ByteString m ()
    keepAliveInitiator ConnectionId { remoteAddress }
                       controlMessageSTM =
      MuxPeerRaw $ \channel ->
        runPeerWithLimits
          nullTracer
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
               (nkKeepAliveCtx nodeKernel)
               (KeepAliveInterval aaKeepAliveInterval))

    keepAliveResponder
      :: MuxPeer ByteString m ()
    keepAliveResponder = MuxPeerRaw $ \channel ->
      runPeerWithLimits
        nullTracer
        keepAliveCodec
        (keepAliveSizeLimits limits)
        (keepAliveTimeLimits limits)
        channel
        (keepAliveServerPeer keepAliveServer)

    pingPongInitiator
      :: ControlMessageSTM m
      -> MuxPeer ByteString m ()
    pingPongInitiator controlMessageSTM = MuxPeerRaw $ \channel ->
        runPeerWithLimits
          nullTracer
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
      :: MuxPeer ByteString m ()
    pingPongResponder = MuxPeerRaw $ \channel ->
      runPeerWithLimits
        nullTracer
        pingPongCodec
        (pingPongSizeLimits limits)
        (pingPongTimeLimits limits)
        channel
        (pingPongServerPeer pingPongServerStandard)



--
-- Orphaned Instances
--

instance ShowProxy PingPong where
    showProxy Proxy = "PingPong"
