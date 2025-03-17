{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

module Cardano.Client.Subscription
  ( -- * Subscription API
    subscribe
  , SubscriptionParams (..)
  , Decision (..)
  , SubscriptionTracers (..)
  , SubscriptionTrace (..)
    -- * Re-exports
    -- ** Mux
  , MuxMode
  , MuxTrace
  , Mx.WithBearer
    -- ** Connections
  , ConnectionId (..)
  , LocalAddress (..)
    -- ** Protocol API
  , NodeToClientProtocols (..)
  , MiniProtocolCb (..)
  , RunMiniProtocol (..)
  , ControlMessage (..)
  ) where

import Codec.CBOR.Term qualified as CBOR
import Control.Exception
import Control.Monad (join)
import Control.Monad.Class.MonadTime.SI
import Control.Monad.Class.MonadTimer.SI
import Control.Tracer (Tracer, traceWith)
import Data.ByteString.Lazy qualified as BSL
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Void (Void)

import Network.Mux qualified as Mx

import Ouroboros.Network.ControlMessage (ControlMessage (..))
import Ouroboros.Network.Magic (NetworkMagic)
import Ouroboros.Network.Mux (MiniProtocolCb (..),
           OuroborosApplicationWithMinimalCtx, RunMiniProtocol (..))

import Ouroboros.Network.ConnectionId (ConnectionId (..))
import Ouroboros.Network.NodeToClient (Handshake, LocalAddress (..),
           NetworkConnectTracers (..), NodeToClientProtocols,
           NodeToClientVersion, NodeToClientVersionData (..), TraceSendRecv,
           Versions)
import Ouroboros.Network.NodeToClient qualified as NtC
import Ouroboros.Network.Snocket qualified as Snocket

type MuxMode  = Mx.Mode
type MuxTrace = Mx.Trace

data SubscriptionParams a = SubscriptionParams
  { spAddress           :: !LocalAddress
  -- ^ unix socket or named pipe address
  , spReconnectionDelay :: !(Maybe DiffTime)
  -- ^ delay between connection attempts.  The default value is `5s`.
  , spCompleteCb        :: Either SomeException a -> Decision
  }

data Decision =
    Abort
    -- ^ abort subscription loop
  | Reconnect
    -- ^ reconnect

data SubscriptionTracers a = SubscriptionTracers {
      stMuxTracer          :: Tracer IO (Mx.WithBearer (ConnectionId LocalAddress) MuxTrace),
      -- ^ low level mux-network tracer, which logs mux sdu (send and received)
      -- and other low level multiplexing events.
      stHandshakeTracer    :: Tracer IO (Mx.WithBearer (ConnectionId LocalAddress)
                                            (TraceSendRecv (Handshake NodeToClientVersion CBOR.Term))),
      -- ^ handshake protocol tracer; it is important for analysing version
      -- negotation mismatches.
      stSubscriptionTracer :: Tracer IO (SubscriptionTrace a)
    }

data SubscriptionTrace a =
    SubscriptionResult a
  | SubscriptionError SomeException
  | SubscriptionReconnect
  | SubscriptionTerminate
  deriving Show

-- | Subscribe using `node-to-client` mini-protocol.
--
-- 'blockVersion' ought to be instantiated with `BlockNodeToClientVersion blk`.
-- The callback receives `blockVersion` associated with each
-- 'NodeToClientVersion' and can be used to create codecs with
-- `Ouroboros.Consensus.Network.NodeToClient.clientCodecs`.
--
subscribe
  :: forall blockVersion a.
     Snocket.LocalSnocket
  -> NetworkMagic
  -> Map NodeToClientVersion blockVersion
  -- ^ Use `supportedNodeToClientVersions` from `ouroboros-consensus`.
  -> SubscriptionTracers a
  -> SubscriptionParams a
  -> (   NodeToClientVersion
      -> blockVersion
      -> NodeToClientProtocols Mx.InitiatorMode LocalAddress BSL.ByteString IO a Void)
  -> IO ()
subscribe snocket networkMagic supportedVersions
                  SubscriptionTracers {
                    stMuxTracer = muxTracer,
                    stHandshakeTracer = handshakeTracer,
                    stSubscriptionTracer = tracer
                  }
                  SubscriptionParams {
                    spAddress = addr,
                    spReconnectionDelay = reConnDelay,
                    spCompleteCb = completeCb
                  }
                  protocols =
    mask $ \unmask ->
      loop unmask $
        NtC.connectTo
          snocket
          NetworkConnectTracers {
            nctMuxTracer       = muxTracer,
            nctHandshakeTracer = handshakeTracer
          }
          (versionedProtocols networkMagic supportedVersions protocols)
          (getFilePath addr)
  where
    loop :: (forall x. IO x -> IO x) -> IO (Either SomeException a) -> IO ()
    loop unmask act = do
      r <- squashLefts <$> try (unmask act)
      case r of
        Right a -> traceWith tracer (SubscriptionResult a)
        Left  e -> traceWith tracer (SubscriptionError e)
      case completeCb r of
        Abort ->
          traceWith tracer SubscriptionTerminate
        Reconnect -> do
          traceWith tracer SubscriptionReconnect
          threadDelay (fromMaybe 5 reConnDelay)
          loop unmask act

    squashLefts :: forall x y. Either x (Either x y) -> Either x y
    squashLefts = join


versionedProtocols ::
     forall m appType bytes blockVersion a.
     NetworkMagic
  -> Map NodeToClientVersion blockVersion
  -- ^ Use `supportedNodeToClientVersions` from `ouroboros-consensus`.
  -> (   NodeToClientVersion
      -> blockVersion
      -> NodeToClientProtocols appType LocalAddress bytes m a Void)
     -- ^ callback which receives codecs, connection id and STM action which
     -- can be checked if the networking runtime system requests the protocols
     -- to stop.
     --
     -- TODO: the 'RunOrStop' might not be needed for @node-to-client@, hence
     -- it's not exposed in 'subscribe'. We should provide
     -- 'OuroborosClientApplication', which does not include it.
  -> Versions
       NodeToClientVersion
       NodeToClientVersionData
       (OuroborosApplicationWithMinimalCtx appType LocalAddress bytes m a Void)
versionedProtocols networkMagic supportedVersions callback =
    NtC.foldMapVersions applyVersion (Map.toList supportedVersions)
  where
    applyVersion
      :: (NodeToClientVersion, blockVersion)
      -> Versions
           NodeToClientVersion
           NodeToClientVersionData
           (OuroborosApplicationWithMinimalCtx appType LocalAddress bytes m a Void)
    applyVersion (version, blockVersion) =
      NtC.versionedNodeToClientProtocols
        version
        NodeToClientVersionData {
          networkMagic,
          query = False
        }
        (callback version blockVersion)
