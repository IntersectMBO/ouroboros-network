{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

module Cardano.Client.Subscription
  ( -- * Subscription API
    subscribe
  , SubscriptionParams (..)
  , SubscriptionTracers (..)
  , SubscriptionTrace (..)
    -- * Re-exports
    -- ** Mux
  , MuxMode (..)
  , MuxTrace
  , WithMuxBearer
    -- ** Connections
  , ConnectionId (..)
  , NtC.LocalAddress (..)
    -- ** Protocol API
  , NtC.Protocols (..)
  , MiniProtocolCb (..)
  , RunMiniProtocol (..)
  , ControlMessage (..)
  ) where

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

import Network.Mux.Trace (MuxTrace, WithMuxBearer)

import Ouroboros.Network.ControlMessage (ControlMessage (..))
import Ouroboros.Network.Magic (NetworkMagic)
import Ouroboros.Network.Mux (MiniProtocolCb (..), MuxMode (..),
           OuroborosApplicationWithMinimalCtx, RunMiniProtocol (..))

import Ouroboros.Network.ConnectionId (ConnectionId (..))
import Ouroboros.Network.NodeToClient qualified as NtC
import Ouroboros.Network.Snocket qualified as Snocket

data SubscriptionParams a = SubscriptionParams
  { spAddress           :: !NtC.Address
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
      stMuxTracer          :: Tracer IO (WithMuxBearer (ConnectionId NtC.Address) MuxTrace),
      -- ^ low level mux-network tracer, which logs mux sdu (send and received)
      -- and other low level multiplexing events.
      stHandshakeTracer    :: Tracer IO NtC.HandshakeTr,
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
-- 'NtC.Version' and can be used to create codecs with
-- `Ouroboros.Consensus.Network.NodeToClient.clientCodecs`.
--
subscribe
  :: forall blockVersion a.
     Snocket.LocalSnocket
  -> NetworkMagic
  -> Map NtC.Version blockVersion
  -- ^ Use `supportedNodeToClientVersions` from `ouroboros-consensus`.
  -> SubscriptionTracers a
  -> SubscriptionParams a
  -> (   NtC.Version
      -> blockVersion
      -> NtC.Protocols 'InitiatorMode NtC.Address BSL.ByteString IO a Void)
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
          NtC.NetworkConnectTracers {
            NtC.nctMuxTracer       = muxTracer,
            NtC.nctHandshakeTracer = handshakeTracer
          }
          (versionedProtocols networkMagic supportedVersions protocols)
          (NtC.getFilePath addr)
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
  -> Map NtC.Version blockVersion
  -- ^ Use `supportedNodeToClientVersions` from `ouroboros-consensus`.
  -> (   NtC.Version
      -> blockVersion
      -> NtC.Protocols appType NtC.Address bytes m a Void)
     -- ^ callback which receives codecs, connection id and STM action which
     -- can be checked if the networking runtime system requests the protocols
     -- to stop.
     --
     -- TODO: the 'RunOrStop' might not be needed for @node-to-client@, hence
     -- it's not exposed in 'subscribe'. We should provide
     -- 'OuroborosClientApplication', which does not include it.
  -> NtC.Versions NtC.Version NtC.VersionData
                  (OuroborosApplicationWithMinimalCtx appType NtC.Address bytes m a Void)
versionedProtocols networkMagic supportedVersions callback =
    NtC.foldMapVersions applyVersion (Map.toList supportedVersions)
  where
    applyVersion
      :: (NtC.Version, blockVersion)
      -> NtC.Versions NtC.Version NtC.VersionData
                      (OuroborosApplicationWithMinimalCtx appType NtC.Address bytes m a Void)
    applyVersion (version, blockVersion) =
      NtC.versionedProtocols
        version
        NtC.VersionData {
          NtC.networkMagic,
          NtC.query = False
        }
        (callback version blockVersion)
