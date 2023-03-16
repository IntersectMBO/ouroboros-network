{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}

module Cardano.Client.Subscription
  ( subscribe
  , MuxMode (..)
  , ConnectionId
  , LocalAddress
  , NodeToClientProtocols (..)
  , MuxPeer (..)
  , MuxTrace
  , RunMiniProtocol (..)
  , WithMuxBearer
  , ControlMessage (..)
  ) where

import qualified Data.ByteString.Lazy as BSL
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Void (Void)

import           Network.Mux.Trace (MuxTrace, WithMuxBearer)

import           Ouroboros.Network.ControlMessage (ControlMessage (..))
import           Ouroboros.Network.Magic (NetworkMagic)
import           Ouroboros.Network.Mux (MuxMode (..), MuxPeer (..),
                     OuroborosApplicationWithMinimalCtx, RunMiniProtocol (..))
import           Ouroboros.Network.NodeToClient (ClientSubscriptionParams (..),
                     ConnectionId, LocalAddress,
                     NetworkClientSubcriptionTracers,
                     NodeToClientProtocols (..), NodeToClientVersion,
                     NodeToClientVersionData (NodeToClientVersionData),
                     ncSubscriptionWorker, newNetworkMutableState,
                     versionedNodeToClientProtocols)
import           Ouroboros.Network.Protocol.Handshake.Version (Versions,
                     foldMapVersions)
import qualified Ouroboros.Network.Snocket as Snocket

-- | Subscribe using `node-to-client` mini-protocol.
--
-- 'blockVersion' ought to be instantiated with `BlockNodeToClientVersion blk`.
-- The callback receives `blockVersion` associated with each
-- 'NodeToClientVersion' and can be used to create codecs with
-- `Ouroboros.Consensus.Network.NodeToClient.clientCodecs`.
--
subscribe
  :: forall blockVersion x y.
     Snocket.LocalSnocket
  -> NetworkMagic
  -> Map NodeToClientVersion blockVersion
  -- ^ Use `supportedNodeToClientVersions` from `ouroboros-consensus`.
  -> NetworkClientSubcriptionTracers
  -> ClientSubscriptionParams ()
  -> (   NodeToClientVersion
      -> blockVersion
      -> NodeToClientProtocols 'InitiatorMode LocalAddress BSL.ByteString IO x y)
  -> IO Void
subscribe snocket networkMagic supportedVersions tracers subscriptionParams protocols = do
    networkState <- newNetworkMutableState
    ncSubscriptionWorker
      snocket
      tracers
      networkState
      subscriptionParams
      (versionedProtocols networkMagic supportedVersions protocols)

versionedProtocols ::
     forall m appType bytes blockVersion a b.
     NetworkMagic
  -> Map NodeToClientVersion blockVersion
  -- ^ Use `supportedNodeToClientVersions` from `ouroboros-consensus`.
  -> (   NodeToClientVersion
      -> blockVersion
      -> NodeToClientProtocols appType LocalAddress bytes m a b)
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
       (OuroborosApplicationWithMinimalCtx appType LocalAddress bytes m a b)
versionedProtocols networkMagic supportedVersions callback =
    foldMapVersions applyVersion $ Map.toList supportedVersions
  where
    applyVersion
      :: (NodeToClientVersion, blockVersion)
      -> Versions
           NodeToClientVersion
           NodeToClientVersionData
           (OuroborosApplicationWithMinimalCtx appType LocalAddress bytes m a b)
    applyVersion (version, blockVersion) =
      versionedNodeToClientProtocols
        version
        (NodeToClientVersionData networkMagic False)
        (callback version blockVersion)
