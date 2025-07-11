{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}


{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Network.OrphanInstances () where

import Data.Aeson
import Data.Aeson qualified as Aeson
import Data.Map qualified as Map

import Cardano.Network.NodeToClient (NodeToClientVersion (..),
           NodeToClientVersionData (..))
import Cardano.Network.NodeToNode (NodeToNodeVersion (..),
           NodeToNodeVersionData (..))
import Cardano.Network.PeerSelection.Bootstrap
import Cardano.Network.PeerSelection.PeerTrustable (PeerTrustable (..))
import Cardano.Network.PeerSelection.PublicRootPeers (CardanoPublicRootPeers,
           getBootstrapPeers, getPublicConfigPeers)
import Cardano.Network.Types

import Ouroboros.Network.Magic (NetworkMagic (..))
import Ouroboros.Network.PeerSelection.PublicRootPeers

instance ToJSON LedgerStateJudgement where
  toJSON YoungEnough = String "YoungEnough"
  toJSON TooOld      = String "TooOld"

instance FromJSON LedgerStateJudgement where
  parseJSON (String "YoungEnough") = pure YoungEnough
  parseJSON (String "TooOld")      = pure TooOld
  parseJSON _                      = fail "Invalid JSON for LedgerStateJudgement"

instance ToJSON UseBootstrapPeers where
  toJSON DontUseBootstrapPeers   = Null
  toJSON (UseBootstrapPeers dps) = toJSON dps

instance FromJSON UseBootstrapPeers where
  parseJSON Null = pure DontUseBootstrapPeers
  parseJSON v    = UseBootstrapPeers <$> parseJSON v

instance ToJSON peerAddr => ToJSON (CardanoPublicRootPeers peerAddr) where
  toJSON prp =
    object [ "kind"              .= String "PublicRootPeers"
           , "bootstrapPeers"    .= getBootstrapPeers prp
           , "ledgerPeers"       .= getLedgerPeers prp
           , "bigLedgerPeers"    .= getBigLedgerPeers prp
           , "publicConfigPeers" .= Map.keysSet (getPublicConfigPeers prp)
           ]

instance FromJSON PeerTrustable where
  parseJSON = Aeson.withBool "PeerTrustable" $ \b ->
    pure $ if b then IsTrustable
                else IsNotTrustable

instance ToJSON PeerTrustable where
  toJSON IsTrustable    = Bool True
  toJSON IsNotTrustable = Bool False

instance ToJSONKey PeerTrustable where

instance FromJSON NodeToNodeVersion where
  parseJSON = \case
    Number 14 -> pure NodeToNodeV_14
    Number 15 -> pure NodeToNodeV_15
    Number x  -> fail $ "FromJSON.NodeToNodeVersion: unsupported node-to-node protocol version " ++ show x
    x         -> fail $ "FromJSON.NodeToNodeVersion: error parsing NodeToNodeVersion: " ++ show x

instance ToJSON NodeToNodeVersion where
  toJSON NodeToNodeV_14 = Number 14
  toJSON NodeToNodeV_15 = Number 15

instance FromJSON NodeToClientVersion where
  parseJSON = \case
    Number 16 -> pure NodeToClientV_16
    Number 17 -> pure NodeToClientV_17
    Number 18 -> pure NodeToClientV_18
    Number 19 -> pure NodeToClientV_19
    Number 20 -> pure NodeToClientV_20
    Number 21 -> pure NodeToClientV_21
    Number x  -> fail $ "FromJSON.NodeToClientVersion: unsupported node-to-client protocol version " ++ show x
    x         -> fail $ "FromJSON.NodeToClientVersion: error parsing NodeToClientVersion: " ++ show x

instance ToJSON NodeToClientVersion where
  toJSON = \case
    NodeToClientV_16 -> Number 16
    NodeToClientV_17 -> Number 17
    NodeToClientV_18 -> Number 18
    NodeToClientV_19 -> Number 19
    NodeToClientV_20 -> Number 20
    NodeToClientV_21 -> Number 21

instance ToJSON NodeToNodeVersionData where
  toJSON (NodeToNodeVersionData (NetworkMagic m) dm ps q) = object
    [ "networkMagic"  .= toJSON m
    , "diffusionMode" .= show dm
    , "peerSharing"   .= show ps
    , "query"         .= toJSON q
    ]

instance ToJSON NodeToClientVersionData where
  toJSON (NodeToClientVersionData (NetworkMagic m) q) = object
    [ "networkMagic" .= toJSON m
    , "query"        .= toJSON q
    ]

