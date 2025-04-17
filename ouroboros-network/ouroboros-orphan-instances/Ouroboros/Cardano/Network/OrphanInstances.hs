{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TypeSynonymInstances #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Ouroboros.Cardano.Network.OrphanInstances () where

import Cardano.Network.PeerSelection.Bootstrap
import Cardano.Network.PeerSelection.PublicRootPeers (CardanoPublicRootPeers,
           getBootstrapPeers, getPublicConfigPeers)
import Cardano.Network.Types
import Data.Aeson
import Data.Map qualified as Map
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
