{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Cardano.Node.Types where

import Data.Aeson (FromJSON)
import GHC.Generics (Generic)
import NoThunks.Class (NoThunks)

-- | Wether the node is caught up or fell too far behind the chain
data LedgerStateJudgement = YoungEnough | TooOld
  deriving (Eq, Show, Generic)

instance NoThunks LedgerStateJudgement

-- | Minimum number of hot big ledger peers in Genesis mode
--   for trusted state to be signalled to Consensus. This number
--   should be smaller than the `targetNumberOfActiveBigLedgerPeers`
--   but greater than 1. In Genesis, we may demote a big ledger peer
--   for underperformance, but not promote a replacement immediately
--   to guard against adversaries which may want to slow down our
--   progress.
--
newtype MinBigLedgerPeersForTrustedState =
  MinBigLedgerPeersForTrustedState { getMinBigLedgerPeersForTrustedState :: Int }
  deriving stock (Eq, Show)
  deriving newtype (FromJSON)

