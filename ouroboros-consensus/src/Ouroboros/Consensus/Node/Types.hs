{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}

module Ouroboros.Consensus.Node.Types (
  CandidateDecline (..),
  CandidateFragment (..),
  CandidateFingerprint (..),
  ) where

import           GHC.Generics (Generic)
import           NoThunks.Class

import           Ouroboros.Network.AnchoredFragment (AnchoredFragment)

import           Ouroboros.Consensus.Block (Point)

-- | What ChainSync provides per peer to BlockFetch.
data CandidateFragment header = CandidateFragment {
    candidateIsLowDensity :: !Bool
  , candidateChain        :: !(AnchoredFragment header)
  }
  deriving stock    (Generic)
  deriving anyclass (NoThunks)

-- | A reason that we might choose to ignore a 'CandidateFragment'.
data CandidateDecline =
    -- | The 'candidateChain' is not better than our current selected chain.
    DeclineNotPlausible
    -- | The candidate is empty (which should only happen if
    -- 'candidateIsLowDensity').
  | DeclineNull
    -- | The candidate does not even intersect the current selected chain. This
    -- can happen if BlockFetch has re-checked before ChainSync has had a chance
    -- to update the candidate.
  | DeclineStale
  deriving (Eq, Show)

-- | A summary of 'CandidateFragment' used to avoid busy work in the BlockFetch
-- decision logic.
data CandidateFingerprint header =
    CandidateFingerprint
      (Point header)
      Bool
  deriving (Eq, Show)
