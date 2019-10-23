{-# LANGUAGE DefaultSignatures     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators         #-}

module Ouroboros.Consensus.Protocol.HardFork.CanHardFork (
    CanHardFork(..)
  ) where

import           Ouroboros.Network.Block (HasHeader (..))

import           Ouroboros.Consensus.BlockchainTime
import           Ouroboros.Consensus.Protocol.Abstract
import           Ouroboros.Consensus.Protocol.HardFork.Config
import           Ouroboros.Consensus.Protocol.HardFork.Forked

{-------------------------------------------------------------------------------
  Class to hard fork between protocols
-------------------------------------------------------------------------------}

-- | Protocol specific functionality required to fork between protocols
class (OuroborosTag p1, OuroborosTag p2) => CanHardFork p1 p2 where
  -- | Length of the segment before the hard fork (in number of slots)
  --
  -- This value depends /only/ on the node configuration, not on the ledger
  -- state. This means that the length of the segment is a constant, not a
  -- votable parameter. This is reasonable: hard forks by their very nature
  -- involve code changes (if the ledger could evolve without any code changes
  -- it would just be a regular ledger rule, not a hard fork).
  segmentLength
    :: NodeConfig (p1 `HardForksTo` p2) -> SegmentLength

  -- | Convert the 'ChainState' from the old protocol to the new at the boundary
  --
  -- This is the chain state before the first block after the hard fork; it can
  -- be thought of as the "genesis state" of the ledger.
  chainStateAfterFork
    :: NodeConfig (p1 `HardForksTo` p2) -> ChainState p1 -> ChainState p2

  -- | Convert the 'LedgerView' from the old protocol to the new at the boundary
  --
  -- Like 'chainStateAfterFork', this is the state of the ledger view before
  -- the first block after the hard fork.
  ledgerViewAfterFork
    :: NodeConfig (p1 `HardForksTo` p2) -> LedgerView p1 -> LedgerView p2

  -- | Do we prefer this candidate?
  --
  -- The default implementation refers the protocol-specific 'preferCandidate'
  -- if our own chain and the candidate are both before the hard fork or both
  -- after; if one is before and the other after, it prefers the longer chain;
  -- if both chains are of equal length, but one is before and the other is
  -- after the hard fork, the candidate is not preferred (due to the Ouroboros
  -- specification requirement that nodes should prefer their own chain over
  -- candidates of equal length).
  hardForkPreferCandidate
    :: ( CanSelect p1 hdr1
       , CanSelect p2 hdr2
       , HasHeader hdr1
       , HasHeader hdr2
       )
    => NodeConfig (p1 `HardForksTo` p2)
    -> Forked hdr1 hdr2 -- ^ Our chain
    -> Forked hdr1 hdr2 -- ^ Candidate
    -> Bool

  -- | Which candidate do we prefer?
  --
  -- We use this because it is tricky to define a generic version for any two
  -- protocols.
  --
  -- The default implementation makes similar assumptions as the default
  -- implementation for 'hardForkPreferCandidate'.
  hardForkCompareCandidates
    :: ( CanSelect p1 hdr1
       , CanSelect p2 hdr2
       , HasHeader hdr1
       , HasHeader hdr2
       )
    => NodeConfig (p1 `HardForksTo` p2)
    -> Forked hdr1 hdr2
    -> Forked hdr1 hdr2
    -> Ordering

  --
  -- Default implementations
  --

  hardForkPreferCandidate cfg ours theirs =
    case (ours, theirs) of
      (BeforeFork ours', BeforeFork theirs') ->
        preferCandidate (nodeConfigBeforeFork cfg) ours' theirs'
      (AfterFork ours', AfterFork theirs') ->
        preferCandidate (nodeConfigAfterFork cfg) ours' theirs'
      (BeforeFork ours', AfterFork theirs') ->
        blockNo theirs' > blockNo ours'
      (AfterFork ours', BeforeFork theirs') ->
        blockNo theirs' > blockNo ours'

  hardForkCompareCandidates cfg cand1 cand2 =
    case (cand1, cand2) of
      (BeforeFork cand1', BeforeFork cand2') ->
        compareCandidates (nodeConfigBeforeFork cfg) cand1' cand2'
      (AfterFork cand1', AfterFork cand2') ->
        compareCandidates (nodeConfigAfterFork cfg) cand1' cand2'
      (BeforeFork cand1', AfterFork cand2') ->
        compare (blockNo cand1') (blockNo cand2')
      (AfterFork cand1', BeforeFork cand2') ->
        compare (blockNo cand1') (blockNo cand2')
