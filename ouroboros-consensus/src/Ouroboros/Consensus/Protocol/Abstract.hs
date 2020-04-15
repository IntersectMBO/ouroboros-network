{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DefaultSignatures          #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}

module Ouroboros.Consensus.Protocol.Abstract (
    -- * Abstract definition of the Ouroboros protocol
    ConsensusProtocol(..)
  , ConsensusConfig
  , SecurityParam(..)
  ) where

import           Codec.Serialise (Serialise)
import           Control.Monad.Except
import           Crypto.Random (MonadRandom (..))
import           Data.Typeable (Typeable)
import           Data.Word (Word64)
import           GHC.Generics (Generic)
import           GHC.Stack

import           Cardano.Prelude (NoUnexpectedThunks)

import           Ouroboros.Network.Block (BlockNo, HeaderHash, Point,
                     SlotNo (..))

-- | Static configuration required to run the consensus protocol
--
-- Every method in the 'ConsensusProtocol' class takes the consensus
-- configuration as a parameter, so having this as a data family rather than a
-- type family resolves most ambiguity.
--
-- Defined out of the class so that protocols can define this type without
-- having to define the entire protocol at the same time (or indeed in the same
-- module).
data family ConsensusConfig p :: *

-- | The (open) universe of Ouroboros protocols
--
-- This class encodes the part that is independent from any particular
-- block representation.
class ( Show (ConsensusState p)
      , Show (ValidationErr  p)
      , Show (LedgerView     p)
      , Eq   (ConsensusState p)
      , Eq   (ValidationErr  p)
      , Eq   (LedgerView     p)
      , NoUnexpectedThunks (ConsensusConfig p)
      , NoUnexpectedThunks (ConsensusState  p)
      , NoUnexpectedThunks (ValidationErr   p)
      , Typeable p -- so that p can appear in exceptions
      ) => ConsensusProtocol p where

  -- | Protocol-specific state
  --
  -- NOTE: This chain is blockchain dependent, i.e., updated when new blocks
  -- come in (more precisely, new /headers/), and subject to rollback.
  type family ConsensusState p :: *

  -- | Evidence that a node is the leader
  type family IsLeader p :: *

  -- | Projection of the ledger state the Ouroboros protocol needs access to
  --
  -- The 'LedgerView' is a summary of the state of the ledger that the consensus
  -- algorithm requires to do its job. Under certain circumstances the consensus
  -- algorithm may require the 'LedgerView' for slots in the past (before the
  -- current tip of the chain) or in the (near) future (beyond the tip of the
  -- current chain, without having seen those future blocks yet).
  --
  -- This puts limitations on what the 'LedgerView' can be. For example, it
  -- cannot be the "current stake distribution", since it is of course
  -- impossible to compute the current stake distibution for a slot in the
  -- future. This means that for a consensus algorithm that requires the
  -- stake distribution such as Praos, the 'LedgerView' for a particular slot
  -- must be the "stake distribution for the purpose of leader selection".
  -- This "relevant" stake distribution /can/ be computed for slots in the
  -- (near) future because it is based on historical stake, not current.
  --
  -- A somewhat unfortunate consequence of this is that some decisions that
  -- ought to live in the consensus layer (such as the decision precisely which
  -- historical stake to sample to determine the relevant stake distribution)
  -- instead live in the ledger layer. It is difficult to disentangle this,
  -- because the ledger may indeed /depend/ on those sampling decisions (for
  -- example, reward calculations /must/ be based on that same stake
  -- distribution).
  --
  -- There are also some /advantages/ to moving these sorts of decisions to the
  -- ledger layer. It means that the consensus algorithm can continue to
  -- function without modifications if we decide that the stake distribution for
  -- leader selection should be based on something else instead (for example,
  -- for some bespoke version of the blockchain we may wish to use a committee
  -- instead of a decentralized blockchain). Having sampling decisions in the
  -- ledger layer rather than the consensus layer means that these decisions can
  -- be made without modifying the consensus algorithm.
  --
  -- Note that for the specific case of Praos, whilst the ledger layer provides
  -- the relevant stake distribution, the precise leader election must still live
  -- in the consensus layer since that depends on the computation (and sampling)
  -- of entropy, which is done consensus side, not ledger side (the reward
  -- calculation does not depend on this).
  type family LedgerView p :: *

  -- | Validation errors
  type family ValidationErr p :: *

  -- | View on a header required to validate it
  type family ValidateView p :: *

  -- | View on a header required for chain selection
  type family SelectView p :: *

  -- | Do we prefer the candidate chain over ours?
  --
  -- Should return 'True' when we prefer the candidate over our chain.
  --
  -- We pass only the tips of the chains; for all consensus protocols we are
  -- interested in, this provides sufficient context. (Ouroboros Genesis is
  -- the only exception, but we will handle the genesis rule elsewhere.)
  --
  -- PRECONDITIONS:
  --
  -- * The candidate chain does not extend into the future.
  -- * The candidate must intersect with our chain within @k@ blocks from
  --   our tip.
  --
  -- NOTE: An assumption that is quite deeply ingrained in the design of the
  -- consensus layer is that if a chain can be extended, it always should (e.g.,
  -- see the chain database spec in @ChainDB.md@). This means that any chain
  -- is always preferred over the empty chain, and 'preferCandidate' does not
  -- need (indeed, cannot) be called if our current chain is empty.
  preferCandidate :: ConsensusConfig p
                  -> SelectView      p      -- ^ Tip of our chain
                  -> SelectView      p      -- ^ Tip of the candidate
                  -> Bool

  -- | Compare two candidates, both of which we prefer to our own chain
  --
  -- PRECONDITION: both candidates must be preferred to our own chain
  compareCandidates :: ConsensusConfig p
                    -> SelectView      p
                    -> SelectView      p
                    -> Ordering

  -- | Check if a node is the leader
  checkIsLeader :: MonadRandom m
                => ConsensusConfig p
                -> SlotNo
                -> LedgerView      p
                -> ConsensusState  p
                -> m (Maybe (IsLeader p))

  -- | Check if a node is configured such that it can be a leader.
  checkIfCanBeLeader :: ConsensusConfig p -> Bool

  -- | Apply a header
  updateConsensusState :: HasCallStack
                       => ConsensusConfig p
                       -> LedgerView      p -- /Updated/ ledger state
                       -> ValidateView    p
                       -> ConsensusState  p -- /Previous/ Ouroboros state
                       -> Except (ValidationErr p) (ConsensusState p)

  -- | We require that protocols support a @k@ security parameter
  protocolSecurityParam :: ConsensusConfig p -> SecurityParam

  -- | We require that it's possible to reverse the chain state up to @k@
  -- blocks.
  --
  -- This function should attempt to rewind the chain state to the state at some
  -- given point.
  --
  -- PRECONDITION: the point to rewind to must correspond to a header (or
  -- 'GenesisPoint') that was previously applied to the chain state using
  -- 'updateConsensusState'.
  --
  -- Rewinding the chain state is intended to be used when switching to a
  -- fork, longer or equally long to the chain to which the current chain
  -- state corresponds. So each rewinding should be followed by rolling
  -- forward (using 'updateConsensusState') at least as many blocks that we have
  -- rewound.
  --
  -- Note that repeatedly rewinding a chain state does not make it possible to
  -- rewind it all the way to genesis (this would mean that the whole
  -- historical chain state is accumulated or derivable from the current chain
  -- state). For example, rewinding a chain state by @i@ blocks and then
  -- rewinding that chain state again by @j@ where @i + j > k@ is not possible
  -- and will yield 'Nothing'.
  --
  -- TODO: The Serialise instance is only required for a hack in PBFT.
  -- Reconsider later.
  rewindConsensusState :: Serialise (HeaderHash hdr)
                       => ConsensusConfig p
                       -> ConsensusState  p
                       -> Point hdr    -- ^ Point to rewind to
                       -> Maybe (ConsensusState p)

  --
  -- Default chain selection
  --
  -- The default preference uses the default comparison. The default comparison
  -- simply uses the block number.
  --

  type SelectView p = BlockNo

  preferCandidate cfg ours cand =
    compareCandidates cfg ours cand == LT

  default compareCandidates :: Ord (SelectView p)
                            => ConsensusConfig p
                            -> SelectView      p
                            -> SelectView      p
                            -> Ordering
  compareCandidates _ = compare

-- | Protocol security parameter
--
-- We interpret this as the number of rollbacks we support.
--
-- i.e., k == 0: we can't roll back at all
--       k == 1: we can roll back at most one block, etc
--
-- NOTE: This talks about the number of /blocks/ we can roll back, not
-- the number of /slots/.
newtype SecurityParam = SecurityParam { maxRollbacks :: Word64 }
  deriving (Show, Eq, Generic, NoUnexpectedThunks)
