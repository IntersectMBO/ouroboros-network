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
    OuroborosTag(..)
  , NodeConfig
  , SecurityParam(..)
    -- * State monad for Ouroboros state
  , HasNodeState
  , HasNodeState_(..)
  , NodeStateT
  , NodeStateT_ -- opaque
  , nodeStateT
  , runNodeStateT
  , evalNodeStateT
  , runNodeState
  , evalNodeState
  ) where

import           Control.Monad.Except
import           Control.Monad.State
import           Crypto.Random (MonadRandom (..))
import           Data.Functor.Identity
import           Data.Kind (Constraint)
import           Data.Typeable (Typeable)
import           Data.Word (Word64)
import           GHC.Generics (Generic)

import           Cardano.Prelude (NoUnexpectedThunks)

import           Ouroboros.Network.AnchoredFragment (AnchoredFragment (..))
import           Ouroboros.Network.Block (HasHeader (..), SlotNo (..))
import           Ouroboros.Network.Point (WithOrigin)

import qualified Ouroboros.Consensus.Util.AnchoredFragment as AF
import           Ouroboros.Consensus.Util.Random

import           GHC.Stack

-- | Static node configuration
--
-- Every method in the 'OuroborosTag' class takes the node configuration as a
-- parameter, so having this as a data family rather than a type family resolves
-- most ambiguity.
--
-- Defined out of the class so that protocols can define this type without
-- having to define the entire protocol at the same time (or indeed in the same
-- module).
data family NodeConfig p :: *

-- | The (open) universe of Ouroboros protocols
--
-- This class encodes the part that is independent from any particular
-- block representation.
class ( Show (ChainState    p)
      , Show (ValidationErr p)
      , NoUnexpectedThunks (NodeConfig  p)
      , NoUnexpectedThunks (ChainState  p)
      , NoUnexpectedThunks (NodeState   p)
      , Typeable p -- so that p can appear in exceptions
      ) => OuroborosTag p where

  -- | State of the node required to run the protocol
  type family NodeState p :: *

  -- | Blockchain dependent protocol-specific state
  type family ChainState p :: *

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

  -- | Headers of blocks that the protocol can run on
  type family SupportedHeader p :: * -> Constraint

  -- | Do we prefer the candidate chain over ours?
  --
  -- Returns 'True' when we prefer the candidate over our chain.
  --
  -- PRECONDITION: the candidate chain does not extend into the future.
  --
  -- Note: we make no assumptions about the anchor points of the fragments.
  --
  -- Note: we do not assume that the candidate fragments fork less than @k@
  -- blocks back.
  preferCandidate :: SupportedHeader p hdr
                  => NodeConfig p
                  -> AnchoredFragment hdr      -- ^ Our chain
                  -> AnchoredFragment hdr      -- ^ Candidate
                  -> Bool

  -- | Compare two candidates, both of which we prefer to our own chain
  --
  -- Note: we make no assumptions about the anchor points of the fragments.
  --
  -- Note: we do not assume that the candidate fragments fork less than @k@
  -- blocks back.
  compareCandidates :: SupportedHeader p hdr
                    => NodeConfig p
                    -> AnchoredFragment hdr
                    -> AnchoredFragment hdr
                    -> Ordering

  -- | Check if a node is the leader
  checkIsLeader :: (HasNodeState p m, MonadRandom m)
                => NodeConfig p
                -> SlotNo
                -> LedgerView p
                -> ChainState p
                -> m (Maybe (IsLeader p))

  -- | Apply a header
  applyChainState :: (SupportedHeader p hdr, HasCallStack)
                  => NodeConfig p
                  -> LedgerView p -- /Updated/ ledger state
                  -> hdr
                  -> ChainState p -- /Previous/ Ouroboros state
                  -> Except (ValidationErr p) (ChainState p)

  -- | We require that protocols support a @k@ security parameter
  protocolSecurityParam :: NodeConfig p -> SecurityParam

  -- | We require that it's possible to reverse the chain state up to @k@
  -- blocks.
  --
  -- This function should attempt to rewind the chain state to the state at some
  -- given slot, or Origin to rewind to the state with no blocks.
  --
  -- Implementers should take care that this function accurately reflects the
  -- slot number, rather than the number of blocks, since naively the
  -- 'ChainState' will be updated only on processing an actual block.
  --
  -- Rewinding the chain state is intended to be used when switching to a
  -- fork, longer or equally long to the chain to which the current chain
  -- state corresponds. So each rewinding should be followed by rolling
  -- forward (using 'applyChainState') at least as many blocks that we have
  -- rewound.
  --
  -- Note that repeatedly rewinding a chain state does not make it possible to
  -- rewind it all the way to genesis (this would mean that the whole
  -- historical chain state is accumulated or derivable from the current chain
  -- state). For example, rewinding a chain state by @i@ blocks and then
  -- rewinding that chain state again by @j@ where @i + j > k@ is not possible
  -- and will yield 'Nothing'.
  rewindChainState :: NodeConfig p
                   -> ChainState p
                   -> WithOrigin SlotNo
                   -- ^ Slot to rewind to
                   --
                   -- This should be the state at the /end/ of the specified
                   -- slot (i.e., after the block in that slot, if any, has
                   -- been applied).
                   -> Maybe (ChainState p)

  -- Default chain selection just depends on 'HasHeader'

  default preferCandidate :: HasHeader hdr
                          => NodeConfig p
                          -> AnchoredFragment hdr      -- ^ Our chain
                          -> AnchoredFragment hdr      -- ^ Candidate
                          -> Bool
  preferCandidate _ ours cand = AF.compareHeadBlockNo cand ours == GT

  default compareCandidates :: HasHeader hdr
                            => NodeConfig p
                            -> AnchoredFragment hdr
                            -> AnchoredFragment hdr
                            -> Ordering
  compareCandidates _ = AF.compareHeadBlockNo

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

{-------------------------------------------------------------------------------
  State monad
-------------------------------------------------------------------------------}

type HasNodeState p = HasNodeState_ (NodeState p)

-- | State monad for the Ouroboros specific state
--
-- We introduce this so that we can have both MonadState and OuroborosState
-- in a monad stack.
class Monad m => HasNodeState_ s m | m -> s where
  getNodeState :: m s
  putNodeState :: s -> m ()

instance HasNodeState_ s m => HasNodeState_ s (ChaChaT m) where
  getNodeState = lift $ getNodeState
  putNodeState = lift . putNodeState

{-------------------------------------------------------------------------------
  Monad transformer introducing 'HasNodeState_'
-------------------------------------------------------------------------------}

newtype NodeStateT_ s m a = NodeStateT { unNodeStateT :: StateT s m a }
  deriving (Functor, Applicative, Monad, MonadTrans)

type NodeStateT p = NodeStateT_ (NodeState p)

nodeStateT :: (s -> m (a, s)) -> NodeStateT_ s m a
nodeStateT = NodeStateT . StateT

runNodeStateT :: NodeStateT_ s m a -> s -> m (a, s)
runNodeStateT = runStateT . unNodeStateT

evalNodeStateT :: Monad m => NodeStateT_ s m a -> s -> m a
evalNodeStateT act = fmap fst . runNodeStateT act

runNodeState :: (forall m. Monad m => NodeStateT_ s m a) -> s -> (a, s)
runNodeState act = runIdentity . runNodeStateT act

evalNodeState :: (forall m. Monad m => NodeStateT_ s m a) -> s -> a
evalNodeState act = fst . runNodeState act

instance Monad m => HasNodeState_ s (NodeStateT_ s m) where
  getNodeState = NodeStateT $ get
  putNodeState = NodeStateT . put

instance MonadRandom m => MonadRandom (NodeStateT_ s m) where
  getRandomBytes = lift . getRandomBytes
