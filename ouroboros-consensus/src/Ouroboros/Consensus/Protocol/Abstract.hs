{-# LANGUAGE ConstraintKinds            #-}
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
    -- * Abstract definition of the Ouroboros protocl
    OuroborosTag(..)
  , HasPreHeader(..)
  , HasPayload(..)
  , SecurityParam(..)
  , selectChain
  , selectUnvalidatedChain
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

import           Codec.Serialise.Encoding (Encoding)
import           Control.Monad.Except
import           Control.Monad.State
import           Crypto.Random (MonadRandom (..))
import           Data.Bifunctor (first)
import           Data.Function (on)
import           Data.Functor.Identity
import           Data.Kind (Constraint)
import           Data.List (sortBy)
import           Data.Maybe (listToMaybe)
import           Data.Typeable (Typeable)
import           Data.Word (Word64)

import           Control.Monad.Class.MonadSay

import           Ouroboros.Network.AnchoredFragment (AnchoredFragment (..))
import qualified Ouroboros.Network.AnchoredFragment as AF
import           Ouroboros.Network.Block (HasHeader (..), SlotNo (..))
import           Ouroboros.Network.Chain (Chain)

-- TODO Better place to put the Empty class?
import           Ouroboros.Consensus.Crypto.DSIGN.Class (Empty)
import qualified Ouroboros.Consensus.Util.AnchoredFragment as AF
import           Ouroboros.Consensus.Util.Random

import           GHC.Stack

-- | The (open) universe of Ouroboros protocols
--
-- This class encodes the part that is independent from any particular
-- block representation.
class ( Show (ChainState    p)
      , Show (ValidationErr p)
      , Typeable p -- so that p can appear in exceptions
      ) => OuroborosTag p where

  -- | Static node configuration
  --
  -- Every method in this class takes the node configuration as a parameter,
  -- so having this as a data family rather than a type family resolves most
  -- ambiguity.
  data family NodeConfig p :: *

  -- | State of the node required to run the protocol
  type family NodeState p :: *

  -- | The protocol specific part that should included in the block
  --
  -- The type argument is the type of the block header /without/ the
  -- Ouroboros specific part
  --
  -- This is a data family because of limitations in GHC's support for
  -- quantified constraints; see
  -- <https://ghc.haskell.org/trac/ghc/ticket/14860>
  -- <https://ghc.haskell.org/trac/ghc/ticket/15347>
  data family Payload p :: * -> *

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

  -- | Blocks that the protocol can run on
  type family SupportedBlock p :: * -> Constraint

  -- | Constraints on the preheader which can be incorporated into a payload.
  type family SupportedPreHeader p :: * -> Constraint
  type SupportedPreHeader p = Empty

  -- | Construct the ouroboros-specific payload of a block
  --
  -- Gets the proof that we are the leader and the preheader as arguments.
  mkPayload :: (SupportedPreHeader p ph, HasNodeState p m, MonadRandom m)
            => (ph -> Encoding)
            -> NodeConfig p
            -> IsLeader p
            -> ph
            -> m (Payload p ph)

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
  preferCandidate :: HasHeader b
                  => NodeConfig p
                  -> AnchoredFragment b      -- ^ Our chain
                  -> AnchoredFragment b      -- ^ Candidate
                  -> Bool
  preferCandidate _ ours cand =
    AF.compareHeadBlockNo cand ours == GT
    -- TODO handle genesis

  -- | Compare two candidates, both of which we prefer to our own chain
  --
  -- Note: we make no assumptions about the anchor points of the fragments.
  --
  -- Note: we do not assume that the candidate fragments fork less than @k@
  -- blocks back.
  compareCandidates :: HasHeader b
                    => NodeConfig p
                    -> AnchoredFragment b -> AnchoredFragment b -> Ordering
  compareCandidates _ = AF.compareHeadBlockNo

  -- | Check if a node is the leader
  checkIsLeader :: (HasNodeState p m, MonadRandom m)
                => NodeConfig p
                -> SlotNo
                -> LedgerView p
                -> ChainState p
                -> m (Maybe (IsLeader p))

  -- | Apply a block
  --
  -- TODO this will only be used with headers
  applyChainState :: (SupportedBlock p b, SupportedPreHeader p (PreHeader b), HasCallStack)
                  => (PreHeader b -> Encoding) -- Serialiser for the preheader
                  -> NodeConfig p
                  -> LedgerView p -- /Updated/ ledger state
                  -> b
                  -> ChainState p -- /Previous/ Ouroboros state
                  -> Except (ValidationErr p) (ChainState p)

  -- | We require that protocols support a @k@ security parameter
  protocolSecurityParam :: NodeConfig p -> SecurityParam

  -- | We require that it's possible to reverse the chain state up to '2k'
  -- slots.
  --
  -- This function should attempt to rewind the chain state to the state at some
  -- given slot.
  --
  -- Implementers should take care that this function accurately reflects the
  -- slot number, rather than the number of blocks, since naively the
  -- 'ChainState' will be updated only on processing an actual block.
  rewindChainState :: NodeConfig p
                   -> ChainState p
                   -> SlotNo -- ^ Slot to rewind to.
                   -> Maybe (ChainState p)

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
  deriving (Eq)

-- | Extract the pre-header from a block
class (HasHeader b) => HasPreHeader b where
  type family PreHeader b :: *
  blockPreHeader :: b -> PreHeader b

-- | Blocks that contain the ouroboros payload
--
-- We do /not/ impose a functional dependency here, so that in principle
-- blocks can be defined to have payloads for multiple protocols. This is
-- important for protocol combinators.
class HasPreHeader b => HasPayload p b where
  blockPayload :: NodeConfig p -> b -> Payload p (PreHeader b)

{-------------------------------------------------------------------------------
  Chain selection
-------------------------------------------------------------------------------}

-- | Chain selection between our chain and list of candidates
--
-- This is only a /model/ of chain selection: in reality of course we will not
-- work with entire chains in memory. This function is intended as an
-- explanation of how chain selection should work conceptually.
--
-- The @l@ parameter here models the ledger state for each chain, and serves as
-- evidence that the chains we are selecting between have been validated. (It
-- would /not/ be  correct to run chain selection on unvalidated chains and then
-- somehow fail if the selected chain turns out to be invalid.)
--
-- Returns 'Nothing' if we stick with our current chain.
selectChain :: forall p b l. (OuroborosTag p, HasHeader b)
            => NodeConfig p
            -> Chain b           -- ^ Our chain
            -> [(Chain b, l)]    -- ^ Upstream chains
            -> Maybe (Chain b, l)
selectChain cfg ours' candidates' =
    fmap (first toChain) $ listToMaybe $
    sortBy (flip (compareCandidates cfg `on` fst)) preferred
  where
    ours       = AF.fromChain ours'
    candidates = map (first AF.fromChain) candidates'
    preferred :: [(AnchoredFragment b, l)]
    preferred = filter (preferCandidate cfg ours . fst) candidates
    toChain :: AnchoredFragment b -> Chain b
    toChain af
      | Just c <- AF.toChain af
      = c
      | otherwise
      = error "impossible: fragment was anchored at genesis"

-- | Chain selection on unvalidated chains
selectUnvalidatedChain :: forall p b. (OuroborosTag p, HasHeader b)
                       => NodeConfig p
                       -> Chain b
                       -> [Chain b]
                       -> Maybe (Chain b)
selectUnvalidatedChain cfg ours = fmap fst . selectChain cfg ours . map (, ())

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
  deriving (Functor, Applicative, Monad, MonadTrans, MonadSay)

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
