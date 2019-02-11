{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}

module Ouroboros.Consensus.Protocol.Abstract (
    -- * Abstract definition of the Ouroboros protocl
    OuroborosTag(..)
  , HasPreHeader(..)
  , HasPayload(..)
  , SecurityParam(..)
  , selectChain
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
import           Data.Function (on)
import           Data.Functor.Identity
import           Data.Kind (Constraint)
import           Data.List (sortBy)
import           Data.Maybe (listToMaybe, mapMaybe)

import           Control.Monad.Class.MonadSay

import           Ouroboros.Network.Block (HasHeader (..), Slot)
import           Ouroboros.Network.Chain (Chain (..))
import qualified Ouroboros.Network.Chain as Chain
import           Ouroboros.Network.Serialise (Serialise)

import           Ouroboros.Consensus.Util.Chain (upToSlot)
import           Ouroboros.Consensus.Util.Random

-- | The (open) universe of Ouroboros protocols
--
-- This class encodes the part that is independent from any particular
-- block representation.
class ( Show (ChainState    p)
      , Show (ValidationErr p)
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
  type family LedgerView p :: *

  -- | Validation errors
  type family ValidationErr p :: *

  -- | Blocks that the protocol can run on
  type family SupportedBlock p :: * -> Constraint

  -- | Construct the ouroboros-specific payload of a block
  --
  -- Gets the proof that we are the leader and the preheader as arguments.
  mkPayload :: (HasNodeState p m, MonadRandom m, Serialise ph)
            => NodeConfig p
            -> IsLeader p
            -> ph
            -> m (Payload p ph)

  -- | Do we prefer the candidate chain over ours?
  --
  -- Returns a (prefix of) the candidate if we do prefer the candidate.
  --
  -- NOTE: Assumes that our chain does not extend into the future.
  preferCandidate :: (Eq b, HasHeader b)
                  => NodeConfig p
                  -> Slot         -- ^ Present slot
                  -> Chain b      -- ^ Our chain
                  -> Chain b      -- ^ Candidate
                  -> Maybe (Chain b)
  preferCandidate _ now ours cand
    | Chain.length cand' > Chain.length ours = Just cand'
    | otherwise                              = Nothing
    where
      -- NOTE: In deviation from the paper, we allow for blocks one slot ahead
      -- in the future. We do this to avoid rejecting chains that are
      -- perfectly fine but appear to contain "blocks in the future" due to
      -- clock skew.
      --
      -- TODO: Review the above.
      -- TODO: The Genesis paper says to /reject/ chains containing blocks
      -- in the future rather than prune.
      cand' = upToSlot (now + 1) cand

  -- | Compare two candidates, both of which we prefer to our own chain
  compareCandidates :: (Eq b, HasHeader b)
                    => NodeConfig p
                    -> Slot         -- ^ Present slot
                    -> Chain b -> Chain b -> Ordering
  compareCandidates _ _ = compare `on` Chain.length

  -- | Check if a node is the leader
  checkIsLeader :: (HasNodeState p m, MonadRandom m)
                => NodeConfig p
                -> Slot
                -> LedgerView p
                -> ChainState p
                -> m (Maybe (IsLeader p))

  -- | Apply a block
  applyChainState :: SupportedBlock p b
                  => NodeConfig p
                  -> LedgerView p -- /Updated/ ledger state
                  -> b
                  -> ChainState p -- /Previous/ Ouroboros state
                  -> Except (ValidationErr p) (ChainState p)

  -- | We require that protocols support a @k@ security parameter
  protocolSecurityParam :: NodeConfig p -> SecurityParam

-- | Protocol security parameter
--
-- We interpret this as the number of rollbacks we support.
--
-- i.e., k == 0: we can't roll back at all
--       k == 1: we can roll back at most one block, etc
--
-- NOTE: This talks about the number of /blocks/ we can roll back, not
-- the number of /slots/.
newtype SecurityParam = SecurityParam { maxRollbacks :: Word }

-- | Extract the pre-header from a block
class (HasHeader b, Serialise (PreHeader b)) => HasPreHeader b where
  type family PreHeader b :: *
  blockPreHeader :: b -> PreHeader b

-- | Blocks that contain the ouroboros payload
--
-- We do /not/ impose a functional dependency here, so that in principle
-- blocks can be defined to have payloads for multiple protocols. This is
-- important for protocol combinators.
class HasPreHeader b => HasPayload p b where
  blockPayload :: proxy p -> b -> Payload p (PreHeader b)

{-------------------------------------------------------------------------------
  Chain selection
-------------------------------------------------------------------------------}

-- | Chain selection between our chain and list of candidates
--
-- Returns 'Nothing' if we stick with our current chain.
selectChain :: forall p b. (OuroborosTag p, Eq b, HasHeader b)
            => NodeConfig p
            -> Slot         -- ^ Present slot
            -> Chain b      -- ^ Our chain
            -> [Chain b]    -- ^ Upstream chains
            -> Maybe (Chain b)
selectChain cfg now ours candidates =
    listToMaybe $ sortBy (flip (compareCandidates cfg now)) preferred
  where
    preferred :: [Chain b]
    preferred = mapMaybe (preferCandidate cfg now ours) candidates

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
