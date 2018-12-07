{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE QuantifiedConstraints      #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}

module Ouroboros.Consensus.Protocol.Abstract (
    -- * Abstract definition of the Ouroboros protocl
    OuroborosTag(..)
  , HasPreHeader(..)
  , HasPayload(..)
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
import           Data.List (sortBy)
import           Data.Ord (comparing)

import           Ouroboros.Network.Block (HasHeader(..), Slot)
import           Ouroboros.Network.Chain (Chain (..))
import           Ouroboros.Network.Serialise (Serialise)
import qualified Ouroboros.Network.Chain as Chain

import           Ouroboros.Consensus.Util
import           Ouroboros.Consensus.Util.Chain (upToSlot)
import           Ouroboros.Consensus.Util.Random

-- | The (open) universe of Ouroboros protocols
--
-- This class encodes the part that is independent from any particular
-- block representation.
class ( Show (ChainState    p)
      , Show (ValidationErr p)
      , forall ph. Show      ph => Show      (Payload p ph)
      , forall ph. Eq        ph => Eq        (Payload p ph)
      , forall ph. Ord       ph => Ord       (Payload p ph)
      , forall ph. Condense  ph => Condense  (Payload p ph)
      , forall ph. Serialise ph => Serialise (Payload p ph)
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

  -- | Chain selection
  selectChain :: (Eq b, HasHeader b, SupportedBlock p b)
              => NodeConfig p
              -> Slot       -- ^ Present slot
              -> Chain b    -- ^ Our chain
              -> [Chain b]  -- ^ Upstream chains
              -> Chain b
  selectChain _ slot ourChain candidates =
      -- will prioritize our own since sortBy is stable
      head $ sortBy (flip (comparing Chain.length))
           $ map (upToSlot slot)
           $ ourChain : candidates

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

instance HasNodeState_ s m => HasNodeState_ s (MonadPseudoRandomT gen m) where
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
