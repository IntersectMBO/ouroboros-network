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
  , HasOuroborosPayload(..)
    -- * State monad for Ouroboros state
  , HasOuroborosNodeState(..)
  , OuroborosNodeStateT -- opaque
  , ouroborosNodeStateT
  , runOuroborosNodeStateT
  , evalOuroborosNodeStateT
  , runOuroborosState
  , evalOuroborosState
  , liftState
  ) where

import           Control.Monad.Except
import           Control.Monad.State
import           Crypto.Random (MonadRandom (..))
import           Data.Coerce
import           Data.Functor.Identity
import           Data.Kind (Constraint)
import           Data.List (sortBy)
import           Data.Ord (comparing)

import           Ouroboros.Network.Block (HasHeader(..), Slot)
import           Ouroboros.Network.Chain (Chain (..))
import           Ouroboros.Network.Serialise (Serialise)
import qualified Ouroboros.Network.Chain as Chain

import           Ouroboros.Consensus.Util
import           Ouroboros.Consensus.Util.Random

-- | The (open) universe of Ouroboros protocols
--
-- This class encodes the part that is independent from any particular
-- block representation.
class ( Show (OuroborosChainState      p)
      , Show (OuroborosValidationError p)
      , forall ph. Show      ph => Show      (OuroborosPayload p ph)
      , forall ph. Condense  ph => Condense  (OuroborosPayload p ph)
      , forall ph. Eq        ph => Eq        (OuroborosPayload p ph)
      , forall ph. Serialise ph => Serialise (OuroborosPayload p ph)
      ) => OuroborosTag p where

  -- | The protocol specific part that should included in the block
  --
  -- The type argument is the type of the block header /without/ the
  -- Ouroboros specific part
  data family OuroborosPayload p :: * -> *

  -- | Blockchain dependent protocol-specific state
  data family OuroborosChainState p :: *

  -- | Static node configuration
  data family OuroborosNodeConfig p :: *

  -- | State of the node required to run the protocol
  data family OuroborosNodeState p :: *

  -- | Evidence that a node is the leader
  data family ProofIsLeader p :: *

  -- | Projection of the ledger state the Ouroboros protocol needs access to
  data family OuroborosLedgerView p :: *

  -- | Validation errors
  type family OuroborosValidationError p :: *

  -- | Blocks that the protocol can run on
  type family SupportedBlock p :: * -> Constraint

  -- | Construct the ouroboros-specific payload of a block
  --
  -- Gets the proof that we are the leader and the preheader as arguments.
  mkOuroborosPayload :: (HasOuroborosNodeState p m, MonadRandom m, Serialise ph)
                     => OuroborosNodeConfig p
                     -> ProofIsLeader p
                     -> ph
                     -> m (OuroborosPayload p ph)

  -- | Chain selection
  selectChain :: SupportedBlock p b
              => OuroborosNodeConfig p
              -> Chain b    -- ^ Our chain
              -> [Chain b]  -- ^ Upstream chains
              -> Chain b
  selectChain _ ourChain candidates =
      -- will prioritize our own since sortBy is stable
      head $ sortBy (flip (comparing Chain.length)) (ourChain : candidates)

  -- | Check if a node is the leader
  checkIsLeader :: (HasOuroborosNodeState p m, MonadRandom m)
                => OuroborosNodeConfig p
                -> Slot
                -> OuroborosLedgerView p
                -> OuroborosChainState p
                -> m (Maybe (ProofIsLeader p))

  -- | Apply a block
  applyOuroborosChainState
    :: SupportedBlock p b
    => OuroborosNodeConfig p
    -> b
    -> OuroborosLedgerView p -- /Updated/ ledger state
    -> OuroborosChainState p -- /Previous/ Ouroboros state
    -> Except (OuroborosValidationError p) (OuroborosChainState p)

-- | Extract the pre-header from a block
class (HasHeader b, Serialise (BlockPreHeader b)) => HasPreHeader b where
  type family BlockPreHeader b :: *
  blockPreHeader :: b -> BlockPreHeader b

-- | Blocks that contain the ouroboros payload
--
-- We do /not/ impose a functional dependency here, so that in principle
-- blocks can be defined to have payloads for multiple protocols. This is
-- important for protocol combinators.
class HasPreHeader b => HasOuroborosPayload p b where
  blockOuroborosPayload :: proxy p -> b -> OuroborosPayload p (BlockPreHeader b)

{-------------------------------------------------------------------------------
  State monad
-------------------------------------------------------------------------------}

-- | State monad for the Ouroboros specific state
--
-- We introduce this so that we can have both MonadState and OuroborosState
-- in a monad stack.
class Monad m => HasOuroborosNodeState p m | m -> p where
  getOuroborosNodeState :: m (OuroborosNodeState p)
  putOuroborosNodeState :: OuroborosNodeState p -> m ()

newtype OuroborosNodeStateT p m a = OuroborosNodeStateT {
      unOuroborosNodeStateT :: StateT (OuroborosNodeState p) m a
    }
  deriving (Functor, Applicative, Monad, MonadTrans)

ouroborosNodeStateT :: (OuroborosNodeState p -> m (a, OuroborosNodeState p))
                    -> OuroborosNodeStateT p m a
ouroborosNodeStateT = OuroborosNodeStateT . StateT

runOuroborosNodeStateT :: OuroborosNodeStateT p m a
                       -> OuroborosNodeState p -> m (a, OuroborosNodeState p)
runOuroborosNodeStateT = runStateT . unOuroborosNodeStateT

evalOuroborosNodeStateT :: Monad m
                        => OuroborosNodeStateT p m a
                        -> OuroborosNodeState p -> m a
evalOuroborosNodeStateT act = fmap fst . runOuroborosNodeStateT act

runOuroborosState :: (forall m. Monad m => OuroborosNodeStateT p m a)
                  -> OuroborosNodeState p -> (a, OuroborosNodeState p)
runOuroborosState act = runIdentity . runOuroborosNodeStateT act

evalOuroborosState :: (forall m. Monad m => OuroborosNodeStateT p m a)
                   -> OuroborosNodeState p -> a
evalOuroborosState act = fst . runOuroborosState act

instance Monad m => HasOuroborosNodeState p (OuroborosNodeStateT p m) where
  getOuroborosNodeState = OuroborosNodeStateT $ get
  putOuroborosNodeState = OuroborosNodeStateT . put

instance HasOuroborosNodeState p m
      => HasOuroborosNodeState p (MonadPseudoRandomT gen m) where
  getOuroborosNodeState = lift $ getOuroborosNodeState
  putOuroborosNodeState = lift . putOuroborosNodeState

instance MonadRandom m => MonadRandom (OuroborosNodeStateT p m) where
  getRandomBytes = lift . getRandomBytes

liftState :: forall p p' m a.
             ( HasOuroborosNodeState p m
             , Coercible (OuroborosNodeState p ) (OuroborosNodeState p')
             , Coercible (OuroborosNodeState p') (OuroborosNodeState p )
             )
          => OuroborosNodeStateT p' m a
          -> m a
liftState k = do
    st <- getOuroborosNodeState
    (a, st') <- runOuroborosNodeStateT k (coerce_p_p' st)
    putOuroborosNodeState (coerce_p'_p st')
    return a
  where
    coerce_p_p' :: OuroborosNodeState p -> OuroborosNodeState p'
    coerce_p_p' = coerce

    coerce_p'_p :: OuroborosNodeState p' -> OuroborosNodeState p
    coerce_p'_p = coerce
