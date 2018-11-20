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
  , HasOuroborosPayload(..)
    -- * State monad for Ouroboros state
  , HasOuroborosNodeState
  , HasOuroborosNodeState_(..)
  , OuroborosNodeStateT
  , OuroborosNodeStateT_ -- opaque
  , ouroborosNodeStateT
  , runOuroborosNodeStateT
  , evalOuroborosNodeStateT
  , runOuroborosState
  , evalOuroborosState
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

  -- | Static node configuration
  --
  -- Every method in this class takes the node configuration as a parameter,
  -- so having this as a data family rather than a type family resolves most
  -- ambiguity.
  data family OuroborosNodeConfig p :: *

  -- | The protocol specific part that should included in the block
  --
  -- The type argument is the type of the block header /without/ the
  -- Ouroboros specific part
  --
  -- This is a data family because of limitations in GHC's support for
  -- quantified constraints; see
  -- <https://ghc.haskell.org/trac/ghc/ticket/14860>
  -- <https://ghc.haskell.org/trac/ghc/ticket/15347>
  data family OuroborosPayload p :: * -> *

  -- | Blockchain dependent protocol-specific state
  type family OuroborosChainState p :: *

  -- | State of the node required to run the protocol
  type family OuroborosNodeState p :: *

  -- | Evidence that a node is the leader
  type family ProofIsLeader p :: *

  -- | Projection of the ledger state the Ouroboros protocol needs access to
  type family OuroborosLedgerView p :: *

  -- | Validation errors
  type family OuroborosValidationError p :: *

  -- | Blocks that the protocol can run on
  type family SupportedBlock p :: * -> Constraint

  -- | Construct the ouroboros-specific payload of a block
  --
  -- Gets the proof that we are the leader and the preheader as arguments.
  mkOuroborosPayload :: ( HasOuroborosNodeState p m
                        , MonadRandom m
                        , Serialise ph)
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
  checkIsLeader :: ( HasOuroborosNodeState p m
                   , MonadRandom m
                   )
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

type HasOuroborosNodeState p = HasOuroborosNodeState_ (OuroborosNodeState p)

-- | State monad for the Ouroboros specific state
--
-- We introduce this so that we can have both MonadState and OuroborosState
-- in a monad stack.
class Monad m => HasOuroborosNodeState_ s m | m -> s where
  getOuroborosNodeState :: m s
  putOuroborosNodeState :: s -> m ()

instance HasOuroborosNodeState_ s m
      => HasOuroborosNodeState_ s (MonadPseudoRandomT gen m) where
  getOuroborosNodeState = lift $ getOuroborosNodeState
  putOuroborosNodeState = lift . putOuroborosNodeState

{-------------------------------------------------------------------------------
  Monad transformer introducing 'HasOuroborosNodeState_'
-------------------------------------------------------------------------------}

newtype OuroborosNodeStateT_ s m a = OuroborosNodeStateT {
      unOuroborosNodeStateT :: StateT s m a
    }
  deriving (Functor, Applicative, Monad, MonadTrans)

type OuroborosNodeStateT p = OuroborosNodeStateT_ (OuroborosNodeState p)

ouroborosNodeStateT :: (s -> m (a, s)) -> OuroborosNodeStateT_ s m a
ouroborosNodeStateT = OuroborosNodeStateT . StateT

runOuroborosNodeStateT :: OuroborosNodeStateT_ s m a -> s -> m (a, s)
runOuroborosNodeStateT = runStateT . unOuroborosNodeStateT

evalOuroborosNodeStateT :: Monad m => OuroborosNodeStateT_ s m a -> s -> m a
evalOuroborosNodeStateT act = fmap fst . runOuroborosNodeStateT act

runOuroborosState :: (forall m. Monad m => OuroborosNodeStateT_ s m a)
                  -> s -> (a, s)
runOuroborosState act = runIdentity . runOuroborosNodeStateT act

evalOuroborosState :: (forall m. Monad m => OuroborosNodeStateT_ s m a)
                   -> s -> a
evalOuroborosState act = fst . runOuroborosState act

instance Monad m => HasOuroborosNodeState_ s (OuroborosNodeStateT_ s m) where
  getOuroborosNodeState = OuroborosNodeStateT $ get
  putOuroborosNodeState = OuroborosNodeStateT . put

instance MonadRandom m => MonadRandom (OuroborosNodeStateT_ s m) where
  getRandomBytes = lift . getRandomBytes
