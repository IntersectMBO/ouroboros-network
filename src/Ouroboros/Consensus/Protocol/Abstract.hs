{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE QuantifiedConstraints      #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}

module Ouroboros.Consensus.Protocol.Abstract (
    OuroborosTag(..)
  , RunOuroboros(..)
    -- * State monad for Ouroboros state
  , MonadOuroborosState(..)
  , OuroborosStateT -- opaque
  , ouroborosStateT
  , runOuroborosStateT
  , evalOuroborosStateT
  , runOuroborosState
  , evalOuroborosState
  ) where

import           Control.Monad.State
import           Crypto.Random (MonadRandom (..))
import           Data.Functor.Identity

import           Ouroboros.Consensus.Util.Random
import           Ouroboros.Network.Block (Slot)
import           Ouroboros.Network.Serialise (Serialise)

-- | The (open) universe of Ouroboros protocols
--
-- This class encodes the part that is independent from any particular
-- block representation.
class ( Show (OuroborosLedgerState p)
      , forall ph. Show      ph => Show      (OuroborosPayload p ph)
      , forall ph. Eq        ph => Eq        (OuroborosPayload p ph)
      , forall ph. Serialise ph => Serialise (OuroborosPayload p ph)
      ) => OuroborosTag (p :: *) where
  -- | The protocol specific part that should included in the block
  --
  -- The type argument is the type of the block header /without/ the
  -- Ouroboros specific part
  data family OuroborosPayload p :: * -> *

  -- | Protocol specific state needed when executing the protocol
  data family OuroborosState p :: *

  -- | Evidence that a node is the leader
  data family ProofIsLeader p :: *

  -- | Protocol-specific part of the ledger state
  data family OuroborosLedgerState p :: *

  -- | Construct the ouroboros-specific payload of a block
  --
  -- Gets the proof that we are the leader and the preheader as arguments.
  mkOuroborosPayload :: (MonadOuroborosState p m, MonadRandom m, Serialise ph)
                     => ProofIsLeader p -> ph -> m (OuroborosPayload p ph)

  applyOuroborosLedgerState :: OuroborosPayload p ph
                            -> OuroborosLedgerState p
                            -> OuroborosLedgerState p

  -- TODO: We need the dual of 'applyOuroborosLedgerState' for rollbacks.
  -- rollbackOuroborosLedgerState :: ...

-- | Interaction between the Ouroboros protocol and the ledger state
class OuroborosTag p => RunOuroboros p l where
  -- | Check if a node is the leader
  checkIsLeader :: (MonadOuroborosState p m, MonadRandom m)
                => Slot -> l -> m (Maybe (ProofIsLeader p))

{-------------------------------------------------------------------------------
  State monad
-------------------------------------------------------------------------------}

-- | State monad for the Ouroboros specific state
--
-- We introduce this so that we can have both MonadState and OuroborosState
-- in a monad stack.
class Monad m => MonadOuroborosState p m | m -> p where
  getOuroborosState :: m (OuroborosState p)
  putOuroborosState :: OuroborosState p -> m ()

newtype OuroborosStateT p m a = OuroborosStateT {
      unOuroborosStateT :: StateT (OuroborosState p) m a
    }
  deriving (Functor, Applicative, Monad, MonadTrans)

ouroborosStateT :: (OuroborosState p -> m (a, OuroborosState p))
                -> OuroborosStateT p m a
ouroborosStateT = OuroborosStateT . StateT

runOuroborosStateT :: OuroborosStateT p m a
                   -> OuroborosState p -> m (a, OuroborosState p)
runOuroborosStateT = runStateT . unOuroborosStateT

evalOuroborosStateT :: Monad m
                    => OuroborosStateT p m a
                    -> OuroborosState p -> m a
evalOuroborosStateT act = fmap fst . runOuroborosStateT act

runOuroborosState :: (forall m. Monad m => OuroborosStateT p m a)
                  -> OuroborosState p -> (a, OuroborosState p)
runOuroborosState act = runIdentity . runOuroborosStateT act

evalOuroborosState :: (forall m. Monad m => OuroborosStateT p m a)
                   -> OuroborosState p -> a
evalOuroborosState act = fst . runOuroborosState act

instance Monad m => MonadOuroborosState p (OuroborosStateT p m) where
  getOuroborosState = OuroborosStateT $ get
  putOuroborosState = OuroborosStateT . put

instance MonadOuroborosState p m
      => MonadOuroborosState p (MonadPseudoRandomT gen m) where
  getOuroborosState = lift $ getOuroborosState
  putOuroborosState = lift . putOuroborosState

instance MonadRandom m => MonadRandom (OuroborosStateT p m) where
  getRandomBytes = lift . getRandomBytes
