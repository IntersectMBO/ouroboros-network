{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE TypeFamilies               #-}

module Ouroboros (
    -- * Typed used across all protocols
    Slot(..)
  , NodeId(..)
    -- * Generalize over the Ouroboros protocols
  , KnownOuroborosProtocol(..)
  , OuroborosProtocol(..)
  , OuroborosPayload(..)
  , OuroborosState(..)
    -- * State monad for Ouroboros state
  , MonadOuroborosState(..)
  , OuroborosStateT -- opaque
  , ouroborosStateT
  , runOuroborosStateT
  , evalOuroborosStateT
  , runOuroborosState
  , evalOuroborosState
    -- *** Convenience re-exports
  , lift
  ) where

import Control.Monad.State
import Data.Functor.Identity
import Data.Hashable
import GHC.Generics

{-------------------------------------------------------------------------------
  Types used across all protocols
-------------------------------------------------------------------------------}

-- | The Ouroboros time slot index for a block.
newtype Slot = Slot { getSlot :: Word }
  deriving (Show, Eq, Ord, Hashable, Enum)

data NodeId = CoreId Word
            | RelayId Word
  deriving (Eq, Ord, Show, Generic)

instance Hashable NodeId -- let generic instance do the job


{-------------------------------------------------------------------------------
  Generalize over the various Ouroboros protocols
-------------------------------------------------------------------------------}

data OuroborosProtocol =
    OuroborosBFT
  | OuroborosPraos

-- TODO. This is WIP. These signatures I'm sure will need to change.
class ( Hashable (OuroborosPayload p)
      , Eq       (OuroborosPayload p)
      , Show     (OuroborosPayload p)
      ) => KnownOuroborosProtocol (p :: OuroborosProtocol) where
  -- | The protocol specific payload in block headers
  data OuroborosPayload p :: *

  -- | The protocol specific state we need when executing the protocol
  data OuroborosState p :: *

  -- | Evidence that a node is the leader
  data ProofIsLeader p :: *

  -- | Check if a node is the leader
  checkIsLeader :: MonadOuroborosState p m -- MonadState (OuroborosPayload p)
                => Slot -> m (Maybe (ProofIsLeader p))

  -- | Construct the ouroboros-specific payload of a block
  mkOuroborosPayload :: MonadOuroborosState p m
                     => ProofIsLeader p -> m (OuroborosPayload p)

{-------------------------------------------------------------------------------
  State monad
-------------------------------------------------------------------------------}

-- | State monad for the Ouroboros specific state
--
-- We introduce this so that we can have both MonadState and OuroborosState
-- in a monad stack.
class (KnownOuroborosProtocol p, Monad m) => MonadOuroborosState p m | m -> p where
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

instance (KnownOuroborosProtocol p, Monad m)
      => MonadOuroborosState p (OuroborosStateT p m) where
  getOuroborosState = OuroborosStateT $ get
  putOuroborosState = OuroborosStateT . put

{-------------------------------------------------------------------------------
  BFT
-------------------------------------------------------------------------------}

instance KnownOuroborosProtocol 'OuroborosBFT where
  -- | An identifier for someone signing a block.
  --
  -- We model this as if there were an enumerated set of valid block signers
  -- (which for Ouroboros BFT is actually the case), and omit the cryptography
  -- and model things as if the signatures were valid.
  newtype OuroborosPayload 'OuroborosBFT = BftSignature NodeId
    deriving (Show, Eq, Ord, Hashable)

  data OuroborosState 'OuroborosBFT = BftState NodeId

  data ProofIsLeader 'OuroborosBFT = BftTrivialProof

  checkIsLeader (Slot n) = do
      BftState nodeId <- getOuroborosState
      return $ case nodeId of
                 RelayId _ -> Nothing -- relays are never leaders
                 CoreId  i -> if n `mod` 7 == i
                                then Just BftTrivialProof
                                else Nothing

  mkOuroborosPayload BftTrivialProof = do
      BftState nodeId <- getOuroborosState
      return $ BftSignature nodeId
