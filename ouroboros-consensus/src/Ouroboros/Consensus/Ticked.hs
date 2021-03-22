{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}

module Ouroboros.Consensus.Ticked (
    Ticked (..)
  ) where

import           Data.Kind (Type)
import           Data.SOP.BasicFunctors
import           NoThunks.Class (NoThunks)

{-------------------------------------------------------------------------------
  Ticked state
-------------------------------------------------------------------------------}

-- | " Ticked " piece of state ('LedgerState', 'LedgerView', 'ChainIndepState')
--
-- Ticking refers to the passage of time (the ticking of the clock). When a
-- piece of state is marked as ticked, it means that time-related
-- changes have been applied to the state (or forecast).
--
-- Some examples of time related changes:
--
-- * Scheduled delegations might have been applied in Byron
-- * New leader schedule computed for Shelley
-- * Transition from Byron to Shelley activated in the hard fork combinator.
-- * Nonces switched out at the start of a new epoch.
data family Ticked st :: Type

-- Standard instance for use with trivial state

data instance Ticked () = TickedTrivial
  deriving (Show)

newtype instance Ticked (K a x) = TickedK { getTickedK :: Ticked a }

{-------------------------------------------------------------------------------
  Forwarding type class instances
-------------------------------------------------------------------------------}

deriving instance
     Show (Ticked a)
  => Show (Ticked (K a x))

deriving newtype instance {-# OVERLAPPING #-}
     Show (Ticked (f a))
  => Show ((Ticked :.: f) a)

deriving newtype instance
     NoThunks (Ticked (f a))
  => NoThunks ((Ticked :.: f) a)
