{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies       #-}
{-# LANGUAGE TypeOperators      #-}

module Ouroboros.Consensus.Protocol.HardFork.Config (
    HardForksTo
  , NodeConfig(..)
  ) where

import           Data.Typeable (typeRep)
import           GHC.Generics (Generic)

import           Cardano.Prelude (NoUnexpectedThunks (..))

import           Ouroboros.Consensus.Protocol.Abstract

-- | A protocol that acts as @p1@ until a hard fork switches to @p2@
--
-- NOTE: 'HardForksTo' is /left/ associative. To see why, consider starting with
-- some chain @p1@ and forking to @p2@
--
-- > p1 `HardForksTo` p2
--
-- If at this point we introduce a new hark fork again, the existing chain, the
-- the one we are transitioning from, /is/ @(p1 `HardForksTo` p2)@, and so the
-- /new/ chain should be
--
-- > (p1 `HardForksTo` p2) `HardForksTo` p3
--
-- A consequence of this associativity is that in @(p `HardForksTo` q)@, @p@
-- always describes the entire history of the chain up until that point. In the
-- example above, @p1@ is the entire history leading up to @p2@; similarly,
-- @(p1 `HardForksTo` p2)@ is the entire history leading up to @p3@.
--
-- The same is /not/ true for the future: in @(p1 `HardForksTo` p2)@, @p2@ may
-- not describe the entire future (which may, for instance, also involve @p3@).
-- For the purposes of the hard fork combinator, it is more important to have a
-- complete picture of the past than of the future. Conceptually, this makes
-- sense also: the past is known, but the future may involve further, as yet
-- unplanned, hard forks.
data HardForksTo p1 p2

infixl `HardForksTo`

-- Store configuration for both protocols
data instance NodeConfig (p1 `HardForksTo` p2) =
    HardForkCfg {
        nodeConfigBeforeFork :: NodeConfig p1
      , nodeConfigAfterFork  :: NodeConfig p2
      }
  deriving (Generic)

instance (OuroborosTag p1, OuroborosTag p2)
      => NoUnexpectedThunks (NodeConfig (p1 `HardForksTo` p2)) where
  showTypeOf = show . typeRep
