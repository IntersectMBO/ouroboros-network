-- | Lifting functions for the various types used in 'HardForkState'
--
-- NOTE: These are internal and not exported in the toplevel @.State@ module.
module Ouroboros.Consensus.HardFork.Combinator.State.Lift (
    -- * Lifting functions on @f@ to @Current @f@
    lift
  , liftM
  ) where

import           Data.Functor.Identity
import           Ouroboros.Consensus.HardFork.Combinator.State.Types

{-------------------------------------------------------------------------------
  Lifting functions on @f@ to @Current @f@
-------------------------------------------------------------------------------}

lift :: (f blk -> f' blk) -> Current f blk -> Current f' blk
lift f = runIdentity . liftM (Identity . f)

liftM :: Functor m
      => (f blk -> m (f' blk)) -> Current f blk -> m (Current f' blk)
liftM f (Current start cur) = Current start <$> f cur
