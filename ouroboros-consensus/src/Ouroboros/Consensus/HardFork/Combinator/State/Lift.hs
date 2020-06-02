-- | Lifting functions for the various types used in 'HardForkState_'
--
-- NOTE: These are internal and not exported in the toplevel @.State@ module.
module Ouroboros.Consensus.HardFork.Combinator.State.Lift (
    -- * Lifting functions on @f@ to @Current @f@
    lift
  , liftM
    -- * Lifting functions on @f@ to @Past f@
  , liftPast
  , liftPastM
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

{-------------------------------------------------------------------------------
  Lifting functions on @f@ to @Past f@
-------------------------------------------------------------------------------}

liftPast :: (f blk -> f' blk) -> Past f blk -> Past f' blk
liftPast f = runIdentity . liftPastM (Identity . f)

liftPastM :: Applicative m
          => (f blk -> m (f' blk)) -> Past f blk -> m (Past f' blk)
liftPastM f (Past start end snapshot) =
    Past start end <$>
      case snapshot of
        NoSnapshot    -> pure NoSnapshot
        Snapshot n st -> Snapshot n <$> f st
