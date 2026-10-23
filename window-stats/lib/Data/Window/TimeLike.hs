{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeFamilyDependencies #-}

-- |
-- Module      : Data.Window.TimeLike
-- Description : Abstraction over absolute time and its duration.
-- Stability   : experimental
--
-- The time-based window machinery in 'Data.Window.Timed' is
-- parametrised over an absolute time type @t@ via the 'TimeLike'
-- class. Two stock instances are provided:
--
-- * 'Data.Time.UTCTime' / 'Data.Time.NominalDiffTime' — wall-clock
--   semantics, standard from the @time@ package.
--
-- * 'Control.Monad.Class.MonadTime.SI.Time' / 'Data.Time.DiffTime'
--   — monotonic semantics from @io-classes@/@si-timers@. Preferred
--   when sliding-window correctness must not be perturbed by NTP
--   adjustments or clock jumps.
--
-- Users may add their own instance for any time representation that
-- satisfies the laws below. The internal windowing code is marked
-- 'INLINEABLE' and 'SPECIALIZE'd for the two stock instances, so a
-- downstream user wanting dictionary-free code for a custom time
-- type only needs to add a 'SPECIALIZE' pragma on the relevant
-- functions in their own module.
--
-- = Laws
--
-- For any @t1, t2 :: t@ and @d :: 'Dur' t@:
--
-- @
-- ('diffT' t2 t1) \`'addT'\` t1  ==  t2
-- 'diffT' ('addT' d t) t        ==  d
-- @
--
module Data.Window.TimeLike (TimeLike (..)) where

import Control.Monad.Class.MonadTime.SI

-- | Absolute time type @t@ paired with its associated duration type
-- @'Dur' t@.
--
-- The 'Dur' type family is __injective__: given the duration GHC can
-- recover the time, so users rarely need explicit type annotations.
--
class (Ord t, Ord (Dur t), Num (Dur t)) => TimeLike t where
  -- | The duration type associated with @t@.
  type Dur t = d | d -> t

  -- | @'diffT' t2 t1@ is the duration from @t1@ to @t2@.
  diffT :: t -> t -> Dur t

  -- | @'addT' d t@ shifts @t@ forward (or backward, for negative @d@)
  -- by @d@.
  addT  :: Dur t -> t -> t


instance TimeLike UTCTime where
  type Dur UTCTime = NominalDiffTime
  diffT = diffUTCTime
  {-# INLINE diffT #-}
  addT = addUTCTime
  {-# INLINE addT #-}


instance TimeLike Time where
  type Dur Time = DiffTime
  diffT (Time a) (Time b) = a - b
  {-# INLINE diffT #-}
  addT  d (Time t) = Time (d + t)
  {-# INLINE addT #-}
