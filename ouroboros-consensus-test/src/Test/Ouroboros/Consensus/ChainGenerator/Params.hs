{-# LANGUAGE PatternSynonyms #-}

module Test.Ouroboros.Consensus.ChainGenerator.Params (
  Asc (Asc, UnsafeAsc),
  Delta (Delta),
  Len (Len),
  Kcp (Kcp),
  Scg (Scg),
  ascFromBits,
  ascFromDouble,
  ascVal,
  ) where

import qualified Data.Bits as B

-----

-- | The Δ parameter of the Praos Race Assumption
--
-- ASSUMPTION: If an honest block @b@ is minted at the start of slot @x@, then
-- every (healthy) honest node will have selected a chain no worse than @b@ by
-- the onset of slot @x + Δ + 1@.
--
-- NOTE: If @Δ=0@, then the best block minted in each slot is selected by every
-- (healthy) honset before the onset of the next slot.
--
-- NOTE: If the honest block @k+1@ after the intersection was minted in slot
-- @x@, then the adversarial block @k+1@ after the intersection can be minted no
-- sooner than slot @x + Δ + 1@. Thus @x + Δ@ is the youngest slot in the Praos
-- Race Window.
newtype Delta = Delta Int
  deriving (Eq, Ord, Show, Read)

-- | The maximum length of any leader schedule
--
-- This can be interpreted as the /end of time/.
newtype Len = Len Int
  deriving (Eq, Ord, Show, Read)

-- | The @k@ parameter of the Praos Common Prefix property
--
-- Also known as the 'Ouroboros.Consensus.Config.SecurityParam.SecurityParam'.
newtype Kcp = Kcp Int
  deriving (Eq, Ord, Show, Read)

-- | The @s@ parameter of the Praos Chain Growth property
--
-- Also known as the width of the /stability window/. In particular, we assume
-- that an adversarial stake holder cannot drastically increase their rate of
-- election until at least @s@ many slots after the first block on an
-- adversarial chain.
--
-- In other words: we're assuming that any serious attempt to corrupt the leader
-- schedule would be isolated to a private adversarial chain.
--
-- Note: this is also a parameter to the separate Enriched Praos Chain Growth
-- property; see 'Test.Ouroboros.Consensus.ChainGenerator.Honest.checkHonestChain'.
newtype Scg = Scg Int
  deriving (Eq, Ord, Show, Read)

-----

-- | The /active slot coefficient/
--
-- INVARIANT: 0 < x < 1
--
-- It's as precise as 'Double', which is likely suffices for all of our needs.
newtype Asc = UnsafeAsc Double
  deriving (Eq, Read, Show)

pattern Asc :: Double -> Asc
pattern Asc d <- UnsafeAsc d

{-# COMPLETE Asc #-}

ascFromDouble :: Double -> Asc
ascFromDouble d
  | d <= 0    = error "Asc must be > 0"
  | 1 <= d    = error "Asc must be < 1"
  | otherwise = UnsafeAsc d

-- | PRECONDITION: the bits aren't all the same
--
-- The 'Asc' that equals the fraction @w \/ 2^widthW@.
ascFromBits :: (Enum w, B.FiniteBits w) => w -> Asc
ascFromBits w = ascFromDouble $ toEnum (fromEnum w) / (2 ^ B.finiteBitSize w)

-- | Interpret 'Asc' as a 'Double'
ascVal :: Asc -> Double
ascVal (Asc x) = x
