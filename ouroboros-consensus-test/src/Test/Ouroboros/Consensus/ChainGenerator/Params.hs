{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}

module Test.Ouroboros.Consensus.ChainGenerator.Params (
  Asc (Half, HalfOf, HalfOfOnePlus),
  Delta (Delta),
  Len (Len),
  Kcp (Kcp),
  Scg (Scg),
  ascFromBits,
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
-- Algebraic data type representing a binary fraction greater than 0 and less than 1.
data Asc =
    Half   -- ^ @0.5@ in decimal, ie @0.1@ in binary
  |
    HalfOf Asc   -- ^ @x \/ 2@ in decimal, ie @x \/ 10@ in binary
  |
    HalfOfOnePlus Asc   -- ^ @(x + 1) \/ 2@ in decimal, ie @0.1 + (x \/ 10)@ in binary
  deriving (Eq, Read, Show)

-- | PRECONDITION: the bits aren't all the same
--
-- The 'Asc' that equals the fraction @w \/ 2^widthW@.
ascFromBits :: B.FiniteBits w => w -> Asc
ascFromBits = \w -> if
    |              B.zeroBits == w -> error "ascFromBits undefined for all bits clear"
    | B.complement B.zeroBits == w -> error "ascFromBits undefined for all bits set"
    | otherwise                    -> go w
  where
    go w =
        let msb = B.testBit w (B.finiteBitSize w - 1)
            w'  = w `B.shiftL` 1   -- ie towards the MSB
        in
        if 0 == B.popCount w' then Half else (if msb then HalfOfOnePlus else HalfOf) (go w')

-- | Interpret 'Asc' as a 'Double'
ascVal :: Asc -> Double
ascVal = \case
    Half              -> 0.5
    HalfOf        asc -> ascVal asc / 2
    HalfOfOnePlus asc -> (1 + ascVal asc) / 2
