{-# LANGUAGE NamedFieldPuns #-}

module DeltaQ.SimpleGS where

import Data.Time.Clock (DiffTime)
import Numeric.Natural

-- FIXME should not use partial record fields.
data SimpleGS
  = SimpleDeltaQ
    { dqG    :: DiffTime        -- ^ seconds
    , dqS    :: Natural -> DiffTime -- ^ seconds per octet
    }
  | Bottom

perfection :: SimpleGS
perfection = SimpleDeltaQ 0 (const 0)

-- | calcuate the seconds per octet from a bps value
dqS' :: Double -> DiffTime
dqS' r = fromRational $ recip $ (toRational r) / 8

instance Semigroup SimpleGS where
  Bottom <> _      = Bottom
  _      <> Bottom = Bottom
  a      <> b      = SimpleDeltaQ (dqG a + dqG b) (\x -> dqS a x + dqS b x)

instance Monoid SimpleGS where
  mempty = perfection

-- | make a simple ∆Q|G,S measure, both measures must be
--   non-negative. S in seconds per octet
mkGS :: DiffTime -> DiffTime -> SimpleGS
mkGS a b
  | a >= 0 && b >= 0
    = SimpleDeltaQ a (\x -> b * fromIntegral x)
  | otherwise
    = error "mkGS: negative G or S"

-- | make a simple ∆Q|G,S measure, G must be non-negative, S must be positive.
mkGS' :: DiffTime -> Double -> SimpleGS
mkGS' a b
  | a >= 0 && b > 0
    = SimpleDeltaQ a (\x -> dqS' b * fromIntegral x)
  | otherwise
    = error "mkGS': negative G or non-positive S"


instance Eq SimpleGS where
  a == b = (compare a b) == EQ

instance Ord SimpleGS where
  Bottom `compare` Bottom = EQ
  _      `compare` Bottom = LT
  Bottom `compare` _      = GT
  a      `compare` b
    = (dqG a + dqS a 1) `compare` (dqG b + dqS b 1)
    

instance Show SimpleGS where
  show Bottom = "⊥"
  show SimpleDeltaQ{dqG, dqS}
    = show dqG ++ "+" ++ show (dqS 1) ++ "/o"

{-
instance QualityAttenuationMeasure SimpleGS where
  bottom = Bottom
  perfection = SimpleDeltaQ 0 (const 0)
  isBottom x = case x of {Bottom -> True; _ -> False}
-}
