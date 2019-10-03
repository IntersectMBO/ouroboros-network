{-# LANGUAGE GADTSyntax #-}

module DeltaQ.SimpleGS where

import Data.Time.Clock (DiffTime)
import Numeric.Natural

data SimpleGS where
  -- | Seconds (G) and seconds per octet (S).
  SimpleDeltaQ :: !DiffTime -> !(Natural -> DiffTime) -> SimpleGS
  Bottom       :: SimpleGS

perfection :: SimpleGS
perfection = SimpleDeltaQ 0 (const 0)

-- | calcuate the seconds per octet from a bps value
dqS' :: Double -> DiffTime
dqS' r = fromRational $ recip $ (toRational r) / 8

instance Semigroup SimpleGS where
  Bottom           <> _                  = Bottom
  _                <> Bottom             = Bottom
  SimpleDeltaQ g s <> SimpleDeltaQ g' s' = SimpleDeltaQ
    ((+)     g     g')
    ((+) <$> s <*> s')

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

instance Show SimpleGS where
  show Bottom             = "⊥"
  show (SimpleDeltaQ g s) = show g ++ "+" ++ show (s 1) ++ "/o"
