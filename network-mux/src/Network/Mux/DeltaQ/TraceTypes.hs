{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Network.Mux.DeltaQ.TraceTypes
  ( SISec (..)
  , SISec2 (..)
  , squareSISec
  ) where

-- Map time intervals to real numbers, for the arithmetic.
newtype SISec  = S  Float -- this is all the precision we need,
  deriving (Eq, Ord, Num)
newtype SISec2 = S2 Float -- are there performance reasons to use Double?
  deriving (Eq, Ord, Num)

squareSISec :: SISec -> SISec2
squareSISec (S x) = S2 $ x * x
