{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE ViewPatterns               #-}

-- @Measured TipMeasure (Tip block)@ is an orphaned instance.
{-# OPTIONS_GHC -Wno-orphans #-}


-- | This is like `ChainFragment` but it does not do some of its assertions.
--
module Ouroboros.Network.TipSample.TipFragment
  ( TipFragment
  , TipFragment' (Empty, (:>), (:<), TipFragment)
  , Timed (..)
  , viewRight
  , viewLeft
  , lookupBySlotNo
  , dropNewestUntilSlotNo
  , dropOldestUntilSlotNo
  , toOldestFirst

    -- * Internal api
  , TipMeasure (..)
  , lookupBySlotFT
  ) where

import           Control.Monad.Class.MonadTime (Time)

import           Cardano.Slotting.Slot (SlotNo, WithOrigin (..))

import qualified Data.Foldable as Foldable
import           Data.FingerTree.Strict (Measured (..), StrictFingerTree)
import qualified Data.FingerTree.Strict as FT
import           GHC.Generics (Generic)
import           Quiet (Quiet (..))

import           Ouroboros.Network.Block (Tip (..), getTipSlotNo)

data TipMeasure = TipMeasure {
    tmMinSlot :: !(WithOrigin SlotNo),
    tmMaxSlot :: !(WithOrigin SlotNo),
    tmSize    :: !Int
  }

instance Semigroup TipMeasure where
  vl <> vr =
    TipMeasure (min (tmMinSlot vl) (tmMinSlot vr))
               (max (tmMaxSlot vl) (tmMaxSlot vr))
               (tmSize vl + tmSize vr)

instance Monoid TipMeasure where
  mempty = TipMeasure (At maxBound) Origin 0
  mappend = (<>)

instance Measured TipMeasure (Tip block) where
  measure (Tip slotNo _ _blockNo) = TipMeasure (At slotNo) (At slotNo) 1
  measure TipGenesis = TipMeasure Origin Origin 1


data Timed a = Timed {
    timedAt   :: !Time,
    timedData :: !a
  }
  deriving (Eq, Generic)
  deriving Show via Quiet (Timed a)

instance Measured TipMeasure a => Measured TipMeasure (Timed a) where
    measure = measure . timedData

newtype TipFragment' tip = TipFragment (StrictFingerTree TipMeasure tip)
  deriving stock   (Show, Eq)
  deriving newtype Foldable

type TipFragment header = TipFragment' (Timed (Tip header))

viewRight :: Measured TipMeasure tip => TipFragment' tip -> FT.ViewR TipFragment' tip
viewRight (TipFragment c) = case FT.viewr c of
  FT.EmptyR  -> FT.EmptyR
  c' FT.:> tip -> TipFragment c' FT.:> tip

viewLeft :: Measured TipMeasure tip => TipFragment' tip -> FT.ViewL TipFragment' tip
viewLeft (TipFragment c) = case FT.viewl c of
  FT.EmptyL -> FT.EmptyL
  tip FT.:< c' -> tip FT.:< TipFragment c'

pattern Empty :: Measured TipMeasure tip => TipFragment' tip
pattern Empty <- (viewRight -> FT.EmptyR) where
  Empty = TipFragment FT.empty

-- | \( O(1) \). Add a tip to the right of the chain fragment.
pattern (:>) :: Measured TipMeasure tip
             => Measured TipMeasure tip
             => TipFragment' tip -> tip -> TipFragment' tip
pattern c :> tip <- (viewRight -> (c FT.:> tip)) where
  TipFragment c :> tip = TipFragment (c FT.|> tip)

-- | \( O(1) \). Add a tip to the left of the chain fragment.
pattern (:<) :: Measured TipMeasure tip
             => Measured TipMeasure tip
             => tip -> TipFragment' tip -> TipFragment' tip
pattern b :< c <- (viewLeft -> (b FT.:< c)) where
  b :< TipFragment c = TipFragment (b FT.<| c)

infixl 5 :>, :<

{-# COMPLETE Empty, (:>) #-}
{-# COMPLETE Empty, (:<) #-}

-- | \( O(log(min(i,n-i))) \).
-- Lookup by 'SlotNo';  note that 'lookupBySlotFT' does not guarantee that it
-- will find the correct slot: it will find the tip with the smallest `SlotNo`
-- which is greater or equal to the given 'slotNo'.
--
lookupBySlotFT :: WithOrigin SlotNo
               -> TipFragment header
               -> FT.SearchResult TipMeasure (Timed (Tip header))
lookupBySlotFT slotNo (TipFragment c) =
    FT.search (\vl vr -> tmMaxSlot vl >= slotNo && tmMinSlot vr > slotNo) c


-- | \( O(log(min(i,n-i))) \).
--
-- Returns the right most tip with the given 'SlotNo' and the 'TipFragment'
-- that follows it.
--
lookupBySlotNo :: WithOrigin SlotNo
               -> TipFragment header
               -> Maybe (Timed (Tip header), TipFragment header)
lookupBySlotNo s tf =
    case lookupBySlotFT s tf of
      FT.Position _ a@(Timed _ tip) tf'
        | getTipSlotNo tip == s -> Just (a,  TipFragment tf')
        | otherwise             -> Nothing
      FT.OnLeft  -> Nothing
      FT.OnRight -> Nothing
      FT.Nowhere -> Nothing

-- | \( O(log(min(i,n-i))) \).
-- Drop all newest @'Tip' header@ which 'SlotNo' is greater than the given one.
--
dropNewestUntilSlotNo :: SlotNo
                      -> TipFragment header
                      -> TipFragment header
dropNewestUntilSlotNo slotNo (TipFragment c) =
    TipFragment $ FT.takeUntil (\v -> tmMaxSlot v > At slotNo) c


-- | \( O(log(min(i,n-i)) \).
-- Drop all @'Tip' header@ which 'SlotNo' is smaller than the given one.
--
dropOldestUntilSlotNo :: SlotNo
                      -> TipFragment header
                      -> TipFragment header
dropOldestUntilSlotNo slotNo (TipFragment c) =
    TipFragment $ FT.dropUntil (\v -> tmMaxSlot v >= At slotNo) c


-- | \( O(n) \). Make a list of blocks from a 'TipFragment', in
-- oldest-to-newest order.
toOldestFirst :: TipFragment header -> [Timed (Tip header)]
toOldestFirst (TipFragment ft) = Foldable.toList ft
