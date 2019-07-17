{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Ouroboros.Storage.EpochInfo.CumulEpochSizes
  ( CumulEpochSizes
  , singleton
  , fromNonEmpty
  , toList
  , lastEpoch
  , maxSlot
  , epochSize
  , slotToEpoch
  , firstSlotOf
  , getNewEpochSizesUntilM
  ) where

import           Control.Monad.State.Strict (StateT (..))

import           Data.Coerce (coerce)
import qualified Data.Foldable as Foldable
import           Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import           Data.Sequence (Seq (..))
import qualified Data.Sequence as Seq

import           GHC.Generics (Generic)
import           GHC.Stack

import           Ouroboros.Network.Block (SlotNo (..))

import           Ouroboros.Storage.Common

{------------------------------------------------------------------------------
  CumulEpochSizes
------------------------------------------------------------------------------}

-- ^ A sequence of the cumulative size of each epoch, from old to new. For
-- example, say we have the following epoch sizes (from old to new):
--
-- > 100, 150, 200, 200, 2160
--
-- These will be represented as:
--
-- > [100, 250, 450, 650, 2810]
--
-- This allows us to recover the original size of each epoch and to convert
-- 'SlotNo's to 'EpochSlot's (and vice versa).
newtype CumulEpochSizes = CES (Seq EpochSize) -- Invariant: non-empty.
    deriving (Show, Eq, Generic)

-- | \( O(1) \). Create a 'CumulEpochSizes' using the given size for the first
-- epoch.
singleton :: EpochSize -> CumulEpochSizes
singleton = CES . Seq.singleton

-- | \( O(1) \). Add a new epoch to the end with the given size.
snoc :: CumulEpochSizes -> EpochSize -> CumulEpochSizes
snoc _         0  = error "EpochNo size must be > 0"
snoc (CES ces) es = case ces of
    Empty        -> error "Impossible: empty CumulEpochSizes"
    _ :|> lastEs -> CES (ces :|> lastEs + es)

-- | \( O(n) \). Build a 'CumulEpochSizes' from a non-empty list of epoch
-- sizes.
fromNonEmpty :: NonEmpty EpochSize -> CumulEpochSizes
fromNonEmpty (es NE.:| ess) = Foldable.foldl' snoc (singleton es) ess

-- | \( O(n) \). Convert to a list of (non-cumulative) epoch sizes.
--
-- > [100, 250, 450, 650, 2810]
-- > ->
-- > [100, 150, 200, 200, 2160]
toList :: CumulEpochSizes -> [EpochSize]
toList (CES ces) = zipWith (-) (Foldable.toList ces) (0 : Foldable.toList ces)

-- | \( O(1) \). Return the last added epoch.
--
-- Epochs start at 0.
lastEpoch :: CumulEpochSizes -> EpochNo
lastEpoch (CES ces) = fromIntegral (Seq.length ces) - 1

-- | \( O(1) \). Return the last slot that a blob could be stored at, i.e. the
-- slot corresponding to the last relative slot of the last epoch.
maxSlot :: HasCallStack => CumulEpochSizes -> SlotNo
maxSlot (CES Empty)          = error "Impossible: empty CumulEpochSizes"
maxSlot (CES (_ :|> lastEs)) = coerce $ pred lastEs

-- | \( O(\log(\min(i,n-i))) \). Return the size of the given epoch if known.
epochSize :: EpochNo -> CumulEpochSizes -> Maybe EpochSize
epochSize epoch (CES ces) =
    case Seq.splitAt (fromIntegral $ unEpochNo epoch) ces of
      (Empty,        at :<| _) -> Just at
      (_ :|> before, at :<| _) -> Just (at - before)
      _                        -> Nothing

-- | \( O(i) \). Convert a 'SlotNo' to an 'EpochSlot'
--
-- For example:
--
-- > epochs:              0    1    2    3     4
-- > epoch sizes:       [100, 150, 200, 200, 2160]
-- > cumul epoch sizes: [100, 250, 450, 650, 2810]
-- > slot: 260 -> epoch slot: (2, 11)
slotToEpoch :: SlotNo -> CumulEpochSizes -> Maybe EpochNo
slotToEpoch slot (CES ces)
    | _ :|> origLastEs <- ces
    , slot' < origLastEs
    = let ces' = Seq.dropWhileR (> slot') ces
      in Just (fromIntegral (Seq.length ces'))
    | otherwise = Nothing
  where
    slot' :: EpochSize
    slot' = coerce slot

-- | Return the first 'SlotNo' of the given 'EpochNo' if the 'EpochNo' is
-- stored in the 'CumulEpochSizes'.
firstSlotOf :: EpochNo -> CumulEpochSizes -> Maybe SlotNo
firstSlotOf epoch (CES ces)
    | fromIntegral (unEpochNo epoch) == Seq.length ces
    , _ :|> lastEs <- ces
    = Just $ coerce lastEs
    | otherwise
    = case Seq.splitAt (fromIntegral $ unEpochNo epoch) ces of
        (_ :|> before, _ :<| _)
          -> Just $ coerce before
        (Empty, _ :<| _)
          -> Just 0
        _ -> Nothing

{------------------------------------------------------------------------------
  StateT-based helpers
------------------------------------------------------------------------------}

-- | Keep requesting and adding epoch sizes to 'CumulEpochSizes' until the
-- function returns a @Just a@, then return that @a@.
getNewEpochSizesUntilM :: forall m a. Monad m
                       => (CumulEpochSizes -> Maybe a)
                       -> (EpochNo -> m EpochSize)
                       -> StateT CumulEpochSizes m a
getNewEpochSizesUntilM untilJust getEpochSize = StateT go
  where
    go :: CumulEpochSizes -> m (a, CumulEpochSizes)
    go ces
      | Just a <- untilJust ces
      = return (a, ces)
      | otherwise
      = do
        newEpochSize <- getEpochSize (succ (lastEpoch ces))
        go (snoc ces newEpochSize)
