{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables        #-}
-- | Conversion between 'Slot's and 'EpochSlot's.
--
-- The chain consists of 'Slot's. Each 'Slot' may be occupied by at most one
-- block. In the 'ImmutableDB', we don't store the chain in one big file, but
-- group blocks per 'Epoch', which are then written to an epoch file. Within
-- each 'Epoch', the blocks are given 'RelativeSlot's. The combination of an
-- 'Epoch' and a 'RelativeSlot' is an 'EpochSlot'. The 'ImmutableDB' will need
-- to be able to convert 'Slot's to 'EpochSlot's and vice versa.
--
-- Additionally, each epoch may store an Epoch Boundary Block (EBB). This EBB
-- logically lives between the last slot of an epoch and the first slot of the
-- next epoch. In the 'ImmutableDB', these are stored at the beginning of each
-- epoch file, namely at relative slot 0.
--
-- For example:
--
-- > Epochs:         <──────── 0 ────────> <────── 1 ──────>
-- > Epoch size:               4                   3
-- >                 ┌───┬───┬───┬───┬───┐ ┌───┬───┬───┬───┐
-- >                 │   │   │   │   │   │ │   │   │   │   │
-- >                 └───┴───┴───┴───┴───┘ └───┴───┴───┴───┘
-- > 'RelativeSlot':   0   1   2   3   4     0   1   2   3
-- > 'Slot':          EBB  0   1   2   3    EBB  4   5   6
--
-- Note that the epoch size does not include the (optional) EBB.
module Ouroboros.Storage.ImmutableDB.CumulEpochSizes
  ( -- * RelativeSlot & EpochSlot
    RelativeSlot(..)
  , lastRelativeSlot
  , EpochSlot(..)

    -- * CumulEpochSizes
  , CumulEpochSizes
  , singleton
  , snoc
  , fromNonEmpty
  , toList
  , lastEpoch
  , lastEpochSize
  , maxSlot
  , epochSize
  , rollBackToEpoch
  , slotToEpochSlot
  , epochSlotToSlot
  , firstSlotOf

    -- * StateT-based helpers
  , getNewEpochSizesUntilM
  , getEpochSizeM
  , slotToEpochSlotM
  , slotToRelativeSlotM
  ) where

import           Control.Exception (assert)
import           Control.Monad.State.Strict (StateT (StateT), get, lift, modify,
                     put)

import           Data.Coerce (coerce)
import qualified Data.Foldable as Foldable
import           Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import           Data.Sequence (Seq (..))
import qualified Data.Sequence as Seq

import           GHC.Generics (Generic)

import           Ouroboros.Network.Block (Slot (..))

import           Ouroboros.Storage.ImmutableDB.Types (Epoch (Epoch),
                     EpochSize (EpochSize))


{------------------------------------------------------------------------------
  RelativeSlot & EpochSlot
------------------------------------------------------------------------------}

-- | A /relative/ slot within an 'Epoch'.
newtype RelativeSlot = RelativeSlot { getRelativeSlot :: Word }
  deriving (Eq, Ord, Enum, Num, Show, Generic, Real, Integral)

-- | Return the last relative slot within the given epoch size.
--
-- Relative slot 0 is reserved for the EBB and regular relative slots start at
-- 0, so the last relative slot is equal to the epoch size.
lastRelativeSlot :: EpochSize -> RelativeSlot
lastRelativeSlot (EpochSize sz) = RelativeSlot sz

-- | The combination of an 'Epoch' and a 'RelativeSlot' within the epoch.
data EpochSlot = EpochSlot
  { _epoch        :: !Epoch
  , _relativeSlot :: !RelativeSlot
  } deriving (Eq, Ord, Generic)

instance Show EpochSlot where
  show (EpochSlot (Epoch e) (RelativeSlot s)) = show (e, s)


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
-- 'Slot's to 'EpochSlot's (and vice versa).
newtype CumulEpochSizes = CES (Seq EpochSize) -- Invariant: non-empty.
    deriving (Show, Eq, Generic)

-- | \( O(1) \). Create a 'CumulEpochSizes' using the given size for the first
-- epoch.
singleton :: EpochSize -> CumulEpochSizes
singleton = CES . Seq.singleton

-- | \( O(1) \). Add a new epoch to the end with the given size.
snoc :: CumulEpochSizes -> EpochSize -> CumulEpochSizes
snoc _         0  = error "Epoch size must be > 0"
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
lastEpoch :: CumulEpochSizes -> Epoch
lastEpoch (CES ces) = fromIntegral (Seq.length ces) - 1

-- | \( O(1) \). Return the size of the last added epoch.
lastEpochSize :: CumulEpochSizes -> EpochSize
lastEpochSize (CES Empty) = error "Impossible: empty CumulEpochSizes"
lastEpochSize (CES (Empty        :|> lastEs)) = lastEs
lastEpochSize (CES (_ :|> prevEs :|> lastEs)) = lastEs - prevEs

-- | \( O(1) \). Return the last slot that a blob could be stored at, i.e. the
-- slot corresponding to the last relative slot of the last epoch.
maxSlot :: CumulEpochSizes -> Slot
maxSlot (CES Empty)          = error "Impossible: empty CumulEpochSizes"
maxSlot (CES (_ :|> lastEs)) = coerce lastEs - 1

-- | \( O(\log(\min(i,n-i))) \). Return the size of the given epoch if known.
epochSize :: CumulEpochSizes -> Epoch -> Maybe EpochSize
epochSize (CES ces) epoch =
    case Seq.splitAt (fromIntegral epoch) ces of
      (Empty,        at :<| _) -> Just at
      (_ :|> before, at :<| _) -> Just (at - before)
      _                        -> Nothing

-- | \( O(\log(\min(i,n-i))) \). Make sure the the given epoch is the last
-- epoch for which the size is stored. No-op if the current last epoch is <=
-- the given epoch.
rollBackToEpoch :: CumulEpochSizes -> Epoch -> CumulEpochSizes
rollBackToEpoch (CES ces) epoch = CES $ Seq.take (succ (fromIntegral epoch)) ces

-- | \( O(i) \). Convert a 'Slot' to an 'EpochSlot'
--
-- For example:
--
-- > epochs:              0    1    2    3     4
-- > epoch sizes:       [100, 150, 200, 200, 2160]
-- > cumul epoch sizes: [100, 250, 450, 650, 2810]
-- > slot: 260 -> epoch slot: (2, 11)
slotToEpochSlot :: CumulEpochSizes -> Slot -> Maybe EpochSlot
slotToEpochSlot (CES ces) slot
    | _ :|> origLastEs <- ces
    , slot' < origLastEs
    = case Seq.dropWhileR (> slot') ces of
        ces'@(_ :|> lastEs) -> Just $ EpochSlot
          { _epoch        = fromIntegral (Seq.length ces')
          , _relativeSlot = succ (coerce (slot' - lastEs))
          }
        Empty -> Just $ EpochSlot
          { _epoch        = 0
          , _relativeSlot = succ (coerce slot)
          }
    | otherwise = Nothing
  where
    slot' :: EpochSize
    slot' = coerce slot

-- | \( O(\log(\min(i,n-i))) \). Convert an 'EpochSlot' to a 'Slot'
--
-- For example:
--
-- > epochs:              0    1    2    3     4
-- > epoch sizes:       [100, 150, 200, 200, 2160]
-- > cumul epoch sizes: [100, 250, 450, 650, 2810]
-- > epoch slot: (2, 11) -> slot: 260
--
-- If the 'EpochSlot' has 'RelativeSlot' 0, i.e. it refers to an EBB, then the
-- first slot of that epoch will be returned.
epochSlotToSlot :: CumulEpochSizes -> EpochSlot -> Maybe Slot
epochSlotToSlot (CES ces) (EpochSlot epoch relSlot) =
    case Seq.splitAt (fromIntegral epoch) ces of
      (_ :|> before, at :<| _)
        | relSlot' < at - before
        -> Just $ coerce (before + relSlot')
      (Empty, at :<| _)
        | relSlot' < at
        -> Just $ coerce relSlot'
      _ -> Nothing
  where
    relSlot' :: EpochSize
    relSlot' | relSlot == 0 = 0
             | otherwise    = coerce relSlot - 1
             -- RelativeSlot 0 and 1 will point to the same Slot


-- | Return the first 'Slot' of the given 'Epoch' if the 'Epoch' is stored in
-- the 'CumulEpochSizes'.
firstSlotOf :: CumulEpochSizes -> Epoch -> Maybe Slot
firstSlotOf (CES ces) epoch
    | fromIntegral epoch == Seq.length ces
    , _ :|> lastEs <- ces
    = Just $ coerce lastEs
    | otherwise
    = case Seq.splitAt (fromIntegral epoch) ces of
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
                       -> (Epoch -> m EpochSize)
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


-- | Get the size of the given epoch from the 'CumulEpochSizes'. Requests and
-- adds epoch sizes until the size of the epoch is in the 'CumulEpochSizes'.
-- The possibly updated 'CumulEpochSizes' is returned as the state.
getEpochSizeM :: Monad m
              => Epoch
              -> (Epoch -> m EpochSize)
              -> StateT CumulEpochSizes m EpochSize
getEpochSizeM epoch getEpochSize = do
    ces <- get
    case epochSize ces epoch of
      Just cachedEpochSize -> return cachedEpochSize
      Nothing -> assert (epoch > lastEpoch ces) $ do
        -- Get the the missing epoch sizes using getEpochSize and add them to
        -- the cumulEpochSizes, so they are cached for the next time.
        newEpochSizes <- mapM (lift . getEpochSize)
          [succ (lastEpoch ces) .. epoch]
        let ces' = Foldable.foldl' snoc ces newEpochSizes
        put ces'
        return $ lastEpochSize ces'


-- | Convert the 'Slot' to an 'EpochSlot', updating the 'CumulEpochSizes'
-- cache if necessary.
slotToEpochSlotM :: Monad m
                 => Slot
                 -> (Epoch -> m EpochSize)
                 -> StateT CumulEpochSizes m EpochSlot
slotToEpochSlotM slot getEpochSize = go
  where
    go = do
      ces <- get
      case slotToEpochSlot ces slot of
        Just epochSlot -> return epochSlot
        Nothing        -> assert (slot > maxSlot ces) $ do
          newEpochSize <- lift $ getEpochSize $ succ (lastEpoch ces)
          modify (`snoc` newEpochSize)
          -- Try again
          go

-- | Convert the 'Slot' to a 'RelativeSlot', updating the 'CumulEpochSizes'
-- cache if necessary.
slotToRelativeSlotM :: Monad m
                    => Slot
                    -> (Epoch -> m EpochSize)
                    -> StateT CumulEpochSizes m RelativeSlot
slotToRelativeSlotM slot getEpochSize =
    _relativeSlot <$> slotToEpochSlotM slot getEpochSize
