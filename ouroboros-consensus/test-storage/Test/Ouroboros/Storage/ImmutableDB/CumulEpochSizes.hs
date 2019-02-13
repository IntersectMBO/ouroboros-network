{-# LANGUAGE DeriveGeneric #-}
-- | Data structure to convert between 'Slot's and 'EpochSlot's.
module Test.Ouroboros.Storage.ImmutableDB.CumulEpochSizes
  ( CumulEpochSizes
  , singleton
  , snoc
  , toList
  , lastEpoch
  , lastEpochSize
  , maxSlot
  , epochSize
  , slotToEpochSlot
  , epochSlotToSlot

  , tests
  )
  where

import qualified Data.Foldable as Foldable
import           Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import           Data.Sequence (Seq (..))
import qualified Data.Sequence as Seq

import           GHC.Generics (Generic)

import           Test.QuickCheck
import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.QuickCheck (testProperty)

import           Ouroboros.Network.Block (Slot (..))

import           Ouroboros.Storage.ImmutableDB.Types



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
--
newtype CumulEpochSizes = CES (Seq EpochSize) -- Guaranteed to be non-empty.
    deriving (Show, Eq, Generic)

-- | Create a 'CumulEpochSizes' using the given size for the first epoch.
singleton :: EpochSize -> CumulEpochSizes
singleton = CES . Seq.singleton

-- | Add a new epoch to the end with the given size.
snoc :: CumulEpochSizes -> EpochSize -> CumulEpochSizes
snoc _         0  = error "Epoch size must be > 0"
snoc (CES ces) es = case ces of
    Empty        -> error "Impossible: empty CumulEpochSizes"
    _ :|> lastEs -> CES (ces :|> lastEs + es)

-- | Helper to build a 'CumulEpochSizes' from a non-empty list of epoc sizes.
fromNonEmpty :: NonEmpty EpochSize -> CumulEpochSizes
fromNonEmpty (es NE.:| ess) =
    Foldable.foldl' snoc (singleton es) ess

-- | Convert to a list of (non-cumulative) epoch sizes.
--
-- > [100, 250, 450, 650, 2810]
-- > ->
-- > [100, 150, 200, 200, 2160]
toList :: CumulEpochSizes -> [EpochSize]
toList (CES ces) = zipWith (-) (Foldable.toList ces) (0 : Foldable.toList ces)

-- | Return the last added epoch.
--
-- Epochs start at 0.
lastEpoch :: CumulEpochSizes -> Epoch
lastEpoch (CES ces) = fromIntegral (Seq.length ces) - 1

-- | Return the size of the last added epoch.
lastEpochSize :: CumulEpochSizes -> EpochSize
lastEpochSize (CES Empty) = error "Impossible: empty CumulEpochSizes"
lastEpochSize (CES (Empty        :|> lastEs)) = lastEs
lastEpochSize (CES (_ :|> prevEs :|> lastEs)) = lastEs - prevEs

-- | Return the last slot that a blob could be stored at, i.e. the slot
-- corresponding to the last relative slot of the last epoch.
maxSlot :: CumulEpochSizes -> Slot
maxSlot (CES Empty)          = error "Impossible: empty CumulEpochSizes"
maxSlot (CES (_ :|> lastEs)) = fromIntegral lastEs - 1

-- | Return the size of the given epoch if known.
epochSize :: CumulEpochSizes -> Epoch -> Maybe EpochSize
epochSize (CES ces) epoch =
    case Seq.splitAt (fromIntegral epoch) ces of
      (Empty,        at :<| _) -> Just at
      (_ :|> before, at :<| _) -> Just (at - before)
      _                        -> Nothing

-- | Make sure the the given epoch is the last epoch for which the size is
-- stored. No-op if the current last epoch is <= the given epoch.
_rollBackToEpoch :: CumulEpochSizes -> Epoch -> CumulEpochSizes
_rollBackToEpoch (CES ces) epoch = CES $ Seq.take (succ (fromIntegral epoch)) ces

-- | Convert a 'Slot' to an 'EpochSlot'
--
-- For example:
--
-- > epochs:              0    1    2    3     4
-- > epoch sizes:       [100, 150, 200, 200, 2160]
-- > cumul epoch sizes: [100, 250, 450, 650, 2810]
-- > slot: 260 -> epoch slot: (2, 10)
slotToEpochSlot :: CumulEpochSizes -> Slot -> Maybe EpochSlot
slotToEpochSlot (CES ces) slot
    | _ :|> origLastEs <- ces
    , slot' < origLastEs
    = case Seq.dropWhileR (> slot') ces of
        ces'@(_ :|> lastEs) -> Just $ EpochSlot
          { _epoch        = fromIntegral (Seq.length ces')
          , _relativeSlot = RelativeSlot (slot' - lastEs)
          }
        Empty -> Just $ EpochSlot
          { _epoch        = 0
          , _relativeSlot = RelativeSlot slot'
          }
    | _ :|> origLastEs <- ces
    , origLastEs == slot'
      -- If the slot == the size of the last epoch, return (epoch + 1, 0).
    = Just $ EpochSlot
        { _epoch        = fromIntegral (Seq.length ces)
        , _relativeSlot = 0
        }
    | otherwise
    = Nothing
  where
    slot' = fromIntegral slot

-- | Convert an 'EpochSlot' to a 'Slot'
--
-- For example:
--
-- > epochs:              0    1    2    3     4
-- > epoch sizes:       [100, 150, 200, 200, 2160]
-- > cumul epoch sizes: [100, 250, 450, 650, 2810]
-- > epoch slot: (2, 10) -> slot: 260
epochSlotToSlot :: CumulEpochSizes -> EpochSlot -> Maybe Slot
epochSlotToSlot (CES ces) (EpochSlot epoch relSlot)
    | relSlot == 0
    , fromIntegral epoch == Seq.length ces
    , _ :|> lastEs <- ces
      -- Handle EpochSlot n 0 where n = the last epoch + 1
    = Just (fromIntegral lastEs)
    | otherwise
    = case Seq.splitAt (fromIntegral epoch) ces of
        (_ :|> before, at :<| _)
          | relSlot' < at - before
          -> Just $ fromIntegral (before + relSlot')
        (Empty, at :<| _)
          | relSlot' < at
          -> Just $ Slot relSlot'
        _ -> Nothing
  where
    relSlot' :: EpochSize
    relSlot' = getRelativeSlot relSlot


{------------------------------------------------------------------------------
  Tests
------------------------------------------------------------------------------}


instance Arbitrary CumulEpochSizes where
  arbitrary =
    fromNonEmpty . NE.fromList . fmap getPositive . getNonEmpty <$> arbitrary
  shrink ces =
    [ fromNonEmpty . NE.fromList . fmap getPositive . getNonEmpty $ es'
    | let es = NonEmpty . fmap Positive $ toList ces
    , es' <- shrink es
    ]

chooseSlot :: Slot -> Slot -> Gen Slot
chooseSlot (Slot start) (Slot end) = Slot <$> choose (start, end)

shrinkSlot :: Slot -> [Slot]
shrinkSlot (Slot w) = [Slot w' | w' <- shrink w]

shrinkRelSlot :: RelativeSlot -> [RelativeSlot]
shrinkRelSlot (RelativeSlot w) = [RelativeSlot w' | w' <- shrink w]

shrinkEpochSlot :: EpochSlot -> [EpochSlot]
shrinkEpochSlot (EpochSlot epoch relSlot) =
    [EpochSlot epoch' relSlot  | epoch' <- shrink epoch] ++
    [EpochSlot epoch  relSlot' | relSlot' <- shrinkRelSlot relSlot]

prop_ces_roundtrip :: CumulEpochSizes -> Property
prop_ces_roundtrip ces = fromNonEmpty (NE.fromList (toList ces)) === ces

prop_epochSize :: CumulEpochSizes -> Epoch -> Property
prop_epochSize ces epoch =
    epochSize ces epoch === toList ces !? fromIntegral epoch
  where
    xs !? i
      | 0 <= i, i < Foldable.length xs = Just (xs !! i)
      | otherwise                      = Nothing

prop_slotToEpochSlot_epochSlotToSlot :: CumulEpochSizes -> Property
prop_slotToEpochSlot_epochSlotToSlot ces =
    forAllShrink genValidSlot shrinkSlot $ \slot ->
      (slotToEpochSlot ces slot >>= epochSlotToSlot ces) === Just slot
  where
    genValidSlot = chooseSlot 0 (maxSlot ces + 1)

prop_epochSlotToSlot_invalid :: CumulEpochSizes -> Property
prop_epochSlotToSlot_invalid ces =
    forAllShrink genInvalidEpochSlot shrinkInvalidEpochSlot $ \epochSlot ->
      epochSlotToSlot ces epochSlot === Nothing
  where
    genInvalidEpochSlot = genEpochSlot `suchThat` isInvalid
    genEpochSlot = EpochSlot <$> arbitrary <*> (RelativeSlot <$> arbitrary)
    shrinkInvalidEpochSlot epochSlot =
      filter isInvalid (shrinkEpochSlot epochSlot)
    isInvalid (EpochSlot epoch relSlot)
      | Just sz <- epochSize ces epoch
      = relSlot >= fromIntegral sz
      | otherwise
      = False

prop_slotToEpochSlot_greater_than_maxSlot :: CumulEpochSizes -> Property
prop_slotToEpochSlot_greater_than_maxSlot ces =
    forAllShrink genInvalidSlot shrinkSlot $ \invalidSlot ->
      slotToEpochSlot ces invalidSlot === Nothing
  where
    genInvalidSlot = chooseSlot (succ (maxSlot ces)) maxBound

tests :: TestTree
tests = testGroup "CumulEpochSizes"
    [ testProperty "CumulEpochSizes/List roundtrip" prop_ces_roundtrip
    , testProperty "EpochSize" prop_epochSize
    , testProperty "slotToEpochSlot/epochSlotToSlot" prop_slotToEpochSlot_epochSlotToSlot
    , testProperty "epochSlotToSlot returns Nothing when the epochSlot is invalid" prop_epochSlotToSlot_invalid
    , testProperty "slotToEpochSlot returns Nothing when the slot is greater than maxSlot" prop_slotToEpochSlot_greater_than_maxSlot
    ]
