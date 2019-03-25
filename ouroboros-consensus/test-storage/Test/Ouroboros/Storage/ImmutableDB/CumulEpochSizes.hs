{-# OPTIONS_GHC -Wno-orphans #-}
-- | Data structure to convert between 'SlotNo's and 'EpochSlot's.
module Test.Ouroboros.Storage.ImmutableDB.CumulEpochSizes ( tests ) where

import qualified Data.Foldable as Foldable
import qualified Data.List.NonEmpty as NE

import           Test.QuickCheck
import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.QuickCheck (testProperty)

import           Ouroboros.Network.Block (SlotNo (..))

import           Ouroboros.Storage.Common
import           Ouroboros.Storage.ImmutableDB.CumulEpochSizes

import           Test.Util.Orphans.Arbitrary (genLimitedEpochSize,
                     genSmallEpochNo)


instance Arbitrary CumulEpochSizes where
  arbitrary =
    fromNonEmpty . NE.fromList . getNonEmpty . NonEmpty <$> listOf1 genLimitedEpochSize
  shrink ces =
    [ fromNonEmpty . NE.fromList . fmap getPositive . getNonEmpty $ es'
    | let es = NonEmpty . fmap Positive $ toList ces
    , es' <- shrink es
    ]

chooseSlot :: SlotNo -> SlotNo -> Gen SlotNo
chooseSlot (SlotNo start) (SlotNo end) = SlotNo <$> choose (start, end)

prop_ces_roundtrip :: CumulEpochSizes -> Property
prop_ces_roundtrip ces = fromNonEmpty (NE.fromList (toList ces)) === ces

prop_epochSize :: CumulEpochSizes -> Property
prop_epochSize ces =
    forAll genSmallEpochNo $ \ epoch ->
    epochSize ces epoch === toList ces !? fromIntegral (unEpochNo epoch)
  where
    xs !? i
      | 0 <= i, i < Foldable.length xs = Just (xs !! i)
      | otherwise                      = Nothing

prop_slotToEpochSlot_epochSlotToSlot :: CumulEpochSizes -> Property
prop_slotToEpochSlot_epochSlotToSlot ces =
    forAllShrink genValidSlot shrink $ \slot ->
      (slotToEpochSlot ces slot >>= epochSlotToSlot ces) === Just slot
  where
    genValidSlot = chooseSlot 0 (maxSlot ces)

prop_epochSlotToSlot_invalid :: CumulEpochSizes -> Property
prop_epochSlotToSlot_invalid ces =
    forAllShrink genInvalidEpochSlot shrinkInvalidEpochSlot $ \epochSlot ->
      epochSlotToSlot ces epochSlot === Nothing
  where
    genInvalidEpochSlot = arbitrary `suchThat` isInvalid
    shrinkInvalidEpochSlot epochSlot =
      filter isInvalid (shrink epochSlot)
    isInvalid (EpochSlot epoch relSlot)
      | Just sz <- epochSize ces epoch
      = relSlot > fromIntegral sz
      | otherwise
      = False

prop_slotToEpochSlot_greater_than_maxSlot :: CumulEpochSizes -> Property
prop_slotToEpochSlot_greater_than_maxSlot ces =
    forAllShrink genInvalidSlot shrink $ \invalidSlot ->
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
