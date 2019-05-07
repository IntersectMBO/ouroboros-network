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
import           Ouroboros.Storage.EpochInfo.CumulEpochSizes
import           Ouroboros.Storage.ImmutableDB.Layout

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
    epochSize epoch ces === toList ces !? fromIntegral (unEpochNo epoch)
  where
    xs !? i
      | 0 <= i, i < Foldable.length xs = Just (xs !! i)
      | otherwise                      = Nothing

prop_slotToEpochSlot_epochSlotToSlot :: CumulEpochSizes -> Property
prop_slotToEpochSlot_epochSlotToSlot ces =
    forAllShrink genValidSlot shrink $ \slot ->
      inBounds slot === Just True
  where
    genValidSlot = chooseSlot 0 (maxSlot ces)

    inBounds :: SlotNo -> Maybe Bool
    inBounds slot = do
        epoch                 <- slotToEpoch slot  ces
        (_size, first, final) <- epochInfo   epoch ces
        return $ first <= slot && slot <= final

epochInfo :: EpochNo -> CumulEpochSizes -> Maybe (EpochSize, SlotNo, SlotNo)
epochInfo epochNo ces = do
    SlotNo    first <- firstSlotOf epochNo ces
    EpochSize size  <- epochSize   epochNo ces
    return (EpochSize size, SlotNo first, SlotNo (first + size - 1))

prop_epochSlotToSlot_invalid :: CumulEpochSizes -> Property
prop_epochSlotToSlot_invalid ces =
    forAllShrink genInvalidEpochSlot shrinkInvalidEpochSlot $
      \(EpochSlot epochNo (RelativeSlot relSlot)) -> do
        case epochInfo epochNo ces of
          Nothing                     -> True
          Just (EpochSize size, _, _) -> relSlot >= size
  where
    genInvalidEpochSlot = arbitrary `suchThat` isInvalid
    shrinkInvalidEpochSlot epochSlot =
      filter isInvalid (shrink epochSlot)
    isInvalid (EpochSlot epoch relSlot)
      | Just sz <- epochSize epoch ces
      = relSlot > fromIntegral sz
      | otherwise
      = False

prop_slotToEpochSlot_greater_than_maxSlot :: CumulEpochSizes -> Property
prop_slotToEpochSlot_greater_than_maxSlot ces =
    forAllShrink genInvalidSlot shrink $ \invalidSlot ->
      slotToEpoch invalidSlot ces === Nothing
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
