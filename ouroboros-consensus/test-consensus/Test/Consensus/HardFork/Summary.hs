{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE NamedFieldPuns            #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE StandaloneDeriving        #-}

module Test.Consensus.HardFork.Summary (tests) where

import           Data.Bifunctor
import           Data.Foldable (toList)
import           Data.Time
import           Data.Word

import           Test.QuickCheck
import           Test.Tasty
import           Test.Tasty.QuickCheck

import           Cardano.Slotting.Slot

import           Ouroboros.Consensus.HardFork.History (ShiftTime (..))
import qualified Ouroboros.Consensus.HardFork.History as HF
import           Ouroboros.Consensus.Util.Counting

import           Test.Consensus.HardFork.Infra
import           Test.Util.Orphans.Arbitrary

tests :: TestTree
tests = testGroup "HardForkHistory" [
      testGroup "Summary" [
          testGroup "Sanity" [
              testProperty "generator" $ checkGenerator $ \ArbitrarySummary{..} ->
                checkInvariant HF.invariantSummary arbitrarySummary
            , testProperty "shrinker"  $ checkShrinker $ \ArbitrarySummary{..} ->
                checkInvariant HF.invariantSummary arbitrarySummary
            ]
        , testGroup "Conversions" [
              testProperty "roundtripWallclockSlot" roundtripWallclockSlot
            , testProperty "roundtripSlotWallclock" roundtripSlotWallclock
            , testProperty "roundtripSlotEpoch"     roundtripSlotEpoch
            , testProperty "roundtripEpochSlot"     roundtripEpochSlot
            , testProperty "reportsPastHorizon"     reportsPastHorizon
            ]
        ]
    ]

{-------------------------------------------------------------------------------
  Dealing with the 'PastHorizonException'
-------------------------------------------------------------------------------}

noPastHorizonException :: ArbitrarySummary
                       -> (forall xs. HF.Query xs Property)
                       -> Property
noPastHorizonException ArbitrarySummary{..} p =
    case HF.runQuery p arbitrarySummary of
      Right prop -> prop
      Left  ex   -> counterexample ("Unexpected " ++ show ex) $
                      property False

isPastHorizonException :: Show a
                       => ArbitrarySummary
                       -> (forall xs. HF.Query xs a)
                       -> Property
isPastHorizonException ArbitrarySummary{..} ma =
    case HF.runQuery ma arbitrarySummary of
      Left  _ -> property True
      Right a -> counterexample ("Unexpected " ++ show a) $
                   property False

{-------------------------------------------------------------------------------
  Tests using just 'Summary'
-------------------------------------------------------------------------------}

roundtripWallclockSlot :: ArbitrarySummary -> Property
roundtripWallclockSlot s@ArbitrarySummary{beforeHorizonTime = time} =
    noPastHorizonException s $ do
      (slot  ,  inSlot ) <- HF.wallclockToSlot time
      (time' , _slotLen) <- HF.slotToWallclock slot
      return $ addUTCTime inSlot time' === time

roundtripSlotWallclock :: ArbitrarySummary -> Property
roundtripSlotWallclock s@ArbitrarySummary{beforeHorizonSlot = slot} =
    noPastHorizonException s $ do
      (time  , _slotLen) <- HF.slotToWallclock slot
      (slot' ,  inSlot ) <- HF.wallclockToSlot time
      return $ slot' === slot .&&. inSlot === 0

roundtripSlotEpoch :: ArbitrarySummary -> Property
roundtripSlotEpoch s@ArbitrarySummary{beforeHorizonSlot = slot} =
    noPastHorizonException s $ do
      (epoch ,  inEpoch  ) <- HF.slotToEpoch slot
      (slot' , _epochSize) <- HF.epochToSlot epoch
      return $ HF.addSlots inEpoch slot' === slot

roundtripEpochSlot :: ArbitrarySummary -> Property
roundtripEpochSlot s@ArbitrarySummary{beforeHorizonEpoch = epoch} =
    noPastHorizonException s $ do
      (slot  , _epochSize) <- HF.epochToSlot epoch
      (epoch',  inEpoch  ) <- HF.slotToEpoch slot
      return $ epoch' === epoch .&&. inEpoch === 0

reportsPastHorizon :: ArbitrarySummary -> Property
reportsPastHorizon s@ArbitrarySummary{..} = conjoin [
      isPastHorizonException s $ HF.wallclockToSlot pastHorizonTime
    , isPastHorizonException s $ HF.slotToWallclock pastHorizonSlot
    , isPastHorizonException s $ HF.slotToEpoch     pastHorizonSlot
    , isPastHorizonException s $ HF.epochToSlot     pastHorizonEpoch
    ]

{-------------------------------------------------------------------------------
  Arbitrary 'Summary'

  We should be able to show properties of the conversion functions independent
  of how the 'Summary' that they use is derived.
-------------------------------------------------------------------------------}

data ArbitrarySummary = forall xs. ArbitrarySummary {
      arbitrarySummaryStart :: HF.SystemStart
    , arbitrarySummary      :: HF.Summary xs
    , beforeHorizonTime     :: UTCTime
    , beforeHorizonSlot     :: SlotNo
    , beforeHorizonEpoch    :: EpochNo
    , pastHorizonTime       :: UTCTime
    , pastHorizonSlot       :: SlotNo
    , pastHorizonEpoch      :: EpochNo
    }

deriving instance Show ArbitrarySummary

instance Arbitrary ArbitrarySummary where
  arbitrary = chooseEras $ \is -> do
      start   <- HF.SystemStart <$> arbitrary
      summary <- HF.Summary . exactlyWeaken <$>
                   erasMapStateM genEraSummary is (HF.initBound start)

      let summaryStart, summaryEnd :: HF.Bound
          (summaryStart, summaryEnd) = summaryBounds summary

          summarySlots, summaryEpochs :: Word64
          summarySlots  = HF.countSlots
                            (HF.boundSlot summaryEnd)
                            (HF.boundSlot summaryStart)
          summaryEpochs = HF.countEpochs
                            (HF.boundEpoch summaryEnd)
                            (HF.boundEpoch summaryStart)

          summaryTimeSpan :: NominalDiffTime
          summaryTimeSpan = diffUTCTime
                              (HF.boundTime summaryEnd)
                              (HF.boundTime summaryStart)

          summaryTimeSpanSeconds :: Double
          summaryTimeSpanSeconds = realToFrac summaryTimeSpan

      -- Pick arbitrary values before the horizon

      beforeHorizonSlots   <- choose (0, summarySlots  - 1)
      beforeHorizonEpochs  <- choose (0, summaryEpochs - 1)
      beforeHorizonSeconds <- choose (0, summaryTimeSpanSeconds)
                                `suchThat` \x -> x /= summaryTimeSpanSeconds

      let beforeHorizonSlot  :: SlotNo
          beforeHorizonEpoch :: EpochNo
          beforeHorizonTime  :: UTCTime

          beforeHorizonSlot  = HF.addSlots
                                 beforeHorizonSlots
                                 (HF.boundSlot summaryStart)
          beforeHorizonEpoch = HF.addEpochs
                                 beforeHorizonEpochs
                                 (HF.boundEpoch summaryStart)
          beforeHorizonTime  = addUTCTime
                                 (realToFrac beforeHorizonSeconds)
                                 (HF.boundTime summaryStart)

      -- Pick arbitrary values past the horizon

      pastHorizonSlots   :: Word64 <- choose (0, 10)
      pastHorizonEpochs  :: Word64 <- choose (0, 10)
      pastHorizonSeconds :: Double <- choose (0, 10)

      let pastHorizonSlot  :: SlotNo
          pastHorizonEpoch :: EpochNo
          pastHorizonTime  :: UTCTime

          pastHorizonSlot  = HF.addSlots
                                pastHorizonSlots
                                (HF.boundSlot summaryEnd)
          pastHorizonEpoch = HF.addEpochs
                                pastHorizonEpochs
                                (HF.boundEpoch summaryEnd)
          pastHorizonTime  = addUTCTime
                                (realToFrac pastHorizonSeconds)
                                (HF.boundTime summaryEnd)

      return ArbitrarySummary{
            arbitrarySummaryStart = start
          , arbitrarySummary      = summary
          , beforeHorizonTime
          , beforeHorizonSlot
          , beforeHorizonEpoch
          , pastHorizonTime
          , pastHorizonSlot
          , pastHorizonEpoch
          }
    where
      genEraSummary :: Era -> HF.Bound -> Gen (HF.EraSummary, HF.Bound)
      genEraSummary _era lo = do
          params <- genEraParams (HF.boundEpoch lo)
          hi     <- genUpperBound lo params
          return (HF.EraSummary lo hi params, hi)

      genUpperBound :: HF.Bound -> HF.EraParams -> Gen HF.Bound
      genUpperBound lo params = do
          startOfNextEra <- genStartOfNextEra (HF.boundEpoch lo) params
          return $ HF.mkUpperBound params lo startOfNextEra

  shrink summary@ArbitrarySummary{..} = concat [
        -- Simplify the system start
        [ resetArbitrarySummaryStart summary
        | HF.getSystemStart arbitrarySummaryStart /= dawnOfTime
        ]

        -- Reduce before-horizon slot
      , [ summary { beforeHorizonSlot = SlotNo s }
        | s <- shrink (unSlotNo beforeHorizonSlot)
        ]

        -- Reduce before-horizon epoch
      , [ summary { beforeHorizonEpoch = EpochNo e }
        | e <- shrink (unEpochNo beforeHorizonEpoch)
        ]

        -- Reduce before-horizon time
      , [ summary { beforeHorizonTime = t }
        | t <- shrink beforeHorizonTime
        , t >= HF.getSystemStart arbitrarySummaryStart
        ]

        -- Drop an era /provided/ this doesn't cause of any of the before
        -- horizon values to become past horizon
      , [ ArbitrarySummary { arbitrarySummary = summary', .. }
        | Just (summary', lastEra) <- [summaryInit arbitrarySummary]
        , beforeHorizonSlot  < HF.boundSlot  (HF.eraStart lastEra)
        , beforeHorizonEpoch < HF.boundEpoch (HF.eraStart lastEra)
        , beforeHorizonTime  < HF.boundTime  (HF.eraStart lastEra)
        ]
      ]

{-------------------------------------------------------------------------------
  Shifting time
-------------------------------------------------------------------------------}

instance ShiftTime ArbitrarySummary where
  shiftTime delta ArbitrarySummary{..} = ArbitrarySummary {
      arbitrarySummaryStart = shiftTime delta arbitrarySummaryStart
    , arbitrarySummary      = shiftTime delta arbitrarySummary
    , beforeHorizonTime     = shiftTime delta beforeHorizonTime
    , beforeHorizonSlot
    , beforeHorizonEpoch
    , pastHorizonTime
    , pastHorizonSlot
    , pastHorizonEpoch
    }

-- | Reset the system start (during shrinking)
--
-- Since this brings the system start /back/, we don't care about the past
-- horizon values. 'EpochNo' and 'SlotNo' are also unaffected.
resetArbitrarySummaryStart :: ArbitrarySummary -> ArbitrarySummary
resetArbitrarySummaryStart summary@ArbitrarySummary{..} =
    shiftTime (negate ahead) summary
  where
    ahead :: NominalDiffTime
    ahead = diffUTCTime (HF.getSystemStart arbitrarySummaryStart) dawnOfTime

{-------------------------------------------------------------------------------
  Additional functions on 'Summary' needed for the tests only
-------------------------------------------------------------------------------}

-- | Lift 'atMostInit' to 'Summary'
summaryInit :: HF.Summary xs -> Maybe (HF.Summary xs, HF.EraSummary)
summaryInit (HF.Summary xs) = first HF.Summary <$> atMostInit xs

-- | Outer bounds of the summary
--
-- We must always have at least one era but the 'Summary' type does not tell
-- us that. This function is therefore partial, but used only in the tests.
summaryBounds :: HF.Summary xs -> (HF.Bound, HF.Bound)
summaryBounds (HF.Summary summary) =
    case toList summary of
      [] -> error "summaryBounds: no eras"
      ss -> (HF.eraStart (head ss), HF.eraEnd (last ss))
