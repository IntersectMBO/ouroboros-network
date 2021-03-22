{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE NamedFieldPuns            #-}
{-# LANGUAGE NumericUnderscores        #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE StandaloneDeriving        #-}

module Test.Consensus.HardFork.Summary (
    tests
  ) where

import           Data.Time
import           Data.Word

import           Test.QuickCheck
import           Test.Tasty
import           Test.Tasty.QuickCheck

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.BlockchainTime
import qualified Ouroboros.Consensus.HardFork.History as HF

import           Test.Util.Orphans.Arbitrary ()
import           Test.Util.QuickCheck

import           Test.Consensus.HardFork.Infra

tests :: TestTree
tests = testGroup "Summary" [
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

{-------------------------------------------------------------------------------
  Dealing with the 'PastHorizonException'
-------------------------------------------------------------------------------}

noPastHorizonException :: ArbitrarySummary
                       -> HF.Qry Property
                       -> Property
noPastHorizonException ArbitrarySummary{..} p =
    case HF.runQuery p arbitrarySummary of
      Right prop -> prop
      Left  ex   -> counterexample ("Unexpected " ++ show ex) $
                      property False

isPastHorizonException :: Show a
                       => ArbitrarySummary
                       -> HF.Qry a
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
      (slot , inSlot, timeLeft) <- HF.wallclockToSlot time
      (time', slotLen) <- HF.slotToWallclock slot
      return $ conjoin [
           addRelTime inSlot time' === time
         , inSlot + timeLeft       === getSlotLength slotLen
         ]

roundtripSlotWallclock :: ArbitrarySummary -> Property
roundtripSlotWallclock s@ArbitrarySummary{beforeHorizonSlot = slot} =
    noPastHorizonException s $ do
      (time , slotLen)          <- HF.slotToWallclock slot
      (slot', inSlot, timeLeft) <- HF.wallclockToSlot time
      return $ conjoin [
          slot'             === slot
        , inSlot            === 0
        , inSlot + timeLeft === getSlotLength slotLen
        ]

roundtripSlotEpoch :: ArbitrarySummary -> Property
roundtripSlotEpoch s@ArbitrarySummary{beforeHorizonSlot = slot} =
    noPastHorizonException s $ do
      (epoch , inEpoch, slotsLeft) <- HF.slotToEpoch slot
      (slot' , epochSize)          <- HF.epochToSlot epoch
      return $ conjoin [
          HF.addSlots inEpoch slot' === slot
        , inEpoch + slotsLeft       === unEpochSize epochSize
        ]

roundtripEpochSlot :: ArbitrarySummary -> Property
roundtripEpochSlot s@ArbitrarySummary{beforeHorizonEpoch = epoch} =
    noPastHorizonException s $ do
      (slot  , epochSize)          <- HF.epochToSlot epoch
      (epoch', inEpoch, slotsLeft) <- HF.slotToEpoch slot
      return $ conjoin [
          epoch'              === epoch
        , inEpoch             === 0
        , inEpoch + slotsLeft === unEpochSize epochSize
        ]

reportsPastHorizon :: ArbitrarySummary -> Property
reportsPastHorizon s@ArbitrarySummary{..} = conjoin [
      case mPastHorizonTime of
        Just x  -> isPastHorizonException s $ HF.wallclockToSlot x
        Nothing -> property True
    , case mPastHorizonSlot of
        Just x  -> isPastHorizonException s $ HF.slotToWallclock x
        Nothing -> property True
    , case mPastHorizonSlot of
        Just x  -> isPastHorizonException s $ HF.slotToEpoch     x
        Nothing -> property True
    , case mPastHorizonEpoch of
        Just x  -> isPastHorizonException s $ HF.epochToSlot     x
        Nothing -> property True
    ]

{-------------------------------------------------------------------------------
  Arbitrary 'Summary'

  We should be able to show properties of the conversion functions independent
  of how the 'Summary' that they use is derived.
-------------------------------------------------------------------------------}

data ArbitrarySummary = forall xs. ArbitrarySummary {
      arbitrarySummary   :: HF.Summary xs
    , beforeHorizonTime  :: RelativeTime
    , beforeHorizonSlot  :: SlotNo
    , beforeHorizonEpoch :: EpochNo
    , mPastHorizonTime   :: Maybe RelativeTime
    , mPastHorizonSlot   :: Maybe SlotNo
    , mPastHorizonEpoch  :: Maybe EpochNo
    }

deriving instance Show ArbitrarySummary

instance Arbitrary ArbitrarySummary where
  arbitrary = chooseEras $ \is@(Eras _) -> do
      summary <- genSummary is

      let summaryStart :: HF.Bound
          mSummaryEnd  :: HF.EraEnd
          (summaryStart, mSummaryEnd) = HF.summaryBounds summary

      case mSummaryEnd of
        HF.EraUnbounded -> do
          -- Don't pick /too/ large numbers to avoid overflow
          beforeHorizonSlots   <- choose (0,   100_000_000)
          beforeHorizonEpochs  <- choose (0,     1_000_000)
          beforeHorizonSeconds <- choose (0, 1_000_000_000)

          let beforeHorizonSlot  :: SlotNo
              beforeHorizonEpoch :: EpochNo
              beforeHorizonTime  :: RelativeTime

              beforeHorizonSlot  = HF.addSlots
                                     beforeHorizonSlots
                                     (HF.boundSlot summaryStart)
              beforeHorizonEpoch = HF.addEpochs
                                     beforeHorizonEpochs
                                     (HF.boundEpoch summaryStart)
              beforeHorizonTime  = addRelTime
                                     (realToFrac (beforeHorizonSeconds :: Double))
                                     (HF.boundTime summaryStart)

          return ArbitrarySummary{
                arbitrarySummary      = summary
              , beforeHorizonTime
              , beforeHorizonSlot
              , beforeHorizonEpoch
              , mPastHorizonTime      = Nothing
              , mPastHorizonSlot      = Nothing
              , mPastHorizonEpoch     = Nothing
              }

        HF.EraEnd summaryEnd -> do
          let summarySlots, summaryEpochs :: Word64
              summarySlots  = HF.countSlots
                                (HF.boundSlot summaryEnd)
                                (HF.boundSlot summaryStart)
              summaryEpochs = HF.countEpochs
                                (HF.boundEpoch summaryEnd)
                                (HF.boundEpoch summaryStart)

              summaryTimeSpan :: NominalDiffTime
              summaryTimeSpan = diffRelTime
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
              beforeHorizonTime  :: RelativeTime

              beforeHorizonSlot  = HF.addSlots
                                     beforeHorizonSlots
                                     (HF.boundSlot summaryStart)
              beforeHorizonEpoch = HF.addEpochs
                                     beforeHorizonEpochs
                                     (HF.boundEpoch summaryStart)
              beforeHorizonTime  = addRelTime
                                     (realToFrac beforeHorizonSeconds)
                                     (HF.boundTime summaryStart)

          -- Pick arbitrary values past the horizon

          pastHorizonSlots   :: Word64 <- choose (0, 10)
          pastHorizonEpochs  :: Word64 <- choose (0, 10)
          pastHorizonSeconds :: Double <- choose (0, 10)

          let pastHorizonSlot  :: SlotNo
              pastHorizonEpoch :: EpochNo
              pastHorizonTime  :: RelativeTime

              pastHorizonSlot  = HF.addSlots
                                    pastHorizonSlots
                                    (HF.boundSlot summaryEnd)
              pastHorizonEpoch = HF.addEpochs
                                    pastHorizonEpochs
                                    (HF.boundEpoch summaryEnd)
              pastHorizonTime  = addRelTime
                                    (realToFrac pastHorizonSeconds)
                                    (HF.boundTime summaryEnd)

          return ArbitrarySummary{
                arbitrarySummary      = summary
              , beforeHorizonTime
              , beforeHorizonSlot
              , beforeHorizonEpoch
              , mPastHorizonTime      = Just pastHorizonTime
              , mPastHorizonSlot      = Just pastHorizonSlot
              , mPastHorizonEpoch     = Just pastHorizonEpoch
              }

  shrink summary@ArbitrarySummary{..} = concat [
        -- Reduce before-horizon slot
        [ summary { beforeHorizonSlot = SlotNo s }
        | s <- shrink (unSlotNo beforeHorizonSlot)
        ]

        -- Reduce before-horizon epoch
      , [ summary { beforeHorizonEpoch = EpochNo e }
        | e <- shrink (unEpochNo beforeHorizonEpoch)
        ]

        -- Reduce before-horizon time
      , [ summary { beforeHorizonTime = RelativeTime t }
        | t <- shrink (getRelativeTime beforeHorizonTime)
        , t >= 0
        ]

        -- Drop an era /provided/ this doesn't cause of any of the before
        -- horizon values to become past horizon
      , [ ArbitrarySummary { arbitrarySummary = summary', .. }
        | (Just summary', lastEra) <- [HF.summaryInit arbitrarySummary]
        , beforeHorizonSlot  < HF.boundSlot  (HF.eraStart lastEra)
        , beforeHorizonEpoch < HF.boundEpoch (HF.eraStart lastEra)
        , beforeHorizonTime  < HF.boundTime  (HF.eraStart lastEra)
        ]
      ]
