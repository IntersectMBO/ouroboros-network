
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Test.Consensus.BlockchainTime.SlotLengths (tests) where

import           Data.Maybe
import           Data.Time
import           Test.QuickCheck
import           Test.Tasty
import           Test.Tasty.QuickCheck

import           Ouroboros.Network.Block (SlotNo (..))

import           Ouroboros.Consensus.BlockchainTime

import           Test.Util.Orphans.Arbitrary (SmallDiffTime (..))
import           Test.Util.QuickCheck

tests :: TestTree
tests = testGroup "SlotLengths" [
      testGroup "Sanity" [
          testProperty "toFromList"       prop_toFromList
        , testProperty "focusDefocus"     prop_focusDefocus
        , testProperty "focusTipDefocus"  prop_focusTipDefocus
        ]
    , testGroup "Refocus" [
          testProperty "refocusToSlot"    prop_refocusToSlot
        , testProperty "refocusToTime"    prop_refocusToTime
        ]
    , testGroup "Conversions" [
          testProperty "slotToDiffTime"   prop_slotToDiffTime
        , testProperty "slotFromDiffTime" prop_slotFromDiffTime
        ]
    , testGroup "DelayUntilNextSlot" [
          testProperty "delayBounds"      prop_delayBounds
        , testProperty "delayMax"         prop_delayMax
        , testProperty "delayEffective"   prop_delayEffective
        ]
    ]

{-------------------------------------------------------------------------------
  Sanity check: convert back and forth
-------------------------------------------------------------------------------}

prop_toFromList :: SlotLengths -> Property
prop_toFromList ls =
    uncurry slotLengthsFromList (slotLengthsToList ls) === ls

prop_focusDefocus :: SlotLengths -> Property
prop_focusDefocus ls =
    defocusSlotLengths (focusSlotLengths ls) === ls

prop_focusTipDefocus :: SlotLengths -> Property
prop_focusTipDefocus ls =
    defocusSlotLengths (focusAtChainTip (focusSlotLengths ls)) === ls

{-------------------------------------------------------------------------------
  Focusing
-------------------------------------------------------------------------------}

prop_refocusToSlot :: SlotLengths -> SlotNo -> Property
prop_refocusToSlot ls absSlot =
    classify (isJust (focusedNext this)) "not last segment" $
    conjoin [
        absSlot `ge` startThis
      , case focusedNext this of
          Nothing   -> property True
          Just next ->
            let startNext = segmentStartSlot (focusedStart next)
                distance  = unSlotNo (startNext - startThis)
            in conjoin [
                   absSlot `lt` startNext
                 , relSlot `lt` distance
                 ]
      ]
  where
    focused         = focusSlotLengths ls
    startThis       = segmentStartSlot (focusedStart this)
    (this, relSlot) = refocusAtSlot absSlot focused

prop_refocusToTime :: SlotLengths -> SmallDiffTime -> Property
prop_refocusToTime ls (SmallDiffTime absTime) =
    classify (isJust (focusedNext this)) "not last segment" $
    conjoin [
        absTime `ge` segmentStartTime (focusedStart this)
      , case focusedNext this of
          Nothing   -> property True
          Just next ->
            let startNext = segmentStartTime (focusedStart next)
                distance  = startNext - startThis
            in conjoin [
                   absTime `lt` startNext
                 , relTime `lt` distance
                 ]
      ]
  where
    focused         = focusSlotLengths ls
    (this, relTime) = refocusAtTime absTime focused
    startThis       = segmentStartTime (focusedStart this)

{-------------------------------------------------------------------------------
  Converting slots to time and back
-------------------------------------------------------------------------------}

prop_slotToDiffTime :: SlotLengths -> SlotNo -> Property
prop_slotToDiffTime ls absSlot =
    conjoin [
        absSlot === absSlot'
      , relTime === 0
      ]
  where
    focused                       = focusSlotLengths ls
    (_this, absTime)              = slotToDiffTime   absSlot focused
    (_this', (absSlot', relTime)) = slotFromDiffTime absTime focused

prop_slotFromDiffTime :: SlotLengths -> SmallDiffTime -> Property
prop_slotFromDiffTime ls (SmallDiffTime absTime) =
    conjoin [
        relTime `ge` 0
      , relTime `lt` slotLength
      , absTime `ge` startOfSlot
      , absTime `lt` startOfSlot + slotLength
      ]
  where
    focused                    = focusSlotLengths ls
    (this, (absSlot, relTime)) = slotFromDiffTime absTime focused
    (_this', startOfSlot)      = slotToDiffTime   absSlot focused
    slotLength                 = getSlotLength (focusedSlotLength this)

{-------------------------------------------------------------------------------
  Delay until next slot
-------------------------------------------------------------------------------}

prop_delayBounds :: SystemStart -> SlotLengths -> SmallDiffTime -> Property
prop_delayBounds (SystemStart start) ls (SmallDiffTime offset) =
    conjoin [
        0          `lt` delay
      , slotLength `ge` delay
      ]
  where
    focused                = focusSlotLengths ls
    now                    = addUTCTime offset start
    (this, (delay, _next)) = delayUntilNextSlot (SystemStart start) now focused
    slotLength             = getSlotLength (focusedSlotLength this)

prop_delayMax :: SystemStart -> SlotLengths -> SlotNo -> Property
prop_delayMax start ls slot =
    conjoin [
        delay === slotLength
      , next  === succ slot
      ]
  where
    focused                 = focusSlotLengths ls
    (this, now)             = slotToUTCTime start slot focused
    (_this', (delay, next)) = delayUntilNextSlot start now focused
    slotLength              = getSlotLength (focusedSlotLength this)

prop_delayEffective :: SystemStart -> SlotLengths -> SmallDiffTime -> Property
prop_delayEffective (SystemStart start) ls (SmallDiffTime offset) =
    conjoin [
        slotAfterDelay === succ slotBeforeDelay
      , timeInSlot     === 0
      ]
  where
    focused                 = focusSlotLengths ls
    now                     = addUTCTime offset start
    (_this, (delay, _next)) = delayUntilNextSlot (SystemStart start) now focused
    timeAfterDelay          = addUTCTime delay now

    (_this', (slotBeforeDelay, _timeInSlot)) =
        slotFromUTCTime (SystemStart start) now focused

    (_this'', (slotAfterDelay, timeInSlot)) =
        slotFromUTCTime (SystemStart start) timeAfterDelay focused
