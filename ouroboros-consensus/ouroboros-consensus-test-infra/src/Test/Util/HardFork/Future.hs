{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE DerivingVia    #-}
{-# LANGUAGE LambdaCase     #-}
{-# LANGUAGE NamedFieldPuns #-}

module Test.Util.HardFork.Future (
  EraSize (..),
  Future (..),
  futureEpochInFirstEra,
  futureFirstEpochSize,
  futureFirstSlotLength,
  futureSlotLengths,
  futureSlotToEpoch,
  futureTimeToSlot,
  singleEraFuture,
  ) where

import           Data.Time (NominalDiffTime)
import           Data.Word (Word64)
import           GHC.Generics (Generic)
import           Quiet (Quiet (..))

import           Cardano.Slotting.Slot (EpochNo (..), EpochSize (..),
                     SlotNo (..))

import           Ouroboros.Consensus.BlockchainTime (SlotLength (..))
import           Ouroboros.Consensus.Util (nTimes)

import           Test.Util.Slots (NumSlots (..))
import           Test.Util.Stream (Stream (..))

{-------------------------------------------------------------------------------
  Careful counts
-------------------------------------------------------------------------------}

-- | Number of epochs
newtype EraSize = EraSize {unEraSize :: Word64}
  deriving (Eq, Generic)
  deriving (Show) via (Quiet EraSize)

{-------------------------------------------------------------------------------
  A test's whole timeline
-------------------------------------------------------------------------------}

-- | Every era in the test
--
-- INVARIANT: every number is @> 0@
data Future =
      EraFinal SlotLength EpochSize
    | EraCons  SlotLength EpochSize EraSize Future
  deriving (Eq, Show)

-- | 'Future' with only one era
singleEraFuture :: SlotLength -> EpochSize -> Future
singleEraFuture = EraFinal

-- | 'SlotLength' of the first era
futureFirstSlotLength :: Future -> SlotLength
futureFirstSlotLength future = case future of
    EraCons  slotLength _epochSize _eraSize _future -> slotLength
    EraFinal slotLength _epochSize                  -> slotLength

-- | 'EpochSize' of the first era
futureFirstEpochSize :: Future -> EpochSize
futureFirstEpochSize future = case future of
    EraCons  _slotLength epochSize _eraSize _future -> epochSize
    EraFinal _slotLength epochSize                  -> epochSize

-- | A variant of 'Future' that records the length of an era by 'NumSlots'
-- instead of by @('EpochSize', 'EraSize')@
--
-- Used to clarify some function definitions
data SlotFuture =
      InEraFinal SlotLength
    | InEraCons  SlotLength NumSlots Future
      -- ^ INVARIANT: @'NumSlots' > 0@

-- | Length of each slot in the whole 'Future'
futureSlotLengths :: Future -> Stream SlotLength
futureSlotLengths = \case
    EraFinal slotLength _epochSize ->
        let x = slotLength :< x in x
    EraCons slotLength epochSize eraSize future ->
        nTimes (slotLength :<) eraSlots $
        futureSlotLengths future
      where
        NumSlots eraSlots = calcEraSlots epochSize eraSize

-- | @(slot, time left in slot, length of slot)@
futureTimeToSlot :: Future
                 -> NominalDiffTime
                 -> (SlotNo, NominalDiffTime, SlotLength)
futureTimeToSlot = \future d -> go2 0 d future
  where
    go acc d (InEraFinal slotLength) =
        (SlotNo $ acc + n, timeInSlot, slotLength)
      where
        n = divide d slotLength
        timeInSlot = d - multiply n slotLength
    go acc d (InEraCons slotLength (NumSlots leftovers) future) =
        case d `safeSub` remaining of
          Nothing -> go  acc               d  (InEraFinal slotLength)
          Just d' -> go2 (acc + leftovers) d' future
      where
        remaining = multiply leftovers slotLength

    go2 acc d (EraFinal slotLength _epochSize) =
        go acc d $ InEraFinal slotLength
    go2 acc d (EraCons slotLength epochSize eraSize future) =
        go acc d $ InEraCons slotLength (NumSlots eraSlots) future
      where
        NumSlots eraSlots = calcEraSlots epochSize eraSize

-- | Which epoch the slot is in
futureSlotToEpoch :: Future
                  -> SlotNo
                  -> EpochNo
futureSlotToEpoch = \future (SlotNo s) -> EpochNo $ go 0 s future
  where
    go acc s = \case
      EraFinal _slotLength (EpochSize epSz) ->
          acc + s `div` epSz
      EraCons _slotLength epochSize eraSize future ->
          case s `safeSub` eraSlots of
            Nothing -> go acc s (EraFinal _slotLength epochSize)
            Just s' -> go (acc + n) s' future
        where
          EraSize n = eraSize
          NumSlots eraSlots = calcEraSlots epochSize eraSize

-- | Whether the epoch is in the first era
futureEpochInFirstEra :: Future -> EpochNo -> Bool
futureEpochInFirstEra = \case
    EraCons _slotLength _epochSize (EraSize n) _future ->
        \(EpochNo e) -> e < n
    EraFinal{} -> const True

{-------------------------------------------------------------------------------
  Miscellany
-------------------------------------------------------------------------------}

divide :: NominalDiffTime -> SlotLength -> Word64
divide d slotLength = floor $ d / getSlotLength slotLength

multiply :: Word64 -> SlotLength -> NominalDiffTime
multiply m sl = fromIntegral m * getSlotLength sl

-- | Find the non-negative difference
safeSub :: (Num a, Ord a) => a -> a -> Maybe a
safeSub x y = if y > x then Nothing else Just (x - y)

calcEraSlots :: EpochSize -> EraSize -> NumSlots
calcEraSlots (EpochSize slotPerEpoch) (EraSize epochPerEra) =
    NumSlots (slotPerEpoch * epochPerEra)
