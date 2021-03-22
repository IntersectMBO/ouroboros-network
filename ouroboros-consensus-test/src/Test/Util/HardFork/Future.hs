{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE DerivingVia    #-}
{-# LANGUAGE LambdaCase     #-}
{-# LANGUAGE NamedFieldPuns #-}

module Test.Util.HardFork.Future (
    EraSize (..)
  , Future (..)
  , futureEpochInFirstEra
  , futureFirstEpochSize
  , futureFirstSlotLength
  , futureSlotLengths
  , futureSlotToEpoch
  , futureSlotToTime
  , futureTimeToSlot
  , singleEraFuture
  ) where

import qualified Data.Fixed
import           Data.Time (NominalDiffTime)
import           Data.Word (Word64)
import           GHC.Generics (Generic)
import           Quiet (Quiet (..))

import           Ouroboros.Consensus.Block
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
futureTimeToSlot = \future d -> go 0 d future
  where
    done acc d slotLength =
        (SlotNo $ acc + n, getSlotLength slotLength - timeInSlot, slotLength)
      where
        n          = divide d slotLength
        timeInSlot = d - multiply n slotLength

    go acc d (EraFinal slotLength _epochSize) =
        done acc d slotLength
    go acc d (EraCons slotLength epochSize eraSize future) =
        case d `safeSub` eraLength of
          Nothing -> done acc d slotLength
          Just d' -> go (acc + eraSlots) d' future
      where
        NumSlots eraSlots = calcEraSlots epochSize eraSize
        eraLength         = multiply eraSlots slotLength

-- | Which epoch the slot is in
futureSlotToEpoch :: Future
                  -> SlotNo
                  -> EpochNo
futureSlotToEpoch = \future (SlotNo s) -> EpochNo $ go 0 s future
  where
    go acc s = \case
      EraFinal _slotLength (EpochSize epSz)                ->
          acc + s `div` epSz
      EraCons  slotLength  epochSize        eraSize future ->
          case s `safeSub` eraSlots of
            Nothing -> go acc s (EraFinal slotLength epochSize)
            Just s' -> go (acc + n) s' future
        where
          EraSize n = eraSize
          NumSlots eraSlots = calcEraSlots epochSize eraSize

-- | When the slot begins
futureSlotToTime :: Future
                 -> SlotNo
                 -> NominalDiffTime
futureSlotToTime = \future (SlotNo s) -> go 0 s future
  where
    done acc s slotLength =
        acc + multiply s slotLength

    go acc s = \case
      EraFinal slotLength _epochSize                ->
          done acc s slotLength
      EraCons  slotLength epochSize  eraSize future ->
          case s `safeSub` eraSlots of
            Nothing -> done acc s slotLength
            Just s' -> go (acc + eraLength) s' future
        where
          NumSlots eraSlots = calcEraSlots epochSize eraSize
          eraLength         = multiply eraSlots slotLength

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
divide d slotLength = Data.Fixed.div' d (getSlotLength slotLength)

multiply :: Word64 -> SlotLength -> NominalDiffTime
multiply m sl = fromIntegral m * getSlotLength sl

-- | Find the non-negative difference
safeSub :: (Num a, Ord a) => a -> a -> Maybe a
safeSub x y = if x < y then Nothing else Just (x - y)

calcEraSlots :: EpochSize -> EraSize -> NumSlots
calcEraSlots (EpochSize slotPerEpoch) (EraSize epochPerEra) =
    NumSlots (slotPerEpoch * epochPerEra)
