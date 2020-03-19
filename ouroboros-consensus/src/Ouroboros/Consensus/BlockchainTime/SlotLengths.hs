{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DerivingVia         #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Ouroboros.Consensus.BlockchainTime.SlotLengths (
    -- * Composable slot lengths
    SlotLengths(..)
  , SegmentLength(..)
  , slotLengthsFromList
  , slotLengthsToList
  , singletonSlotLengths
  , introduceHardFork
  , tickSlotLengths
    -- * Focused slot lengths and supported conversions
  , FocusedSlotLengths(..)
  , focusSlotLengths
  , focusAtSystemStart
  , focusAtChainTip
    -- * Conversions requiring focus
  , slotToDiffTime
  , slotFromDiffTime
    -- * Wrappers using absolute system start time
  , SystemStart(..)
  , slotToUTCTime
  , slotFromUTCTime
  , delayUntilNextSlot
    -- * Low-level API (exported for the benefit of tests)
  , SegmentStart(..)
  , segmentDistance
  , refocusAtSlot
  , defocusSlotLengths
  -- * The specified time must be greater than or equal to the start of the slot
  , refocusAtTime
  ) where

import           Data.Bifunctor
import           Data.Fixed
import           Data.Time
import           Data.Word

import           Cardano.Prelude (NoUnexpectedThunks, OnlyCheckIsWHNF (..),
                     UseIsNormalForm (..))

import           Ouroboros.Network.Block (SlotNo (..))

import           Ouroboros.Consensus.BlockchainTime.SlotLength

{-------------------------------------------------------------------------------
  Composable slot lengths
-------------------------------------------------------------------------------}

-- | Slot lengths of all chain segments, from old to new
--
-- Hard forks, by their very nature, are typically not anticipated. So the
-- evolution of a blockchain looks something like this. Initially, chain A is
-- running, with no hard fork planned
--
-- > +----------------------------
-- > | A (indefinite length) ...
-- > +----------------------------
--
-- At some point X the realization kicks in that a hard fork will be needed,
-- which gets scheduled for some point Y.
--
-- >                            X     Y
-- > +--------------------------------+------------
-- > | A (now of bounded length)      | B ...
-- > +--------------------------------+------------
--
-- Now chain B runs for an indefinite amount of time, until the cycle repeats
-- for some new points X', Y'.
--
-- >                            X     Y                   X'   Y'
-- > +--------------------------------+------------------------+----------
-- > | A (bounded length)             | B (bounded length)     | C ...
-- > +--------------------------------+------------------------+----------
--
-- Thus, we always have a number of chain segments of finite length, followed by
-- a final segment of indefinite length. For each segment we record a separate
-- slot length.
--
-- 'SlotLengths' is composable (see 'introduceHardFork'), but does not
-- support any (efficient) computation; see 'FocusedSlotLengths'.
data SlotLengths = SlotLengths {
      -- | Slot length in current segment
      currentSlotLength :: SlotLength

      -- | Segment length and future slot lengths (if hard fork planned)
    , nextSlotLengths   :: Maybe (SegmentLength, SlotLengths)
    }
  deriving (Show, Eq)
  deriving NoUnexpectedThunks via OnlyCheckIsWHNF "SlotLengths" SlotLengths

-- | Length of a segment of the blockchain (in terms of number of slots)
--
-- See also 'SlotLengths'.
newtype SegmentLength = SegmentLength Word64
  deriving (Show, Eq)

-- | Construct 'SlotLengths'
slotLengthsFromList :: [(SegmentLength, SlotLength)]
                    -> SlotLength  -- ^ Slot length in final segment
                    -> SlotLengths
slotLengthsFromList = go
  where
    go :: [(SegmentLength, SlotLength)] -> SlotLength -> SlotLengths
    go []          final = SlotLengths final Nothing
    go ((n, l):ls) final = SlotLengths l $ Just (n, go ls final)

-- | 'SlotLengths' for a single 'SlotLength' of indefinite length
singletonSlotLengths :: SlotLength -> SlotLengths
singletonSlotLengths = slotLengthsFromList []

-- | Convert 'SlotLengths' to list
--
-- Primarily for testing.
slotLengthsToList :: SlotLengths -> ([(SegmentLength, SlotLength)], SlotLength)
slotLengthsToList SlotLengths{..} =
    case nextSlotLengths of
      Nothing      -> ([], currentSlotLength)
      Just (n, ls) -> let (ls', final) = slotLengthsToList ls
                      in ((n, currentSlotLength) : ls', final)

-- | Introduce a new hard fork
--
-- When we introduce a new hard fork, the segment that was previously the final
-- segment must now be given a length (it had been of indefinite length).
introduceHardFork :: SlotLengths    -- ^ Existing slot lengths
                  -> SegmentLength  -- ^ Length of the final segment
                  -> SlotLengths    -- ^ Slot lengths after the new hard fork
                  -> SlotLengths
introduceHardFork xs n next = go xs
  where
    go :: SlotLengths -> SlotLengths
    go (SlotLengths l mNext) =
        case mNext of
          Just (n', next') -> SlotLengths l (Just (n' , go next'))
          Nothing          -> SlotLengths l (Just (n  ,    next ))

-- | Current slot length, and 'SlotLengths' after waiting one slot
tickSlotLengths :: SlotLengths -> (SlotLength, SlotLengths)
tickSlotLengths SlotLengths{..} = (
      currentSlotLength
    , case nextSlotLengths of
        Nothing ->
          SlotLengths currentSlotLength Nothing
        Just (SegmentLength 1, next) ->
          next
        Just (SegmentLength n, next) ->
          SlotLengths currentSlotLength (Just (SegmentLength (n - 1), next))
    )

{-------------------------------------------------------------------------------
  Segments
-------------------------------------------------------------------------------}

-- | Information about the start of a segment
data SegmentStart = SegmentStart {
      -- | Slot number of the first slot in this segment
      segmentStartSlot :: SlotNo

      -- | Starting time (as offset from system start) of this segment
    , segmentStartTime :: NominalDiffTime
    }

-- | Start of the first segment
firstSegmentStart :: SegmentStart
firstSegmentStart = SegmentStart {
      segmentStartSlot = SlotNo 0
    , segmentStartTime = 0
    }

-- | Compute start of the next segment
nextSegmentStart :: SegmentLength -> SlotLength -> SegmentStart -> SegmentStart
nextSegmentStart (SegmentLength n) (SlotLength l) SegmentStart{..} =
    SegmentStart {
        segmentStartSlot = segmentStartSlot +
                             (fromIntegral n :: SlotNo)
      , segmentStartTime = segmentStartTime +
                             (fromIntegral n :: NominalDiffTime) * l
      }

-- | Distance between two segments (in terms of number of slots)
segmentDistance :: SegmentStart -> SegmentStart -> Word64
segmentDistance = \a b ->
    if segmentStartSlot b > segmentStartSlot a
      then distanceTo (segmentStartSlot a) (segmentStartSlot b)
      else distanceTo (segmentStartSlot b) (segmentStartSlot a)
  where
    distanceTo :: SlotNo -> SlotNo -> Word64
    distanceTo (SlotNo a) (SlotNo b) = b - a

{-------------------------------------------------------------------------------
  Slot length (that support efficient computation)
-------------------------------------------------------------------------------}

-- | Slot lengths across all chain segments, supporting efficient conversions
--
-- Unlike 'SlotLengths', which always starts at the beginning of time,
-- 'FocusedSlotLengths' can freely be focused at any segment. As long as the
-- focus is maintained near where it should be, this means conversions will
-- always be efficient (do not need to traverse the entire chain history).
data FocusedSlotLengths = FocusedSlotLengths {
      -- | Slot length of the segment in focus
      focusedSlotLength :: SlotLength

      -- | Start of this segment
    , focusedStart      :: SegmentStart

      -- | Previous slot length (if any)
    , focusedPrev       :: Maybe FocusedSlotLengths

      -- | Next slot length
    , focusedNext       :: Maybe FocusedSlotLengths
    }
  deriving NoUnexpectedThunks via OnlyCheckIsWHNF "FocusedSlotLengths" FocusedSlotLengths

-- | Convert 'SlotLengths' to 'FocusedSlotLengths'
--
-- The initial focus will be on the start of the chain.
focusSlotLengths :: SlotLengths -> FocusedSlotLengths
focusSlotLengths = go Nothing firstSegmentStart
  where
    go :: Maybe FocusedSlotLengths -- Previous
       -> SegmentStart             -- Start of current segment
       -> SlotLengths
       -> FocusedSlotLengths
    go prev now SlotLengths{..} =
        let this = FocusedSlotLengths {
                focusedSlotLength = currentSlotLength
              , focusedStart      = now
              , focusedPrev       = prev
              , focusedNext       =
                  case nextSlotLengths of
                    Nothing        -> Nothing
                    Just (n, next) ->
                      let now' = nextSegmentStart n currentSlotLength now
                      in Just (go (Just this) now' next)
              }
        in this

-- | Move focus back to the start of time
focusAtSystemStart :: FocusedSlotLengths -> FocusedSlotLengths
focusAtSystemStart this =
    case focusedPrev this of
      Nothing   -> this
      Just prev -> focusAtSystemStart prev

-- | Move the focus to the tip of the chain
focusAtChainTip :: FocusedSlotLengths -> FocusedSlotLengths
focusAtChainTip this =
    case focusedNext this of
      Nothing   -> this
      Just next -> focusAtChainTip next

-- | Convert 'FocusedSlotLengths' back to 'SlotLengths'
--
-- Primarily for testing.
defocusSlotLengths :: FocusedSlotLengths -> SlotLengths
defocusSlotLengths = go . focusAtSystemStart
  where
    go :: FocusedSlotLengths -> SlotLengths
    go this = SlotLengths{
          currentSlotLength = focusedSlotLength this
        , nextSlotLengths   =
            case focusedNext this of
              Nothing   -> Nothing
              Just next -> Just ( SegmentLength $
                                    segmentDistance (focusedStart this)
                                                    (focusedStart next)
                                , go next
                                )
        }

{-------------------------------------------------------------------------------
  (Primarily internal:) Refocusing
-------------------------------------------------------------------------------}

data Direction a = Prev | Next | Here a

refocus :: forall a.
           (SegmentStart -> Maybe SegmentStart -> Direction a)
        -> FocusedSlotLengths
        -> (FocusedSlotLengths, a)
refocus f = go
  where
    go :: FocusedSlotLengths -> (FocusedSlotLengths, a)
    go ls =
        case ( f (focusedStart ls) (focusedStart <$> focusedNext ls)
             , focusedPrev ls
             , focusedNext ls
             ) of
          (Here a, _, _)       -> (ls, a)
          (Prev, Just prev, _) -> go prev
          (Prev, Nothing  , _) -> error "refocus: before start"
          (Next, _, Just next) -> go next
          (Next, _, Nothing)   -> error "refocus: after end"

-- | Refocus so that the given slot lies within the focused segment
--
-- Additionally returns the relative offset of the slot within the segment.
refocusAtSlot :: SlotNo
              -> FocusedSlotLengths
              -> (FocusedSlotLengths, Word64)
refocusAtSlot absSlot = refocus contains
  where
    contains :: SegmentStart -> Maybe SegmentStart -> Direction Word64
    contains this Nothing
      | absSlot <  segmentStartSlot this = Prev
      | otherwise                        = Here (relSlot this)
    contains this (Just next)
      | absSlot <  segmentStartSlot this = Prev
      | absSlot >= segmentStartSlot next = Next
      | otherwise                        = Here (relSlot this)

    relSlot :: SegmentStart -> Word64
    relSlot this = unSlotNo (absSlot - segmentStartSlot this)

-- | Refocus so that the given time lies within the focused segment
--
-- Additionally returns the relative time offset within the segment.
refocusAtTime :: NominalDiffTime
              -> FocusedSlotLengths
              -> (FocusedSlotLengths, NominalDiffTime)
refocusAtTime absTime = refocus contains
  where
    contains :: SegmentStart -> Maybe SegmentStart -> Direction NominalDiffTime
    contains this Nothing
      | absTime <  segmentStartTime this = Prev
      | otherwise                        = Here (relTime this)
    contains this (Just next)
      | absTime <  segmentStartTime this = Prev
      | absTime >= segmentStartTime next = Next
      | otherwise                        = Here (relTime this)

    relTime :: SegmentStart -> NominalDiffTime
    relTime this = absTime - segmentStartTime this

{-------------------------------------------------------------------------------
  Conversions
-------------------------------------------------------------------------------}

-- | Convert slot number to time offset (since system start)
--
-- Returns the slot lengths, in case they had to be refocused. This return value
-- can safely be ignored, but when doing many conversions of adjacent slot
-- numbers, keeping the slot lengths focused can help efficiency.
--
-- Postcondition: converting the time back to a slot ('slotFromDiffTime')
-- will result in the same slot and a time within the slot of exactly 0.
slotToDiffTime :: SlotNo
               -> FocusedSlotLengths
               -> (FocusedSlotLengths, NominalDiffTime)
slotToDiffTime absSlot = modifySnd go . refocusAtSlot absSlot
  where
    go :: FocusedSlotLengths -> Word64 -> NominalDiffTime
    go ls relSlot = segmentStartTime (focusedStart ls)
                  + fromIntegral relSlot * getSlotLength (focusedSlotLength ls)

-- | Convert time (in terms of absolute offset from system start) to slot number
--
-- Also returns how far we are into the slot (relative time since slot start)
--
-- Postconditions:
--
-- * Time within current slot will be strictly less than the slot length.
-- * The argument time will be greater than or equal to the start of the slot
-- * The argument time will be strictly less than the start of the next slot
slotFromDiffTime :: NominalDiffTime
                 -> FocusedSlotLengths
                 -> (FocusedSlotLengths, (SlotNo, NominalDiffTime))
slotFromDiffTime absTime = modifySnd go . refocusAtTime absTime
  where
    go :: FocusedSlotLengths -> NominalDiffTime -> (SlotNo, NominalDiffTime)
    go ls relTime = first (toAbsSlot ls) $
                      relTime `divMod'` getSlotLength (focusedSlotLength ls)

    toAbsSlot :: FocusedSlotLengths -> Word64 -> SlotNo
    toAbsSlot ls relSlot = segmentStartSlot (focusedStart ls) + SlotNo relSlot

{-------------------------------------------------------------------------------
  Wrappers using absolute system start time
-------------------------------------------------------------------------------}

-- | System start
--
-- Slots are counted from the system start.
newtype SystemStart = SystemStart { getSystemStart :: UTCTime }
  deriving (Eq, Show)
  deriving NoUnexpectedThunks via UseIsNormalForm SystemStart

-- | Wrapper around 'slotToDiffTime' that takes system start into account
slotToUTCTime :: SystemStart
              -> SlotNo
              -> FocusedSlotLengths
              -> (FocusedSlotLengths, UTCTime)
slotToUTCTime (SystemStart start) slot =
    second (`addUTCTime` start) . slotToDiffTime slot

-- | Wrapper around 'slotFromDiffTime' that takes system start into account
slotFromUTCTime :: SystemStart
                -> UTCTime
                -> FocusedSlotLengths
                -> (FocusedSlotLengths, (SlotNo, NominalDiffTime))
slotFromUTCTime (SystemStart start) =
    slotFromDiffTime . (`diffUTCTime` start)

-- | Compute time until the next slot and the number of that next slot
--
-- Postconditions:
--
-- * Delay will be less than the slot length and strictly greater than 0
-- * At the start of slot @n@, the delay will the maximum delay (slot length)
-- * After the delay the system will be at the start of the next slot
delayUntilNextSlot :: SystemStart
                   -> UTCTime
                   -> FocusedSlotLengths
                   -> (FocusedSlotLengths, (NominalDiffTime, SlotNo))
delayUntilNextSlot start now =
    modifySnd go . slotFromUTCTime start now
  where
    go :: FocusedSlotLengths
       -> (SlotNo, NominalDiffTime) -- /This/ slot, and time spent in this slot
       -> (NominalDiffTime, SlotNo) -- Time until /next/ slot, and next slot
    go ls (this, timeSpent) = (slotLength - timeSpent, succ this)
      where
        slotLength = getSlotLength (focusedSlotLength ls)

{-------------------------------------------------------------------------------
  Auxiliary
-------------------------------------------------------------------------------}

-- | Modify the second component of a pair, using the first as context
modifySnd :: (a -> b -> c) -> (a, b) -> (a, c)
modifySnd f (a, b) = (a, f a b)
