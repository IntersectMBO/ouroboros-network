{-# LANGUAGE GADTs           #-}
{-# LANGUAGE KindSignatures  #-}
{-# LANGUAGE RecordWildCards #-}

module Ouroboros.Consensus.HardFork.History.Qry (
    Qry(..)
  , runQuery
  , runQueryThrow
  , runQueryPure
    -- * Specific queries
  , wallclockToSlot
  , slotToWallclock
  , slotToEpoch'
  , slotToEpoch
  , epochToSlot'
  , epochToSlot
  ) where

import           Control.Exception (throw)
import           Control.Monad.Except
import           Data.Bifunctor
import           Data.Fixed (divMod')
import           Data.Foldable (asum, toList)
import           Data.Time hiding (UTCTime)
import           Data.Word
import           GHC.Stack

import           Cardano.Slotting.Slot

import           Ouroboros.Consensus.BlockchainTime.WallClock.Types
import           Ouroboros.Consensus.Util.IOLike

import           Ouroboros.Consensus.HardFork.History.EraParams
import           Ouroboros.Consensus.HardFork.History.Summary
import           Ouroboros.Consensus.HardFork.History.Util

{-------------------------------------------------------------------------------
  Internal: reified queries

  NOTE. The lower bound of every era is inclusive, while the upper bound is
  really exclusive, making the upper bound of every era equal to the lower
  bound of the next.

  >         era A         era B         era C
  >        [.....) [...............) [..........)
  > epoch         e                 e'
  > slot          s                 s'
  > time          t                 t'

  Now let's consider what happens when we do translations of the values at
  the boundary.

   1. Slot-to-epoch translation. Using era C, we get

      > e' + ((s' - s') / epochSizeC) == e'

      Using era B (technically the wrong era to be using, since the upper bound
      is exclusive), we get

      > e + ((s' - s) / epochSizeB)

      These are equal by (INV-1a).

   2. Epoch-to-slot translation. Using era C, we get

      > s' + ((e' - e') * epochSizeC) == s'

      Using era B, we'd get

      > s + ((e' - e) * epochSizeB

      These are equal by (INV-1b).

   3. Slot to time translation. Using era C, we get

      > t' + ((s' - s') * slotLenC) == t'

      Using era C, we get

      > t + ((s' - s) * slotLenB)

      These are equal by (INV-2b)

   4. Time to slot translation. Using era C, we get

      > s' + ((t' - t') / slotLenC) == s'

      Using era B, we get

      > s + ((t' - t) / slotLenB)

      These are equal by (INV-2a).

  This means that for values at that boundary, it does not matter if we use
  this era or the next era for the translation. However, this is only true for
  these 4 translations. If we are returning the era parameters directly, then
  of course we can't use the era parameters from the wrong era.

  There is however a benefit to using the current era: there might not /be/
  a next era, and so if we use the current era, we extend the period for which
  we do calculations just that tiny bit more. This might be important for
  ledger implementations. For example, suppose we want to know if a particular
  slot @s@ is far enough away from the next epoch boundary (e.g., to determine
  if an update proposal should take effect in this epoch or the next). One
  natural way to write this would be to translate @s@ to the corresponding
  epoch @e@, then translate @e + 1@ back to a slot @s'@, and check the
  distance @s' - s@. However, it is conceivable that the safe zone stops at
  that epoch boundary; if it does, this computation would result in a
  'PastHorizonException', even if a different way to write the same computation
  (translating @s + delta@ to an epoch number, and then comparing that to @e@)
  might succeed. Rather than imposing an unnecessary limitation on the ledger,
  we therefore treat the upper bound as inclusive, so that both ways to do the
  check would succeed.
-------------------------------------------------------------------------------}

newtype TimeInEra   = TimeInEra   { getTimeInEra   :: NominalDiffTime }
newtype TimeInSlot  = TimeInSlot  { getTimeInSlot  :: NominalDiffTime }
newtype SlotInEra   = SlotInEra   { getSlotInEra   :: Word64 }
newtype SlotInEpoch = SlotInEpoch { getSlotInEpoch :: Word64 }
newtype EpochInEra  = EpochInEra  { getEpochInEra  :: Word64 }

-- | Query
data Qry :: * -> * where
  QPure :: a -> Qry a
  QBind :: Qry a -> (a -> Qry b) -> Qry b

  -- Convert from absolute to era-relative

  QAbsToRelTime  :: RelativeTime -> Qry TimeInEra
  QAbsToRelSlot  :: SlotNo       -> Qry SlotInEra
  QAbsToRelEpoch :: EpochNo      -> Qry EpochInEra

  -- Convert from era-relative to absolute

  QRelToAbsTime  :: TimeInEra                 -> Qry RelativeTime
  QRelToAbsSlot  :: (SlotInEra, TimeInSlot)   -> Qry SlotNo
  QRelToAbsEpoch :: (EpochInEra, SlotInEpoch) -> Qry EpochNo

  -- Convert between relative values

  QRelTimeToSlot  :: TimeInEra  -> Qry (SlotInEra, TimeInSlot)
  QRelSlotToTime  :: SlotInEra  -> Qry TimeInEra
  QRelSlotToEpoch :: SlotInEra  -> Qry (EpochInEra, SlotInEpoch)
  QRelEpochToSlot :: EpochInEra -> Qry SlotInEra

  -- Get era parameters
  -- The arguments are used for bound checks

  QSlotLength :: SlotNo  -> Qry SlotLength
  QEpochSize  :: EpochNo -> Qry EpochSize

instance Functor Qry where
  fmap = liftM

instance Applicative Qry where
  pure  = QPure
  (<*>) = ap

instance Monad Qry where
  return = pure
  (>>=)  = QBind

{-------------------------------------------------------------------------------
  Interpreter
-------------------------------------------------------------------------------}

-- | Evaluate a query in an era
--
-- Returns 'Nothing' if the query is out of bounds in this era.
evalQryInEra :: EraSummary -> Qry a -> Maybe a
evalQryInEra EraSummary{..} = go
  where
    EraParams{..} = eraParams
    slotLen   = getSlotLength eraSlotLength
    epochSize = unEpochSize   eraEpochSize

    guardEnd :: (Bound -> Bool) -> Maybe ()
    guardEnd p =
        case eraEnd of
          EraUnbounded -> return ()
          EraEnd b     -> guard $ p b

    go :: Qry a -> Maybe a
    go (QPure a) =
        return a
    go (QBind x f) = do
        go x >>= go . f

    -- Convert absolute to relative
    --
    -- The guards here justify the subtractions.

    go (QAbsToRelTime t) = do
        guard (t >= boundTime eraStart)
        return $ TimeInEra (t `diffRelTime` boundTime eraStart)
    go (QAbsToRelSlot s) = do
        guard (s >= boundSlot eraStart)
        return $ SlotInEra (countSlots s (boundSlot eraStart))
    go (QAbsToRelEpoch e) = do
        guard (e >= boundEpoch eraStart)
        return $ EpochInEra (countEpochs e (boundEpoch eraStart))

    -- Convert relative to absolute
    --
    -- As justified by the proof above, the guards treat the upper bound
    -- as inclusive.

    go (QRelToAbsTime t) = do
        let absTime = getTimeInEra t `addRelTime` boundTime eraStart
        guardEnd $ \end -> absTime <= boundTime end
        return absTime
    go (QRelToAbsSlot (s, t)) = do
        let absSlot = addSlots (getSlotInEra s) (boundSlot eraStart)
        guardEnd $ \end -> absSlot <  boundSlot end
                        || absSlot == boundSlot end && getTimeInSlot t == 0
        return absSlot
    go (QRelToAbsEpoch (e, s)) = do
        let absEpoch = addEpochs (getEpochInEra e) (boundEpoch eraStart)
        guardEnd $ \end -> absEpoch <  boundEpoch end
                        || absEpoch == boundEpoch end && getSlotInEpoch s == 0
        return absEpoch

    -- Convert between relative values
    --
    -- No guards necessary

    go (QRelTimeToSlot t) =
        return $ bimap SlotInEra TimeInSlot (getTimeInEra t `divMod'` slotLen)
    go (QRelSlotToTime s) =
        return $ TimeInEra (fromIntegral (getSlotInEra s) * slotLen)
    go (QRelSlotToEpoch s) =
        return $ bimap EpochInEra SlotInEpoch $ getSlotInEra s `divMod` epochSize
    go (QRelEpochToSlot e) =
        return $ SlotInEra (getEpochInEra e * epochSize)

    -- Get era parameters
    --
    -- Here the upper bound must definitely be exclusive, or we'd return the
    -- era parameters from the wrong era.

    go (QSlotLength s) = do
        guard    $ s >= boundSlot eraStart
        guardEnd $ \end -> s < boundSlot end
        return eraSlotLength
    go (QEpochSize e) = do
        guard    $ e >= boundEpoch eraStart
        guardEnd $ \end -> e < boundEpoch end
        return eraEpochSize

{-------------------------------------------------------------------------------
  Very thin layer for running queries
-------------------------------------------------------------------------------}

runQuery :: HasCallStack => Qry a -> Summary xs -> Either PastHorizonException a
runQuery qry (Summary summary) =
    case asum $ map (flip evalQryInEra qry) eras of
      Just answer -> Right answer
      Nothing     -> Left $ PastHorizon callStack eras
  where
    eras :: [EraSummary]
    eras = toList summary

runQueryThrow :: (HasCallStack, MonadThrow m )=> Qry a -> Summary xs -> m a
runQueryThrow q = either throwM return . runQuery q

runQueryPure :: HasCallStack => Qry a -> Summary xs -> a
runQueryPure q = either throw id . runQuery q

{-------------------------------------------------------------------------------
  Specific queries

  The primed forms are the ones used in the 'EpochInfo' construction.
  Critically, they do not ask for any of the era parameters. This means that
  their valid range /includes/ the end bound.
-------------------------------------------------------------------------------}

-- | Translate 'UTCTime' to 'SlotNo'
--
-- Additionally returns the time spent and time left in this slot.
wallclockToSlot :: RelativeTime -> Qry (SlotNo, NominalDiffTime, NominalDiffTime)
wallclockToSlot absTime = do
    relTime <- QAbsToRelTime  absTime
    relSlot <- QRelTimeToSlot relTime
    absSlot <- QRelToAbsSlot  relSlot
    slotLen <- QSlotLength    absSlot
    let timeInSlot = getTimeInSlot (snd relSlot)
    return (
        absSlot
      , timeInSlot
      , getSlotLength slotLen - timeInSlot
      )

-- | Translate 'SlotNo' to the 'UTCTime' at the start of that slot
--
-- Additionally returns the length of the slot.
slotToWallclock :: SlotNo -> Qry (RelativeTime, SlotLength)
slotToWallclock absSlot = do
    relSlot <- QAbsToRelSlot  absSlot
    relTime <- QRelSlotToTime relSlot
    absTime <- QRelToAbsTime  relTime
    slotLen <- QSlotLength    absSlot
    return (absTime, slotLen)

-- | Convert 'SlotNo' to 'EpochNo' and the relative slot within the epoch
slotToEpoch' :: SlotNo -> Qry (EpochNo, Word64)
slotToEpoch' absSlot = do
    relSlot   <- QAbsToRelSlot   absSlot
    epochSlot <- QRelSlotToEpoch relSlot
    absEpoch  <- QRelToAbsEpoch  epochSlot
    return (absEpoch, getSlotInEpoch (snd epochSlot))

-- | Translate 'SlotNo' to its corresponding 'EpochNo'
--
-- Additionally returns the relative slot within this epoch and how many
-- slots are left in this slot.
slotToEpoch :: SlotNo -> Qry (EpochNo, Word64, Word64)
slotToEpoch absSlot = do
    (absEpoch, slotInEpoch) <- slotToEpoch' absSlot
    epochSize               <- QEpochSize absEpoch
    return (absEpoch, slotInEpoch, unEpochSize epochSize - slotInEpoch)

epochToSlot' :: EpochNo -> Qry SlotNo
epochToSlot' absEpoch = do
    relEpoch  <- QAbsToRelEpoch  absEpoch
    slotInEra <- QRelEpochToSlot relEpoch
    absSlot   <- QRelToAbsSlot   (slotInEra, TimeInSlot 0)
    return absSlot

-- | Translate 'EpochNo' to the 'SlotNo' of the first slot in that epoch
--
-- Additionally returns the size of the epoch.
epochToSlot :: EpochNo -> Qry (SlotNo, EpochSize)
epochToSlot absEpoch = (,) <$> epochToSlot' absEpoch <*> QEpochSize absEpoch
