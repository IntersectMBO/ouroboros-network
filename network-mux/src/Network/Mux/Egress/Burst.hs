{-# LANGUAGE BangPatterns   #-}
{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE NamedFieldPuns #-}

-- | Bursts of credited demand on the budget: what its credited bearers took
-- from the first grant after a gap to the last before the next, one record
-- each, for the counters loop to trace.
--
-- A grant counts when its bearer holds a credited tier; a grant at rest rank
-- or unranked is filler, the capacity the credited left over, so a peer
-- catching up at rest rank never keeps a burst open. The burst ends when no
-- credited bearer has been granted for the gap and none is queued, at its
-- last grant: a credited bearer queued across a gap is a stall inside the
-- burst, not its end. A record is closed after 'burstCap' whatever the
-- demand, so a saturated hour reads as capped records rather than one.
--
-- Nothing here keeps time. The end is noticed at the next event, a grant, a
-- change of the queue or the counters loop's sweep, and the record is closed
-- retroactively, at the last grant.
--
module Network.Mux.Egress.Burst
  ( Burst (..)
  , burstDuration
  , TierGrants (..)
  , Detector
  , Open
  , emptyDetector
  , burstGap
  , burstCap
  , burstKeep
  , gapFor
  , creditedTier
  , burstGrant
  , burstQueued
  , burstSweep
  , takeBursts
  , burstsDropped
  ) where

import Control.Monad.Class.MonadTime.SI

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Word (Word64, Word8)
import GHC.Generics (Generic)

import Network.Mux.Types (Tier (..), tierNumber)

-- | Grants to bearers of one tier.
data TierGrants = TierGrants {
  tgBytes   :: !Word64,
  tgBatches :: !Word64
  }
  deriving (Eq, Show, Generic)

instance Semigroup TierGrants where
  TierGrants b1 n1 <> TierGrants b2 n2 = TierGrants (b1 + b2) (n1 + n2)

-- | One burst, closed.
data Burst = Burst {
  bStart        :: !Time,      -- ^ the first credited grant
  bEnd          :: !Time,      -- ^ the last
  bCapped       :: !Bool,      -- ^ closed by 'burstCap', not by a gap
  bBytes        :: !Word64,    -- ^ granted to credited bearers
  bBatches      :: !Word64,
  bTiers        :: !(Map Word8 TierGrants),
                               -- ^ the same, by tier
  bFiller       :: !Word64,    -- ^ granted at rest rank or unranked meanwhile
  bWidth        :: !Int,       -- ^ distinct credited bearers granted
  bPeakQueued   :: !Int,       -- ^ the most credited bearers queued at once
  bWaitTokens   :: !DiffTime,  -- ^ the longest token wait of a credited grant
  bWaitWritable :: !DiffTime   -- ^ the writability waits of credited grants, summed
  }
  deriving (Eq, Show, Generic)

-- | From the first credited grant to the last.
burstDuration :: Burst -> DiffTime
burstDuration Burst { bStart, bEnd } = bEnd `diffTime` bStart

-- | The burst in progress.
data Open = Open {
  oStart        :: !Time,
  oLast         :: !Time,
  oBytes        :: !Word64,
  oBatches      :: !Word64,
  oTiers        :: !(Map Word8 TierGrants),
  oFiller       :: !Word64,
  oBearers      :: !(Set Word64),
  oPeakQueued   :: !Int,
  oWaitTokens   :: !DiffTime,
  oWaitWritable :: !DiffTime
  }
  deriving (Eq, Show, Generic)

-- | The burst in progress, if any, the records closed since the last
-- 'takeBursts', newest first and at most 'burstKeep' of them, and how many
-- credited bearers are queued now.
data Detector = Detector {
  dOpen    :: !(Maybe Open),
  dClosed  :: ![Burst],
  dKept    :: !Int,
  dDropped :: !Word64,   -- ^ records closed with no room to keep them, ever
  dQueued  :: !Int
  }
  deriving (Eq, Show, Generic)

emptyDetector :: Detector
emptyDetector = Detector Nothing [] 0 0 0

-- | The least idle time that ends a burst: a round trip, longer than the
-- spacing of one peer's successive requests.
burstGap :: DiffTime
burstGap = 0.1

-- | The longest record: a longer burst is closed and the next opened.
burstCap :: DiffTime
burstCap = 60

-- | How many closed records are kept between sweeps.
burstKeep :: Int
burstKeep = 128

-- | The gap for a bucket: at least the time it takes to refill from empty,
-- so a burst at a low budget is not cut by the budget's own pacing.
gapFor :: Double  -- ^ bytes/s
       -> Int     -- ^ capacity, bytes
       -> DiffTime
gapFor rate capacity
  | rate > 0  = max burstGap (realToFrac (fromIntegral capacity / rate :: Double))
  | otherwise = burstGap

-- | A tier whose bearers hold credit: everything before the rest tier.
creditedTier :: Word8 -> Bool
creditedTier tier = tier < tierNumber RestTier

-- | A grant at @now@ to bearer @bearer@ holding @tier@: credited, it opens
-- or extends the burst; otherwise it is filler while one is open.
burstGrant :: DiffTime  -- ^ the gap
           -> Time -> Word8 -> Word64 -> Int
           -> DiffTime  -- ^ its token wait
           -> DiffTime  -- ^ its writability wait
           -> Detector -> Detector
burstGrant gap now tier bearer need waitTokens waitWritable d0
  | creditedTier tier
  = let !o = maybe fresh extend (dOpen d) in d { dOpen = Just o }
  | otherwise
  = case dOpen d of
         Just o  -> let !o' = o { oFiller = oFiller o + n } in d { dOpen = Just o' }
         Nothing -> d
  where
    d = closeIfOver gap now d0
    n = fromIntegral need
    fresh = Open { oStart = now, oLast = now, oBytes = n, oBatches = 1
                 , oTiers = Map.singleton tier (TierGrants n 1), oFiller = 0
                 , oBearers = Set.singleton bearer, oPeakQueued = dQueued d
                 , oWaitTokens = waitTokens, oWaitWritable = waitWritable }
    extend o = o { oLast = now
                 , oBytes = oBytes o + n
                 , oBatches = oBatches o + 1
                 , oTiers = Map.insertWith (<>) tier (TierGrants n 1) (oTiers o)
                 , oBearers = Set.insert bearer (oBearers o)
                 , oWaitTokens = max (oWaitTokens o) waitTokens
                 , oWaitWritable = oWaitWritable o + waitWritable }

-- | The credited bearers queued went to @n@ at @now@.
burstQueued :: DiffTime -> Time -> Int -> Detector -> Detector
burstQueued gap now n d0 =
  let d = closeIfOver gap now d0
  in case dOpen d of
          Just o  -> let !o' = o { oPeakQueued = max (oPeakQueued o) n }
                     in d { dQueued = n, dOpen = Just o' }
          Nothing -> d { dQueued = n }

-- | Nothing happened; the burst in progress may be over by now.
burstSweep :: DiffTime -> Time -> Detector -> Detector
burstSweep = closeIfOver

-- | Close the burst in progress at @now@ if it is over: the gap has passed
-- since its last grant with no credited bearer queued, or it is past the cap.
closeIfOver :: DiffTime -> Time -> Detector -> Detector
closeIfOver gap now d@Detector { dOpen = Just o, dQueued }
  | now `diffTime` oLast o > gap, dQueued == 0 = close False o d
  | now `diffTime` oStart o > burstCap         = close True o d
closeIfOver _ _ d = d

close :: Bool -> Open -> Detector -> Detector
close capped o d@Detector { dClosed, dKept, dDropped }
  | dKept >= burstKeep = d { dOpen = Nothing, dDropped = dDropped + 1 }
  | otherwise          = d { dOpen = Nothing, dClosed = record : dClosed, dKept = dKept + 1 }
  where
    -- evaluated here: the list holds it until the sweep
    !record = Burst { bStart = oStart o, bEnd = oLast o, bCapped = capped
                   , bBytes = oBytes o, bBatches = oBatches o, bTiers = oTiers o
                   , bFiller = oFiller o, bWidth = Set.size (oBearers o)
                   , bPeakQueued = oPeakQueued o
                   , bWaitTokens = oWaitTokens o, bWaitWritable = oWaitWritable o }

-- | The records closed so far, oldest first, and the detector without them.
takeBursts :: Detector -> ([Burst], Detector)
takeBursts d@Detector { dClosed } = (reverse dClosed, d { dClosed = [], dKept = 0 })

-- | How many records were closed with no room to keep them, ever.
burstsDropped :: Detector -> Word64
burstsDropped = dDropped
