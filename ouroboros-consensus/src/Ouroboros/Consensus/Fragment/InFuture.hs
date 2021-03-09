{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DerivingVia         #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

-- | Intended for qualified import
--
-- > import Ouroboros.Consensus.Fragment.InFuture (CheckInFuture(..), ClockSkew(..))
-- > import qualified Ouroboros.Consensus.Fragment.InFuture as InFuture
module Ouroboros.Consensus.Fragment.InFuture (
    CheckInFuture(..)
  , InFuture(..)
  , reference
    -- * Clock skew
  , ClockSkew -- opaque
  , clockSkewInSeconds
  , defaultClockSkew
    -- * Testing
  , dontCheck
  , miracle
  ) where

import           Data.Bifunctor
import           Data.Time (NominalDiffTime)
import           Data.Word
import           NoThunks.Class (NoThunks, OnlyCheckWhnfNamed (..))

import           Control.Monad.Class.MonadSTM


import           Ouroboros.Network.AnchoredFragment (AnchoredFragment,
                     AnchoredSeq (Empty, (:>)))

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.BlockchainTime
import           Ouroboros.Consensus.Fragment.Validated (ValidatedFragment)
import qualified Ouroboros.Consensus.Fragment.Validated as VF
import           Ouroboros.Consensus.HardFork.Abstract
import qualified Ouroboros.Consensus.HardFork.History as HF
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Util.Time

data CheckInFuture m blk = CheckInFuture {
       -- | POSTCONDITION:
       -- > checkInFuture vf >>= \(af, fut) ->
       -- >   validatedFragment vf == af <=> null fut
       checkInFuture :: ValidatedFragment (Header blk) (LedgerState blk)
                     -> m (AnchoredFragment (Header blk), [InFuture blk])
    }
  deriving NoThunks
       via OnlyCheckWhnfNamed "CheckInFuture" (CheckInFuture m blk)

-- | Header of block that we found to be in the future
data InFuture blk = InFuture {
      -- | The header itself
      inFutureHeader           :: Header blk

      -- | Whether or not this header exceeded the allowed clock skew
      --
      -- Headers that do exceed the clock skew should be considered invalid.
    , inFutureExceedsClockSkew :: Bool
    }

{-------------------------------------------------------------------------------
  Clock skew
-------------------------------------------------------------------------------}

-- | Maximum permissible clock skew
--
-- When running NTP, systems clocks will never be perfectly synchronized. The
-- maximum clock skew records how much of a difference we consider acceptable.
--
-- For example. Suppose
--
-- * Two nodes A and B
-- * A's clock is 0.5 ahead of B's
-- * A produces a block and sends it to B
-- * When B translates the 'SlotNo' of that block to a time, it may find that
--   it is 0.5 seconds ahead of its current clock (worst case).
--
-- The maximum permissible clock skew decides if B will consider this block to
-- be valid (even if it will not yet consider it for chain seleciton) or as
-- invalid (and disconnect from A, since A is sending it invalid blocks).
--
-- Use 'defaultClockSkew' when unsure.
newtype ClockSkew = ClockSkew { unClockSkew :: NominalDiffTime }
  deriving (Show, Eq, Ord)

-- | Default maximum permissible clock skew
--
-- See 'ClockSkew' for details. We allow for 5 seconds skew by default.
defaultClockSkew :: ClockSkew
defaultClockSkew = clockSkewInSeconds 5

-- | Specify maximum clock skew in seconds
clockSkewInSeconds :: Double -> ClockSkew
clockSkewInSeconds = ClockSkew . secondsToNominalDiffTime

{-------------------------------------------------------------------------------
  Reference implementation
-------------------------------------------------------------------------------}

reference :: forall m blk. (Monad m, UpdateLedger blk, HasHardForkHistory blk)
          => LedgerConfig blk
          -> ClockSkew
          -> SystemTime m
          -> CheckInFuture m blk
reference cfg (ClockSkew clockSkew) SystemTime{..} = CheckInFuture {
      checkInFuture = \validated -> do
        now <- systemTimeCurrent
        -- Since we have the ledger state /after/ the fragment, the derived
        -- summary can be used to check all of the blocks in the fragment
        return $
          checkFragment
            (hardForkSummary cfg (VF.validatedLedger validated))
            now
            (VF.validatedFragment validated)
    }
  where
    checkFragment :: HF.Summary (HardForkIndices blk)
                  -> RelativeTime
                  -> AnchoredFragment (Header blk)
                  -> (AnchoredFragment (Header blk), [InFuture blk])
    checkFragment summary now = go
      where
        -- We work from newest to oldest, because as soon as we reach any block
        -- that is not ahead of @no@, the older blocks certainly aren't either.
        go :: AnchoredFragment (Header blk)
           -> (AnchoredFragment (Header blk), [InFuture blk])
        go (Empty a) = (Empty a, [])
        go (hs :> h) =
            case HF.runQuery
                   (HF.slotToWallclock (blockSlot h))
                   summary of
              Left _err ->
                error "CheckInFuture.reference: impossible"
              Right (hdrTime, _) ->
                if hdrTime > now then
                  second (inFuture h hdrTime:) $ go hs
                else
                  (hs :> h, [])

        inFuture :: Header blk -> RelativeTime -> InFuture blk
        inFuture hdr hdrTime = InFuture {
              inFutureHeader           = hdr
            , inFutureExceedsClockSkew = (hdrTime `diffRelTime` now)
                                       > clockSkew
            }

{-------------------------------------------------------------------------------
  Test infrastructure
-------------------------------------------------------------------------------}

-- | Trivial 'InFuture' check that doesn't do any check at all
--
-- This is useful for testing and tools such as the DB converter.
dontCheck :: Monad m => CheckInFuture m blk
dontCheck = CheckInFuture {
      checkInFuture = \validated -> return (VF.validatedFragment validated, [])
    }

-- | If by some miracle we have a function that can always tell us what the
-- correct slot is, implementing 'CheckInFuture' is easy
--
-- NOTE: Use of 'miracle' in tests means that none of the hard fork
-- infrastructure for converting slots to time is tested.
miracle :: forall m blk. (MonadSTM m, HasHeader (Header blk))
        => STM m SlotNo          -- ^ Get current slot
        -> Word64                -- ^ Maximum clock skew (in terms of slots)
        -> CheckInFuture m blk
miracle oracle clockSkew = CheckInFuture {
      checkInFuture = \validated -> do
        now <- atomically $ oracle
        return $ checkFragment now (VF.validatedFragment validated)
    }
  where
    checkFragment :: SlotNo
                  -> AnchoredFragment (Header blk)
                  -> (AnchoredFragment (Header blk), [InFuture blk])
    checkFragment now = go
      where
        go :: AnchoredFragment (Header blk)
           -> (AnchoredFragment (Header blk), [InFuture blk])
        go (Empty a) = (Empty a, [])
        go (hs :> h) =
            if blockSlot h > now then
              second (inFuture h:) $ go hs
            else
              (hs :> h, [])

        inFuture :: Header blk -> InFuture blk
        inFuture hdr = InFuture {
              inFutureHeader           = hdr
            , inFutureExceedsClockSkew = HF.countSlots (blockSlot hdr) now
                                       > clockSkew
            }
