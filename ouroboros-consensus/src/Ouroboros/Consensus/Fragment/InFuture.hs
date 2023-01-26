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
    CheckInFuture (..)
  , InFuture (..)
  , reference
    -- * Clock skew
  , clockSkewInSeconds
  , defaultClockSkew
    -- ** opaque
  , ClockSkew
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
import           Ouroboros.Consensus.HardFork.Abstract
import qualified Ouroboros.Consensus.HardFork.History as HF
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Storage.ChainDB.API.Types.InvalidBlockPunishment
                     (InvalidBlockPunishment)
import qualified Ouroboros.Consensus.Storage.ChainDB.API.Types.InvalidBlockPunishment as InvalidBlockPunishment
import           Ouroboros.Consensus.Util.Time

data CheckInFuture m blk = CheckInFuture {
       -- | POSTCONDITION:
       -- > checkInFuture _ af >>= \(af', fut) ->
       -- >   af == af' <=> null fut
       checkInFuture :: LedgerState blk -> AnchoredFragment (Header blk)
                     -> m (AnchoredFragment (Header blk), [InFuture m blk])
    }
  deriving NoThunks
       via OnlyCheckWhnfNamed "CheckInFuture" (CheckInFuture m blk)

-- | Header of block that we found to be in the future
data InFuture m blk = InFuture {
      -- | The header itself
      inFutureHeader           :: Header blk

      -- | Whether or not this header exceeded the allowed clock skew
      --
      -- Headers that do exceed the clock skew should be considered invalid.
    , inFutureExceedsClockSkew :: Bool

      -- | 'Ouroboros.Consensus.Storage.ChainDB.Impl.Types.blockPunish'
    , inFuturePunish           :: InvalidBlockPunishment m
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
      checkInFuture = \ledgerState af -> do
          now <- systemTimeCurrent
          return $ checkFragment (hardForkSummary cfg ledgerState) now af
    }
  where
    checkFragment :: HF.Summary (HardForkIndices blk)
                  -> RelativeTime
                  -> AnchoredFragment (Header blk)
                  -> (AnchoredFragment (Header blk), [InFuture m blk])
    checkFragment summary now = go
      where
        -- We work from newest to oldest, because as soon as we reach any block
        -- that is not ahead of @no@, the older blocks certainly aren't either.
        go :: AnchoredFragment (Header blk)
           -> (AnchoredFragment (Header blk), [InFuture m blk])
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

        inFuture :: Header blk -> RelativeTime -> InFuture m blk
        inFuture hdr hdrTime = InFuture {
              inFutureHeader           = hdr
            , inFutureExceedsClockSkew = (hdrTime `diffRelTime` now)
                                       > clockSkew
            , inFuturePunish           = InvalidBlockPunishment.noPunishment
            }

{-------------------------------------------------------------------------------
  Test infrastructure
-------------------------------------------------------------------------------}

-- | Trivial 'InFuture' check that doesn't do any check at all
--
-- This is useful for testing and tools such as the DB converter.
dontCheck :: Monad m => CheckInFuture m blk
dontCheck = CheckInFuture {
      checkInFuture = \_ af -> return (af, [])
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
      checkInFuture = \_ af -> do
        now <- atomically $ oracle
        return $ checkFragment now af
    }
  where
    checkFragment :: SlotNo
                  -> AnchoredFragment (Header blk)
                  -> (AnchoredFragment (Header blk), [InFuture m blk])
    checkFragment now = go
      where
        go :: AnchoredFragment (Header blk)
           -> (AnchoredFragment (Header blk), [InFuture m blk])
        go (Empty a) = (Empty a, [])
        go (hs :> h) =
            if blockSlot h > now then
              second (inFuture h:) $ go hs
            else
              (hs :> h, [])

        inFuture :: Header blk -> InFuture m blk
        inFuture hdr = InFuture {
              inFutureHeader           = hdr
            , inFutureExceedsClockSkew = HF.countSlots (blockSlot hdr) now
                                       > clockSkew
            , inFuturePunish           = InvalidBlockPunishment.noPunishment
            }
