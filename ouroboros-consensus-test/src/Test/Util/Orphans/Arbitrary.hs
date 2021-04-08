{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DerivingVia          #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE NumericUnderscores   #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Util.Orphans.Arbitrary (
    SmallDiffTime (..)
  , genLimitedEpochSize
  , genLimitedSlotNo
  , genSmallEpochNo
  , genSmallSlotNo
  ) where

import           Data.Coerce (coerce)
import           Data.SOP.Dict (Dict (..), all_NP, mapAll)
import           Data.SOP.Strict
import           Data.Time
import           Data.Word (Word64)
import           Test.QuickCheck hiding (Fixed (..))

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.BlockchainTime
import           Ouroboros.Consensus.Fragment.InFuture (ClockSkew)
import qualified Ouroboros.Consensus.Fragment.InFuture as InFuture
import           Ouroboros.Consensus.HardFork.History (Bound (..))
import           Ouroboros.Consensus.HeaderValidation (TipInfo)
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Ledger.Query
import           Ouroboros.Consensus.Ledger.SupportsMempool
import           Ouroboros.Consensus.Node.ProtocolInfo
import           Ouroboros.Consensus.Protocol.Abstract (ChainDepState)
import           Ouroboros.Consensus.TypeFamilyWrappers
import           Ouroboros.Consensus.Util.SOP (IsNonEmpty, ProofNonEmpty (..),
                     checkIsNonEmpty, isNonEmpty)

import           Ouroboros.Consensus.HardFork.Combinator (HardForkBlock,
                     HardForkChainDepState, HardForkState (..),
                     LedgerEraInfo (..), LedgerState (..), Mismatch (..),
                     MismatchEraInfo (..), SingleEraBlock (..), SingleEraInfo,
                     Telescope (..), proxySingle)
import           Ouroboros.Consensus.HardFork.Combinator.State (Current (..),
                     Past (..))

import           Ouroboros.Consensus.Storage.ImmutableDB.Chunks.Internal
                     (ChunkNo (..), ChunkSize (..), RelativeSlot (..))
import           Ouroboros.Consensus.Storage.ImmutableDB.Chunks.Layout

import           Test.Util.Serialisation.Roundtrip
import           Test.Util.Time

minNumCoreNodes :: Word64
minNumCoreNodes = 2

instance Arbitrary a => Arbitrary (WithOrigin a) where
  arbitrary = oneof [
        pure Origin
      , NotOrigin <$> arbitrary
      ]

  shrink Origin        = []
  shrink (NotOrigin x) = Origin : map NotOrigin (shrink x)

instance Arbitrary NumCoreNodes where
  arbitrary = NumCoreNodes <$> choose (minNumCoreNodes, 5)
  shrink (NumCoreNodes n) = NumCoreNodes <$> (filter (>= minNumCoreNodes) $ shrink n)

-- | Picks time span between 0 seconds and (roughly) 50 years
instance Arbitrary NominalDiffTime where
  arbitrary = conv <$> choose (0, 50 * daysPerYear * secondsPerDay)
    where
      conv :: Double -> NominalDiffTime
      conv = realToFrac

-- | Picks moment between 'dawnOfTime' and (roughly) 50 years later
--
-- Uses instance for 'NominalDiffTime'
instance Arbitrary UTCTime where
  arbitrary = (`addUTCTime` dawnOfTime) <$> arbitrary

-- | Length between 0.001 and 20 seconds, millisecond granularity
instance Arbitrary SlotLength where
  arbitrary = slotLengthFromMillisec <$> choose (1, 20 * 1_000)

  -- Try to shrink the slot length to just "1", for tests where the slot length
  -- really doesn't matter very much
  shrink slotLen = if slotLen /= oneSec then [oneSec] else []
    where
      oneSec = slotLengthFromSec 1

deriving via UTCTime         instance Arbitrary SystemStart
deriving via Positive Word64 instance Arbitrary BlockNo

instance Arbitrary RelativeSlot where
  arbitrary = RelativeSlot <$> arbitrary <*> arbitrary <*> arbitrary

-- | The functions 'slotAtTime' and 'timeUntilNextSlot' suffer from arithmetic
-- overflow for very large values, so generate values that avoid overflow when
-- used in these two functions. The largest value generated is still sufficently
-- large to allow for 5e12 years worth of slots at a slot interval of 20
-- seconds.
genLimitedSlotNo :: Gen SlotNo
genLimitedSlotNo =
    SlotNo <$> arbitrary `suchThat` (< 0x8000000000000000)

-- | Generate a small SlotNo for the state machine tests. The runtime of the
-- StateMachine prop_sequential tests is proportional the the upper bound.
genSmallSlotNo :: Gen SlotNo
genSmallSlotNo =
    SlotNo <$> choose (0, 1000)

-- | The tests for 'CumulEpochSizes' requires that the sum of a list of these
-- values does not overflow.
--
-- An epoch size must be > 0.
genLimitedEpochSize :: Gen EpochSize
genLimitedEpochSize =
    EpochSize <$> choose (1, 100_000)

genSmallEpochNo :: Gen EpochNo
genSmallEpochNo =
    EpochNo <$> choose (0, 10000)

-- | This picks an 'EpochNo' between 0 and 10000
--
-- We don't pick larger values because we're not interested in testing overflow
-- due to huge epoch numbers and even huger slot numbers.
instance Arbitrary ChunkNo where
  arbitrary = ChunkNo <$> choose (0, 10000)
  shrink    = genericShrink

-- | Picks a 'ChunkSize' between 1 and 100, and randomly choose to enable EBBs
instance Arbitrary ChunkSize where
  arbitrary = ChunkSize <$> arbitrary <*> choose (1, 100)
  shrink    = genericShrink

instance Arbitrary ChunkSlot where
  arbitrary = UnsafeChunkSlot <$> arbitrary <*> arbitrary
  shrink    = genericShrink

instance Arbitrary ClockSkew where
  arbitrary = InFuture.clockSkewInSeconds <$> choose (0, 5)
  shrink skew = concat [
     -- Shrink to some simple values, including 0
     -- (it would be useful to know if a test fails only when having non-zero
     -- clock skew)
       [ skew0 | skew0 < skew ]
     , [ skew1 | skew1 < skew ]
     ]
    where
      skew0, skew1 :: ClockSkew
      skew0 = InFuture.clockSkewInSeconds 0
      skew1 = InFuture.clockSkewInSeconds 1

{-------------------------------------------------------------------------------
  SmallDiffTime
-------------------------------------------------------------------------------}

-- | Wrapper around NominalDiffTime with custom 'Arbitrary' instance
--
-- The default 'Arbitrary' instance for 'NominalDiffTime' isn't very useful:
--
-- * It tends to pick huge values
-- * It tends not to pick integer values
-- * It does not shrink
--
-- Our custom instance
--
-- * Picks values between 0 and (1000 * 20 * 10) seconds:
--   - Maximum segment length: 1000
--   - Maximum slot length: 20 seconds
--   - Maximum number of segments: 10
-- * With a 0.1 second precision
-- * Shrinks
newtype SmallDiffTime = SmallDiffTime NominalDiffTime
  deriving (Show)

instance Arbitrary SmallDiffTime where
  arbitrary = conv <$> choose (0, 1000 * 20 * 10 * 10)
    where
      -- NominalDiffTime conversion functions treat it as seconds
      conv :: Integer -> SmallDiffTime
      conv n = SmallDiffTime $ realToFrac seconds
        where
          seconds :: Double
          seconds = fromInteger n / 10

  -- try to shrink to some small, simple values
  -- (include 1.5 so that we can shrink to a simple, but yet not whole, value)
  shrink (SmallDiffTime d) = map SmallDiffTime $
      filter (< d) [1, 1.5, 2, 3, 100]

{-------------------------------------------------------------------------------
  Auxiliary: time
-------------------------------------------------------------------------------}

-- | Average number of days per year
--
-- <https://en.wikipedia.org/wiki/Year>
daysPerYear :: Double
daysPerYear = 365.2425

-- | Seconds per day
secondsPerDay :: Double
secondsPerDay = 24 * 60 * 60

{-------------------------------------------------------------------------------
  Forwarding instances
-------------------------------------------------------------------------------}

-- | Forwarding
instance Arbitrary (ChainDepState (BlockProtocol blk))
      => Arbitrary (WrapChainDepState blk) where
  arbitrary = WrapChainDepState <$> arbitrary
  shrink x  = WrapChainDepState <$> shrink (unwrapChainDepState x)

-- | Forwarding
instance Arbitrary (HeaderHash blk)
      => Arbitrary (WrapHeaderHash blk) where
  arbitrary = WrapHeaderHash <$> arbitrary
  shrink x  = WrapHeaderHash <$> shrink (unwrapHeaderHash x)

-- | Forwarding
instance Arbitrary (TipInfo blk)
      => Arbitrary (WrapTipInfo blk) where
  arbitrary = WrapTipInfo <$> arbitrary
  shrink x  = WrapTipInfo <$> shrink (unwrapTipInfo x)

-- | Forwarding
instance Arbitrary a => Arbitrary (I a) where
  arbitrary = I <$> arbitrary
  shrink  x = I <$> shrink (unI x)

-- | Forwarding
instance Arbitrary (ApplyTxErr blk)
      => Arbitrary (WrapApplyTxErr blk) where
  arbitrary = WrapApplyTxErr <$> arbitrary
  shrink x  = WrapApplyTxErr <$> shrink (unwrapApplyTxErr x)

{-------------------------------------------------------------------------------
  NS
-------------------------------------------------------------------------------}

instance (All (Arbitrary `Compose` f) xs, IsNonEmpty xs)
      => Arbitrary (NS f xs) where
  arbitrary = case isNonEmpty (Proxy @xs) of
      ProofNonEmpty _ pf -> case checkIsNonEmpty pf of
        Nothing                    -> Z <$> arbitrary
        Just (ProofNonEmpty _ pf') -> frequency
          [ (1, Z <$> arbitrary)
            -- Use the number of remaining cases (one less than @xs@) as the
            -- weight so that the distribution is uniform
          , (lengthSList pf', S <$> arbitrary)
          ]
  shrink = hctraverse' (Proxy @(Arbitrary `Compose` f)) shrink

{-------------------------------------------------------------------------------
  Telescope & HardForkState
-------------------------------------------------------------------------------}

instance Arbitrary Bound where
  arbitrary =
      Bound
        <$> (RelativeTime <$> arbitrary)
        <*> (SlotNo       <$> arbitrary)
        <*> (EpochNo      <$> arbitrary)

instance Arbitrary (K Past blk) where
  arbitrary = K <$> (Past <$> arbitrary <*> arbitrary)

instance Arbitrary (f blk) => Arbitrary (Current f blk) where
  arbitrary = Current <$> arbitrary <*> arbitrary

instance ( IsNonEmpty xs
         , All (Arbitrary `Compose` f) xs
         , All (Arbitrary `Compose` g) xs
         ) => Arbitrary (Telescope g f xs) where
  arbitrary = case isNonEmpty (Proxy @xs) of
      ProofNonEmpty _ pf -> case checkIsNonEmpty pf of
        Nothing                    -> TZ <$> arbitrary
        Just (ProofNonEmpty _ pf') -> frequency
          [ (1, TZ <$> arbitrary)
          , (lengthSList pf', TS <$> arbitrary <*> arbitrary)
          ]
  shrink = hctraverse' (Proxy @(Arbitrary `Compose` f)) shrink

instance (IsNonEmpty xs, SListI xs, All (Arbitrary `Compose` LedgerState) xs)
      => Arbitrary (LedgerState (HardForkBlock xs)) where
  arbitrary = case (dictKPast, dictCurrentLedgerState) of
      (Dict, Dict) -> inj <$> arbitrary
    where
      inj ::
           Telescope (K Past) (Current LedgerState) xs
        -> LedgerState (HardForkBlock xs)
      inj = coerce

      dictKPast :: Dict (All (Arbitrary `Compose` (K Past))) xs
      dictKPast = all_NP $ hpure Dict

      dictCurrentLedgerState ::
           Dict (All (Arbitrary `Compose` (Current LedgerState))) xs
      dictCurrentLedgerState =
          mapAll
            @(Arbitrary `Compose` LedgerState)
            @(Arbitrary `Compose` Current LedgerState)
            (\Dict -> Dict)
            Dict

instance (IsNonEmpty xs, SListI xs, All (Arbitrary `Compose` WrapChainDepState) xs)
      => Arbitrary (HardForkChainDepState xs) where
  arbitrary = case (dictKPast, dictCurrentWrapChainDepState) of
      (Dict, Dict) -> inj <$> arbitrary
    where
      inj ::
           Telescope (K Past) (Current WrapChainDepState) xs
        -> HardForkChainDepState xs
      inj = coerce

      dictKPast :: Dict (All (Arbitrary `Compose` (K Past))) xs
      dictKPast = all_NP $ hpure Dict

      dictCurrentWrapChainDepState ::
           Dict (All (Arbitrary `Compose` (Current WrapChainDepState))) xs
      dictCurrentWrapChainDepState =
          mapAll
            @(Arbitrary `Compose` WrapChainDepState)
            @(Arbitrary `Compose` Current WrapChainDepState)
            (\Dict -> Dict)
            Dict

{-------------------------------------------------------------------------------
  Mismatch & MismatchEraInfo
-------------------------------------------------------------------------------}

instance ( IsNonEmpty xs
         , All (Arbitrary `Compose` f) (x ': xs)
         , All (Arbitrary `Compose` g) (x ': xs)
         ) => Arbitrary (Mismatch f g (x ': xs)) where
 arbitrary = case isNonEmpty (Proxy @xs) of
    ProofNonEmpty _ pf -> frequency $ mconcat [
        -- length (x ': xs) = n + 1
        -- This line: n cases, the line below: also n cases.
        [ (1, ML <$> arbitrary <*> arbitrary)
        , (1, MR <$> arbitrary <*> arbitrary)
        ]
      , case checkIsNonEmpty pf of
          Nothing                     -> []
          -- The line below: n * (n - 1) cases. We want the weights to be
          -- proportional so that the distribution is uniform. We divide each
          -- weight by n to get 1 and 1 for the ML and MR cases above and n - 1 (=
          -- lengthSList pxs') for the MS case below.
          Just (ProofNonEmpty _ pxs') -> [(lengthSList pxs', MS <$> arbitrary)]
      ]

instance SingleEraBlock blk => Arbitrary (SingleEraInfo blk) where
  arbitrary = return $ singleEraInfo (Proxy @blk)

instance SingleEraBlock blk => Arbitrary (LedgerEraInfo blk) where
  arbitrary = return $ LedgerEraInfo $ singleEraInfo (Proxy @blk)

instance (All SingleEraBlock (x ': xs), IsNonEmpty xs)
      => Arbitrary (MismatchEraInfo (x ': xs)) where
  arbitrary =
      case (dictSingleEraInfo, dictLedgerEraInfo) of
        (Dict, Dict) -> MismatchEraInfo <$> arbitrary
    where
      dictSingleEraInfo ::
           Dict (All (Arbitrary `Compose` SingleEraInfo)) (x ': xs)
      dictSingleEraInfo = all_NP $ hcpure proxySingle Dict

      dictLedgerEraInfo ::
           Dict (All (Arbitrary `Compose` LedgerEraInfo)) (x ': xs)
      dictLedgerEraInfo = all_NP $ hcpure proxySingle Dict

{-------------------------------------------------------------------------------
  Query
-------------------------------------------------------------------------------}

instance Arbitrary (SomeSecond BlockQuery blk)
      => Arbitrary (SomeSecond Query blk) where
  arbitrary = do
    SomeSecond someBlockQuery <- arbitrary
    return (SomeSecond (BlockQuery someBlockQuery))

instance Arbitrary (WithVersion version (SomeSecond BlockQuery blk))
      => Arbitrary (WithVersion version (SomeSecond Query blk)) where
  arbitrary = do
    WithVersion v (SomeSecond someBlockQuery) <- arbitrary
    return (WithVersion v (SomeSecond (BlockQuery someBlockQuery)))

-- | This is @OVERLAPPABLE@ because we have to override the default behaviour
-- for e.g. 'Query's.
instance {-# OVERLAPPABLE #-} (Arbitrary version, Arbitrary a)
      => Arbitrary (WithVersion version a) where
  arbitrary = WithVersion <$> arbitrary <*> arbitrary
