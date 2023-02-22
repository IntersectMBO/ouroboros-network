{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE NamedFieldPuns     #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE TypeApplications   #-}

module Test.Ouroboros.Storage.LedgerDB.DiskPolicy (tests) where

import           Data.Time.Clock (DiffTime, diffTimeToPicoseconds,
                     picosecondsToDiffTime, secondsToDiffTime)
import           Data.Word

import           Test.QuickCheck
import           Test.Tasty
import           Test.Tasty.QuickCheck

import           Ouroboros.Consensus.Config.SecurityParam (SecurityParam (..))
import           Ouroboros.Consensus.Storage.LedgerDB.DiskPolicy
                     (DiskPolicy (..), SnapshotInterval (..),
                     TimeSinceLast (..), defaultDiskPolicy)

tests :: TestTree
tests =
    testGroup "DiskPolicy" [
        testGroup "defaultDiskPolicy" [
            testProperty "onDiskNumSnapshots"       prop_onDiskNumSnapshots
          , testProperty "onDiskShouldTakeSnapshot" prop_onDiskShouldTakeSnapshot
          ]
      ]

{-------------------------------------------------------------------------------
  Test inputs
-------------------------------------------------------------------------------}

-- | This contains sufficient inputs for each property in this module.
data TestSetup = TestSetup {
    -- | argument to 'onDiskShouldTakeSnapshot'
    tsBlocksSince      :: Word64
    -- | argument to 'defaultDiskPolicy'
  , tsK                :: SecurityParam
    -- | argument to 'defaultDiskPolicy'
  , tsSnapshotInterval :: SnapshotInterval
    -- | argument to 'onDiskShouldTakeSnapshot'
  , tsTimeSince        :: TimeSinceLast DiffTime
  }
  deriving (Show)

-- | The represented default 'DiskPolicy'
toDiskPolicy :: TestSetup -> DiskPolicy
toDiskPolicy ts = defaultDiskPolicy (tsK ts) (tsSnapshotInterval ts)

-- | The result of the represented call to 'onDiskShouldTakeSnapshot'
shouldTakeSnapshot :: TestSetup -> Bool
shouldTakeSnapshot ts = onDiskShouldTakeSnapshot
    (toDiskPolicy ts)
    (tsTimeSince ts)
    (tsBlocksSince ts)

{-------------------------------------------------------------------------------
  Generator and shrinker
-------------------------------------------------------------------------------}

instance Arbitrary TestSetup where
  arbitrary = do
      k <- frequency [
          (9, choose (0, 3000))
        , (1, choose (0, maxBound))
        ]

      -- values within usual expectations
      let nominal =
                (,)

            -- 20 k is average number in a Shelley epoch
            <$> choose (0, 20 * k)

            -- a week is a defensible upper bound on the user input
            <*> just95 (chooseSeconds 0 oneWeekInSeconds)

      -- values near known cutoffs
      let interesting =
                (,)

            <$> curry choose
                  (minBlocksBeforeSnapshot `div` 2)
                  (minBlocksBeforeSnapshot * 2)

            <*> ( Just <$> chooseSeconds
                    (minSecondsBeforeSnapshot `div` 2)
                    (minSecondsBeforeSnapshot * 2)
                )

      -- all other conceivable values
      let wild =
                (,)
            <$> choose (0, maxBound)
            <*> just95 (chooseSeconds 0 oneCenturyInSeconds)

      (b, t) <- frequency [
          (80, nominal)
        , (15, interesting)
        , (5,  wild)
        ]

      -- this argument is provided from node via flag, we must anticipate values
      -- to be completely arbitrary. However we still want to keep the distribution
      -- of those values in such way that more probable values will be
      -- more frequently test
      tsSnapshotInterval <- frequency [
          (45, pure DefaultSnapshotInterval)
        , (45, RequestedSnapshotInterval <$> chooseSeconds 0      oneWeekInSeconds)
        , ( 4, RequestedSnapshotInterval <$> chooseSeconds 0 (2 * oneWeekInSeconds))
        , ( 4, RequestedSnapshotInterval <$> chooseSeconds 0 (3 * oneWeekInSeconds))
        , ( 1, RequestedSnapshotInterval <$> chooseSeconds 0 (4 * oneWeekInSeconds))
        , ( 1, RequestedSnapshotInterval <$> chooseSeconds 0 oneCenturyInSeconds)
        ]

      pure TestSetup {
          tsBlocksSince = b
        , tsK           = SecurityParam k
        , tsSnapshotInterval
        , tsTimeSince   = maybe NoSnapshotTakenYet TimeSinceLast t
        }
    where
      -- 100 years seems a reasonable upper bound for consideration
      oneCenturyInSeconds = 100 * 365 * oneDayInSeconds
      -- one week seems a reasonable upper bound for relevance
      oneWeekInSeconds = 7 * oneDayInSeconds
      oneDayInSeconds  = 24 * 60 * 60

      just95 :: Gen a -> Gen (Maybe a)
      just95 m = frequency [(5, pure Nothing), (95, Just <$> m)]

      -- both bounds are inclusive and in seconds
      chooseSeconds :: Integer -> Integer -> Gen DiffTime
      chooseSeconds lo hi = do
          -- pick a second
          s <- choose (lo, hi)
          -- jitter within it
          let nines = 10 ^ (12 :: Int) - 1
          offset <- choose (negate nines, nines)
          pure $ picosecondsToDiffTime $ max lo $ min hi $ s + offset

  shrink (TestSetup x1 x2 x3 x4) = mconcat [
        (\y -> TestSetup y x2 x3 x4) <$> shrink @Word64 x1
      , (\y -> TestSetup x1 y x3 x4) <$> shrinkSecurityParam x2
      , (\y -> TestSetup x1 x2 y x4) <$> shrinkSnapshotInterval x3
      , (\y -> TestSetup x1 x2 x3 y) <$> shrinkTSL shrinkDiffTime x4
      ]
    where
      shrinkSecurityParam =
          fmap SecurityParam . shrink @Word64 . maxRollbacks

      shrinkDiffTime =
          fmap picosecondsToDiffTime
        . shrink @Integer
        . diffTimeToPicoseconds

      shrinkTSL shnk = \case
        NoSnapshotTakenYet -> []
        TimeSinceLast    d -> NoSnapshotTakenYet : fmap TimeSinceLast (shnk d)

      shrinkSnapshotInterval = \case
        DefaultSnapshotInterval     -> []
        RequestedSnapshotInterval d ->
              DefaultSnapshotInterval
            : (RequestedSnapshotInterval <$> shrinkDiffTime d)

{-------------------------------------------------------------------------------
  Properties
-------------------------------------------------------------------------------}

-- | Check 'onDiskNumSnapshots' of 'defaultDiskPolicy'
prop_onDiskNumSnapshots :: TestSetup -> Property
prop_onDiskNumSnapshots ts =
    -- 'TestSetup' has more information than we need for this property
      counterexample "should always be 2"
    $ onDiskNumSnapshots (toDiskPolicy ts) === 2

minBlocksBeforeSnapshot :: Word64
minBlocksBeforeSnapshot = 50_000

minSecondsBeforeSnapshot :: Integer
minSecondsBeforeSnapshot = 6 * 60

-- | Check 'onDiskShouldTakeSnapshot' of 'defaultDiskPolicy'
prop_onDiskShouldTakeSnapshot :: TestSetup -> Property
prop_onDiskShouldTakeSnapshot ts =
    counterexample ("decided to take snapshot? " ++ show (shouldTakeSnapshot ts)) $
    case t of
      NoSnapshotTakenYet ->
            counterexample "haven't taken a snapshot yet"
          $ counterexample "should take snapshot if it processed at least k blocks"
          $ shouldTakeSnapshot ts === (blocksSinceLast >= k)
      TimeSinceLast    timeSinceLast ->
            counterexample "have previously taken a snapshot"
          $ isDisjunctionOf (shouldTakeSnapshot ts `named` "the decision")
              [ systemChecksHowMuchTimeHasPassed timeSinceLast
              , systemChecksHowManyBlocksWereProcessed timeSinceLast
              ]
  where
    TestSetup {
        tsBlocksSince      = blocksSinceLast
      , tsK                = SecurityParam k
      , tsSnapshotInterval = snapshotInterval
      , tsTimeSince        = t
      } = ts

    kTimes2 :: DiffTime
    kTimes2 = secondsToDiffTime $ fromIntegral $ k * 2

    systemChecksHowMuchTimeHasPassed :: DiffTime -> NamedValue Bool
    systemChecksHowMuchTimeHasPassed timeSinceLast =
        case snapshotInterval of

          DefaultSnapshotInterval ->
              (timeSinceLast >= kTimes2) `named`
                "time since last is greater then 2 * k seconds if snapshot interval is set to default"

          RequestedSnapshotInterval interval ->
              (timeSinceLast >= interval) `named`
                "time since last is greater then explicitly requested interval"

    systemChecksHowManyBlocksWereProcessed :: DiffTime -> NamedValue Bool
    systemChecksHowManyBlocksWereProcessed timeSinceLast =
        disjunct `named` msg
      where
        msg = unwords [
            "we have processed", show minBlocksBeforeSnapshot
          , "blocks and it's been more than", show minSecondsBeforeSnapshot
          , "seconds since last snapshot was taken"
          ]

        disjunct =
             blocksSinceLast >= minBlocksBeforeSnapshot
          && timeSinceLast >= secondsToDiffTime minSecondsBeforeSnapshot

{-------------------------------------------------------------------------------
  Auxiliary   -- TODO relocate this somewhere more general
-------------------------------------------------------------------------------}

-- | A value with an associated user-friendly string
data NamedValue a = NamedValue String a

forgetName :: NamedValue a -> a
forgetName (NamedValue _s a) = a

infix 0 `named`

named :: a -> String -> NamedValue a
named = flip NamedValue

-- | Use this instead of @x '===' 'or' ys@ to get a 'counterexample' message
-- that explains which of the disjuncts were mismatched
isDisjunctionOf :: NamedValue Bool -> [NamedValue Bool] -> Property
isDisjunctionOf (NamedValue s b) ds =
    counterexample msg $ b === any forgetName ds
  where
    msg =
      unlines $
          (    show b <> " for " <> s
            <> ", but the " <> show (length ds) <> " disjuncts were: "
          )
        : [    "  "
            <> "disjunct " <> show (i :: Int) <> ": "
            <> show b' <> " for " <> s'
          | (i, NamedValue s' b') <- zip [0..] ds
          ]
