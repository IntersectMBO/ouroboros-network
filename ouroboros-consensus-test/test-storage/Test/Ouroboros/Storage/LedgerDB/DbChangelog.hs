{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE DerivingStrategies   #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE PatternSynonyms      #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Ouroboros.Storage.LedgerDB.DbChangelog (tests) where

import           Cardano.Slotting.Slot (WithOrigin (..), withOrigin)
import           Control.Monad hiding (ap)
import           Control.Monad.Trans.Class (lift)
import           Control.Monad.Trans.State.Strict hiding (state)
import           Data.Foldable
import qualified Data.Map.Diff.Strict.Internal as Diff
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Maybe (catMaybes, isJust, isNothing)
import           Data.Set (Set)
import qualified Data.Set as Set
import           GHC.Generics (Generic)
import           NoThunks.Class (NoThunks)
import           Ouroboros.Consensus.Config.SecurityParam (SecurityParam (..))
import           Ouroboros.Consensus.Ledger.Basics hiding (LedgerState)
import           Ouroboros.Consensus.Storage.LedgerDB.DbChangelog
                     (DbChangelog (..))
import qualified Ouroboros.Consensus.Storage.LedgerDB.DbChangelog as DbChangelog
import           Ouroboros.Consensus.Storage.LedgerDB.DiffSeq as DS
import qualified Ouroboros.Network.AnchoredSeq as AS
import           Ouroboros.Network.Block (HeaderHash, Point (..), SlotNo (..),
                     StandardHash, castPoint, pattern GenesisPoint)
import qualified Ouroboros.Network.Point as Point
import           Test.Ouroboros.Storage.LedgerDB.OrphanArbitrary ()
import           Test.QuickCheck
import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.QuickCheck (testProperty)
import           Test.Util.QuickCheck (frequency', oneof')
import           Text.Show.Pretty (ppShow)

samples :: Int
samples = 1000

tests :: TestTree
tests = testGroup "Ledger" [ testGroup "DbChangelog"
      [ testProperty "flushing" $ withMaxSuccess samples $ conjoin
        [ counterexample "flushing keeps immutable tip"
          prop_flushingSplitsTheChangelog
        ]
      , testProperty "rolling back" $ withMaxSuccess samples $ conjoin
        [ counterexample "rollback after extension is noop"
          prop_rollbackAfterExtendIsNoop
        , counterexample "prefixing back to anchor is rolling back volatile states"
          prop_prefixBackToAnchorIsRollingBackVolatileStates
        , counterexample "prefix back to volatile tip is a noop"
          prop_rollBackToVolatileTipIsNoop
        ]
      , testProperty "extending adds head to volatile states"
        $ withMaxSuccess samples prop_extendingAdvancesTipOfVolatileStates
      , testProperty "pruning leaves at most maxRollback volatile states"
        $ withMaxSuccess samples prop_pruningLeavesAtMostMaxRollbacksVolatileStates
      ]
  ]


{-------------------------------------------------------------------------------
  Test setup
-------------------------------------------------------------------------------}

data TestLedger (mk :: MapKind) = TestLedger {
  tlUtxos :: mk Key Int,
  tlTip   :: Point TestLedger
}

nextState :: DbChangelog TestLedger -> TestLedger DiffMK
nextState dblog = TestLedger {
              tlTip = pointAtSlot $ nextSlot (getTipSlot old)
            , tlUtxos = DiffMK mempty
            }
  where
    old = either id id $ AS.head (DbChangelog.changelogVolatileStates dblog)
    nextSlot = At . withOrigin 1 (+1)


deriving instance Show (mk Key Int) => Show (TestLedger mk)

instance GetTip TestLedger where
  getTip = castPoint . tlTip

data H = H deriving (Eq, Ord, Show, Generic)
deriving anyclass instance NoThunks H
type instance HeaderHash TestLedger = H

instance StandardHash TestLedger

deriving instance Eq (TestLedger EmptyMK)

instance HasLedgerTables TestLedger where
  data LedgerTables TestLedger mk = TestTables { unTestTables :: mk Key Int } deriving (Generic)
  projectLedgerTables                                                 = TestTables . tlUtxos
  withLedgerTables st    (TestTables x)                               = st { tlUtxos = x }
  pureLedgerTables                                                    = TestTables
  mapLedgerTables f      (TestTables x)                               = TestTables (f x)
  traverseLedgerTables f (TestTables x)                               = TestTables <$> f x
  zipLedgerTables f      (TestTables x) (TestTables y)                = TestTables (f x y)
  zipLedgerTables3 f     (TestTables x) (TestTables y) (TestTables z) = TestTables (f x y z)
  zipLedgerTablesA f     (TestTables x) (TestTables y)                = TestTables <$> f x y
  zipLedgerTables3A f    (TestTables x) (TestTables y) (TestTables z) = TestTables <$> f x y z
  foldLedgerTables f     (TestTables x)                               = f x
  foldLedgerTables2 f    (TestTables x) (TestTables y)                = f x y
  namesLedgerTables = TestTables $ NameMK "TestTables"

deriving instance (IsMapKind mk, Eq (mk Key Int)) => Eq (LedgerTables TestLedger mk)
deriving instance (IsMapKind mk, NoThunks (mk Key Int)) => NoThunks (LedgerTables TestLedger mk)
deriving instance (IsMapKind mk, Show (mk Key Int)) => Show (LedgerTables TestLedger mk)

data DbChangelogTestSetup = DbChangelogTestSetup {
  -- The operations are applied on the right, i.e., the newest operation is at the head of the list.
    operations          :: [Operation TestLedger]
  , dbChangelogStartsAt :: WithOrigin SlotNo
  }

data Operation l = Extend (l DiffMK) | Prune SecurityParam
deriving instance Show (l DiffMK) => Show (Operation l)

data DbChangelogTestSetupWithRollbacks = DbChangelogTestSetupWithRollbacks
  { testSetup :: DbChangelogTestSetup
  , rollbacks :: Int
  } deriving (Show)

instance Show DbChangelogTestSetup where
  show = ppShow . operations

instance Arbitrary DbChangelogTestSetup where
  arbitrary = sized $ \n -> do
    slotNo <- oneof [pure Origin, At . SlotNo <$> chooseEnum (1, 1000)]
    ops <- genOperations slotNo n
    pure $ DbChangelogTestSetup
      { operations = ops
      , dbChangelogStartsAt = slotNo
      }

  -- TODO: Shrinking might not be optimal. Shrinking finds the shortest prefix of the list of
  -- operations that result in a failed property, by simply testing prefixes in increasing order.
  shrink setup = reverse $ takeWhileJust $ tail (iterate reduce (Just setup))
    where
      reduce (Just (DbChangelogTestSetup (_:ops) dblog)) = Just $ DbChangelogTestSetup ops dblog
      reduce _ = Nothing
      takeWhileJust = catMaybes . takeWhile isJust

instance Arbitrary DbChangelogTestSetupWithRollbacks where
  arbitrary = do
    setup <- arbitrary
    let dblog = resultingDbChangelog setup
    rolls <- chooseInt (0, AS.length (changelogVolatileStates dblog))
    pure $ DbChangelogTestSetupWithRollbacks
      { testSetup = setup
      , rollbacks = rolls
      }

  shrink setupWithRollback = toWithRollbacks <$> setups
    where
      setups = shrink (testSetup setupWithRollback)
      shrinkRollback :: DbChangelogTestSetup -> Int -> Int
      shrinkRollback setup rollback =
        AS.length (changelogVolatileStates $ resultingDbChangelog setup) `min` rollback
      toWithRollbacks setup = DbChangelogTestSetupWithRollbacks {
           testSetup = setup
         , rollbacks = shrinkRollback setup (rollbacks setupWithRollback)
         }

resultingDbChangelog :: DbChangelogTestSetup -> DbChangelog TestLedger
resultingDbChangelog setup = applyOperations (operations setup) originalDbChangelog
  where
    originalDbChangelog = DbChangelog.empty $ TestLedger EmptyMK anchor
    anchor = pointAtSlot (dbChangelogStartsAt setup)

applyOperations :: (HasLedgerTables l, GetTip l)
  => [Operation l] -> DbChangelog l -> DbChangelog l
applyOperations ops dblog = foldr' apply' dblog ops
  where apply' (Extend newState) dblog' = DbChangelog.extend dblog' newState
        apply' (Prune sp) dblog'        = DbChangelog.pruneVolatilePart sp dblog'

{-------------------------------------------------------------------------------
  Properties
-------------------------------------------------------------------------------}

-- | Changelog states and diffs appear in one either the changelog to flush or the changelog to
-- keep, moreover, the to flush changelog has no volatile states, and the to keep changelog has no
-- immutable states.
prop_flushingSplitsTheChangelog :: DbChangelogTestSetup -> SecurityParam -> Property
prop_flushingSplitsTheChangelog setup sp = isNothing toFlush .||. (
         (toKeepTip === At toFlushTip)
    .&&. (At toFlushTip === dblogTip)
    .&&. changelogVolatileStates toKeep === changelogVolatileStates dblog
    .&&. cumulativeDiff diffs === toFlushDiffs <> cumulativeDiff toKeepDiffs
         )
  where
    dblog                                    = resultingDbChangelog setup
    (toFlush, toKeep)                        = DbChangelog.flush (DbChangelog.FlushAllImmutable sp) dblog
    dblogTip                                 = DbChangelog.immutableTipSlot dblog
    toFlushTip                               = maybe undefined DbChangelog.toFlushSlot toFlush
    toKeepTip                                = DbChangelog.immutableTipSlot toKeep
    TestTables (SeqDiffMK toKeepDiffs)  = changelogDiffs toKeep
    TestTables (DiffMK toFlushDiffs)    = maybe undefined DbChangelog.toFlushDiffs toFlush
    TestTables (SeqDiffMK diffs)        = changelogDiffs dblog

-- | Extending the changelog adds the correct head to the volatile states.
prop_extendingAdvancesTipOfVolatileStates :: DbChangelogTestSetup -> Property
prop_extendingAdvancesTipOfVolatileStates setup =
  property $ tlTip state == tlTip new
  where
    dblog  = resultingDbChangelog setup
    state  = nextState dblog
    dblog' = DbChangelog.extend dblog state
    new    = either id id $ AS.head (changelogVolatileStates dblog')

-- | Rolling back n extensions is the same as doing nothing.
prop_rollbackAfterExtendIsNoop :: DbChangelogTestSetup -> Positive Int -> Property
prop_rollbackAfterExtendIsNoop setup (Positive n) =
  property (dblog == DbChangelog.rollbackN n (nExtensions n dblog))
  where
    dblog = resultingDbChangelog setup

-- | The number of volatile states left after pruning is at most the maximum number of rollbacks.
prop_pruningLeavesAtMostMaxRollbacksVolatileStates ::
  DbChangelogTestSetup -> SecurityParam -> Property
prop_pruningLeavesAtMostMaxRollbacksVolatileStates setup sp@(SecurityParam k) =
  property $ AS.length (changelogVolatileStates dblog') <= fromIntegral k
  where
    dblog = resultingDbChangelog setup
    dblog' = DbChangelog.pruneVolatilePart sp dblog

-- | The prefixBackToAnchor function rolls back all volatile states.
prop_prefixBackToAnchorIsRollingBackVolatileStates :: DbChangelogTestSetup -> Property
prop_prefixBackToAnchorIsRollingBackVolatileStates setup =
  property $ rolledBack == toAnchor
  where
    dblog = resultingDbChangelog setup
    n = AS.length (changelogVolatileStates dblog)
    rolledBack = DbChangelog.rollbackN n dblog
    toAnchor = DbChangelog.rollbackToAnchor dblog

-- | Rolling back to the last state is the same as doing nothing.
prop_rollBackToVolatileTipIsNoop ::
  Positive Int -> DbChangelogTestSetup -> Property
prop_rollBackToVolatileTipIsNoop (Positive n) setup = property $ Just dblog == dblog'
  where
    dblog = resultingDbChangelog setup
    pt = getTip $ AS.headAnchor $ changelogVolatileStates dblog
    dblog' = DbChangelog.rollbackToPoint pt $ nExtensions n dblog

nExtensions :: Int -> DbChangelog TestLedger -> DbChangelog TestLedger
nExtensions n dblog = iterate ext dblog !! n
  where ext dblog' = DbChangelog.extend dblog' (nextState dblog')

{-------------------------------------------------------------------------------
  Generators
-------------------------------------------------------------------------------}

pointAtSlot :: WithOrigin SlotNo -> Point TestLedger
pointAtSlot = Point.withOrigin GenesisPoint (\slotNo -> Point $ At $ Point.Block slotNo H)

type Key = String

data GenOperationsState = GenOperationsState {
    gosSlotNo            :: !(WithOrigin SlotNo)
  , gosOps               :: ![Operation TestLedger]
  , gosActiveUtxos       :: !(Map Key Int)
  , gosPendingInsertions :: !(Map Key Int)
  , gosConsumedUtxos     :: !(Set Key)
  } deriving (Show)

applyPending :: GenOperationsState -> GenOperationsState
applyPending gosState = gosState
  { gosActiveUtxos = Map.union (gosActiveUtxos gosState) (gosPendingInsertions gosState)
  , gosPendingInsertions = Map.empty
  }

genOperations :: WithOrigin SlotNo -> Int -> Gen [Operation TestLedger]
genOperations slotNo nOps = gosOps <$> execStateT (replicateM_ nOps genOperation) initState
  where
    initState = GenOperationsState {
        gosSlotNo = slotNo
      , gosActiveUtxos = Map.empty
      , gosPendingInsertions = Map.empty
      , gosConsumedUtxos = Set.empty
      , gosOps = []
      }

    genOperation :: StateT GenOperationsState Gen ()
    genOperation = do
      op <- frequency' [ (1, genPrune), (10, genExtend) ]
      modify' $ \st -> st { gosOps = op:gosOps st }

    genPrune :: StateT GenOperationsState Gen (Operation TestLedger)
    genPrune = Prune . SecurityParam <$> lift (chooseEnum (0, 10))

    genExtend :: StateT GenOperationsState Gen (Operation TestLedger)
    genExtend = do
      nextSlotNo <- advanceSlotNo =<< lift (chooseEnum (1, 5))
      d <- genUtxoDiff
      pure $ Extend $ TestLedger (DiffMK d) (castPoint $ pointAtSlot nextSlotNo)

    advanceSlotNo :: SlotNo -> StateT GenOperationsState Gen (WithOrigin SlotNo)
    advanceSlotNo by = do
      nextSlotNo <- gets (At . Point.withOrigin by (+ by) . gosSlotNo)
      modify' $ \st -> st { gosSlotNo = nextSlotNo }
      pure nextSlotNo

    genUtxoDiff :: StateT GenOperationsState Gen (Diff.Diff Key Int)
    genUtxoDiff = do
      nEntries <- lift $ chooseInt (1, 10)
      entries <- replicateM nEntries genUtxoDiffEntry
      modify' applyPending
      pure $ Diff.fromList entries

    genUtxoDiffEntry :: StateT GenOperationsState Gen (Key, Diff.DiffEntry Int)
    genUtxoDiffEntry = do
      activeUtxos <- gets gosActiveUtxos
      consumedUtxos <- gets gosConsumedUtxos
      oneof' $ catMaybes [
        genDelEntry activeUtxos,
        genInsertEntry consumedUtxos]

    genDelEntry :: Map Key Int -> Maybe (StateT GenOperationsState Gen (Key, Diff.DiffEntry Int))
    genDelEntry activeUtxos =
      if Map.null activeUtxos then Nothing
      else Just $ do
        (k, v) <- lift $ elements (Map.toList activeUtxos)
        modify' $ \st -> st
          { gosActiveUtxos = Map.delete k (gosActiveUtxos st)
          }
        pure (k, Diff.Delete v )

    genInsertEntry :: Set Key -> Maybe (StateT GenOperationsState Gen (Key, Diff.DiffEntry Int))
    genInsertEntry consumedUtxos = Just $ do
      k <- lift $ genKey `suchThat` (`Set.notMember` consumedUtxos)
      v <- lift arbitrary
      modify' $ \st -> st
        { gosPendingInsertions = Map.insert k v (gosPendingInsertions st)
        , gosConsumedUtxos = Set.insert k (gosConsumedUtxos st)
        }
      pure (k, Diff.Insert v)

genKey :: Gen Key
genKey = replicateM 2 $ elements ['A'..'Z']
