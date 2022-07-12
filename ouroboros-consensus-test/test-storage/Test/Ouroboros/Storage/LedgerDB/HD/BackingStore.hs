{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}

module Test.Ouroboros.Storage.LedgerDB.HD.BackingStore (tests) where

import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Monoid
import           Data.Set (Set)
import qualified Data.Set as Set
import           GHC.Generics (Generic)

import           Test.QuickCheck (Arbitrary, Gen)
import qualified Test.QuickCheck as QC
import qualified Test.QuickCheck.Monadic as QC
import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.QuickCheck (testProperty)

import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Ledger.Basics ()
import           Ouroboros.Consensus.Util.IOLike

import           Ouroboros.Consensus.Storage.FS.API
import           Ouroboros.Consensus.Storage.FS.API.Types
import qualified Ouroboros.Consensus.Storage.FS.IO as FSIO
import qualified Ouroboros.Consensus.Storage.LedgerDB.HD as HD
import qualified Ouroboros.Consensus.Storage.LedgerDB.HD.BackingStore as BS
import qualified Ouroboros.Consensus.Storage.LedgerDB.HD.LMDB as LMDB
import           Ouroboros.Consensus.Storage.LedgerDB.InMemory
import           Ouroboros.Consensus.Storage.LedgerDB.OnDisk

tests :: TestTree
tests = testGroup "BackingStore" [
    testProperty "range read keys strictly larger than prev" $
      prop_rangeReadKeysLargerThanPrev @IO @Int @Char
        QC.ioProperty
        InMemoryBackingStore
  ]

{- Possible properties:
  * All read keys should be larger than prev
  * All values in result should be a substring of the input values
  * If values exist that can be read, at least one should be read.
  * Key before first key read should not be larger than prev
  * No result if no values to read
-}

-- | All keys that are in the result of a range read should be strictly larger
-- than the largest key in @'BS.rqPrev'@.
prop_rangeReadKeysLargerThanPrev ::
     forall m k v. (Monad m, Ord k)
  => (m QC.Property -> QC.Property)
     -- ^ Extract property from a monad
  -> BackingStoreSelector m
     -- ^ The backing store to test
  -> Init m k v
     -- ^ Initialisation of a backing store
  -> SimpleRangeQuery k v
  -> QC.Property
prop_rangeReadKeysLargerThanPrev unliftProp bss ini srq =
  QC.monadic unliftProp $ do
    -- Initialise backing store and perform range read
    LedgerBackingStore bs <- QC.run $ mkLBS bss
    (_, vh)               <- QC.run $ BS.bsValueHandle bs
    readValsTables        <- QC.run $ BS.bsvhRangeRead vh rq
    let SimpleLedgerTables (ApplyValuesMK (HD.UtxoValues readVals)) =
          readValsTables

    -- Monitoring test cases
    QC.monitor $
      QC.classify (0 < Map.size readVals) "non-empty range-read results"

    -- The property to check for
    case Set.lookupMax . maybe Set.empty extractKeys $ rqPrev of
      Nothing ->
        pure True
      Just k ->
        pure $ getAll $ foldMap (All . (k<)) (Map.keys readVals)
  where
    Init{..} = ini
    SimpleRangeQuery rq@BS.RangeQuery{..} = srq

    extractKeys :: LedgerTables (SimpleLedgerState k v) KeysMK -> Set k
    extractKeys (SimpleLedgerTables (ApplyKeysMK (HD.UtxoKeys ks))) = ks

{-------------------------------------------------------------------------------
  Types
-------------------------------------------------------------------------------}

data Init m k v = Init {
    mkLBS    :: BackingStoreSelector m
             -> m (LedgerBackingStore m (SimpleLedgerState k v))
  , initVals :: LedgerTables (SimpleLedgerState k v) ValuesMK
  }

instance Show (Init m k v) where
  show _ = "Init"

newtype SimpleRangeQuery k v =
    SimpleRangeQuery (BS.RangeQuery (LedgerTables (SimpleLedgerState k v) KeysMK))
  deriving newtype Show

{-------------------------------------------------------------------------------
  Arbitrary instances
-------------------------------------------------------------------------------}

instance ( Ord k
         , Eq v
         , Show k
         , Show v
         , IOLike m
         , NoThunks k
         , NoThunks v
         , Arbitrary k
         , Arbitrary v
         )
      => Arbitrary (Init m k v) where
  arbitrary = do
      initVals <- QC.arbitrary
      let
        mkLBS bss = LedgerBackingStore <$> mkBackingStore bss initVals
      pure $ Init {..}
    where
      mkBackingStore bss vals = case bss of
        LMDBBackingStore _limits ->
          error "Init.arbitrary: not implemented"
        InMemoryBackingStore     ->
          newInMemoryBackingStore vals

instance Ord k => Arbitrary (SimpleLedgerState k v EmptyMK) where
  arbitrary = pure . SimpleLedgerState $ emptyAppliedMK SEmptyMK

instance (Ord k, Arbitrary k, Arbitrary v)
      => Arbitrary (SimpleLedgerState k v KeysMK) where
  arbitrary = SimpleLedgerState . ApplyKeysMK . HD.UtxoKeys <$> QC.arbitrary

instance (Ord k, Arbitrary k, Arbitrary v)
      => Arbitrary (SimpleLedgerState k v ValuesMK) where
  arbitrary = SimpleLedgerState . ApplyValuesMK . HD.UtxoValues <$> QC.arbitrary

instance ( Ord k
         , Eq v
         , Show k
         , Show v
         , Arbitrary (SimpleLedgerState k v mk)
         , IsApplyMapKind mk
         ) => Arbitrary (LedgerTables (SimpleLedgerState k v) mk) where
  arbitrary = projectLedgerTables <$> QC.arbitrary


instance ( Arbitrary (LedgerTables (SimpleLedgerState k v) KeysMK)
         ) => Arbitrary (SimpleRangeQuery k v) where
  arbitrary = SimpleRangeQuery <$>
    (BS.RangeQuery <$> QC.arbitrary <*> QC.arbitrary)

{-------------------------------------------------------------------------------
  Simple ledgers
-------------------------------------------------------------------------------}

-- Todo: Can we think of a more general datatype that can contain an
-- arbitrary number of states/tables, i.e., a number of tables that is not
-- fixed?
-- Todo: Should we compe up with unified test @'LedgerState'@s and
-- @'LedgerTables'@, like we are now doing for test blocks?
newtype SimpleLedgerState k v (mk :: MapKind) = SimpleLedgerState {
    lsSimple :: mk k v
  }

deriving instance (Eq (mk k v)) => Eq (SimpleLedgerState k v mk)
deriving instance (Eq (mk k v))
               => Eq (LedgerTables (SimpleLedgerState k v) mk)
deriving anyclass instance ShowLedgerState (SimpleLedgerState k v)
deriving anyclass instance ShowLedgerState (LedgerTables (SimpleLedgerState k v))
deriving instance (Show (mk k v))
               => Show (LedgerTables (SimpleLedgerState k v) mk)

instance (Ord k, Eq v)
      => TableStuff (SimpleLedgerState k v) where
  newtype LedgerTables (SimpleLedgerState k v) mk = SimpleLedgerTables {
    ltSimple :: mk k v
  } deriving Generic

  projectLedgerTables SimpleLedgerState{..} =
    SimpleLedgerTables lsSimple

  withLedgerTables st SimpleLedgerTables{..} =
    st { lsSimple = ltSimple }

  pureLedgerTables f =
    SimpleLedgerTables { ltSimple = f }

  mapLedgerTables f SimpleLedgerTables{ltSimple} =
    SimpleLedgerTables $ f ltSimple

  traverseLedgerTables f SimpleLedgerTables{ltSimple} =
    SimpleLedgerTables <$> f ltSimple

  zipLedgerTables f l r =
    SimpleLedgerTables (f (ltSimple l) (ltSimple r))

  zipLedgerTablesA f l r =
    SimpleLedgerTables <$> f (ltSimple l) (ltSimple r)

  zipLedgerTables2 f l m r =
    SimpleLedgerTables $ f (ltSimple l) (ltSimple m) (ltSimple r)

  zipLedgerTables2A f l c r =
    SimpleLedgerTables <$> f (ltSimple l) (ltSimple c) (ltSimple r)

  foldLedgerTables f SimpleLedgerTables{ltSimple} =
    f ltSimple

  foldLedgerTables2 f l r =
    f (ltSimple l) (ltSimple r)

  namesLedgerTables =
    SimpleLedgerTables { ltSimple = NameMK "ltSimple" }

deriving newtype instance NoThunks (mk k v)
               => NoThunks (LedgerTables (SimpleLedgerState k v) mk)
deriving anyclass instance SufficientSerializationForAnyBackingStore (SimpleLedgerState k v)
