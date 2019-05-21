{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE UndecidableInstances       #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Test.Ouroboros.Storage.LedgerDB.InMemory (
    tests
  ) where

import           Data.Word
import           GHC.Stack
import           Test.QuickCheck
import           Test.Tasty
import           Test.Tasty.QuickCheck

import           Ouroboros.Consensus.Protocol.Abstract (SecurityParam (..))
import           Ouroboros.Consensus.Util

import           Ouroboros.Network.Testing.Serialise (prop_serialise)

import           Ouroboros.Storage.Common

import           Ouroboros.Storage.LedgerDB.Conf
import           Ouroboros.Storage.LedgerDB.InMemory
import           Ouroboros.Storage.LedgerDB.MemPolicy
import           Ouroboros.Storage.LedgerDB.Offsets

tests :: TestTree
tests = testGroup "InMemory" [
      testGroup "Serialisation" [
          testProperty "ChainSummary" prop_serialise_ChainSummary
        ]
    , testGroup "MemPolicy" [
          testProperty "invariant"     prop_memPolicy_invariant
        , testProperty "fromToSkips"   prop_memPolicy_fromToSkips
        , testProperty "toFromSkips"   prop_memPolicy_toFromSkips
        , testProperty "fromToOffsets" prop_memPolicy_fromToOffsets
        , testProperty "toFromOffsets" prop_memPolicy_toFromOffsets
        ]
    , testGroup "Genesis" [
          testProperty "length"  prop_genesisLength
        , testProperty "current" prop_genesisCurrent
        ]
    , testGroup "Push" [
          testProperty "incrementsLength"       prop_pushIncrementsLength
        , testProperty "preservesShape"         prop_pushPreservesShape
        , testProperty "lengthMatchesNumBlocks" prop_lengthMatchesNumBlocks
        , testProperty "shapeMatchesPolicy"     prop_shapeMatchesPolicy
        , testProperty "matchesPolicy"          prop_pushMatchesPolicy
        , testProperty "expectedLedger"         prop_pushExpectedLedger
        ]
    , testGroup "Rollback" [
          testProperty "maxRollbackGenesisZero" prop_maxRollbackGenesisZero
        , testProperty "ledgerDbMaxRollback"    prop_snapshotsMaxRollback
        , testProperty "ledgerDbMaxRollbackSat" prop_snapshotsMaxRollbackSat
        , testProperty "toFromSuffix"           prop_toFromSuffix
        , testProperty "preservesShape"         prop_rollbackPreservesShape
        , testProperty "switchSameChain"        prop_switchSameChain
        , testProperty "switchExpectedLedger"   prop_switchExpectedLedger
        ]
    ]

{-------------------------------------------------------------------------------
  Serialisation
-------------------------------------------------------------------------------}

prop_serialise_ChainSummary :: Trivial (ChainSummary Int Int) -> Property
prop_serialise_ChainSummary (Trivial summary) = prop_serialise summary

{-------------------------------------------------------------------------------
  Memory policy
-------------------------------------------------------------------------------}

-- | Check the generator
prop_memPolicy_invariant :: MemPolicy -> Property
prop_memPolicy_invariant = (Right () ===) . memPolicyVerify

prop_memPolicy_fromToSkips :: [Word64] -> Property
prop_memPolicy_fromToSkips skips =
    memPolicyToSkips (memPolicyFromSkips skips) === skips

prop_memPolicy_toFromSkips :: MemPolicy -> Property
prop_memPolicy_toFromSkips policy =
    memPolicyFromSkips (memPolicyToSkips policy) === policy

prop_memPolicy_fromToOffsets :: Offsets () -> Property
prop_memPolicy_fromToOffsets offsets =
    memPolicyToOffsets (memPolicyFromOffsets offsets) === offsets

prop_memPolicy_toFromOffsets :: MemPolicy -> Property
prop_memPolicy_toFromOffsets policy =
    memPolicyFromOffsets (memPolicyToOffsets policy) === policy

{-------------------------------------------------------------------------------
  Genesis
-------------------------------------------------------------------------------}

prop_genesisCurrent :: MemPolicy -> Property
prop_genesisCurrent policy =
    ledgerDbCurrent genSnaps === initLedger
  where
    genSnaps = ledgerDbFromGenesis policy initLedger

prop_genesisLength :: MemPolicy -> Property
prop_genesisLength policy =
   ledgerDbChainLength genSnaps === 0
  where
    genSnaps = ledgerDbFromGenesis policy initLedger

{-------------------------------------------------------------------------------
  Constructing snapshots
-------------------------------------------------------------------------------}

prop_pushIncrementsLength :: ChainSetup 'Unsaturated -> Property
prop_pushIncrementsLength setup@ChainSetup{..} =
    collect (saturation setup) $
          ledgerDbChainLength (ledgerDbPush' callbacks blockInfo csPushed)
      === ledgerDbChainLength csPushed + 1
  where
    blockInfo = Block maxBound 0

prop_lengthMatchesNumBlocks :: ChainSetup 'Unsaturated -> Property
prop_lengthMatchesNumBlocks setup@ChainSetup{..} =
    collect (saturation setup) $
          ledgerDbChainLength csPushed
      === csNumBlocks

prop_shapeMatchesPolicy :: ChainSetup 'Unsaturated -> Property
prop_shapeMatchesPolicy setup@ChainSetup{..} =
    collect (saturation setup) $
          snapshotsShape csPushed
      === memPolicyShape csPolicy

prop_pushPreservesShape :: ChainSetup 'Unsaturated -> Property
prop_pushPreservesShape setup@ChainSetup{..} =
    collect (saturation setup) $
          snapshotsShape (ledgerDbPush' callbacks blockInfo csPushed)
      === snapshotsShape csPushed
  where
    blockInfo = Block maxBound 0

prop_pushMatchesPolicy :: ChainSetup 'Saturated -> Property
prop_pushMatchesPolicy setup@ChainSetup{..} =
    conjoin [
        saturation setup === Saturated
      ,     (const () <$> ledgerDbToList csPushed)
        === memPolicyToOffsets csPolicy
      ]

prop_pushExpectedLedger :: ChainSetup 'Unsaturated -> Property
prop_pushExpectedLedger setup@ChainSetup{..} =
    collect (saturation setup) $
      conjoin [
          l === applyMany (expectedChain o) initLedger
        | (o, l) <- offsetsToPairs $ ledgerDbToList csPushed
        ]
  where
    expectedChain :: Word64 -> [Block]
    expectedChain o = take (fromIntegral (csNumBlocks - o)) csChain

{-------------------------------------------------------------------------------
  Rollback
-------------------------------------------------------------------------------}

prop_maxRollbackGenesisZero :: MemPolicy -> Property
prop_maxRollbackGenesisZero policy =
        ledgerDbMaxRollback (ledgerDbFromGenesis policy (Ledger []))
    === 0

prop_snapshotsMaxRollback :: ChainSetup 'Unsaturated -> Property
prop_snapshotsMaxRollback setup@ChainSetup{..} =
    collect (saturation setup) $
          ledgerDbMaxRollback csPushed
      === min (memPolicyMaxRollback csPolicy) csNumBlocks

prop_snapshotsMaxRollbackSat :: ChainSetup 'Saturated -> Property
prop_snapshotsMaxRollbackSat setup@ChainSetup{..} =
    conjoin [
        saturation setup === Saturated
      , ledgerDbMaxRollback csPushed === memPolicyMaxRollback csPolicy
      ]

prop_toFromSuffix :: ChainSetup 'Unsaturated -> Property
prop_toFromSuffix setup@ChainSetup{..} =
    collect (saturation setup) $
          fromSuffix' neverCalled [] (toSuffix csPushed)
      === csPushed

prop_rollbackPreservesShape :: SwitchSetup 'Unsaturated -> Property
prop_rollbackPreservesShape setup@SwitchSetup{..} =
    collect (saturation setup) $
          snapshotsShape csPushed
      === snapshotsShape ssSwitched
  where
    ChainSetup{..} = ssChainSetup

prop_switchSameChain :: SwitchSetup 'Unsaturated -> Property
prop_switchSameChain setup@SwitchSetup{..} =
    collect (saturation setup) $
          ledgerDbSwitch' callbacks ssNumRollback blockInfo csPushed
      === csPushed
  where
    ChainSetup{..} = ssChainSetup
    blockInfo      = ssRemoved

prop_switchExpectedLedger :: SwitchSetup 'Unsaturated -> Property
prop_switchExpectedLedger setup@SwitchSetup{..} =
    collect (saturation setup) $
      conjoin [
          l === applyMany (expectedChain o) initLedger
        | (o, l) <- offsetsToPairs $ ledgerDbToList ssSwitched
        ]
  where
    ChainSetup{..} = ssChainSetup

    expectedChain :: Word64 -> [Block]
    expectedChain o = take (fromIntegral (ssNumBlocks - o)) ssChain

{-------------------------------------------------------------------------------
  Saturation
-------------------------------------------------------------------------------}

data Saturation =
    -- | Saturated (all snapshots have been filled)
    Saturated

    -- | Unsaturated (not all snapshots may have been filled)
  | Unsaturated
  deriving (Show, Eq)

class CheckSaturation (f :: Saturation -> *) where
  saturation :: f s -> Saturation

{-------------------------------------------------------------------------------
  Test setup
-------------------------------------------------------------------------------}

data ChainSetup (s :: Saturation) = ChainSetup {
      -- | Memory policy
      csPolicy    :: MemPolicy

      -- | Number of blocks applied
    , csNumBlocks :: Word64

      -- | Derived: genesis snapshots
    , csGenSnaps  :: LedgerDB Ledger Block

      -- | Derived: the actual blocks that got applied (old to new)
    , csChain     :: [Block]

      -- | Derived: the snapshots after all blocks were applied
    , csPushed    :: LedgerDB Ledger Block
    }
  deriving (Show)

instance CheckSaturation ChainSetup where
  saturation ChainSetup{..}
    | ledgerDbIsSaturated csPushed = Saturated
    | otherwise                    = Unsaturated

data SwitchSetup s = SwitchSetup {
      -- | Chain setup
      ssChainSetup  :: ChainSetup s

      -- | Number of blocks to roll back
    , ssNumRollback :: Word64

      -- | Number of new blocks (to be applied after the rollback)
    , ssNumNew      :: Word64

      -- | Derived: number of blocks in the new chain
    , ssNumBlocks   :: Word64

      -- | Derived: the blocks that were removed
    , ssRemoved     :: [Block]

      -- | Derived: the new blocks themselves
    , ssNewBlocks   :: [Block]

      -- | Derived: the full chain after switching to this fork
    , ssChain       :: [Block]

      -- | Derived; the snapshots after the switch was performed
    , ssSwitched    :: LedgerDB Ledger Block
    }
  deriving (Show)

instance CheckSaturation SwitchSetup where
  saturation = saturation . ssChainSetup

mkTestSetup :: MemPolicy -> Word64 -> ChainSetup s
mkTestSetup csPolicy csNumBlocks =
    ChainSetup {..}
  where
    csGenSnaps = ledgerDbFromGenesis csPolicy initLedger
    csChain    = mkChain csNumBlocks 0
    csPushed   = ledgerDbPushMany' callbacks csChain csGenSnaps

mkRollbackSetup :: ChainSetup s -> Word64 -> Word64 -> SwitchSetup s
mkRollbackSetup ssChainSetup ssNumRollback ssNumNew =
    SwitchSetup {..}
  where
    ChainSetup{..} = ssChainSetup

    ssNumBlocks = csNumBlocks - ssNumRollback + ssNumNew
    ssRemoved   = takeLast ssNumRollback csChain
    ssNewBlocks = drop (fromIntegral (csNumBlocks - ssNumRollback)) $
                    mkChain ssNumBlocks 1
    ssChain     = concat [
                         take (fromIntegral (csNumBlocks - ssNumRollback)) csChain
                       , ssNewBlocks
                       ]
    ssSwitched  = ledgerDbSwitch' callbacks ssNumRollback ssNewBlocks csPushed

instance Arbitrary (ChainSetup 'Unsaturated) where
  arbitrary = sized $ \n -> do
      policy    <- arbitrary
      numBlocks <- choose (0, min (fromIntegral n) maxNumBlocks)
      return $ mkTestSetup policy numBlocks

  shrink ChainSetup{..} = concat [
        [ mkTestSetup csPolicy' csNumBlocks
        | csPolicy' <- shrink csPolicy
        ]
      , [ mkTestSetup csPolicy csNumBlocks'
        | csNumBlocks' <- shrink csNumBlocks
        ]
      ]

instance Arbitrary (ChainSetup 'Saturated) where
  arbitrary = do
      policy    <- arbitrary
      let minNumBlocks = memPolicyMaxRollback policy
      numBlocks <- choose (minNumBlocks, minNumBlocks * 2)
      return $ mkTestSetup policy numBlocks

  shrink ChainSetup{..} = concat [
        -- If we shrink the policy, it can't be that we need /more/ blocks
        [ mkTestSetup csPolicy' csNumBlocks
        | csPolicy' <- shrink csPolicy
        ]
        -- Shrinking the number of blocks may make the snapshots unsaturated
      , [ mkTestSetup csPolicy csNumBlocks'
        | csNumBlocks' <- shrink csNumBlocks
        , let minNumBlocks = memPolicyMaxRollback csPolicy
        , csNumBlocks' >= minNumBlocks
        ]
      ]

instance Arbitrary (ChainSetup s) => Arbitrary (SwitchSetup s) where
  arbitrary = do
      chainSetup  <- arbitrary
      numRollback <- choose (0, ledgerDbMaxRollback (csPushed chainSetup))
      numNew      <- choose (numRollback, 2 * numRollback)
      return $ mkRollbackSetup chainSetup numRollback numNew

  shrink SwitchSetup{..} = concat [
        -- If we shrink the chain setup, we might restrict max rollback
        [ mkRollbackSetup ssChainSetup' ssNumRollback ssNumNew
        | ssChainSetup' <- shrink ssChainSetup
        , ssNumRollback <= ledgerDbMaxRollback (csPushed ssChainSetup')
        ]
        -- Number of new blocks must be at least the rollback
      , [ mkRollbackSetup ssChainSetup ssNumRollback ssNumNew'
        | ssNumNew' <- shrink ssNumNew
        , ssNumNew' >= ssNumRollback
        ]
        -- But rolling back less is always possible
      , [ mkRollbackSetup ssChainSetup ssNumRollback' ssNumNew
        | ssNumRollback' <- shrink ssNumRollback
        ]
      ]

-- | Block
--
-- For the purposes of the tests of the ledger DB, we don't really care what's
-- inside blocks; we simply mark blocks with their position in the chain and
-- the number of the fork. For example,
--
-- > Block 2 0
--
-- is the second block in the chain, on fork 0.
data Block = Block Word64 Word64
  deriving (Show, Eq)

mkChain :: Word64 -> Word64 -> [Block]
mkChain numBlocks fork = [Block i fork | i <- [1 .. numBlocks]]

-- | "Free" ledger
--
-- This ledger is free in that we don't interpret in any way the " blocks " that
-- we apply, we merely record them.
newtype Ledger = Ledger [Block]
  deriving (Show, Eq)

initLedger :: Ledger
initLedger = Ledger []

-- | Apply a block
apply :: Block -> Ledger -> Ledger
apply b (Ledger bs) = Ledger (b:bs)

-- | Apply a list of blocks (old to new)
applyMany :: [Block] -> Ledger -> Ledger
applyMany = repeatedly apply

callbacks :: PureLedgerDbConf Ledger Block
callbacks = pureLedgerDbConf initLedger apply

{-------------------------------------------------------------------------------
  Orphan Arbitrary instances
-------------------------------------------------------------------------------}

instance Arbitrary a => Arbitrary (Offsets a) where
  arbitrary = do
      offsets <- arbitrary
      offsetsFromPairs <$> go 0 (imposeLimits offsets)
    where
      imposeLimits :: [Word64] -> [Word64]
      imposeLimits = take maxNumSnapshots . map (min maxSkip) . (0 :)

      -- Make sure the list is strictly monotonically increasing
      go :: Word64 -> [Word64] -> Gen [(Word64, a)]
      go _ []     = return []
      go p (o:os) = do a    <- arbitrary
                       rest <- go (p + o + 1) os
                       return (((p + o), a) : rest)

  shrink = map offsetsFromPairs
         . filter isValid
         . shrink
         . offsetsToPairs
    where
      isValid ((0, _):_) = True
      isValid _otherwise = False

instance Arbitrary MemPolicy where
  arbitrary = oneof [
        -- Sufficiently often return a small mempolicy to keep examples small
        return $ defaultMemPolicy (SecurityParam 3)
      , memPolicyFromSkips . imposeLimits <$> arbitrary
      ]
    where
      imposeLimits :: [Word64] -> [Word64]
      imposeLimits = take maxNumSnapshots . map (min maxSkip) . (0 :)

  shrink = filter memPolicyValid
         . map memPolicyFromSkips
         . shrink
         . memPolicyToSkips

{-------------------------------------------------------------------------------
  Testing constants
-------------------------------------------------------------------------------}

-- | Maximum number of snapshots we want to keep
maxNumSnapshots :: Int
maxNumSnapshots = 10

-- | Maximum distance between the snapshots
maxSkip :: Word64
maxSkip = 100

-- | Maximum number of blocks we want on a chain
maxNumBlocks :: Word64
maxNumBlocks = 1500

{-------------------------------------------------------------------------------
  Auxiliary
-------------------------------------------------------------------------------}

neverCalled :: HasCallStack => a
neverCalled = error "this should not be called"

{-------------------------------------------------------------------------------
  Serialisation roundtrip
-------------------------------------------------------------------------------}

-- | Marker that we're not recording anything interesting, but merely testing
-- roundtrip properties
newtype Trivial a = Trivial { trivial :: a }
  deriving (Show)

instance Arbitrary (Trivial (ChainSummary Int Int)) where
  arbitrary = fmap Trivial $
                ChainSummary <$> (trivial <$> arbitrary)
                             <*> arbitrary
                             <*> arbitrary

instance Arbitrary (Trivial (Tip Int)) where
  arbitrary = fmap Trivial $ do
                gen <- arbitrary
                if gen then return TipGen
                       else Tip <$> arbitrary
