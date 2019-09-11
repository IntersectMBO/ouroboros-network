{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE UndecidableInstances       #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Test.Ouroboros.Storage.LedgerDB.InMemory (
    tests
  ) where

import           Data.Word
import           Test.QuickCheck
import           Test.Tasty
import           Test.Tasty.QuickCheck

import           Ouroboros.Consensus.Protocol.Abstract (SecurityParam (..))
import           Ouroboros.Consensus.Util

import           Ouroboros.Network.Testing.Serialise (prop_serialise)

import           Ouroboros.Storage.Common

import           Ouroboros.Storage.LedgerDB.Conf
import           Ouroboros.Storage.LedgerDB.InMemory

tests :: TestTree
tests = testGroup "InMemory" [
      testGroup "Serialisation" [
          testProperty "ChainSummary" prop_serialise_ChainSummary
        ]
    , testGroup "Genesis" [
          testProperty "length"  prop_genesisLength
        , testProperty "current" prop_genesisCurrent
        ]
    , testGroup "Push" [
          testProperty "incrementsLength"       prop_pushIncrementsLength
        , testProperty "lengthMatchesNumBlocks" prop_lengthMatchesNumBlocks
        , testProperty "matchesPolicy"          prop_pushMatchesPolicy
        , testProperty "expectedLedger"         prop_pushExpectedLedger
        ]
    , testGroup "Rollback" [
          testProperty "maxRollbackGenesisZero" prop_maxRollbackGenesisZero
        , testProperty "ledgerDbMaxRollback"    prop_snapshotsMaxRollback
        , testProperty "ledgerDbMaxRollbackSat" prop_snapshotsMaxRollbackSat
        , testProperty "switchSameChain"        prop_switchSameChain
        , testProperty "switchMatchesPolicy"    prop_switchMatchesPolicy
        , testProperty "switchExpectedLedger"   prop_switchExpectedLedger
        ]
    ]

{-------------------------------------------------------------------------------
  Serialisation
-------------------------------------------------------------------------------}

prop_serialise_ChainSummary :: Trivial (ChainSummary Int Int) -> Property
prop_serialise_ChainSummary (Trivial summary) = prop_serialise summary

{-------------------------------------------------------------------------------
  Genesis
-------------------------------------------------------------------------------}

prop_genesisCurrent :: LedgerDbParams -> Property
prop_genesisCurrent params =
    ledgerDbCurrent genSnaps === initLedger
  where
    genSnaps = ledgerDbFromGenesis params initLedger

prop_genesisLength :: LedgerDbParams -> Property
prop_genesisLength params =
   ledgerDbChainLength genSnaps === 0
  where
    genSnaps = ledgerDbFromGenesis params initLedger

{-------------------------------------------------------------------------------
  Verifying shape of the Ledger DB
-------------------------------------------------------------------------------}

-- | Verify the snap of the ledger DB
--
-- No matter how many snapshots we have, we always expected a snapshot every
-- @snapEvery@ blocks.
verifyShape :: (Show r, Show l) => Word64 -> [(r, Maybe l)] -> Property
verifyShape snapEvery = \ss ->
    counterexample ("snapshots: " ++ show ss) $
      go 1 ss
  where
    go :: Word64 -> [(r, Maybe l)] -> Property
    go _ []           = property True
    go n ((_, ms):ss) =
        case (n `mod` snapEvery == 0, ms) of
          (True  , Just _ ) -> go (n + 1) ss
          (False , Nothing) -> go (n + 1) ss
          (True  , Nothing) -> counterexample "missing snapshot" $
                                 property False
          (False , Just _)  -> counterexample "unexpected snapshot" $
                                 property False

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

prop_pushMatchesPolicy :: ChainSetup 'Unsaturated -> Property
prop_pushMatchesPolicy setup@ChainSetup{..} =
    collect (saturation setup) $
      verifyShape (ledgerDbSnapEvery csParams) (ledgerDbToList csPushed)

prop_pushExpectedLedger :: ChainSetup 'Unsaturated -> Property
prop_pushExpectedLedger setup@ChainSetup{..} =
    collect (saturation setup) $
      conjoin [
          l === applyMany (expectedChain o) initLedger
        | (o, l) <- ledgerDbSnapshots csPushed
        ]
  where
    expectedChain :: Word64 -> [Block]
    expectedChain o = take (fromIntegral (csNumBlocks - o)) csChain

{-------------------------------------------------------------------------------
  Rollback
-------------------------------------------------------------------------------}

prop_maxRollbackGenesisZero :: LedgerDbParams -> Property
prop_maxRollbackGenesisZero params =
        ledgerDbMaxRollback (ledgerDbFromGenesis params (Ledger []))
    === 0

prop_snapshotsMaxRollback :: ChainSetup 'Unsaturated -> Property
prop_snapshotsMaxRollback setup@ChainSetup{..} =
    collect (saturation setup) $
      conjoin [
          (ledgerDbMaxRollback csPushed) `ge` (min k csNumBlocks)
        , (ledgerDbMaxRollback csPushed) `lt` (k + snapEvery)
        ]
  where
    SecurityParam k = ledgerDbSecurityParam csParams
    snapEvery       = ledgerDbSnapEvery     csParams

prop_snapshotsMaxRollbackSat :: ChainSetup 'Saturated -> Property
prop_snapshotsMaxRollbackSat setup@ChainSetup{..} =
    conjoin [
        saturation setup === Saturated
      , (ledgerDbMaxRollback csPushed) `ge` k
      , (ledgerDbMaxRollback csPushed) `lt` (k + snapEvery)
      ]
  where
    SecurityParam k = ledgerDbSecurityParam csParams
    snapEvery       = ledgerDbSnapEvery     csParams

prop_switchSameChain :: SwitchSetup 'Unsaturated -> Property
prop_switchSameChain setup@SwitchSetup{..} =
    collect (saturation setup) $
          ledgerDbSwitch' callbacks ssNumRollback blockInfo csPushed
      === csPushed
  where
    ChainSetup{csPushed} = ssChainSetup
    blockInfo            = ssRemoved

prop_switchMatchesPolicy :: SwitchSetup 'Unsaturated -> Property
prop_switchMatchesPolicy setup@SwitchSetup{..} =
    collect (saturation setup) $
      verifyShape (ledgerDbSnapEvery csParams) (ledgerDbToList ssSwitched)
  where
    ChainSetup{csParams} = ssChainSetup

prop_switchExpectedLedger :: SwitchSetup 'Unsaturated -> Property
prop_switchExpectedLedger setup@SwitchSetup{..} =
    collect (saturation setup) $
      conjoin [
          l === applyMany (expectedChain o) initLedger
        | (o, l) <- ledgerDbSnapshots ssSwitched
        ]
  where
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
      -- | Ledger DB parameters
      csParams    :: LedgerDbParams

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

mkTestSetup :: LedgerDbParams -> Word64 -> ChainSetup s
mkTestSetup csParams csNumBlocks =
    ChainSetup {..}
  where
    csGenSnaps = ledgerDbFromGenesis csParams initLedger
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
      params    <- arbitrary
      numBlocks <- choose (0, min (fromIntegral n) maxNumBlocks)
      return $ mkTestSetup params numBlocks
    where
      -- Maximum number of blocks we want on a chain
      maxNumBlocks :: Word64
      maxNumBlocks = 1500

  shrink ChainSetup{..} = concat [
        [ mkTestSetup csParams' csNumBlocks
        | csParams' <- shrink csParams
        ]
      , [ mkTestSetup csParams csNumBlocks'
        | csNumBlocks' <- shrink csNumBlocks
        ]
      ]

instance Arbitrary (ChainSetup 'Saturated) where
  arbitrary = do
      params <- arbitrary
      let minNumBlocks = maxRollbacks (ledgerDbSecurityParam params)
      numBlocks <- choose (minNumBlocks, minNumBlocks * 2)
      return $ mkTestSetup params numBlocks

  shrink ChainSetup{..} = concat [
        -- If we shrink the policy, it can't be that we need /more/ blocks
        [ mkTestSetup csParams' csNumBlocks
        | csParams' <- shrink csParams
        ]
        -- Shrinking the number of blocks may make the snapshots unsaturated
      , [ mkTestSetup csParams csNumBlocks'
        | csNumBlocks' <- shrink csNumBlocks
        , let minNumBlocks = maxRollbacks (ledgerDbSecurityParam csParams)
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

instance Arbitrary LedgerDbParams where
  arbitrary = do
      k <- choose (0, 6)
      snapEvery <- choose (1, 8)
      return $ LedgerDbParams {
            ledgerDbSnapEvery     = snapEvery
          , ledgerDbSecurityParam = SecurityParam k
          }
  shrink params@LedgerDbParams{..} = concat [
        [ params {ledgerDbSnapEvery = snapEvery'}
        | snapEvery' <- shrink ledgerDbSnapEvery
        , snapEvery' > 0
        ]
      , [ params {ledgerDbSecurityParam = SecurityParam k'}
        | k' <- shrink (maxRollbacks ledgerDbSecurityParam)
        ]
      ]

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

{-------------------------------------------------------------------------------
  Auxiliary
-------------------------------------------------------------------------------}

-- | Like '>=', but prints a counterexample when it fails.
ge :: (Ord a, Show a) => a -> a -> Property
x `ge` y = counterexample (show x ++ " < " ++ show y) $ x >= y

-- | Like '<', but prints a counterexample when it fails.
lt :: (Ord a, Show a) => a -> a -> Property
x `lt` y = counterexample (show x ++ " >= " ++ show y) $ x < y
