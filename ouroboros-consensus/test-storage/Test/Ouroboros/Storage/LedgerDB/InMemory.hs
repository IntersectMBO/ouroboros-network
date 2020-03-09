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

import           Data.Maybe (fromJust)
import           Data.Word
import           Test.QuickCheck
import           Test.Tasty
import           Test.Tasty.QuickCheck

import           Cardano.Slotting.Slot

import           Ouroboros.Network.Testing.Serialise (prop_serialise)

import           Ouroboros.Consensus.Protocol.Abstract (SecurityParam (..))
import           Ouroboros.Consensus.Util

import           Ouroboros.Consensus.Storage.LedgerDB.Conf
import           Ouroboros.Consensus.Storage.LedgerDB.InMemory

import           Test.Util.QuickCheck

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
        , testProperty "pastLedger"             prop_pastLedger
        ]
    , testGroup "Rollback" [
          testProperty "maxRollbackGenesisZero" prop_maxRollbackGenesisZero
        , testProperty "ledgerDbMaxRollback"    prop_snapshotsMaxRollback
        , testProperty "switchSameChain"        prop_switchSameChain
        , testProperty "switchMatchesPolicy"    prop_switchMatchesPolicy
        , testProperty "switchExpectedLedger"   prop_switchExpectedLedger
        , testProperty "pastAfterSwitch"        prop_pastAfterSwitch
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

prop_pushIncrementsLength :: ChainSetup -> Property
prop_pushIncrementsLength setup@ChainSetup{..} =
    classify (chainSetupSaturated setup) "saturated" $
          ledgerDbChainLength (ledgerDbPush' callbacks blockInfo csPushed)
      === ledgerDbChainLength csPushed + 1
  where
    blockInfo = Block maxBound 0

prop_lengthMatchesNumBlocks :: ChainSetup -> Property
prop_lengthMatchesNumBlocks setup@ChainSetup{..} =
    classify (chainSetupSaturated setup) "saturated" $
          ledgerDbChainLength csPushed
      === csNumBlocks

prop_pushMatchesPolicy :: ChainSetup -> Property
prop_pushMatchesPolicy setup@ChainSetup{..} =
    classify (chainSetupSaturated setup) "saturated" $
      verifyShape (ledgerDbSnapEvery csParams) (ledgerDbToList csPushed)

prop_pushExpectedLedger :: ChainSetup -> Property
prop_pushExpectedLedger setup@ChainSetup{..} =
    classify (chainSetupSaturated setup) "saturated" $
      conjoin [
          l === applyMany (expectedChain o) initLedger
        | (o, l) <- ledgerDbSnapshots csPushed
        ]
  where
    expectedChain :: Word64 -> [Block]
    expectedChain o = take (fromIntegral (csNumBlocks - o)) csChain

prop_pastLedger :: ChainSetup -> Property
prop_pastLedger setup@ChainSetup{..} =
    classify (chainSetupSaturated setup) "saturated"    $
    classify withinReach                 "within reach" $
          ledgerDbPast' callbacks tip csPushed
      === if withinReach
            then Just (ledgerDbCurrent afterPrefix)
            else Nothing
  where
    prefix :: [Block]
    prefix = take (fromIntegral csPrefixLen) csChain

    tip :: WithOrigin Block
    tip = case prefix of
            []         -> Origin
            _otherwise -> At (last prefix)

    afterPrefix :: LedgerDB Ledger Block
    afterPrefix = ledgerDbPushMany' callbacks prefix csGenSnaps

    -- Maximum rollback can be at most k + snapEvery
    -- See 'prop_snapshotsMaxRollback'
    withinReach :: Bool
    withinReach = (csNumBlocks - csPrefixLen) <= ledgerDbMaxRollback csPushed

{-------------------------------------------------------------------------------
  Rollback
-------------------------------------------------------------------------------}

prop_maxRollbackGenesisZero :: LedgerDbParams -> Property
prop_maxRollbackGenesisZero params =
        ledgerDbMaxRollback (ledgerDbFromGenesis params (Ledger []))
    === 0

prop_snapshotsMaxRollback :: ChainSetup -> Property
prop_snapshotsMaxRollback setup@ChainSetup{..} =
    classify (chainSetupSaturated setup) "saturated" $
      conjoin [
          if chainSetupSaturated setup
            then (ledgerDbMaxRollback csPushed) `ge` k
            else (ledgerDbMaxRollback csPushed) `ge` (min k csNumBlocks)
        , (ledgerDbMaxRollback csPushed) `lt` (k + snapEvery)
        ]
  where
    SecurityParam k = ledgerDbSecurityParam csParams
    snapEvery       = ledgerDbSnapEvery     csParams

prop_switchSameChain :: SwitchSetup -> Property
prop_switchSameChain setup@SwitchSetup{..} =
    classify (switchSetupSaturated setup) "saturated" $
          ledgerDbSwitch' callbacks ssNumRollback blockInfo csPushed
      === Just csPushed
  where
    ChainSetup{csPushed} = ssChainSetup
    blockInfo            = ssRemoved

prop_switchMatchesPolicy :: SwitchSetup -> Property
prop_switchMatchesPolicy setup@SwitchSetup{..} =
    classify (switchSetupSaturated setup) "saturated" $
      verifyShape (ledgerDbSnapEvery csParams) (ledgerDbToList ssSwitched)
  where
    ChainSetup{csParams} = ssChainSetup

prop_switchExpectedLedger :: SwitchSetup -> Property
prop_switchExpectedLedger setup@SwitchSetup{..} =
    classify (switchSetupSaturated setup) "saturated" $
      conjoin [
          l === applyMany (expectedChain o) initLedger
        | (o, l) <- ledgerDbSnapshots ssSwitched
        ]
  where
    expectedChain :: Word64 -> [Block]
    expectedChain o = take (fromIntegral (ssNumBlocks - o)) ssChain

-- | Check 'prop_pastLedger' still holds after switching to a fork
prop_pastAfterSwitch :: SwitchSetup -> Property
prop_pastAfterSwitch setup@SwitchSetup{..} =
    classify (switchSetupSaturated setup) "saturated"    $
    classify withinReach                  "within reach" $
          ledgerDbPast' callbacks tip ssSwitched
      === if withinReach
            then Just (ledgerDbCurrent afterPrefix)
            else Nothing
  where
    prefix :: [Block]
    prefix = take (fromIntegral ssPrefixLen) ssChain

    tip :: WithOrigin Block
    tip = case prefix of
            []         -> Origin
            _otherwise -> At (last prefix)

    afterPrefix :: LedgerDB Ledger Block
    afterPrefix = ledgerDbPushMany' callbacks prefix (csGenSnaps ssChainSetup)

    -- Maximum rollback can be at most k + snapEvery
    -- See 'prop_snapshotsMaxRollback'
    withinReach :: Bool
    withinReach = (ssNumBlocks - ssPrefixLen) <= ledgerDbMaxRollback ssSwitched

{-------------------------------------------------------------------------------
  Test setup
-------------------------------------------------------------------------------}

data ChainSetup = ChainSetup {
      -- | Ledger DB parameters
      csParams    :: LedgerDbParams

      -- | Number of blocks applied
    , csNumBlocks :: Word64

      -- | Some prefix of the chain
      --
      -- Although we choose this to be less than or equal to 'csNumBlocks',
      -- we don't guarantee this during shrinking. If 'csPrefixLen' is larger
      -- than 'csNumBlocks', the prefix should simply be considered to be the
      -- entire chain.
    , csPrefixLen :: Word64

      -- | Derived: genesis snapshots
    , csGenSnaps  :: LedgerDB Ledger Block

      -- | Derived: the actual blocks that got applied (old to new)
    , csChain     :: [Block]

      -- | Derived: the snapshots after all blocks were applied
    , csPushed    :: LedgerDB Ledger Block
    }
  deriving (Show)

chainSetupSaturated :: ChainSetup -> Bool
chainSetupSaturated = ledgerDbIsSaturated . csPushed

data SwitchSetup = SwitchSetup {
      -- | Chain setup
      ssChainSetup  :: ChainSetup

      -- | Number of blocks to roll back
    , ssNumRollback :: Word64

      -- | Number of new blocks (to be applied after the rollback)
    , ssNumNew      :: Word64

      -- | Prefix of the new chain
      --
      -- See also 'csPrefixLen'
    , ssPrefixLen   :: Word64

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

switchSetupSaturated :: SwitchSetup -> Bool
switchSetupSaturated = chainSetupSaturated . ssChainSetup

mkTestSetup :: LedgerDbParams -> Word64 -> Word64 -> ChainSetup
mkTestSetup csParams csNumBlocks csPrefixLen =
    ChainSetup {..}
  where
    csGenSnaps = ledgerDbFromGenesis csParams initLedger
    csChain    = mkChain csNumBlocks 0
    csPushed   = ledgerDbPushMany' callbacks csChain csGenSnaps

mkRollbackSetup :: ChainSetup -> Word64 -> Word64 -> Word64 -> SwitchSetup
mkRollbackSetup ssChainSetup ssNumRollback ssNumNew ssPrefixLen =
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
    ssSwitched  = fromJust $ ledgerDbSwitch' callbacks ssNumRollback ssNewBlocks csPushed

instance Arbitrary ChainSetup where
  arbitrary = do
      params <- arbitrary
      let k = maxRollbacks (ledgerDbSecurityParam params)
      numBlocks <- choose (0, k * 2)
      prefixLen <- choose (0, numBlocks)
      return $ mkTestSetup params numBlocks prefixLen

  shrink ChainSetup{..} = concat [
        -- Shrink the policy
        [ mkTestSetup csParams' csNumBlocks csPrefixLen
        | csParams' <- shrink csParams
        ]

        -- Reduce number of blocks
      , [ mkTestSetup csParams csNumBlocks' csPrefixLen
        | csNumBlocks' <- shrink csNumBlocks
        ]
      ]

instance Arbitrary SwitchSetup where
  arbitrary = do
      chainSetup  <- arbitrary
      numRollback <- choose (0, ledgerDbMaxRollback (csPushed chainSetup))
      numNew      <- choose (numRollback, 2 * numRollback)
      prefixLen   <- choose (0, csNumBlocks chainSetup - numRollback + numNew)
      return $ mkRollbackSetup chainSetup numRollback numNew prefixLen

  shrink SwitchSetup{..} = concat [
        -- If we shrink the chain setup, we might restrict max rollback
        [ mkRollbackSetup ssChainSetup' ssNumRollback ssNumNew ssPrefixLen
        | ssChainSetup' <- shrink ssChainSetup
        , ssNumRollback <= ledgerDbMaxRollback (csPushed ssChainSetup')
        ]
        -- Number of new blocks must be at least the rollback
      , [ mkRollbackSetup ssChainSetup ssNumRollback ssNumNew' ssPrefixLen
        | ssNumNew' <- shrink ssNumNew
        , ssNumNew' >= ssNumRollback
        ]
        -- But rolling back less is always possible
      , [ mkRollbackSetup ssChainSetup ssNumRollback' ssNumNew ssPrefixLen
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

instance Arbitrary (Trivial (WithOrigin Int)) where
  arbitrary = fmap Trivial $ do
                gen <- arbitrary
                if gen then return Origin
                       else At <$> arbitrary
