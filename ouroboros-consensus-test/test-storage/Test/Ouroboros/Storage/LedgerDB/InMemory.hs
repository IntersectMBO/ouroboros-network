{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE UndecidableInstances       #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Ouroboros.Storage.LedgerDB.InMemory (tests) where

import           Codec.CBOR.FlatTerm (FlatTerm, TermToken (..), fromFlatTerm,
                     toFlatTerm)
import           Codec.Serialise (decode, encode)
import           Data.Maybe (fromJust)
import           Data.Word
import           Test.QuickCheck
import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.BlockchainTime
import           Ouroboros.Consensus.Config
import qualified Ouroboros.Consensus.HardFork.History as HardFork
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Util

import           Ouroboros.Consensus.Storage.LedgerDB.InMemory

import           Test.Util.QuickCheck
import           Test.Util.TestBlock

tests :: TestTree
tests = testGroup "InMemory" [
      testGroup "Serialisation" [
          testCase     "encode"                 test_encode_ledger
        , testCase     "decode"                 test_decode_ledger
        , testCase     "decode ChainSummary"    test_decode_ChainSummary
        ]
    , testGroup "Genesis" [
          testProperty "current"                prop_genesisCurrent
        ]
    , testGroup "Push" [
          testProperty "expectedLedger"         prop_pushExpectedLedger
        , testProperty "pastLedger"             prop_pastLedger
        ]
    , testGroup "Rollback" [
          testProperty "maxRollbackGenesisZero" prop_maxRollbackGenesisZero
        , testProperty "ledgerDbMaxRollback"    prop_snapshotsMaxRollback
        , testProperty "switchSameChain"        prop_switchSameChain
        , testProperty "switchExpectedLedger"   prop_switchExpectedLedger
        , testProperty "pastAfterSwitch"        prop_pastAfterSwitch
        ]
    ]

{-------------------------------------------------------------------------------
  Serialisation
-------------------------------------------------------------------------------}

-- | The LedgerDB is parametric in the ledger @l@. We use @Int@ for simplicity.
example_ledger :: Int
example_ledger = 100

golden_ledger :: FlatTerm
golden_ledger =
    [ TkListLen 2
      -- VersionNumber
    , TkInt 1
      -- ledger: Int
    , TkInt 100
    ]

-- | The old format based on the @ChainSummary@. To remain backwards compatible
-- we still accept this old format.
golden_ChainSummary :: FlatTerm
golden_ChainSummary =
    [ TkListLen 3
      -- tip: WithOrigin (RealPoint TestBlock)
    , TkListLen 1
    , TkListLen 2
    , TkInt 3
    , TkListBegin, TkInt 0, TkInt 0, TkBreak
      -- chain length: Word64
    , TkInt 10
      -- ledger: Int for simplicity
    , TkInt 100
    ]

test_encode_ledger :: Assertion
test_encode_ledger =
    toFlatTerm (enc example_ledger) @?= golden_ledger
  where
    enc = encodeSnapshot encode

test_decode_ledger :: Assertion
test_decode_ledger =
    fromFlatTerm dec golden_ledger @?= Right example_ledger
  where
    dec = decodeSnapshotBackwardsCompatible (Proxy @TestBlock) decode decode

-- | For backwards compatibility
test_decode_ChainSummary :: Assertion
test_decode_ChainSummary =
    fromFlatTerm dec golden_ChainSummary @?= Right example_ledger
  where
    dec = decodeSnapshotBackwardsCompatible (Proxy @TestBlock) decode decode

{-------------------------------------------------------------------------------
  Genesis
-------------------------------------------------------------------------------}

prop_genesisCurrent :: Property
prop_genesisCurrent =
    ledgerDbCurrent genSnaps === testInitLedger
  where
    genSnaps = ledgerDbWithAnchor testInitLedger

{-------------------------------------------------------------------------------
  Constructing snapshots
-------------------------------------------------------------------------------}

prop_pushExpectedLedger :: ChainSetup -> Property
prop_pushExpectedLedger setup@ChainSetup{..} =
    classify (chainSetupSaturated setup) "saturated" $
      conjoin [
          l === refoldLedger cfg (expectedChain o) testInitLedger
        | (o, l) <- ledgerDbSnapshots csPushed
        ]
  where
    expectedChain :: Word64 -> [TestBlock]
    expectedChain o = take (fromIntegral (csNumBlocks - o)) csChain

    cfg :: LedgerConfig TestBlock
    cfg = ledgerDbCfg (csBlockConfig setup)

prop_pastLedger :: ChainSetup -> Property
prop_pastLedger setup@ChainSetup{..} =
    classify (chainSetupSaturated setup) "saturated"    $
    classify withinReach                 "within reach" $
          ledgerDbPast tip csPushed
      === if withinReach
            then Just (ledgerDbCurrent afterPrefix)
            else Nothing
  where
    prefix :: [TestBlock]
    prefix = take (fromIntegral csPrefixLen) csChain

    tip :: Point TestBlock
    tip = maybe GenesisPoint blockPoint (lastMaybe prefix)

    afterPrefix :: LedgerDB (LedgerState TestBlock)
    afterPrefix = ledgerDbPushMany' (csBlockConfig setup) prefix csGenSnaps

    -- See 'prop_snapshotsMaxRollback'
    withinReach :: Bool
    withinReach = (csNumBlocks - csPrefixLen) <= ledgerDbMaxRollback csPushed

{-------------------------------------------------------------------------------
  Rollback
-------------------------------------------------------------------------------}

prop_maxRollbackGenesisZero :: Property
prop_maxRollbackGenesisZero =
        ledgerDbMaxRollback (ledgerDbWithAnchor testInitLedger)
    === 0

prop_snapshotsMaxRollback :: ChainSetup -> Property
prop_snapshotsMaxRollback setup@ChainSetup{..} =
    classify (chainSetupSaturated setup) "saturated" $
      conjoin [
          if chainSetupSaturated setup
            then (ledgerDbMaxRollback csPushed) `ge` k
            else (ledgerDbMaxRollback csPushed) `ge` (min k csNumBlocks)
        , (ledgerDbMaxRollback csPushed) `le` k
        ]
  where
    SecurityParam k = csSecParam

prop_switchSameChain :: SwitchSetup -> Property
prop_switchSameChain setup@SwitchSetup{..} =
    classify (switchSetupSaturated setup) "saturated" $
          ledgerDbSwitch' (csBlockConfig ssChainSetup) ssNumRollback blockInfo csPushed
      === Just csPushed
  where
    ChainSetup{csPushed} = ssChainSetup
    blockInfo            = ssRemoved

prop_switchExpectedLedger :: SwitchSetup -> Property
prop_switchExpectedLedger setup@SwitchSetup{..} =
    classify (switchSetupSaturated setup) "saturated" $
      conjoin [
          l === refoldLedger cfg (expectedChain o) testInitLedger
        | (o, l) <- ledgerDbSnapshots ssSwitched
        ]
  where
    expectedChain :: Word64 -> [TestBlock]
    expectedChain o = take (fromIntegral (ssNumBlocks - o)) ssChain

    cfg :: LedgerConfig TestBlock
    cfg = ledgerDbCfg (csBlockConfig ssChainSetup)

-- | Check 'prop_pastLedger' still holds after switching to a fork
prop_pastAfterSwitch :: SwitchSetup -> Property
prop_pastAfterSwitch setup@SwitchSetup{..} =
    classify (switchSetupSaturated setup) "saturated"    $
    classify withinReach                  "within reach" $
          ledgerDbPast tip ssSwitched
      === if withinReach
            then Just (ledgerDbCurrent afterPrefix)
            else Nothing
  where
    prefix :: [TestBlock]
    prefix = take (fromIntegral ssPrefixLen) ssChain

    tip :: Point TestBlock
    tip = maybe GenesisPoint blockPoint (lastMaybe prefix)

    afterPrefix :: LedgerDB (LedgerState TestBlock)
    afterPrefix = ledgerDbPushMany' (csBlockConfig ssChainSetup) prefix (csGenSnaps ssChainSetup)

    -- See 'prop_snapshotsMaxRollback'
    withinReach :: Bool
    withinReach = (ssNumBlocks - ssPrefixLen) <= ledgerDbMaxRollback ssSwitched

{-------------------------------------------------------------------------------
  Test setup
-------------------------------------------------------------------------------}

data ChainSetup = ChainSetup {
      -- | Security parameter
      csSecParam  :: SecurityParam

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
    , csGenSnaps  :: LedgerDB (LedgerState TestBlock)

      -- | Derived: the actual blocks that got applied (old to new)
    , csChain     :: [TestBlock]

      -- | Derived: the snapshots after all blocks were applied
    , csPushed    :: LedgerDB (LedgerState TestBlock)
    }
  deriving (Show)

csBlockConfig :: ChainSetup -> LedgerDbCfg (LedgerState TestBlock)
csBlockConfig = csBlockConfig' . csSecParam

csBlockConfig' :: SecurityParam -> LedgerDbCfg (LedgerState TestBlock)
csBlockConfig' secParam = LedgerDbCfg {
      ledgerDbCfgSecParam = secParam
    , ledgerDbCfg         = HardFork.defaultEraParams secParam slotLength
    }
  where
    slotLength = slotLengthFromSec 20

chainSetupSaturated :: ChainSetup -> Bool
chainSetupSaturated ChainSetup{..} = ledgerDbIsSaturated csSecParam csPushed

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
    , ssRemoved     :: [TestBlock]

      -- | Derived: the new blocks themselves
    , ssNewBlocks   :: [TestBlock]

      -- | Derived: the full chain after switching to this fork
    , ssChain       :: [TestBlock]

      -- | Derived; the snapshots after the switch was performed
    , ssSwitched    :: LedgerDB (LedgerState TestBlock)
    }
  deriving (Show)

switchSetupSaturated :: SwitchSetup -> Bool
switchSetupSaturated = chainSetupSaturated . ssChainSetup

mkTestSetup :: SecurityParam -> Word64 -> Word64 -> ChainSetup
mkTestSetup csSecParam csNumBlocks csPrefixLen =
    ChainSetup {..}
  where
    csGenSnaps = ledgerDbWithAnchor testInitLedger
    csChain    = take (fromIntegral csNumBlocks) $
                   iterate successorBlock (firstBlock 0)
    csPushed   = ledgerDbPushMany' (csBlockConfig' csSecParam) csChain csGenSnaps

mkRollbackSetup :: ChainSetup -> Word64 -> Word64 -> Word64 -> SwitchSetup
mkRollbackSetup ssChainSetup ssNumRollback ssNumNew ssPrefixLen =
    SwitchSetup {..}
  where
    ChainSetup{..} = ssChainSetup

    ssNumBlocks = csNumBlocks - ssNumRollback + ssNumNew
    ssRemoved   = takeLast ssNumRollback csChain
    ssNewBlocks = let afterRollback      = dropLast ssNumRollback csChain
                      firstAfterRollback =
                        case lastMaybe afterRollback of
                          Nothing -> firstBlock 1
                          Just b  -> modifyFork (+ 1) $ successorBlock b
                  in take (fromIntegral ssNumNew) $
                        iterate successorBlock firstAfterRollback
    ssChain     = concat [
                         take (fromIntegral (csNumBlocks - ssNumRollback)) csChain
                       , ssNewBlocks
                       ]
    ssSwitched  = fromJust $ ledgerDbSwitch' (csBlockConfig ssChainSetup) ssNumRollback ssNewBlocks csPushed

instance Arbitrary ChainSetup where
  arbitrary = do
      secParam <- arbitrary
      let k = maxRollbacks secParam
      numBlocks <- choose (0, k * 2)
      prefixLen <- choose (0, numBlocks)
      return $ mkTestSetup secParam numBlocks prefixLen

  shrink ChainSetup{..} = concat [
        -- Shrink the policy
        [ mkTestSetup csSecParam' csNumBlocks csPrefixLen
        | csSecParam' <- shrink csSecParam
        ]

        -- Reduce number of blocks
      , [ mkTestSetup csSecParam csNumBlocks' csPrefixLen
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

{-------------------------------------------------------------------------------
  Orphan Arbitrary instances
-------------------------------------------------------------------------------}

instance Arbitrary SecurityParam where
  arbitrary = SecurityParam <$> choose (0, 6)
  shrink (SecurityParam k) = SecurityParam <$> shrink k
