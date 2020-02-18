{-# LANGUAGE NamedFieldPuns #-}

module Test.ThreadNet.OddChain where

import           Test.QuickCheck
import           Test.Tasty
import           Test.Tasty.QuickCheck
import           Data.Word (Word64)

import           Ouroboros.Consensus.BlockchainTime.Mock
import           Ouroboros.Consensus.Node.ProtocolInfo
import           Ouroboros.Consensus.Protocol

import           Test.ThreadNet.General
import           Test.ThreadNet.Util

import           Test.Consensus.BlockchainTime.SlotLengths ()
import           Test.Util.Orphans.Arbitrary ()

import           Ouroboros.Consensus.Ledger.OddChain
import           Test.ThreadNet.TxGen (TxGen, testGenTx)
import           Crypto.Number.Generate (generateBetween)
import           Data.Time.Clock (UTCTime (UTCTime), utctDay, utctDayTime)
import           Data.Time.Calendar (Day (ModifiedJulianDay))

tests :: TestTree
tests = testGroup "Odd Chain" [
      testProperty "simple convergence" $
        prop_simple_oddchain_convergence k
    ]
  where
    k = SecurityParam 5

prop_simple_oddchain_convergence :: SecurityParam
                                 -> TestConfig
                                 -> Property
prop_simple_oddchain_convergence
  k
  testConfig@TestConfig{numCoreNodes, numSlots, slotLengths}
  =
  tabulate "slot length changes" [show $ countSlotLengthChanges numSlots slotLengths] $
  prop_general
    countOddTxs
    k
    testConfig
    (Just $ roundRobinLeaderSchedule numCoreNodes numSlots) -- Q: What happens if I pass 'Nothing' here?
    (const False)
    testOutput
  where
    testOutput =
        runTestNetwork testConfig TestConfigBlock
            { forgeEBB = Nothing
            , nodeInfo = \nid -> protocolInfo $
                ProtocolOdd numCoreNodes nid k slotLengths numSlotsPerEpoch startTime currentSlot
            , rekeying = Nothing
            }

    ----------------------------------------------------------------------------
    -- We're using constants for now:
    ----------------------------------------------------------------------------
    numSlotsPerEpoch = 10
    startTime =
        UTCTime
        { utctDay = ModifiedJulianDay 58849 -- 01-01-2020
        , utctDayTime = 0
        }
    currentSlot = 0
    ----------------------------------------------------------------------------
    -- END: We're using constants for now.
    ----------------------------------------------------------------------------


countOddTxs :: OddBlock -> Word64
countOddTxs = undefined

instance TxGen OddBlock where
  testGenTx _numCoreNodes _slotNo _cfg _ledgerSt =
    OddTx . Tx . fromIntegral <$> generateBetween (-100) 100
