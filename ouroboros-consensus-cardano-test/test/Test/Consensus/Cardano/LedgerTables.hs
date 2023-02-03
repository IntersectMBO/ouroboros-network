{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeFamilies      #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Consensus.Cardano.LedgerTables (tests) where

import qualified Data.Map.Strict as Map
import           Data.SOP.Strict

import           Ouroboros.Consensus.Cardano.Block
import           Ouroboros.Consensus.Cardano.CanHardFork
import           Ouroboros.Consensus.HardFork.Combinator
import           Ouroboros.Consensus.HardFork.Combinator.State.Types
import           Ouroboros.Consensus.HardFork.Combinator.Util.Functors
import           Ouroboros.Consensus.HardFork.Combinator.Util.Telescope
import           Ouroboros.Consensus.Ledger.Tables
import           Ouroboros.Consensus.Protocol.Praos.Translate ()
import           Ouroboros.Consensus.Shelley.HFEras ()
import           Ouroboros.Consensus.Shelley.Ledger.SupportsProtocol ()
import           Ouroboros.Consensus.Shelley.ShelleyHFC (ShelleyTxOut (..))
import           Ouroboros.Consensus.Util.SOP

import qualified Cardano.Ledger.Shelley.API as SL

import           Test.Cardano.Ledger.Alonzo.Serialisation.Generators ()
import           Test.Cardano.Ledger.Babbage.Serialisation.Generators ()

import           Test.LedgerTables

import           Test.Consensus.Byron.Generators ()
import           Test.Consensus.Cardano.MockCrypto (MockCryptoCompatByron)
import           Test.Consensus.Shelley.Generators ()

import           Test.Tasty
import           Test.Tasty.QuickCheck

type Crypto = MockCryptoCompatByron

tests :: TestTree
tests = testGroup "LedgerTables"
  [ testGroup "Cardano"
    [ testProperty "Stowable laws" (prop_stowable_laws @(CardanoBlock Crypto))
    , testProperty "TableStuff laws"
        (\(InSameEra ls tbs) -> prop_tablestuff_laws @(CardanoBlock Crypto) ls tbs)
    ]
  ]

instance Arbitrary (LedgerTables (LedgerState (CardanoBlock Crypto)) ValuesMK) where
  arbitrary = projectLedgerTables . unstowLedgerTables <$> arbitrary

instance Show (LedgerTables (LedgerState (CardanoBlock Crypto)) EmptyMK) where
  show x = showsLedgerState x ""

instance Show (LedgerTables (LedgerState (CardanoBlock Crypto)) ValuesMK) where
  show x = showsLedgerState x ""

-- | We need to generate Ledger states and tables in the same era or otherwise
-- it wouldn't make sense to try to attach them to a hard fork ledger state,
-- this data definition does just that.
data InSameEra =
  InSameEra
    !(LedgerState (CardanoBlock Crypto) EmptyMK)
    !(LedgerTables (LedgerState (CardanoBlock Crypto)) ValuesMK)
  deriving Show

instance Arbitrary InSameEra where
  arbitrary = do
    ls <- arbitrary
    tbs <- arbitrary `suchThat` (sameEra (getHardForkState $ hardForkLedgerStatePerEra ls) . cardanoUTxOTable)
    pure $ InSameEra ls tbs

-- | This function asserts that:
--
-- - Producing a table to be attached to a Byron HardFork State requires the
--   table to be empty as that is the only kind of table such a state can have.
--
-- - Producing a table to be attached to a Shelley HardFork State requires all
-- - the entries in the map to be on the same era as the state. We do so by
-- - checking that all of them and the state produce the same index through
-- - @'nsToIndex'@.
sameEra ::
     Telescope (K Past) (Current (Flip LedgerState EmptyMK)) (CardanoEras Crypto)
  -> ApplyMapKind ValuesMK (SL.TxIn Crypto) (ShelleyTxOut (ShelleyBasedEras Crypto))
  -> Bool
sameEra (TZ{})      (ApplyValuesMK m)      = Map.null m
sameEra (TS _ tele) (ApplyValuesMK values) =
    all (nsToIndex (tip tele) ==) $ map (nsToIndex . unShelleyTxOut . snd) (Map.toList values)
