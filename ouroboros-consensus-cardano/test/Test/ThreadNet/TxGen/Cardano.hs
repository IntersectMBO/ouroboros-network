{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeFamilies      #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Test.ThreadNet.TxGen.Cardano () where

import           Ouroboros.Consensus.HardFork.Combinator.Degenerate
import           Ouroboros.Consensus.HardFork.Combinator.Unary

import           Ouroboros.Consensus.Shelley.Ledger (ShelleyBlock)

import           Ouroboros.Consensus.Cardano

import           Test.ThreadNet.TxGen

import           Test.Consensus.Shelley.MockCrypto (TPraosMockCrypto)
import           Test.ThreadNet.TxGen.Shelley

-- | Reuse the generator for Shelley
instance TxGen (CardanoBlock TPraosMockCrypto) where
  type TxGenExtra (CardanoBlock TPraosMockCrypto) = ShelleyTxGenExtra
  testGenTxs numCoreNodes curSlotNo cfg txGenExtra ledger =
      map (DTx . injGenTx) <$>
      testGenTxs
        @(ShelleyBlock TPraosMockCrypto)
        numCoreNodes
        curSlotNo
        (projCfg cfg)
        txGenExtra
        (projLedgerState (unDLgr ledger))
