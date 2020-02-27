{-# OPTIONS_GHC -Wno-orphans #-}
module Test.ThreadNet.TxGen.Byron () where

import           Ouroboros.Consensus.Byron.Ledger

import           Test.ThreadNet.TxGen

instance TxGen ByronBlock where
  testGenTx = error "TODO #855 testGenTx"
  -- 'testGenTxs' is used by the tests, not 'testGenTx'.
  testGenTxs _ _ _ _ = return []
