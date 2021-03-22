{-# OPTIONS_GHC -Wno-orphans #-}
module Test.ThreadNet.TxGen.Byron (

  ) where

import           Ouroboros.Consensus.Byron.Ledger

import           Test.ThreadNet.TxGen

instance TxGen ByronBlock where
  -- We don't generate transactions for 'ByronBlock', but we do for
  -- 'DualByronBlock'.
  testGenTxs _ _ _ _ _ _ = return []
