{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Test.ThreadNet.TxGen.Shelley () where

import           Test.ThreadNet.TxGen (TxGen (..))

import           Ouroboros.Consensus.Shelley.Ledger (ShelleyBlock)

import           Test.Consensus.Shelley.MockCrypto (TPraosMockCrypto)


instance TxGen (ShelleyBlock TPraosMockCrypto) where
  -- TODO #1823
  testGenTxs _ _ _ _ = return []
