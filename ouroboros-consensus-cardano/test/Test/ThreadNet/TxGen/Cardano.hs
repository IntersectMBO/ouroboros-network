{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeFamilies      #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Test.ThreadNet.TxGen.Cardano () where

import           Ouroboros.Consensus.Cardano

import           Test.ThreadNet.TxGen

import           Test.Consensus.Shelley.MockCrypto (TPraosMockCryptoCompatByron)

instance TxGen (CardanoBlock (TPraosMockCryptoCompatByron h)) where
  -- TODO
  testGenTxs _ _ _ _ _ = return []
