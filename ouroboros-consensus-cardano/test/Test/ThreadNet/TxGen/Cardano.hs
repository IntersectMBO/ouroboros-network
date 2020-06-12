{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeFamilies      #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Test.ThreadNet.TxGen.Cardano () where

import           Ouroboros.Consensus.Cardano

import           Test.ThreadNet.TxGen

import           Test.Consensus.Shelley.MockCrypto (TPraosMockCrypto)

instance TxGen (CardanoBlock TPraosMockCrypto) where
  -- TODO
  testGenTxs _ _ _ _ _ = return []
