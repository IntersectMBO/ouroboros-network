{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies      #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Test.ThreadNet.TxGen.Alonzo () where

import           Ouroboros.Consensus.Shelley.Eras
import           Ouroboros.Consensus.Shelley.Ledger

import           Test.ThreadNet.TxGen (TxGen (..))

-- | Dummy generator until CAD-2119 is done, i.e., the transaction generator in
-- the ledger has been generalised over the eras.
instance TxGen (ShelleyBlock (AlonzoEra c)) where

  type TxGenExtra (ShelleyBlock (AlonzoEra c)) = ()

  testGenTxs _ _ _ _ _ _ = pure []
