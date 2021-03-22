{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE DerivingVia    #-}
{-# LANGUAGE TypeFamilies   #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Ouroboros.Consensus.ByronSpec.Ledger.Mempool (
    -- * Type family instances
    GenTx (..)
  ) where

import           Codec.Serialise
import           GHC.Generics (Generic)
import           NoThunks.Class (AllowThunk (..), NoThunks)

import           Ouroboros.Consensus.Ledger.SupportsMempool

import           Ouroboros.Consensus.ByronSpec.Ledger.Block
import           Ouroboros.Consensus.ByronSpec.Ledger.GenTx
                     (ByronSpecGenTx (..), ByronSpecGenTxErr (..))
import qualified Ouroboros.Consensus.ByronSpec.Ledger.GenTx as GenTx
import           Ouroboros.Consensus.ByronSpec.Ledger.Ledger
import           Ouroboros.Consensus.ByronSpec.Ledger.Orphans ()

newtype instance GenTx ByronSpecBlock = ByronSpecGenTx {
      unByronSpecGenTx :: ByronSpecGenTx
    }
  deriving stock (Show, Generic)
  deriving anyclass (Serialise)
  deriving NoThunks via AllowThunk (GenTx ByronSpecBlock)

type instance ApplyTxErr ByronSpecBlock = ByronSpecGenTxErr

instance LedgerSupportsMempool ByronSpecBlock where
  applyTx cfg _slot tx (TickedByronSpecLedgerState tip st) =
      TickedByronSpecLedgerState tip <$>
        GenTx.apply cfg (unByronSpecGenTx tx) st

  -- Byron spec doesn't have multiple validation modes
  reapplyTx = applyTx

  -- Dummy values, as these are not used in practice.
  maxTxCapacity = const maxBound
  txInBlockSize = const 0
