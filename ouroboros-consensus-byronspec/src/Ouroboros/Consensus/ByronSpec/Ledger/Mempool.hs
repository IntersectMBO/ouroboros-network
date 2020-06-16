{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE DerivingVia    #-}
{-# LANGUAGE TypeFamilies   #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Ouroboros.Consensus.ByronSpec.Ledger.Mempool (
    -- * Type family instances
    GenTx(..)
  ) where

import           Codec.Serialise
import           GHC.Generics (Generic)

import           Cardano.Prelude (AllowThunk (..), NoUnexpectedThunks)

import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Ledger.SupportsMempool

import           Ouroboros.Consensus.ByronSpec.Ledger.Block
import           Ouroboros.Consensus.ByronSpec.Ledger.GenTx
                     (ByronSpecGenTx (..), ByronSpecGenTxErr (..))
import qualified Ouroboros.Consensus.ByronSpec.Ledger.GenTx as GenTx
import           Ouroboros.Consensus.ByronSpec.Ledger.Ledger
import           Ouroboros.Consensus.ByronSpec.Ledger.Orphans ()

instance LedgerSupportsMempool ByronSpecBlock where
  newtype GenTx ByronSpecBlock = ByronSpecGenTx {
        unByronSpecGenTx :: ByronSpecGenTx
      }
    deriving stock (Show, Generic)
    deriving anyclass (Serialise)
    deriving NoUnexpectedThunks via AllowThunk (GenTx ByronSpecBlock)

  type ApplyTxErr ByronSpecBlock = ByronSpecGenTxErr

  applyTx cfg tx (Ticked slot st) =
      (Ticked slot . updateByronSpecLedgerStateKeepTip st) <$>
        GenTx.apply
          cfg
          (unByronSpecGenTx     tx)
          (byronSpecLedgerState st)

  -- Byron spec doesn't have multiple validation modes
  reapplyTx = applyTx

  -- Dummy values, as these are not used in practice.
  maxTxCapacity = const maxBound
  txInBlockSize = const 0
