{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies               #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Ouroboros.Consensus.ByronSpec.Ledger.Mempool (
    -- * Type family instances
    GenTx (..)
  , Validated (..)
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

newtype instance Validated (GenTx ByronSpecBlock) = ValidatedByronSpecGenTx {
      forgetValidatedByronSpecGenTx :: GenTx ByronSpecBlock
    }
  deriving stock (Show, Generic)
  deriving anyclass NoThunks

type instance ApplyTxErr ByronSpecBlock = ByronSpecGenTxErr

instance LedgerSupportsMempool ByronSpecBlock where
  applyTx cfg _wti _slot tx (TickedByronSpecLedgerState tip st) =
        fmap (\st' ->
               ( TickedByronSpecLedgerState tip st'
               , ValidatedByronSpecGenTx tx
               )
             )
      $ GenTx.apply cfg (unByronSpecGenTx tx) st

  -- Byron spec doesn't have multiple validation modes
  reapplyTx cfg slot vtx st =
        fmap fst
      $ applyTx cfg DoNotIntervene slot (forgetValidatedByronSpecGenTx vtx) st

  -- Dummy values, as these are not used in practice.
  maxTxCapacity = const maxBound
  txInBlockSize = const 0

  txForgetValidated = forgetValidatedByronSpecGenTx
