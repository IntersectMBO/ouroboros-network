{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE DerivingVia    #-}
{-# LANGUAGE TypeFamilies   #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Ouroboros.Consensus.Ledger.ByronSpec.Mempool (
    -- * Type family instances
    GenTx(..)
  ) where

import           Codec.Serialise
import           Control.Monad.Trans.Except
import           GHC.Generics (Generic)

import           Cardano.Prelude (AllowThunk (..), NoUnexpectedThunks)

import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Ledger.Byron
import           Ouroboros.Consensus.Ledger.ByronSpec.Orphans ()
import           Ouroboros.Consensus.Mempool.API

import           Ouroboros.Consensus.Ledger.ByronSpec.Block
import           Ouroboros.Consensus.Ledger.ByronSpec.GenTx
                     (ByronSpecGenTx (..), ByronSpecGenTxErr (..))
import qualified Ouroboros.Consensus.Ledger.ByronSpec.GenTx as GenTx
import           Ouroboros.Consensus.Ledger.ByronSpec.Ledger

instance ApplyTx ByronSpecBlock where
  newtype GenTx ByronSpecBlock = ByronSpecGenTx {
        unByronSpecGenTx :: ByronSpecGenTx
      }
    deriving stock (Show, Generic)
    deriving anyclass (Serialise)
    deriving NoUnexpectedThunks via AllowThunk (GenTx ByronSpecBlock)

  type ApplyTxErr ByronSpecBlock = ByronSpecGenTxErr

  -- We pretend the abstract transactions have no size, and let the size of the
  -- mempool be limited by concrete transactions only. This is ok, because the
  -- spec does not impose a maximum block size.
  txSize _ = 0

  applyTx cfg tx (TickedLedgerState st) =
      (TickedLedgerState . updateByronSpecLedgerStateKeepTip st) <$>
        GenTx.apply
          (unByronSpecLedgerConfig cfg)
          (unByronSpecGenTx        tx)
          (byronSpecLedgerState    st)

  -- Byron spec doesn't have multiple validation modes

  reapplyTx          cfg tx =                   applyTx cfg tx
  reapplyTxSameState cfg tx = dontExpectError . applyTx cfg tx
    where
      dontExpectError :: Except a b -> b
      dontExpectError mb = case runExcept mb of
        Left  _ -> error "reapplyTxSameState: unexpected error"
        Right b -> b
