{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DerivingStrategies   #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Ouroboros.Consensus.Shelley.TxLimits (TxLimits (..)) where

import           Data.Word (Word32)
import           GHC.Records

import           Cardano.Ledger.Alonzo.PParams
import           Cardano.Ledger.Alonzo.Scripts (ExUnits (..), pointWiseExUnits)
import           Ouroboros.Consensus.Shelley.Ledger.Ledger
import qualified Shelley.Spec.Ledger.API as SL

import           Ouroboros.Consensus.Ledger.SupportsMempool
import           Ouroboros.Consensus.Shelley.Eras (AllegraEra, AlonzoEra,
                     MaryEra, ShelleyEra)
import           Ouroboros.Consensus.Shelley.Ledger.Block (ShelleyBlock)
import           Ouroboros.Consensus.Shelley.Ledger.Mempool

class ( Monoid (Measure era)
      ) => TxLimits era where
  type Measure era
  lessEq :: Measure era -> Measure era -> Bool
  txMeasure :: Validated (GenTx (ShelleyBlock era)) -> Measure era
  maxCapacity :: Ticked (LedgerState (ShelleyBlock era)) -> Measure era

newtype ByteSize = ByteSize { unByteSize :: Word32 }
  deriving stock (Show, Eq, Ord)

instance Semigroup ByteSize where
  (ByteSize bs1) <> (ByteSize bs2) = ByteSize $ bs1 + bs2

instance Monoid ByteSize where
  mempty = ByteSize 0

instance (SL.PraosCrypto c) => TxLimits (ShelleyEra c) where
  type Measure (ShelleyEra c) = ByteSize
  lessEq      = (<=)
  txMeasure   = ByteSize . txInBlockSize . txForgetValidated
  maxCapacity = ByteSize . maxTxCapacity

instance (SL.PraosCrypto c) => TxLimits (AllegraEra c) where
  type Measure (AllegraEra c) = ByteSize
  lessEq      = (<=)
  txMeasure   = ByteSize . txInBlockSize . txForgetValidated
  maxCapacity = ByteSize . maxTxCapacity

instance (SL.PraosCrypto c) => TxLimits (MaryEra c) where
  type Measure (MaryEra c) = ByteSize
  lessEq      = (<=)
  txMeasure   = ByteSize . txInBlockSize . txForgetValidated
  maxCapacity = ByteSize . maxTxCapacity

data AlonzoMeasure = AlonzoMeasure {
    byteSize :: ByteSize
  , exUnits  :: ExUnits
  } deriving stock (Show, Eq)

instance Semigroup AlonzoMeasure where
  (AlonzoMeasure bs1 exu1) <> (AlonzoMeasure bs2 exu2) =
    AlonzoMeasure (bs1 <> bs2) (exu1 <> exu2)

instance Monoid AlonzoMeasure where
  mempty = AlonzoMeasure mempty mempty

instance ( SL.PraosCrypto c
         ) => TxLimits (AlonzoEra c) where

  type Measure (AlonzoEra c) = AlonzoMeasure

  lessEq (AlonzoMeasure bs1 exu1) (AlonzoMeasure bs2 exu2) =
    bs1 <= bs2 && pointWiseExUnits (<=) exu1 exu2

  txMeasure validatedGenTx@(ShelleyValidatedTx _ tx) =
    AlonzoMeasure {
        byteSize = ByteSize . txInBlockSize $ txForgetValidated validatedGenTx
      , exUnits  = getField @"totExunits" tx
      }

  maxCapacity ledgerState =
    let pparams  = getPParams $ tickedShelleyLedgerState ledgerState
    in AlonzoMeasure {
        byteSize = ByteSize $ maxTxCapacity ledgerState
      , exUnits  = getField @"_maxTxExUnits" pparams
      }
