{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE DisambiguateRecordFields   #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE Rank2Types                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Shelley mempool integration
module Ouroboros.Consensus.Shelley.Ledger.Mempool (
    GenTx (..)
  , SL.ApplyTxError (..)
  , TxId (..)
  , Validated (..)
  , fixedBlockBodyOverhead
  , mkShelleyTx
  , mkShelleyValidatedTx
  , perTxOverhead
  ) where

import           Control.Monad.Except (Except, throwError)
import           Control.Monad.Identity (Identity (..))
import           Data.Foldable (toList)
import           Data.Typeable (Typeable)
import           GHC.Generics (Generic)
import           GHC.Records
import           NoThunks.Class (NoThunks (..))

import           Cardano.Binary (Annotator (..), FromCBOR (..),
                     FullByteString (..), ToCBOR (..))

import           Ouroboros.Network.Block (unwrapCBORinCBOR, wrapCBORinCBOR)

import           Cardano.Ledger.Alonzo.Scripts (ExUnits (..), pointWiseExUnits)
import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Ledger.SupportsMempool
import           Ouroboros.Consensus.Mempool.TxLimits
import           Ouroboros.Consensus.Shelley.Eras (AllegraEra, AlonzoEra,
                     MaryEra, ShelleyEra)
import           Ouroboros.Consensus.Util (ShowProxy (..))
import           Ouroboros.Consensus.Util.Condense

import           Cardano.Ledger.Alonzo.PParams
import           Cardano.Ledger.Alonzo.Tx (ValidatedTx (..), totExUnits)
import qualified Cardano.Ledger.Era as SL (Crypto, TxInBlock, TxSeq, fromTxSeq)
import qualified Shelley.Spec.Ledger.API as SL
import qualified Shelley.Spec.Ledger.UTxO as SL (txid)

import           Ouroboros.Consensus.Shelley.Eras (EraCrypto, scriptsWereOK)
import           Ouroboros.Consensus.Shelley.Ledger.Block
import           Ouroboros.Consensus.Shelley.Ledger.Ledger
                     (ShelleyLedgerConfig (shelleyLedgerGlobals),
                     Ticked (TickedShelleyLedgerState, tickedShelleyLedgerState),
                     getPParams)

data instance GenTx (ShelleyBlock era) = ShelleyTx !(SL.TxId (EraCrypto era)) !(SL.Tx era)
  deriving stock    (Generic)

deriving instance ShelleyBasedEra era => NoThunks (GenTx (ShelleyBlock era))

deriving instance ShelleyBasedEra era => Eq (GenTx (ShelleyBlock era))

instance Typeable era => ShowProxy (GenTx (ShelleyBlock era)) where

data instance Validated (GenTx (ShelleyBlock era)) =
    ShelleyValidatedTx
      !(SL.TxId (EraCrypto era))
      !(SL.TxInBlock era)
  deriving stock (Generic)

deriving instance ShelleyBasedEra era => NoThunks (Validated (GenTx (ShelleyBlock era)))

deriving instance ShelleyBasedEra era => Eq (Validated (GenTx (ShelleyBlock era)))

deriving instance ShelleyBasedEra era => Show (Validated (GenTx (ShelleyBlock era)))

instance Typeable era => ShowProxy (Validated (GenTx (ShelleyBlock era))) where

type instance ApplyTxErr (ShelleyBlock era) = SL.ApplyTxError era

-- orphaned instance
instance Typeable era => ShowProxy (SL.ApplyTxError era) where


-- |'txInBlockSize' is used to estimate how many transactions we can grab from
-- the Mempool to put into the block we are going to forge without exceeding
-- the maximum block body size according to the ledger. If we exceed that
-- limit, we will have forged a block that is invalid according to the ledger.
-- We ourselves won't even adopt it, causing us to lose our slot, something we
-- must try to avoid.
--
-- For this reason it is better to overestimate the size of a transaction than
-- to underestimate. The only downside is that we maybe could have put one (or
-- more?) transactions extra in that block.
--
-- As the sum of the serialised transaction sizes is not equal to the size of
-- the serialised block body ('SL.TxSeq') consisting of those transactions
-- (see cardano-node#1545 for an example), we account for some extra overhead
-- per transaction as a safety margin.
--
-- Also see 'perTxOverhead'.
fixedBlockBodyOverhead :: Num a => a
fixedBlockBodyOverhead = 1024

-- | See 'fixedBlockBodyOverhead'.
perTxOverhead :: Num a => a
perTxOverhead = 4

instance ShelleyBasedEra era
      => LedgerSupportsMempool (ShelleyBlock era) where
  txInvariant = const True

  applyTx = applyShelleyTx

  reapplyTx = applyShelleyValidatedTx

  maxTxCapacity TickedShelleyLedgerState { tickedShelleyLedgerState = shelleyState } =
      fromIntegral maxBlockBodySize - fixedBlockBodyOverhead
    where
      maxBlockBodySize = getField @"_maxBBSize" $ getPParams shelleyState

  txInBlockSize (ShelleyTx _ tx) = txSize + perTxOverhead
    where
      txSize = fromIntegral $ getField @"txsize" tx

  txForgetValidated (ShelleyValidatedTx txid tx) = ShelleyTx txid (SL.extractTx tx)

mkShelleyTx :: forall era. ShelleyBasedEra era => SL.Tx era -> GenTx (ShelleyBlock era)
mkShelleyTx tx = ShelleyTx (SL.txid @era (getField @"body" tx)) tx

mkShelleyValidatedTx :: forall era.
     ShelleyBasedEra era
  => SL.TxInBlock era
  -> Validated (GenTx (ShelleyBlock era))
mkShelleyValidatedTx tx =
    ShelleyValidatedTx (SL.txid @era (getField @"body" tx)) tx

newtype instance TxId (GenTx (ShelleyBlock era)) = ShelleyTxId (SL.TxId (EraCrypto era))
  deriving newtype (Eq, Ord, NoThunks)

deriving newtype instance (SL.PraosCrypto (EraCrypto era), Typeable era)
                       => ToCBOR (TxId (GenTx (ShelleyBlock era)))
deriving newtype instance (SL.PraosCrypto (EraCrypto era), Typeable era)
                       => FromCBOR (TxId (GenTx (ShelleyBlock era)))

instance Typeable era => ShowProxy (TxId (GenTx (ShelleyBlock era))) where

instance ShelleyBasedEra era => HasTxId (GenTx (ShelleyBlock era)) where
  txId (ShelleyTx i _) = ShelleyTxId i

instance ShelleyBasedEra era => HasTxs (ShelleyBlock era) where
  extractTxs =
        map mkShelleyValidatedTx
      . txSeqToList
      . SL.bbody
      . shelleyBlockRaw
    where
      txSeqToList :: SL.TxSeq era -> [SL.TxInBlock era]
      txSeqToList = toList . SL.fromTxSeq @era

{-------------------------------------------------------------------------------
  Serialisation
-------------------------------------------------------------------------------}

instance ShelleyBasedEra era => ToCBOR (GenTx (ShelleyBlock era)) where
  -- No need to encode the 'TxId', it's just a hash of the 'SL.TxBody' inside
  -- 'SL.Tx', so it can be recomputed.
  toCBOR (ShelleyTx _txid tx) = wrapCBORinCBOR toCBOR tx

instance ShelleyBasedEra era => FromCBOR (GenTx (ShelleyBlock era)) where
  fromCBOR = fmap mkShelleyTx $ unwrapCBORinCBOR
    $ (. Full) . runAnnotator <$> fromCBOR

{-------------------------------------------------------------------------------
  Pretty-printing
-------------------------------------------------------------------------------}

instance ShelleyBasedEra era => Condense (GenTx (ShelleyBlock era)) where
  condense (ShelleyTx _ tx ) = show tx

instance Condense (GenTxId (ShelleyBlock era)) where
  condense (ShelleyTxId i) = "txid: " <> show i

instance ShelleyBasedEra era => Show (GenTx (ShelleyBlock era)) where
  show = condense

instance Show (GenTxId (ShelleyBlock era)) where
  show = condense

{-------------------------------------------------------------------------------
  Applying transactions
-------------------------------------------------------------------------------}

applyShelleyTx :: forall era.
     ShelleyBasedEra era
  => LedgerConfig (ShelleyBlock era)
  -> WhetherToIntervene
  -> SlotNo
  -> GenTx (ShelleyBlock era)
  -> TickedLedgerState (ShelleyBlock era)
  -> Except (ApplyTxErr (ShelleyBlock era))
       ( TickedLedgerState (ShelleyBlock era)
       , Validated (GenTx (ShelleyBlock era))
       )
applyShelleyTx cfg wti slot (ShelleyTx _ tx) st = do
    (mempoolState', vtx) <-
       SL.applyTx
         (shelleyLedgerGlobals cfg)
         (SL.mkMempoolEnv   innerSt slot)
         (SL.mkMempoolState innerSt)
         tx

    case wti of
      Intervene | not (scriptsWereOK @era Proxy vtx) ->
        throwError $ SL.ApplyTxError []   -- TODO what to put in this list?
      _ -> pure ()

    let st' = set theLedgerLens mempoolState' st

    pure (st', mkShelleyValidatedTx vtx)
  where
    innerSt = tickedShelleyLedgerState st

applyShelleyValidatedTx :: forall era.
     ShelleyBasedEra era
  => LedgerConfig (ShelleyBlock era)
  -> SlotNo
  -> Validated (GenTx (ShelleyBlock era))
  -> TickedLedgerState (ShelleyBlock era)
  -> Except (ApplyTxErr (ShelleyBlock era)) (TickedLedgerState (ShelleyBlock era))
applyShelleyValidatedTx cfg slot (ShelleyValidatedTx _ tx) st = do
    mempoolState' <-
       SL.applyTxInBlock
         (shelleyLedgerGlobals cfg)
         (SL.mkMempoolEnv   innerSt slot)
         (SL.mkMempoolState innerSt)
         tx

    pure $ set theLedgerLens mempoolState' st
  where
    innerSt = tickedShelleyLedgerState st

-- | The lens combinator
set ::
     (forall f. Applicative f => (a -> f b) -> s -> f t)
  -> b -> s -> t
set lens inner outer =
    runIdentity $ lens (\_ -> Identity inner) outer

theLedgerLens ::
     -- TODO SL.overNewEpochState should not require 'Applicative'
     Applicative f
  => (      (SL.UTxOState era, SL.DPState (SL.Crypto era))
       -> f (SL.UTxOState era, SL.DPState (SL.Crypto era))
     )
  ->    TickedLedgerState (ShelleyBlock era)
  -> f (TickedLedgerState (ShelleyBlock era))
theLedgerLens f x =
        (\y -> x{tickedShelleyLedgerState = y})
    <$> SL.overNewEpochState f (tickedShelleyLedgerState x)

{-------------------------------------------------------------------------------
  Tx Limits
-------------------------------------------------------------------------------}
type instance Overrides (ShelleyBlock era) =
  Measure (ShelleyBlock era) -> Measure (ShelleyBlock era)

instance (SL.PraosCrypto c) => TxLimits (ShelleyBlock (ShelleyEra c)) where
  type Measure (ShelleyBlock (ShelleyEra c)) = ByteSize
  lessEq       = (<=)
  txMeasure    = ByteSize . txInBlockSize . txForgetValidated
  maxCapacity  = ByteSize . maxTxCapacity
  pointwiseMin = min

instance (SL.PraosCrypto c) => TxLimits (ShelleyBlock (AllegraEra c)) where
  type Measure (ShelleyBlock (AllegraEra c)) = ByteSize
  lessEq       = (<=)
  txMeasure    = ByteSize . txInBlockSize . txForgetValidated
  maxCapacity  = ByteSize . maxTxCapacity
  pointwiseMin = min

instance (SL.PraosCrypto c) => TxLimits (ShelleyBlock (MaryEra c)) where
  type Measure (ShelleyBlock (MaryEra c)) = ByteSize
  lessEq       = (<=)
  txMeasure    = ByteSize . txInBlockSize . txForgetValidated
  maxCapacity  = ByteSize . maxTxCapacity
  pointwiseMin = min

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
         ) => TxLimits (ShelleyBlock (AlonzoEra c)) where

  type Measure (ShelleyBlock (AlonzoEra c)) = AlonzoMeasure

  lessEq (AlonzoMeasure bs1 exu1) (AlonzoMeasure bs2 exu2) =
    bs1 <= bs2 && pointWiseExUnits (<=) exu1 exu2

  txMeasure validatedGenTx@(ShelleyValidatedTx _ tx) =
    AlonzoMeasure {
        byteSize = ByteSize . txInBlockSize $ txForgetValidated validatedGenTx
      , exUnits  = totExUnits tx
      }

  maxCapacity ledgerState =
    let pparams  = getPParams $ tickedShelleyLedgerState ledgerState
    in AlonzoMeasure {
        byteSize = ByteSize $ maxTxCapacity ledgerState
      , exUnits  = getField @"_maxBlockExUnits" pparams
      }

  pointwiseMin (AlonzoMeasure bs1 (ExUnits mem1 steps1)) (AlonzoMeasure bs2 (ExUnits mem2 steps2)) =
    AlonzoMeasure (bs1 `min` bs2) (ExUnits (mem1 `min` mem2) (steps1 `min` steps2))
