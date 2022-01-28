{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE DisambiguateRecordFields   #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
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
  , WithTop (..)
  , fixedBlockBodyOverhead
  , mkShelleyTx
  , mkShelleyValidatedTx
  , perTxOverhead
    -- * Exported for tests
  , AlonzoMeasure (..)
  , fromExUnits
  ) where

import           Control.Monad.Except (Except)
import           Control.Monad.Identity (Identity (..))
import           Data.Foldable (toList)
import           Data.Typeable (Typeable)
import           GHC.Generics (Generic)
import           GHC.Natural (Natural)
import           GHC.Records
import           NoThunks.Class (NoThunks (..))

import           Cardano.Binary (Annotator (..), FromCBOR (..),
                     FullByteString (..), ToCBOR (..))
import           Data.DerivingVia (InstantiatedAt (..))
import           Data.Measure (BoundedMeasure, Measure)
import qualified Data.Measure as Measure

import           Ouroboros.Network.Block (unwrapCBORinCBOR, wrapCBORinCBOR)

import           Cardano.Ledger.Alonzo.Scripts (ExUnits, ExUnits',
                     unWrapExUnits)
import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Ledger.SupportsMempool
import           Ouroboros.Consensus.Mempool.TxLimits
import           Ouroboros.Consensus.Util (ShowProxy (..))
import           Ouroboros.Consensus.Util.Condense

import           Cardano.Ledger.Alonzo.PParams
import           Cardano.Ledger.Alonzo.Tx (totExUnits)
import qualified Cardano.Ledger.Core as Core (Tx)
import qualified Cardano.Ledger.Era as SL (Crypto, TxSeq, fromTxSeq)
import qualified Cardano.Ledger.Shelley.API as SL
import qualified Cardano.Ledger.TxIn as SL (txid)

import qualified Cardano.Protocol.TPraos.API as SL
import           Ouroboros.Consensus.Shelley.Eras
import           Ouroboros.Consensus.Shelley.Ledger.Block
import           Ouroboros.Consensus.Shelley.Ledger.Ledger
                     (ShelleyLedgerConfig (shelleyLedgerGlobals),
                     Ticked (TickedShelleyLedgerState, tickedShelleyLedgerState),
                     getPParams)

data instance GenTx (ShelleyBlock era) = ShelleyTx !(SL.TxId (EraCrypto era)) !(Core.Tx era)
  deriving stock    (Generic)

deriving instance ShelleyBasedEra era => NoThunks (GenTx (ShelleyBlock era))

deriving instance ShelleyBasedEra era => Eq (GenTx (ShelleyBlock era))

instance Typeable era => ShowProxy (GenTx (ShelleyBlock era)) where

data instance Validated (GenTx (ShelleyBlock era)) =
    ShelleyValidatedTx
      !(SL.TxId (EraCrypto era))
      !(SL.Validated (Core.Tx era))
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

  reapplyTx = reapplyShelleyTx

  txsMaxBytes TickedShelleyLedgerState { tickedShelleyLedgerState = shelleyState } =
      fromIntegral maxBlockBodySize - fixedBlockBodyOverhead
    where
      maxBlockBodySize = getField @"_maxBBSize" $ getPParams shelleyState

  txInBlockSize (ShelleyTx _ tx) = txSize + perTxOverhead
    where
      txSize = fromIntegral $ getField @"txsize" tx

  txForgetValidated (ShelleyValidatedTx txid vtx) = ShelleyTx txid (SL.extractTx vtx)

mkShelleyTx :: forall era. ShelleyBasedEra era => Core.Tx era -> GenTx (ShelleyBlock era)
mkShelleyTx tx = ShelleyTx (SL.txid @era (getField @"body" tx)) tx

mkShelleyValidatedTx :: forall era.
     ShelleyBasedEra era
  => SL.Validated (Core.Tx era)
  -> Validated (GenTx (ShelleyBlock era))
mkShelleyValidatedTx vtx = ShelleyValidatedTx txid vtx
  where
    txid = SL.txid @era (getField @"body" (SL.extractTx vtx))

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
        map mkShelleyTx
      . txSeqToList
      . SL.bbody
      . shelleyBlockRaw
    where
      txSeqToList :: SL.TxSeq era -> [Core.Tx era]
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
       applyShelleyBasedTx
         (shelleyLedgerGlobals cfg)
         (SL.mkMempoolEnv   innerSt slot)
         (SL.mkMempoolState innerSt)
         wti
         tx

    let st' = set theLedgerLens mempoolState' st

    pure (st', mkShelleyValidatedTx vtx)
  where
    innerSt = tickedShelleyLedgerState st

reapplyShelleyTx ::
     ShelleyBasedEra era
  => LedgerConfig (ShelleyBlock era)
  -> SlotNo
  -> Validated (GenTx (ShelleyBlock era))
  -> TickedLedgerState (ShelleyBlock era)
  -> Except (ApplyTxErr (ShelleyBlock era)) (TickedLedgerState (ShelleyBlock era))
reapplyShelleyTx cfg slot vgtx st = do
    mempoolState' <-
        SL.reapplyTx
          (shelleyLedgerGlobals cfg)
          (SL.mkMempoolEnv   innerSt slot)
          (SL.mkMempoolState innerSt)
          vtx

    pure $ set theLedgerLens mempoolState' st
  where
    ShelleyValidatedTx _txid vtx = vgtx

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

instance (ShelleyBasedEra (ShelleyEra c)) => TxLimits (ShelleyBlock (ShelleyEra c)) where
  type TxMeasure (ShelleyBlock (ShelleyEra c)) = ByteSize
  txMeasure        = ByteSize . txInBlockSize . txForgetValidated
  txsBlockCapacity = ByteSize . txsMaxBytes

instance (ShelleyBasedEra (AllegraEra c)) => TxLimits (ShelleyBlock (AllegraEra c)) where
  type TxMeasure (ShelleyBlock (AllegraEra c)) = ByteSize
  txMeasure        = ByteSize . txInBlockSize . txForgetValidated
  txsBlockCapacity = ByteSize . txsMaxBytes

instance (ShelleyBasedEra (MaryEra c)) => TxLimits (ShelleyBlock (MaryEra c)) where
  type TxMeasure (ShelleyBlock (MaryEra c)) = ByteSize
  txMeasure        = ByteSize . txInBlockSize . txForgetValidated
  txsBlockCapacity = ByteSize . txsMaxBytes

instance ( ShelleyBasedEra (AlonzoEra c)
         ) => TxLimits (ShelleyBlock (AlonzoEra c)) where

  type TxMeasure (ShelleyBlock (AlonzoEra c)) = AlonzoMeasure

  txMeasure (ShelleyValidatedTx _txid vtx) =
    AlonzoMeasure {
        byteSize = ByteSize $ txInBlockSize (mkShelleyTx @(AlonzoEra c) (SL.extractTx vtx))
      , exUnits  = fromExUnits $ totExUnits (SL.extractTx vtx)
      }

  txsBlockCapacity ledgerState =
      AlonzoMeasure {
          byteSize = ByteSize $ txsMaxBytes ledgerState
        , exUnits  = fromExUnits $ getField @"_maxBlockExUnits" pparams
        }
    where
      pparams = getPParams $ tickedShelleyLedgerState ledgerState

data AlonzoMeasure = AlonzoMeasure {
    byteSize :: !ByteSize
  , exUnits  :: !(ExUnits' (WithTop Natural))
  } deriving stock (Eq, Generic, Show)
    deriving (BoundedMeasure, Measure)
         via (InstantiatedAt Generic AlonzoMeasure)

fromExUnits :: ExUnits -> ExUnits' (WithTop Natural)
fromExUnits = fmap NotTop . unWrapExUnits

{-------------------------------------------------------------------------------
  WithTop
-------------------------------------------------------------------------------}

-- | Add a unique top element to a lattice.
--
-- TODO This should be relocated to `cardano-base:Data.Measure'.
data WithTop a = NotTop a | Top
  deriving (Eq, Generic, Show)

instance Ord a => Ord (WithTop a) where
  compare = curry $ \case
    (Top     , Top     ) -> EQ
    (Top     , _       ) -> GT
    (_       , Top     ) -> LT
    (NotTop l, NotTop r) -> compare l r

instance Measure a => Measure (WithTop a) where
  zero = NotTop Measure.zero
  plus = curry $ \case
    (Top     , _       ) -> Top
    (_       , Top     ) -> Top
    (NotTop l, NotTop r) -> NotTop $ Measure.plus l r
  min  = curry $ \case
    (Top     , r       ) -> r
    (l       , Top     ) -> l
    (NotTop l, NotTop r) -> NotTop $ Measure.min l r
  max  = curry $ \case
    (Top     , _       ) -> Top
    (_       , Top     ) -> Top
    (NotTop l, NotTop r) -> NotTop $ Measure.max l r

instance Measure a => BoundedMeasure (WithTop a) where
  maxBound = Top
