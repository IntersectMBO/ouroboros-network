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
{-# LANGUAGE RecordWildCards            #-}
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
import           Lens.Micro ((^.))
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
import qualified Ouroboros.Consensus.Storage.LedgerDB.HD.DiffSeq as DS
import           Ouroboros.Consensus.Util (ShowProxy (..))
import           Ouroboros.Consensus.Util.Condense

import           Cardano.Ledger.Alonzo.PParams
import           Cardano.Ledger.Alonzo.Tx (totExUnits)
import           Cardano.Ledger.Babbage.PParams
import qualified Cardano.Ledger.Block as SL (txid)
import           Cardano.Ledger.Core (Tx, TxSeq, bodyTxL, fromTxSeq, sizeTxF)
import qualified Cardano.Ledger.Core as Core (Tx)
import qualified Cardano.Ledger.Era as SL (TxSeq, fromTxSeq, getAllTxInputs)
import qualified Cardano.Ledger.Shelley.API as SL

import           Cardano.Ledger.Crypto (Crypto)
import           Ouroboros.Consensus.Shelley.Eras
import           Ouroboros.Consensus.Shelley.Ledger.Block
import           Ouroboros.Consensus.Shelley.Ledger.Ledger
                     (ShelleyLedgerConfig (shelleyLedgerGlobals),
                     Ticked1 (TickedShelleyLedgerState, tickedShelleyLedgerState),
                     getPParams)
import qualified Ouroboros.Consensus.Shelley.Ledger.Ledger as ShelleyLedger

data instance GenTx (ShelleyBlock proto era) = ShelleyTx !(SL.TxId (EraCrypto era)) !(Tx era)
  deriving stock    (Generic)

deriving instance ShelleyBasedEra era => NoThunks (GenTx (ShelleyBlock proto era))

deriving instance ShelleyBasedEra era => Eq (GenTx (ShelleyBlock proto era))

instance (Typeable era, Typeable proto)
  => ShowProxy (GenTx (ShelleyBlock proto era)) where

data instance Validated (GenTx (ShelleyBlock proto era)) =
    ShelleyValidatedTx
      !(SL.TxId (EraCrypto era))
      !(SL.Validated (Tx era))
  deriving stock (Generic)

deriving instance ShelleyBasedEra era => NoThunks (Validated (GenTx (ShelleyBlock proto era)))

deriving instance ShelleyBasedEra era => Eq (Validated (GenTx (ShelleyBlock proto era)))

deriving instance ShelleyBasedEra era => Show (Validated (GenTx (ShelleyBlock proto era)))

instance (Typeable era, Typeable proto)
  => ShowProxy (Validated (GenTx (ShelleyBlock proto era))) where

type instance ApplyTxErr (ShelleyBlock proto era) = SL.ApplyTxError era

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
-- the serialised block body ('TxSeq') consisting of those transactions
-- (see cardano-node#1545 for an example), we account for some extra overhead
-- per transaction as a safety margin.
--
-- Also see 'perTxOverhead'.
fixedBlockBodyOverhead :: Num a => a
fixedBlockBodyOverhead = 1024

-- | See 'fixedBlockBodyOverhead'.
perTxOverhead :: Num a => a
perTxOverhead = 4

instance ShelleyCompatible proto era
      => LedgerSupportsMempool (ShelleyBlock proto era) where
  txInvariant = const True

  applyTx = applyShelleyTx

  reapplyTx = reapplyShelleyTx

  txsMaxBytes TickedShelleyLedgerState { tickedShelleyLedgerState = shelleyState } =
      fromIntegral maxBlockBodySize - fixedBlockBodyOverhead
    where
      maxBlockBodySize = getField @"_maxBBSize" $ getPParams shelleyState

  txInBlockSize (ShelleyTx _ tx) = txSize + perTxOverhead
    where
      txSize = fromIntegral $ tx ^. sizeTxF

  txForgetValidated (ShelleyValidatedTx txid vtx) = ShelleyTx txid (SL.extractTx vtx)

<<<<<<< HEAD
mkShelleyTx :: forall era proto. ShelleyBasedEra era => Tx era -> GenTx (ShelleyBlock proto era)
mkShelleyTx tx = ShelleyTx (SL.txid @era (tx ^. bodyTxL)) tx
=======
  getTransactionKeySets (ShelleyTx _ tx) =
        ShelleyLedger.ShelleyLedgerTables
      $ ApplyKeysMK
      $ DS.Keys
      $ SL.getAllTxInputs (getField @"body"  tx)

mkShelleyTx :: forall era proto. ShelleyBasedEra era => Core.Tx era -> GenTx (ShelleyBlock proto era)
mkShelleyTx tx = ShelleyTx (SL.txid @era (getField @"body" tx)) tx
>>>>>>> 106ee892b (Integrate UTxO-HD)

mkShelleyValidatedTx :: forall era proto.
     ShelleyBasedEra era
  => SL.Validated (Tx era)
  -> Validated (GenTx (ShelleyBlock proto era))
mkShelleyValidatedTx vtx = ShelleyValidatedTx txid vtx
  where
    txid = SL.txid @era (SL.extractTx vtx ^. bodyTxL)

newtype instance TxId (GenTx (ShelleyBlock proto era)) = ShelleyTxId (SL.TxId (EraCrypto era))
  deriving newtype (Eq, Ord, NoThunks)

deriving newtype instance (Crypto (EraCrypto era), Typeable era, Typeable proto)
                       => ToCBOR (TxId (GenTx (ShelleyBlock proto era)))
deriving newtype instance (Crypto (EraCrypto era), Typeable era, Typeable proto)
                       => FromCBOR (TxId (GenTx (ShelleyBlock proto era)))

instance (Typeable era, Typeable proto)
  => ShowProxy (TxId (GenTx (ShelleyBlock proto era))) where

instance ShelleyBasedEra era => HasTxId (GenTx (ShelleyBlock proto era)) where
  txId (ShelleyTx i _) = ShelleyTxId i

instance ShelleyBasedEra era => HasTxs (ShelleyBlock proto era) where
  extractTxs =
        map mkShelleyTx
      . txSeqToList
      . SL.bbody
      . shelleyBlockRaw
    where
      txSeqToList :: TxSeq era -> [Tx era]
      txSeqToList = toList . fromTxSeq @era

{-------------------------------------------------------------------------------
  Serialisation
-------------------------------------------------------------------------------}

instance ShelleyCompatible proto era => ToCBOR (GenTx (ShelleyBlock proto era)) where
  -- No need to encode the 'TxId', it's just a hash of the 'SL.TxBody' inside
  -- 'SL.Tx', so it can be recomputed.
  toCBOR (ShelleyTx _txid tx) = wrapCBORinCBOR toCBOR tx

instance ShelleyCompatible proto era => FromCBOR (GenTx (ShelleyBlock proto era)) where
  fromCBOR = fmap mkShelleyTx $ unwrapCBORinCBOR
    $ (. Full) . runAnnotator <$> fromCBOR

{-------------------------------------------------------------------------------
  Pretty-printing
-------------------------------------------------------------------------------}

instance ShelleyBasedEra era => Condense (GenTx (ShelleyBlock proto era)) where
  condense (ShelleyTx _ tx ) = show tx

instance Condense (GenTxId (ShelleyBlock proto era)) where
  condense (ShelleyTxId i) = "txid: " <> show i

instance ShelleyBasedEra era => Show (GenTx (ShelleyBlock proto era)) where
  show = condense

instance Show (GenTxId (ShelleyBlock proto era)) where
  show = condense

{-------------------------------------------------------------------------------
  Applying transactions
-------------------------------------------------------------------------------}

applyShelleyTx :: forall era proto.
     ShelleyBasedEra era
  => LedgerConfig (ShelleyBlock proto era)
  -> WhetherToIntervene
  -> SlotNo
  -> GenTx (ShelleyBlock proto era)
  -> TickedLedgerState (ShelleyBlock proto era) ValuesMK
  -> Except (ApplyTxErr (ShelleyBlock proto era))
       ( TickedLedgerState (ShelleyBlock proto era) TrackingMK
       , Validated (GenTx (ShelleyBlock proto era))
       )
applyShelleyTx cfg wti slot (ShelleyTx _ tx) st0 = do
    let st1 :: TickedLedgerState (ShelleyBlock proto era) EmptyMK
        st1 = ShelleyLedger.cnv $ stowLedgerTables $ ShelleyLedger.vnc st0

        innerSt :: SL.NewEpochState era
        innerSt = tickedShelleyLedgerState st1

    (mempoolState', vtx) <-
       applyShelleyBasedTx
         (shelleyLedgerGlobals cfg)
         (SL.mkMempoolEnv   innerSt slot)
         (SL.mkMempoolState innerSt)
         wti
         tx

    let st2 :: TickedLedgerState (ShelleyBlock proto era) EmptyMK
        st2 = set theLedgerLens mempoolState' st1

        st3 :: TickedLedgerState (ShelleyBlock proto era) ValuesMK
        st3 = ShelleyLedger.cnv $ unstowLedgerTables $ ShelleyLedger.vnc st2

        st4 :: TickedLedgerState (ShelleyBlock proto era) TrackingMK
        st4 = calculateDifferenceTicked st0 st3

    pure (st4, mkShelleyValidatedTx vtx)

reapplyShelleyTx ::
     ShelleyBasedEra era
  => LedgerConfig (ShelleyBlock proto era)
  -> SlotNo
  -> Validated (GenTx (ShelleyBlock proto era))
  -> TickedLedgerState (ShelleyBlock proto era) ValuesMK
  -> Except (ApplyTxErr (ShelleyBlock proto era)) (TickedLedgerState (ShelleyBlock proto era) TrackingMK)
reapplyShelleyTx cfg slot vgtx st0 = do
    let st1     = ShelleyLedger.cnv $ stowLedgerTables $ ShelleyLedger.vnc st0
        innerSt = tickedShelleyLedgerState st1

    mempoolState' <-
        SL.reapplyTx
          (shelleyLedgerGlobals cfg)
          (SL.mkMempoolEnv   innerSt slot)
          (SL.mkMempoolState innerSt)
          vtx

    let st2 = calculateDifferenceTicked st0
          $ ShelleyLedger.cnv $ unstowLedgerTables $ ShelleyLedger.vnc
          $ set theLedgerLens mempoolState' st1

    pure st2
  where
    ShelleyValidatedTx _txid vtx = vgtx

-- | The lens combinator
set ::
     (forall f. Applicative f => (a -> f b) -> s -> f t)
  -> b -> s -> t
set lens inner outer =
    runIdentity $ lens (\_ -> Identity inner) outer

theLedgerLens ::
     Functor f
  => (SL.LedgerState era -> f (SL.LedgerState era))
  -> TickedLedgerState (ShelleyBlock proto era) mk
  -> f (TickedLedgerState (ShelleyBlock proto era) mk)
theLedgerLens f x =
        (\y -> x{tickedShelleyLedgerState = y})
    <$> SL.overNewEpochState f (tickedShelleyLedgerState x)

{-------------------------------------------------------------------------------
  Tx Limits
-------------------------------------------------------------------------------}

instance ShelleyCompatible p (ShelleyEra c) => TxLimits (ShelleyBlock p (ShelleyEra c)) where
  type TxMeasure (ShelleyBlock p (ShelleyEra c)) = ByteSize
  txMeasure        = ByteSize . txInBlockSize . txForgetValidated
  txsBlockCapacity = ByteSize . txsMaxBytes

instance ShelleyCompatible p (AllegraEra c) => TxLimits (ShelleyBlock p (AllegraEra c)) where
  type TxMeasure (ShelleyBlock p (AllegraEra c)) = ByteSize
  txMeasure        = ByteSize . txInBlockSize . txForgetValidated
  txsBlockCapacity = ByteSize . txsMaxBytes

instance ShelleyCompatible p (MaryEra c) => TxLimits (ShelleyBlock p (MaryEra c)) where
  type TxMeasure (ShelleyBlock p (MaryEra c)) = ByteSize
  txMeasure        = ByteSize . txInBlockSize . txForgetValidated
  txsBlockCapacity = ByteSize . txsMaxBytes

instance ( ShelleyCompatible p (AlonzoEra c)
         ) => TxLimits (ShelleyBlock p (AlonzoEra c)) where

  type TxMeasure (ShelleyBlock p (AlonzoEra c)) = AlonzoMeasure

  txMeasure (ShelleyValidatedTx _txid vtx) =
    AlonzoMeasure {
        byteSize = ByteSize $ txInBlockSize (mkShelleyTx @(AlonzoEra c) @p (SL.extractTx vtx))
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

instance ( ShelleyCompatible p (BabbageEra c)
         ) => TxLimits (ShelleyBlock p (BabbageEra c)) where

  type TxMeasure (ShelleyBlock p (BabbageEra c)) = AlonzoMeasure

  txMeasure (ShelleyValidatedTx _txid vtx) =
    AlonzoMeasure {
        byteSize = ByteSize $ txInBlockSize (mkShelleyTx @(BabbageEra c) @p (SL.extractTx vtx))
      , exUnits  = fromExUnits $ totExUnits (SL.extractTx vtx)
      }

  txsBlockCapacity ledgerState =
      AlonzoMeasure {
          byteSize = ByteSize $ txsMaxBytes ledgerState
        , exUnits  = fromExUnits $ getField @"_maxBlockExUnits" pparams
        }
    where
      pparams = getPParams $ tickedShelleyLedgerState ledgerState

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
