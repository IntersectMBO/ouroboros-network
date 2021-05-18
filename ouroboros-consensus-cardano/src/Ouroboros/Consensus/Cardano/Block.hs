{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE DeriveGeneric            #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE GADTs                    #-}
{-# LANGUAGE PatternSynonyms          #-}
{-# LANGUAGE ViewPatterns             #-}
module Ouroboros.Consensus.Cardano.Block (
    -- * Eras
    CardanoEras
  , module Ouroboros.Consensus.Shelley.Eras
  , ShelleyBasedEras
    -- * Block
  , CardanoBlock
    -- Note: by exporting the pattern synonyms as part of the matching data
    -- type (instead of as separate patterns), we get better exhaustiveness
    -- checks from GHC. But GHC expects a data type, not a type family, that's
    -- why we sometimes mention the data type of the instance in these exports
    -- instead of the abstract type family.
  , HardForkBlock (BlockAllegra, BlockAlonzo, BlockByron, BlockMary, BlockShelley)
    -- * Headers
  , CardanoHeader
  , Header (HeaderAllegra, HeaderAlonzo, HeaderByron, HeaderMary, HeaderShelley)
    -- * Generalised transactions
  , CardanoApplyTxErr
  , CardanoGenTx
  , CardanoGenTxId
  , GenTx (GenTxAllegra, GenTxAlonzo, GenTxByron, GenTxMary, GenTxShelley)
  , HardForkApplyTxErr (ApplyTxErrAllegra, ApplyTxErrAlonzo, ApplyTxErrByron, ApplyTxErrMary, ApplyTxErrShelley, ApplyTxErrWrongEra)
  , TxId (GenTxIdAllegra, GenTxIdAlonzo, GenTxIdByron, GenTxIdMary, GenTxIdShelley)
    -- * LedgerError
  , CardanoLedgerError
  , HardForkLedgerError (LedgerErrorAllegra, LedgerErrorAlonzo, LedgerErrorByron, LedgerErrorMary, LedgerErrorShelley, LedgerErrorWrongEra)
    -- * OtherEnvelopeError
  , CardanoOtherHeaderEnvelopeError
  , HardForkEnvelopeErr (OtherHeaderEnvelopeErrorAllegra, OtherHeaderEnvelopeErrorAlonzo, OtherHeaderEnvelopeErrorByron, OtherHeaderEnvelopeErrorMary, OtherHeaderEnvelopeErrorShelley, OtherHeaderEnvelopeErrorWrongEra)
    -- * TipInfo
  , CardanoTipInfo
  , OneEraTipInfo (TipInfoAllegra, TipInfoAlonzo, TipInfoByron, TipInfoMary, TipInfoShelley)
    -- * Query
  , BlockQuery (QueryAnytimeAllegra, QueryAnytimeAlonzo, QueryAnytimeByron, QueryAnytimeMary, QueryAnytimeShelley, QueryHardFork, QueryIfCurrentAllegra, QueryIfCurrentAlonzo, QueryIfCurrentByron, QueryIfCurrentMary, QueryIfCurrentShelley)
  , CardanoQuery
  , CardanoQueryResult
  , Either (QueryResultSuccess, QueryResultEraMismatch)
    -- * CodecConfig
  , CardanoCodecConfig
  , CodecConfig (CardanoCodecConfig)
    -- * BlockConfig
  , BlockConfig (CardanoBlockConfig)
  , CardanoBlockConfig
    -- * StorageConfig
  , CardanoStorageConfig
  , StorageConfig (CardanoStorageConfig)
    -- * ConsensusConfig
  , CardanoConsensusConfig
  , ConsensusConfig (CardanoConsensusConfig)
    -- * LedgerConfig
  , CardanoLedgerConfig
  , HardForkLedgerConfig (CardanoLedgerConfig)
    -- * LedgerState
  , CardanoLedgerState
  , LedgerState (LedgerStateAllegra, LedgerStateAlonzo, LedgerStateByron, LedgerStateMary, LedgerStateShelley)
    -- * ChainDepState
  , CardanoChainDepState
  , HardForkState (ChainDepStateAllegra, ChainDepStateAlonzo, ChainDepStateByron, ChainDepStateMary, ChainDepStateShelley)
    -- * EraMismatch
  , EraMismatch (..)
  ) where

import           Data.SOP.Strict

import           Ouroboros.Consensus.Block (BlockProtocol)
import           Ouroboros.Consensus.HeaderValidation (OtherHeaderEnvelopeError,
                     TipInfo)
import           Ouroboros.Consensus.Ledger.Abstract (LedgerError)
import           Ouroboros.Consensus.Ledger.SupportsMempool (ApplyTxErr,
                     GenTxId)
import           Ouroboros.Consensus.Protocol.Abstract (ChainDepState)
import           Ouroboros.Consensus.TypeFamilyWrappers

import           Ouroboros.Consensus.HardFork.Combinator
import           Ouroboros.Consensus.HardFork.Combinator.AcrossEras
import qualified Ouroboros.Consensus.HardFork.Combinator.State as State

import           Ouroboros.Consensus.Byron.Ledger.Block (ByronBlock)

import           Ouroboros.Consensus.Shelley.Eras
import           Ouroboros.Consensus.Shelley.Ledger (ShelleyBlock)

{-------------------------------------------------------------------------------
  The eras of the Cardano bock chain
-------------------------------------------------------------------------------}

-- | The eras in the Cardano blockchain.
--
-- We parameterise over the crypto used in the post-Byron eras: @c@.
--
-- TODO: parameterise ByronBlock over crypto too
type CardanoEras c =
  '[ ByronBlock
   , ShelleyBlock (ShelleyEra c)
   , ShelleyBlock (AllegraEra c)
   , ShelleyBlock (MaryEra c)
   , ShelleyBlock (AlonzoEra c)
   ]

-- | The Shelley-based eras in the Cardano chain
type ShelleyBasedEras c = '[ShelleyEra c, AllegraEra c, MaryEra c, AlonzoEra c]

{-------------------------------------------------------------------------------
  INTERNAL A tag function for each era
-------------------------------------------------------------------------------}

-- Here we use layout and adjacency to make it obvious that we haven't
-- miscounted.

pattern TagByron   :: f ByronBlock                    -> NS f (CardanoEras c)
pattern TagShelley :: f (ShelleyBlock (ShelleyEra c)) -> NS f (CardanoEras c)
pattern TagAllegra :: f (ShelleyBlock (AllegraEra c)) -> NS f (CardanoEras c)
pattern TagMary    :: f (ShelleyBlock (MaryEra    c)) -> NS f (CardanoEras c)
pattern TagAlonzo  :: f (ShelleyBlock (AlonzoEra  c)) -> NS f (CardanoEras c)

pattern TagByron   x =             Z x
pattern TagShelley x =          S (Z x)
pattern TagAllegra x =       S (S (Z x))
pattern TagMary    x =    S (S (S (Z x)))
pattern TagAlonzo  x = S (S (S (S (Z x))))

{-------------------------------------------------------------------------------
  INTERNAL A telescope function for each era

-------------------------------------------------------------------------------}

pattern TeleByron   ::
     f ByronBlock
  -> Telescope g f (CardanoEras c)

pattern TeleShelley ::
     g ByronBlock
  -> f (ShelleyBlock (ShelleyEra c))
  -> Telescope g f (CardanoEras c)

pattern TeleAllegra ::
     g ByronBlock
  -> g (ShelleyBlock (ShelleyEra c))
  -> f (ShelleyBlock (AllegraEra c))
  -> Telescope g f (CardanoEras c)

pattern TeleMary    ::
     g ByronBlock
  -> g (ShelleyBlock (ShelleyEra c))
  -> g (ShelleyBlock (AllegraEra c))
  -> f (ShelleyBlock (MaryEra    c))
  -> Telescope g f (CardanoEras c)

pattern TeleAlonzo  ::
     g ByronBlock
  -> g (ShelleyBlock (ShelleyEra c))
  -> g (ShelleyBlock (AllegraEra c))
  -> g (ShelleyBlock (MaryEra    c))
  -> f (ShelleyBlock (AlonzoEra  c))
  -> Telescope g f (CardanoEras c)

-- Here we use layout and adjacency to make it obvious that we haven't
-- miscounted.

pattern TeleByron                              x =                                            TZ x
pattern TeleShelley byron                      x = TS byron                                  (TZ x)
pattern TeleAllegra byron shelley              x = TS byron (TS shelley                      (TZ x))
pattern TeleMary    byron shelley allegra      x = TS byron (TS shelley (TS allegra          (TZ x)))
pattern TeleAlonzo  byron shelley allegra mary x = TS byron (TS shelley (TS allegra (TS mary (TZ x))))

{-------------------------------------------------------------------------------
  The block type of the Cardano block chain
-------------------------------------------------------------------------------}

-- | /The/ Cardano block.
--
-- Thanks to the pattern synonyms, you can treat this as a sum type with
-- constructors 'BlockByron' and 'BlockShelley'.
--
-- > f :: CardanoBlock c -> _
-- > f (BlockByron   b) = _
-- > f (BlockShelley s) = _
-- > f (BlockAllegra a) = _
-- > f (BlockMary    m) = _
-- > f (BlockAlonzo  m) = _
--
type CardanoBlock c = HardForkBlock (CardanoEras c)

pattern BlockByron :: ByronBlock -> CardanoBlock c
pattern BlockByron b = HardForkBlock (OneEraBlock (TagByron (I b)))

pattern BlockShelley :: ShelleyBlock (ShelleyEra c) -> CardanoBlock c
pattern BlockShelley b = HardForkBlock (OneEraBlock (TagShelley (I b)))

pattern BlockAllegra :: ShelleyBlock (AllegraEra c) -> CardanoBlock c
pattern BlockAllegra b = HardForkBlock (OneEraBlock (TagAllegra (I b)))

pattern BlockMary :: ShelleyBlock (MaryEra c) -> CardanoBlock c
pattern BlockMary b = HardForkBlock (OneEraBlock (TagMary (I b)))

pattern BlockAlonzo :: ShelleyBlock (AlonzoEra c) -> CardanoBlock c
pattern BlockAlonzo b = HardForkBlock (OneEraBlock (TagAlonzo (I b)))

{-# COMPLETE BlockByron, BlockShelley, BlockAllegra, BlockMary, BlockAlonzo #-}

{-------------------------------------------------------------------------------
  Headers
-------------------------------------------------------------------------------}

-- | The Cardano header.
type CardanoHeader c = Header (CardanoBlock c)

pattern HeaderByron :: Header ByronBlock -> CardanoHeader c
pattern HeaderByron h = HardForkHeader (OneEraHeader (TagByron h))

pattern HeaderShelley ::
     Header (ShelleyBlock (ShelleyEra c))
  -> CardanoHeader c
pattern HeaderShelley h = HardForkHeader (OneEraHeader (TagShelley h))

pattern HeaderAllegra ::
     Header (ShelleyBlock (AllegraEra c))
  -> CardanoHeader c
pattern HeaderAllegra h = HardForkHeader (OneEraHeader (TagAllegra h))

pattern HeaderMary ::
     Header (ShelleyBlock (MaryEra c))
  -> CardanoHeader c
pattern HeaderMary h = HardForkHeader (OneEraHeader (TagMary h))

pattern HeaderAlonzo ::
     Header (ShelleyBlock (AlonzoEra c))
  -> CardanoHeader c
pattern HeaderAlonzo h = HardForkHeader (OneEraHeader (TagAlonzo h))

{-# COMPLETE HeaderByron
           , HeaderShelley
           , HeaderAllegra
           , HeaderMary
           , HeaderAlonzo #-}

{-------------------------------------------------------------------------------
  Generalised transactions
-------------------------------------------------------------------------------}

-- | The Cardano transaction.
type CardanoGenTx c = GenTx (CardanoBlock c)

pattern GenTxByron :: GenTx ByronBlock -> CardanoGenTx c
pattern GenTxByron tx = HardForkGenTx (OneEraGenTx (TagByron tx))

pattern GenTxShelley :: GenTx (ShelleyBlock (ShelleyEra c)) -> CardanoGenTx c
pattern GenTxShelley tx = HardForkGenTx (OneEraGenTx (TagShelley tx))

pattern GenTxAllegra :: GenTx (ShelleyBlock (AllegraEra c)) -> CardanoGenTx c
pattern GenTxAllegra tx = HardForkGenTx (OneEraGenTx (TagAllegra tx))

pattern GenTxMary :: GenTx (ShelleyBlock (MaryEra c)) -> CardanoGenTx c
pattern GenTxMary tx = HardForkGenTx (OneEraGenTx (TagMary tx))

pattern GenTxAlonzo :: GenTx (ShelleyBlock (AlonzoEra c)) -> CardanoGenTx c
pattern GenTxAlonzo tx = HardForkGenTx (OneEraGenTx (TagAlonzo tx))

{-# COMPLETE GenTxByron, GenTxShelley, GenTxAllegra, GenTxMary, GenTxAlonzo #-}

-- | The ID of a Cardano transaction.
type CardanoGenTxId c = GenTxId (CardanoBlock c)

pattern GenTxIdByron :: GenTxId ByronBlock -> CardanoGenTxId c
pattern GenTxIdByron txid =
    HardForkGenTxId (OneEraGenTxId (TagByron (WrapGenTxId txid)))

pattern GenTxIdShelley ::
     GenTxId (ShelleyBlock (ShelleyEra c))
  -> CardanoGenTxId c
pattern GenTxIdShelley txid =
    HardForkGenTxId (OneEraGenTxId (TagShelley (WrapGenTxId txid)))

pattern GenTxIdAllegra ::
     GenTxId (ShelleyBlock (AllegraEra c))
  -> CardanoGenTxId c
pattern GenTxIdAllegra txid =
    HardForkGenTxId (OneEraGenTxId (TagAllegra (WrapGenTxId txid)))

pattern GenTxIdMary ::
     GenTxId (ShelleyBlock (MaryEra c))
  -> CardanoGenTxId c
pattern GenTxIdMary txid =
    HardForkGenTxId (OneEraGenTxId (TagMary (WrapGenTxId txid)))

pattern GenTxIdAlonzo ::
     GenTxId (ShelleyBlock (AlonzoEra c))
  -> CardanoGenTxId c
pattern GenTxIdAlonzo txid =
    HardForkGenTxId (OneEraGenTxId (TagAlonzo (WrapGenTxId txid)))

{-# COMPLETE GenTxIdByron
           , GenTxIdShelley
           , GenTxIdAllegra
           , GenTxIdMary
           , GenTxIdAlonzo #-}

-- | An error resulting from applying a 'CardanoGenTx' to the ledger.
--
-- Thanks to the pattern synonyms, you can treat this as a sum type with
-- constructors 'ApplyTxByronErr', 'ApplyTxErrShelley', and
-- 'ApplyTxErrWrongEra'.
--
-- > toText :: CardanoApplyTxErr c -> Text
-- > toText (ApplyTxErrByron b) = byronApplyTxErrToText b
-- > toText (ApplyTxErrShelley s) = shelleyApplyTxErrToText s
-- > toText (ApplyTxErrAllegra a) = allegraApplyTxErrToText a
-- > toText (ApplyTxErrMary m) = maryApplyTxErrToText m
-- > toText (ApplyTxErrWrongEra eraMismatch) =
-- >   "Transaction from the " <> otherEraName eraMismatch <>
-- >   " era applied to a ledger from the " <>
-- >   ledgerEraName eraMismatch <> " era"
--
type CardanoApplyTxErr c = HardForkApplyTxErr (CardanoEras c)

pattern ApplyTxErrByron :: ApplyTxErr ByronBlock -> CardanoApplyTxErr c
pattern ApplyTxErrByron err =
    HardForkApplyTxErrFromEra (OneEraApplyTxErr (TagByron (WrapApplyTxErr err)))

pattern ApplyTxErrShelley ::
     ApplyTxErr (ShelleyBlock (ShelleyEra c))
  -> CardanoApplyTxErr c
pattern ApplyTxErrShelley err =
    HardForkApplyTxErrFromEra (OneEraApplyTxErr (TagShelley (WrapApplyTxErr err)))

pattern ApplyTxErrAllegra ::
     ApplyTxErr (ShelleyBlock (AllegraEra c))
  -> CardanoApplyTxErr c
pattern ApplyTxErrAllegra err =
    HardForkApplyTxErrFromEra (OneEraApplyTxErr (TagAllegra (WrapApplyTxErr err)))

pattern ApplyTxErrMary ::
     ApplyTxErr (ShelleyBlock (MaryEra c))
  -> CardanoApplyTxErr c
pattern ApplyTxErrMary err =
    HardForkApplyTxErrFromEra (OneEraApplyTxErr (TagMary (WrapApplyTxErr err)))

pattern ApplyTxErrAlonzo ::
     ApplyTxErr (ShelleyBlock (AlonzoEra c))
  -> CardanoApplyTxErr c
pattern ApplyTxErrAlonzo err =
    HardForkApplyTxErrFromEra (OneEraApplyTxErr (TagAlonzo (WrapApplyTxErr err)))

pattern ApplyTxErrWrongEra :: EraMismatch -> CardanoApplyTxErr c
pattern ApplyTxErrWrongEra eraMismatch <-
    HardForkApplyTxErrWrongEra (mkEraMismatch -> eraMismatch)

{-# COMPLETE ApplyTxErrByron
           , ApplyTxErrShelley
           , ApplyTxErrAllegra
           , ApplyTxErrMary
           , ApplyTxErrAlonzo
           , ApplyTxErrWrongEra #-}

{-------------------------------------------------------------------------------
  LedgerError
-------------------------------------------------------------------------------}

-- | An error resulting from applying a 'CardanoBlock' to the ledger.
--
-- Thanks to the pattern synonyms, you can treat this as a sum type with
-- constructors 'LedgerErrorByron', 'LedgerErrorShelley', and
-- 'LedgerErrorWrongEra'.
--
-- > toText :: CardanoLedgerError c -> Text
-- > toText (LedgerErrorByron b) = byronLedgerErrorToText b
-- > toText (LedgerErrorShelley s) = shelleyLedgerErrorToText s
-- > toText (LedgerErrorAllegra a) = allegraLedgerErrorToText a
-- > toText (LedgerErrorMary m) = maryLedgerErrorToText m
-- > toText (LedgerErrorWrongEra eraMismatch) =
-- >   "Block from the " <> otherEraName eraMismatch <>
-- >   " era applied to a ledger from the " <>
-- >   ledgerEraName eraMismatch <> " era"
--
type CardanoLedgerError c = HardForkLedgerError (CardanoEras c)

pattern LedgerErrorByron :: LedgerError ByronBlock -> CardanoLedgerError c
pattern LedgerErrorByron err =
    HardForkLedgerErrorFromEra (OneEraLedgerError (TagByron (WrapLedgerErr err)))

pattern LedgerErrorShelley ::
     LedgerError (ShelleyBlock (ShelleyEra c))
  -> CardanoLedgerError c
pattern LedgerErrorShelley err =
    HardForkLedgerErrorFromEra
      (OneEraLedgerError (TagShelley (WrapLedgerErr err)))

pattern LedgerErrorAllegra ::
     LedgerError (ShelleyBlock (AllegraEra c))
  -> CardanoLedgerError c
pattern LedgerErrorAllegra err =
    HardForkLedgerErrorFromEra
      (OneEraLedgerError (TagAllegra (WrapLedgerErr err)))

pattern LedgerErrorMary ::
     LedgerError (ShelleyBlock (MaryEra c))
  -> CardanoLedgerError c
pattern LedgerErrorMary err =
    HardForkLedgerErrorFromEra
      (OneEraLedgerError (TagMary (WrapLedgerErr err)))

pattern LedgerErrorAlonzo ::
     LedgerError (ShelleyBlock (AlonzoEra c))
  -> CardanoLedgerError c
pattern LedgerErrorAlonzo err =
    HardForkLedgerErrorFromEra
      (OneEraLedgerError (TagAlonzo (WrapLedgerErr err)))

pattern LedgerErrorWrongEra :: EraMismatch -> CardanoLedgerError c
pattern LedgerErrorWrongEra eraMismatch <-
    HardForkLedgerErrorWrongEra (mkEraMismatch -> eraMismatch)

{-# COMPLETE LedgerErrorByron
           , LedgerErrorShelley
           , LedgerErrorAllegra
           , LedgerErrorMary
           , LedgerErrorAlonzo
           , LedgerErrorWrongEra #-}

{-------------------------------------------------------------------------------
  OtherEnvelopeError
-------------------------------------------------------------------------------}

-- | An error resulting from validating a 'CardanoHeader'.
type CardanoOtherHeaderEnvelopeError c = HardForkEnvelopeErr (CardanoEras c)

pattern OtherHeaderEnvelopeErrorByron
  :: OtherHeaderEnvelopeError ByronBlock
  -> CardanoOtherHeaderEnvelopeError c
pattern OtherHeaderEnvelopeErrorByron err =
    HardForkEnvelopeErrFromEra
      (OneEraEnvelopeErr (TagByron (WrapEnvelopeErr err)))

pattern OtherHeaderEnvelopeErrorShelley
  :: OtherHeaderEnvelopeError (ShelleyBlock (ShelleyEra c))
  -> CardanoOtherHeaderEnvelopeError c
pattern OtherHeaderEnvelopeErrorShelley err =
    HardForkEnvelopeErrFromEra (OneEraEnvelopeErr (TagShelley (WrapEnvelopeErr err)))

pattern OtherHeaderEnvelopeErrorAllegra
  :: OtherHeaderEnvelopeError (ShelleyBlock (AllegraEra c))
  -> CardanoOtherHeaderEnvelopeError c
pattern OtherHeaderEnvelopeErrorAllegra err =
    HardForkEnvelopeErrFromEra (OneEraEnvelopeErr (TagAllegra (WrapEnvelopeErr err)))

pattern OtherHeaderEnvelopeErrorMary
  :: OtherHeaderEnvelopeError (ShelleyBlock (MaryEra c))
  -> CardanoOtherHeaderEnvelopeError c
pattern OtherHeaderEnvelopeErrorMary err =
    HardForkEnvelopeErrFromEra (OneEraEnvelopeErr (TagMary (WrapEnvelopeErr err)))

pattern OtherHeaderEnvelopeErrorAlonzo
  :: OtherHeaderEnvelopeError (ShelleyBlock (AlonzoEra c))
  -> CardanoOtherHeaderEnvelopeError c
pattern OtherHeaderEnvelopeErrorAlonzo err =
    HardForkEnvelopeErrFromEra (OneEraEnvelopeErr (TagAlonzo (WrapEnvelopeErr err)))

pattern OtherHeaderEnvelopeErrorWrongEra
  :: EraMismatch
  -> CardanoOtherHeaderEnvelopeError c
pattern OtherHeaderEnvelopeErrorWrongEra eraMismatch <-
    HardForkEnvelopeErrWrongEra (mkEraMismatch -> eraMismatch)

{-# COMPLETE OtherHeaderEnvelopeErrorByron
           , OtherHeaderEnvelopeErrorShelley
           , OtherHeaderEnvelopeErrorAllegra
           , OtherHeaderEnvelopeErrorMary
           , OtherHeaderEnvelopeErrorAlonzo
           , OtherHeaderEnvelopeErrorWrongEra #-}

{-------------------------------------------------------------------------------
  TipInfo
-------------------------------------------------------------------------------}

-- | The 'TipInfo' of the Cardano chain.
type CardanoTipInfo c = OneEraTipInfo (CardanoEras c)

pattern TipInfoByron :: TipInfo ByronBlock -> CardanoTipInfo c
pattern TipInfoByron ti = OneEraTipInfo (TagByron (WrapTipInfo ti))

pattern TipInfoShelley ::
     TipInfo (ShelleyBlock (ShelleyEra c))
  -> CardanoTipInfo c
pattern TipInfoShelley ti = OneEraTipInfo (TagShelley (WrapTipInfo ti))

pattern TipInfoAllegra ::
     TipInfo (ShelleyBlock (AllegraEra c))
  -> CardanoTipInfo c
pattern TipInfoAllegra ti = OneEraTipInfo (TagAllegra (WrapTipInfo ti))

pattern TipInfoMary ::
     TipInfo (ShelleyBlock (MaryEra c))
  -> CardanoTipInfo c
pattern TipInfoMary ti = OneEraTipInfo (TagMary (WrapTipInfo ti))

pattern TipInfoAlonzo ::
     TipInfo (ShelleyBlock (AlonzoEra c))
  -> CardanoTipInfo c
pattern TipInfoAlonzo ti = OneEraTipInfo (TagAlonzo (WrapTipInfo ti))

{-# COMPLETE TipInfoByron
           , TipInfoShelley
           , TipInfoAllegra
           , TipInfoMary
           , TipInfoAlonzo #-}

{-------------------------------------------------------------------------------
  Query
-------------------------------------------------------------------------------}

-- | The 'Query' of Cardano chain.
type CardanoQuery c = BlockQuery (CardanoBlock c)

-- | Byron-specific query that can only be answered when the ledger is in the
-- Byron era.
pattern QueryIfCurrentByron
  :: ()
  => CardanoQueryResult c result ~ a
  => BlockQuery ByronBlock result
  -> CardanoQuery c a

-- | Shelley-specific query that can only be answered when the ledger is in the
-- Shelley era.
pattern QueryIfCurrentShelley
  :: ()
  => CardanoQueryResult c result ~ a
  => BlockQuery (ShelleyBlock (ShelleyEra c)) result
  -> CardanoQuery c a

-- | Allegra-specific query that can only be answered when the ledger is in the
-- Allegra era.
pattern QueryIfCurrentAllegra
  :: ()
  => CardanoQueryResult c result ~ a
  => BlockQuery (ShelleyBlock (AllegraEra c)) result
  -> CardanoQuery c a

-- | Mary-specific query that can only be answered when the ledger is in the
-- Mary era.
pattern QueryIfCurrentMary
  :: ()
  => CardanoQueryResult c result ~ a
  => BlockQuery (ShelleyBlock (MaryEra c)) result
  -> CardanoQuery c a

-- | Alonzo-specific query that can only be answered when the ledger is in the
-- Alonzo era.
pattern QueryIfCurrentAlonzo
  :: ()
  => CardanoQueryResult c result ~ a
  => BlockQuery (ShelleyBlock (AlonzoEra c)) result
  -> CardanoQuery c a

-- Here we use layout and adjacency to make it obvious that we haven't
-- miscounted.

pattern QueryIfCurrentByron   q = QueryIfCurrent                 (QZ q)
pattern QueryIfCurrentShelley q = QueryIfCurrent             (QS (QZ q))
pattern QueryIfCurrentAllegra q = QueryIfCurrent         (QS (QS (QZ q)))
pattern QueryIfCurrentMary    q = QueryIfCurrent     (QS (QS (QS (QZ q))))
pattern QueryIfCurrentAlonzo  q = QueryIfCurrent (QS (QS (QS (QS (QZ q)))))

-- | Query about the Byron era that can be answered anytime, i.e.,
-- independent from where the tip of the ledger is.
--
-- For example, to ask for the start of the Byron era (whether the tip of
-- the ledger is in the Byron, Shelley, ... era), use:
--
-- > QueryAnytimeByron EraStart
--
pattern QueryAnytimeByron
  :: QueryAnytime result
  -> CardanoQuery c result
pattern QueryAnytimeByron q = QueryAnytime q (EraIndex (TagByron (K ())))

-- | Query about the Shelley era that can be answered anytime, i.e.,
-- independent from where the tip of the ledger is.
--
-- For example, to ask for the start of the Shelley era (whether the tip of the
-- ledger is in the Byron, Shelley, ... era), use:
--
-- > QueryAnytimeShelley EraStart
--
pattern QueryAnytimeShelley
  :: QueryAnytime result
  -> CardanoQuery c result
pattern QueryAnytimeShelley q = QueryAnytime q (EraIndex (TagShelley (K ())))

-- | Query about the Allegra era that can be answered anytime, i.e.,
-- independent from where the tip of the ledger is.
--
-- For example, to ask for the start of the Allegra era (whether the tip of the
-- ledger is in the Byron, Shelley, ... era), use:
--
-- > QueryAnytimeAllegra EraStart
--
pattern QueryAnytimeAllegra
  :: QueryAnytime result
  -> CardanoQuery c result
pattern QueryAnytimeAllegra q = QueryAnytime q (EraIndex (TagAllegra (K ())))

-- | Query about the Mary era that can be answered anytime, i.e.,
-- independent from where the tip of the ledger is.
--
-- For example, to ask for the start of the Mary era (whether the tip of the
-- ledger is in the Byron, Shelley, ... era), use:
--
-- > QueryAnytimeMary EraStart
--
pattern QueryAnytimeMary
  :: QueryAnytime result
  -> CardanoQuery c result
pattern QueryAnytimeMary q = QueryAnytime q (EraIndex (TagMary (K ())))

-- | Query about the Alonzo era that can be answered anytime, i.e., independent
-- from where the tip of the ledger is.
--
-- For example, to ask for the start of the Alonzo era (whether the tip of the
-- ledger is in the Byron, Shelley, ... era), use:
--
-- > QueryAnytimeAlonzo EraStart
--
pattern QueryAnytimeAlonzo
  :: QueryAnytime result
  -> CardanoQuery c result
pattern QueryAnytimeAlonzo q = QueryAnytime q (EraIndex (TagAlonzo (K ())))

{-# COMPLETE QueryIfCurrentByron
           , QueryIfCurrentShelley
           , QueryIfCurrentAllegra
           , QueryIfCurrentMary
           , QueryIfCurrentAlonzo
           , QueryAnytimeByron
           , QueryAnytimeShelley
           , QueryAnytimeAllegra
           , QueryAnytimeMary
           , QueryAnytimeAlonzo
           , QueryHardFork #-}

-- | The result of a 'CardanoQuery'
--
-- Thanks to the pattern synonyms, you can treat this as a sum type with
-- constructors 'QueryResultSuccess' and 'QueryResultEraMismatch'.
type CardanoQueryResult c = HardForkQueryResult (CardanoEras c)

pattern QueryResultSuccess :: result -> CardanoQueryResult c result
pattern QueryResultSuccess result = Right result

-- | A query from a different era than the ledger's era was sent.
pattern QueryResultEraMismatch :: EraMismatch -> CardanoQueryResult c result
pattern QueryResultEraMismatch eraMismatch <- Left (mkEraMismatch -> eraMismatch)

{-# COMPLETE QueryResultSuccess, QueryResultEraMismatch #-}

{-------------------------------------------------------------------------------
  CodecConfig
-------------------------------------------------------------------------------}

-- | The 'CodecConfig' for 'CardanoBlock'.
--
-- Thanks to the pattern synonyms, you can treat this as the product of
-- the Byron, Shelley, ... 'CodecConfig's.
type CardanoCodecConfig c = CodecConfig (CardanoBlock c)

pattern CardanoCodecConfig
  :: CodecConfig ByronBlock
  -> CodecConfig (ShelleyBlock (ShelleyEra c))
  -> CodecConfig (ShelleyBlock (AllegraEra c))
  -> CodecConfig (ShelleyBlock (MaryEra c))
  -> CodecConfig (ShelleyBlock (AlonzoEra c))
  -> CardanoCodecConfig c
pattern CardanoCodecConfig cfgByron cfgShelley cfgAllegra cfgMary cfgAlonzo =
    HardForkCodecConfig {
        hardForkCodecConfigPerEra = PerEraCodecConfig
          (  cfgByron
          :* cfgShelley
          :* cfgAllegra
          :* cfgMary
          :* cfgAlonzo
          :* Nil
          )
      }

{-# COMPLETE CardanoCodecConfig #-}

{-------------------------------------------------------------------------------
  BlockConfig
-------------------------------------------------------------------------------}

-- | The 'BlockConfig' for 'CardanoBlock'.
--
-- Thanks to the pattern synonyms, you can treat this as the product of
-- the Byron, Shelley, ... 'BlockConfig's.
type CardanoBlockConfig c = BlockConfig (CardanoBlock c)

pattern CardanoBlockConfig
  :: BlockConfig ByronBlock
  -> BlockConfig (ShelleyBlock (ShelleyEra c))
  -> BlockConfig (ShelleyBlock (AllegraEra c))
  -> BlockConfig (ShelleyBlock (MaryEra c))
  -> BlockConfig (ShelleyBlock (AlonzoEra c))
  -> CardanoBlockConfig c
pattern CardanoBlockConfig cfgByron cfgShelley cfgAllegra cfgMary cfgAlonzo =
    HardForkBlockConfig {
        hardForkBlockConfigPerEra = PerEraBlockConfig
          (  cfgByron
          :* cfgShelley
          :* cfgAllegra
          :* cfgMary
          :* cfgAlonzo
          :* Nil
          )
      }

{-# COMPLETE CardanoBlockConfig #-}

{-------------------------------------------------------------------------------
  StorageConfig
-------------------------------------------------------------------------------}

-- | The 'StorageConfig' for 'CardanoBlock'.
--
-- Thanks to the pattern synonyms, you can treat this as the product of
-- the Byron, Shelley, ... 'StorageConfig's.
type CardanoStorageConfig c = StorageConfig (CardanoBlock c)

pattern CardanoStorageConfig
  :: StorageConfig ByronBlock
  -> StorageConfig (ShelleyBlock (ShelleyEra c))
  -> StorageConfig (ShelleyBlock (AllegraEra c))
  -> StorageConfig (ShelleyBlock (MaryEra c))
  -> StorageConfig (ShelleyBlock (AlonzoEra c))
  -> CardanoStorageConfig c
pattern CardanoStorageConfig cfgByron cfgShelley cfgAllegra cfgMary cfgAlonzo =
    HardForkStorageConfig {
        hardForkStorageConfigPerEra = PerEraStorageConfig
          (  cfgByron
          :* cfgShelley
          :* cfgAllegra
          :* cfgMary
          :* cfgAlonzo
          :* Nil
          )
      }

{-# COMPLETE CardanoStorageConfig #-}

{-------------------------------------------------------------------------------
  ConsensusConfig
-------------------------------------------------------------------------------}

-- | The 'ConsensusConfig' for 'CardanoBlock'.
--
-- Thanks to the pattern synonyms, you can treat this as the product of the
-- Byron, Shelley, ... 'PartialConsensusConfig's.
--
-- NOTE: not 'ConsensusConfig', but 'PartialConsensusConfig'.
type CardanoConsensusConfig c =
  ConsensusConfig (HardForkProtocol (CardanoEras c))

pattern CardanoConsensusConfig
  :: PartialConsensusConfig (BlockProtocol ByronBlock)
  -> PartialConsensusConfig (BlockProtocol (ShelleyBlock (ShelleyEra c)))
  -> PartialConsensusConfig (BlockProtocol (ShelleyBlock (AllegraEra c)))
  -> PartialConsensusConfig (BlockProtocol (ShelleyBlock (MaryEra c)))
  -> PartialConsensusConfig (BlockProtocol (ShelleyBlock (AlonzoEra c)))
  -> CardanoConsensusConfig c
pattern CardanoConsensusConfig cfgByron cfgShelley cfgAllegra cfgMary cfgAlonzo <-
    HardForkConsensusConfig {
        hardForkConsensusConfigPerEra = PerEraConsensusConfig
          (  WrapPartialConsensusConfig cfgByron
          :* WrapPartialConsensusConfig cfgShelley
          :* WrapPartialConsensusConfig cfgAllegra
          :* WrapPartialConsensusConfig cfgMary
          :* WrapPartialConsensusConfig cfgAlonzo
          :* Nil
          )
      }

{-# COMPLETE CardanoConsensusConfig #-}

{-------------------------------------------------------------------------------
  LedgerConfig
-------------------------------------------------------------------------------}

-- | The 'LedgerConfig' for 'CardanoBlock'.
--
-- Thanks to the pattern synonyms, you can treat this as the product of the
-- Byron, Shelley, ... 'PartialLedgerConfig's.
--
-- NOTE: not 'LedgerConfig', but 'PartialLedgerConfig'.
type CardanoLedgerConfig c = HardForkLedgerConfig (CardanoEras c)

pattern CardanoLedgerConfig
  :: PartialLedgerConfig ByronBlock
  -> PartialLedgerConfig (ShelleyBlock (ShelleyEra c))
  -> PartialLedgerConfig (ShelleyBlock (AllegraEra c))
  -> PartialLedgerConfig (ShelleyBlock (MaryEra c))
  -> PartialLedgerConfig (ShelleyBlock (AlonzoEra c))
  -> CardanoLedgerConfig c
pattern CardanoLedgerConfig cfgByron cfgShelley cfgAllegra cfgMary cfgAlonzo <-
    HardForkLedgerConfig {
        hardForkLedgerConfigPerEra = PerEraLedgerConfig
          (  WrapPartialLedgerConfig cfgByron
          :* WrapPartialLedgerConfig cfgShelley
          :* WrapPartialLedgerConfig cfgAllegra
          :* WrapPartialLedgerConfig cfgMary
          :* WrapPartialLedgerConfig cfgAlonzo
          :* Nil
          )
      }

{-# COMPLETE CardanoLedgerConfig #-}

{-------------------------------------------------------------------------------
  LedgerState
-------------------------------------------------------------------------------}

-- | The 'LedgerState' for 'CardanoBlock'.
--
-- NOTE: the 'CardanoLedgerState' contains more than just the current era's
-- 'LedgerState'. We don't give access to those internal details through the
-- pattern synonyms. This is also the reason the pattern synonyms are not
-- bidirectional.
type CardanoLedgerState c = LedgerState (CardanoBlock c)

pattern LedgerStateByron
  :: LedgerState ByronBlock
  -> CardanoLedgerState c
pattern LedgerStateByron st <-
    HardForkLedgerState
      (State.HardForkState
        (TeleByron (State.Current { currentState = st })))

pattern LedgerStateShelley
  :: LedgerState (ShelleyBlock (ShelleyEra c))
  -> CardanoLedgerState c
pattern LedgerStateShelley st <-
    HardForkLedgerState
      (State.HardForkState
        (TeleShelley _ (State.Current { currentState = st })))

pattern LedgerStateAllegra
  :: LedgerState (ShelleyBlock (AllegraEra c))
  -> CardanoLedgerState c
pattern LedgerStateAllegra st <-
    HardForkLedgerState
      (State.HardForkState
        (TeleAllegra _ _  (State.Current { currentState = st })))

pattern LedgerStateMary
  :: LedgerState (ShelleyBlock (MaryEra c))
  -> CardanoLedgerState c
pattern LedgerStateMary st <-
    HardForkLedgerState
      (State.HardForkState
        (TeleMary _ _ _ (State.Current { currentState = st })))

pattern LedgerStateAlonzo
  :: LedgerState (ShelleyBlock (AlonzoEra c))
  -> CardanoLedgerState c
pattern LedgerStateAlonzo st <-
    HardForkLedgerState
      (State.HardForkState
        (TeleAlonzo _ _ _ _ (State.Current { currentState = st })))

{-# COMPLETE LedgerStateByron
           , LedgerStateShelley
           , LedgerStateAllegra
           , LedgerStateMary
           , LedgerStateAlonzo #-}

{-------------------------------------------------------------------------------
  ChainDepState
-------------------------------------------------------------------------------}

-- | The 'ChainDepState' for 'CardanoBlock'.
--
-- NOTE: the 'CardanoChainDepState' contains more than just the current era's
-- 'ChainDepState'. We don't give access to those internal details through the
-- pattern synonyms. This is also the reason the pattern synonyms are not
-- bidirectional.
type CardanoChainDepState c = HardForkChainDepState (CardanoEras c)

pattern ChainDepStateByron
  :: ChainDepState (BlockProtocol ByronBlock)
  -> CardanoChainDepState c
pattern ChainDepStateByron st <-
    State.HardForkState
      (TeleByron (State.Current { currentState = WrapChainDepState st }))

pattern ChainDepStateShelley
  :: ChainDepState (BlockProtocol (ShelleyBlock (ShelleyEra c)))
  -> CardanoChainDepState c
pattern ChainDepStateShelley st <-
    State.HardForkState
      (TeleShelley _ (State.Current { currentState = WrapChainDepState st }))

pattern ChainDepStateAllegra
  :: ChainDepState (BlockProtocol (ShelleyBlock (AllegraEra c)))
  -> CardanoChainDepState c
pattern ChainDepStateAllegra st <-
    State.HardForkState
      (TeleAllegra _ _ (State.Current { currentState = WrapChainDepState st }))

pattern ChainDepStateMary
  :: ChainDepState (BlockProtocol (ShelleyBlock (MaryEra c)))
  -> CardanoChainDepState c
pattern ChainDepStateMary st <-
    State.HardForkState
      (TeleMary _ _ _ (State.Current { currentState = WrapChainDepState st }))

pattern ChainDepStateAlonzo
  :: ChainDepState (BlockProtocol (ShelleyBlock (MaryEra c)))
  -> CardanoChainDepState c
pattern ChainDepStateAlonzo st <-
    State.HardForkState
      (TeleAlonzo _ _ _ _ (State.Current { currentState = WrapChainDepState st }))

{-# COMPLETE ChainDepStateByron
           , ChainDepStateShelley
           , ChainDepStateAllegra
           , ChainDepStateMary
           , ChainDepStateAlonzo #-}
