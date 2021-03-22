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
  , HardForkBlock (BlockByron, BlockShelley, BlockAllegra, BlockMary)
    -- * Headers
  , CardanoHeader
  , Header (HeaderByron, HeaderShelley, HeaderAllegra, HeaderMary)
    -- * Generalised transactions
  , CardanoApplyTxErr
  , CardanoGenTx
  , CardanoGenTxId
  , GenTx (GenTxByron, GenTxShelley, GenTxAllegra, GenTxMary)
  , HardForkApplyTxErr (ApplyTxErrByron, ApplyTxErrShelley, ApplyTxErrAllegra, ApplyTxErrMary, ApplyTxErrWrongEra)
  , TxId (GenTxIdByron, GenTxIdShelley, GenTxIdAllegra, GenTxIdMary)
    -- * LedgerError
  , CardanoLedgerError
  , HardForkLedgerError (LedgerErrorByron, LedgerErrorShelley, LedgerErrorAllegra, LedgerErrorMary, LedgerErrorWrongEra)
    -- * OtherEnvelopeError
  , CardanoOtherHeaderEnvelopeError
  , HardForkEnvelopeErr (OtherHeaderEnvelopeErrorByron, OtherHeaderEnvelopeErrorShelley, OtherHeaderEnvelopeErrorAllegra, OtherHeaderEnvelopeErrorMary, OtherHeaderEnvelopeErrorWrongEra)
    -- * TipInfo
  , CardanoTipInfo
  , OneEraTipInfo (TipInfoByron, TipInfoShelley, TipInfoAllegra, TipInfoMary)
    -- * Query
  , CardanoQuery
  , CardanoQueryResult
  , Either (QueryResultSuccess, QueryResultEraMismatch)
  , Query (QueryIfCurrentByron, QueryIfCurrentShelley, QueryIfCurrentAllegra, QueryIfCurrentMary, QueryAnytimeByron, QueryAnytimeShelley, QueryAnytimeAllegra, QueryAnytimeMary, QueryHardFork)
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
  , LedgerState (LedgerStateByron, LedgerStateShelley, LedgerStateAllegra, LedgerStateMary)
    -- * ChainDepState
  , CardanoChainDepState
  , HardForkState (ChainDepStateByron, ChainDepStateShelley, ChainDepStateAllegra, ChainDepStateMary)
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
   ]

-- | The Shelley-based eras in the Cardano chain
type ShelleyBasedEras c = '[ShelleyEra c, AllegraEra c, MaryEra c]

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
--
type CardanoBlock c = HardForkBlock (CardanoEras c)

pattern BlockByron :: ByronBlock -> CardanoBlock c
pattern BlockByron b = HardForkBlock (OneEraBlock (Z (I b)))

pattern BlockShelley :: ShelleyBlock (ShelleyEra c) -> CardanoBlock c
pattern BlockShelley b = HardForkBlock (OneEraBlock (S (Z (I b))))

pattern BlockAllegra :: ShelleyBlock (AllegraEra c) -> CardanoBlock c
pattern BlockAllegra b = HardForkBlock (OneEraBlock (S (S (Z (I b)))))

pattern BlockMary :: ShelleyBlock (MaryEra c) -> CardanoBlock c
pattern BlockMary b = HardForkBlock (OneEraBlock (S (S (S (Z (I b))))))

{-# COMPLETE BlockByron, BlockShelley, BlockAllegra, BlockMary #-}

{-------------------------------------------------------------------------------
  Headers
-------------------------------------------------------------------------------}

-- | The Cardano header.
type CardanoHeader c = Header (CardanoBlock c)

pattern HeaderByron :: Header ByronBlock -> CardanoHeader c
pattern HeaderByron h = HardForkHeader (OneEraHeader (Z h))

pattern HeaderShelley ::
     Header (ShelleyBlock (ShelleyEra c))
  -> CardanoHeader c
pattern HeaderShelley h = HardForkHeader (OneEraHeader (S (Z h)))

pattern HeaderAllegra ::
     Header (ShelleyBlock (AllegraEra c))
  -> CardanoHeader c
pattern HeaderAllegra h = HardForkHeader (OneEraHeader (S (S (Z h))))

pattern HeaderMary ::
     Header (ShelleyBlock (MaryEra c))
  -> CardanoHeader c
pattern HeaderMary h = HardForkHeader (OneEraHeader (S (S (S (Z h)))))

{-# COMPLETE HeaderByron, HeaderShelley, HeaderAllegra, HeaderMary #-}

{-------------------------------------------------------------------------------
  Generalised transactions
-------------------------------------------------------------------------------}

-- | The Cardano transaction.
type CardanoGenTx c = GenTx (CardanoBlock c)

pattern GenTxByron :: GenTx ByronBlock -> CardanoGenTx c
pattern GenTxByron tx = HardForkGenTx (OneEraGenTx (Z tx))

pattern GenTxShelley :: GenTx (ShelleyBlock (ShelleyEra c)) -> CardanoGenTx c
pattern GenTxShelley tx = HardForkGenTx (OneEraGenTx (S (Z tx)))

pattern GenTxAllegra :: GenTx (ShelleyBlock (AllegraEra c)) -> CardanoGenTx c
pattern GenTxAllegra tx = HardForkGenTx (OneEraGenTx (S (S (Z tx))))

pattern GenTxMary :: GenTx (ShelleyBlock (MaryEra c)) -> CardanoGenTx c
pattern GenTxMary tx = HardForkGenTx (OneEraGenTx (S (S (S (Z tx)))))

{-# COMPLETE GenTxByron, GenTxShelley, GenTxAllegra, GenTxMary #-}

-- | The ID of a Cardano transaction.
type CardanoGenTxId c = GenTxId (CardanoBlock c)

pattern GenTxIdByron :: GenTxId ByronBlock -> CardanoGenTxId c
pattern GenTxIdByron txid =
    HardForkGenTxId (OneEraGenTxId (Z (WrapGenTxId txid)))

pattern GenTxIdShelley ::
     GenTxId (ShelleyBlock (ShelleyEra c))
  -> CardanoGenTxId c
pattern GenTxIdShelley txid =
    HardForkGenTxId (OneEraGenTxId (S (Z (WrapGenTxId txid))))

pattern GenTxIdAllegra ::
     GenTxId (ShelleyBlock (AllegraEra c))
  -> CardanoGenTxId c
pattern GenTxIdAllegra txid =
    HardForkGenTxId (OneEraGenTxId (S (S (Z (WrapGenTxId txid)))))

pattern GenTxIdMary ::
     GenTxId (ShelleyBlock (MaryEra c))
  -> CardanoGenTxId c
pattern GenTxIdMary txid =
    HardForkGenTxId (OneEraGenTxId (S (S (S (Z (WrapGenTxId txid))))))

{-# COMPLETE GenTxIdByron, GenTxIdShelley, GenTxIdAllegra, GenTxIdMary #-}

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
    HardForkApplyTxErrFromEra (OneEraApplyTxErr (Z (WrapApplyTxErr err)))

pattern ApplyTxErrShelley ::
     ApplyTxErr (ShelleyBlock (ShelleyEra c))
  -> CardanoApplyTxErr c
pattern ApplyTxErrShelley err =
    HardForkApplyTxErrFromEra (OneEraApplyTxErr (S (Z (WrapApplyTxErr err))))

pattern ApplyTxErrAllegra ::
     ApplyTxErr (ShelleyBlock (AllegraEra c))
  -> CardanoApplyTxErr c
pattern ApplyTxErrAllegra err =
    HardForkApplyTxErrFromEra (OneEraApplyTxErr (S (S (Z (WrapApplyTxErr err)))))

pattern ApplyTxErrMary ::
     ApplyTxErr (ShelleyBlock (MaryEra c))
  -> CardanoApplyTxErr c
pattern ApplyTxErrMary err =
    HardForkApplyTxErrFromEra (OneEraApplyTxErr (S (S (S (Z (WrapApplyTxErr err))))))

pattern ApplyTxErrWrongEra :: EraMismatch -> CardanoApplyTxErr c
pattern ApplyTxErrWrongEra eraMismatch <-
    HardForkApplyTxErrWrongEra (mkEraMismatch -> eraMismatch)

{-# COMPLETE ApplyTxErrByron
           , ApplyTxErrShelley
           , ApplyTxErrAllegra
           , ApplyTxErrMary
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
    HardForkLedgerErrorFromEra (OneEraLedgerError (Z (WrapLedgerErr err)))

pattern LedgerErrorShelley ::
     LedgerError (ShelleyBlock (ShelleyEra c))
  -> CardanoLedgerError c
pattern LedgerErrorShelley err =
    HardForkLedgerErrorFromEra
      (OneEraLedgerError (S (Z (WrapLedgerErr err))))

pattern LedgerErrorAllegra ::
     LedgerError (ShelleyBlock (AllegraEra c))
  -> CardanoLedgerError c
pattern LedgerErrorAllegra err =
    HardForkLedgerErrorFromEra
      (OneEraLedgerError (S (S (Z (WrapLedgerErr err)))))

pattern LedgerErrorMary ::
     LedgerError (ShelleyBlock (MaryEra c))
  -> CardanoLedgerError c
pattern LedgerErrorMary err =
    HardForkLedgerErrorFromEra
      (OneEraLedgerError (S (S (S (Z (WrapLedgerErr err))))))

pattern LedgerErrorWrongEra :: EraMismatch -> CardanoLedgerError c
pattern LedgerErrorWrongEra eraMismatch <-
    HardForkLedgerErrorWrongEra (mkEraMismatch -> eraMismatch)

{-# COMPLETE LedgerErrorByron
           , LedgerErrorShelley
           , LedgerErrorAllegra
           , LedgerErrorMary
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
      (OneEraEnvelopeErr (Z (WrapEnvelopeErr err)))

pattern OtherHeaderEnvelopeErrorShelley
  :: OtherHeaderEnvelopeError (ShelleyBlock (ShelleyEra c))
  -> CardanoOtherHeaderEnvelopeError c
pattern OtherHeaderEnvelopeErrorShelley err =
    HardForkEnvelopeErrFromEra (OneEraEnvelopeErr (S (Z (WrapEnvelopeErr err))))

pattern OtherHeaderEnvelopeErrorAllegra
  :: OtherHeaderEnvelopeError (ShelleyBlock (AllegraEra c))
  -> CardanoOtherHeaderEnvelopeError c
pattern OtherHeaderEnvelopeErrorAllegra err =
    HardForkEnvelopeErrFromEra (OneEraEnvelopeErr (S (S (Z (WrapEnvelopeErr err)))))

pattern OtherHeaderEnvelopeErrorMary
  :: OtherHeaderEnvelopeError (ShelleyBlock (MaryEra c))
  -> CardanoOtherHeaderEnvelopeError c
pattern OtherHeaderEnvelopeErrorMary err =
    HardForkEnvelopeErrFromEra (OneEraEnvelopeErr (S (S (S (Z (WrapEnvelopeErr err))))))

pattern OtherHeaderEnvelopeErrorWrongEra
  :: EraMismatch
  -> CardanoOtherHeaderEnvelopeError c
pattern OtherHeaderEnvelopeErrorWrongEra eraMismatch <-
    HardForkEnvelopeErrWrongEra (mkEraMismatch -> eraMismatch)

{-# COMPLETE OtherHeaderEnvelopeErrorByron
           , OtherHeaderEnvelopeErrorShelley
           , OtherHeaderEnvelopeErrorAllegra
           , OtherHeaderEnvelopeErrorMary
           , OtherHeaderEnvelopeErrorWrongEra #-}

{-------------------------------------------------------------------------------
  TipInfo
-------------------------------------------------------------------------------}

-- | The 'TipInfo' of the Cardano chain.
type CardanoTipInfo c = OneEraTipInfo (CardanoEras c)

pattern TipInfoByron :: TipInfo ByronBlock -> CardanoTipInfo c
pattern TipInfoByron ti = OneEraTipInfo (Z (WrapTipInfo ti))

pattern TipInfoShelley ::
     TipInfo (ShelleyBlock (ShelleyEra c))
  -> CardanoTipInfo c
pattern TipInfoShelley ti = OneEraTipInfo (S (Z (WrapTipInfo ti)))

pattern TipInfoAllegra ::
     TipInfo (ShelleyBlock (AllegraEra c))
  -> CardanoTipInfo c
pattern TipInfoAllegra ti = OneEraTipInfo (S (S (Z (WrapTipInfo ti))))

pattern TipInfoMary ::
     TipInfo (ShelleyBlock (MaryEra c))
  -> CardanoTipInfo c
pattern TipInfoMary ti = OneEraTipInfo (S (S (S (Z (WrapTipInfo ti)))))

{-# COMPLETE TipInfoByron, TipInfoShelley, TipInfoAllegra, TipInfoMary #-}

{-------------------------------------------------------------------------------
  Query
-------------------------------------------------------------------------------}

-- | The 'Query' of Cardano chain.
type CardanoQuery c = Query (CardanoBlock c)

-- | Byron-specific query that can only be answered when the ledger is in the
-- Byron era.
pattern QueryIfCurrentByron
  :: ()
  => CardanoQueryResult c result ~ a
  => Query ByronBlock result
  -> CardanoQuery c a
pattern QueryIfCurrentByron q = QueryIfCurrent (QZ q)

-- | Shelley-specific query that can only be answered when the ledger is in the
-- Shelley era.
pattern QueryIfCurrentShelley
  :: ()
  => CardanoQueryResult c result ~ a
  => Query (ShelleyBlock (ShelleyEra c)) result
  -> CardanoQuery c a
pattern QueryIfCurrentShelley q = QueryIfCurrent (QS (QZ q))

-- | Allegra-specific query that can only be answered when the ledger is in the
-- Allegra era.
pattern QueryIfCurrentAllegra
  :: ()
  => CardanoQueryResult c result ~ a
  => Query (ShelleyBlock (AllegraEra c)) result
  -> CardanoQuery c a
pattern QueryIfCurrentAllegra q = QueryIfCurrent (QS (QS (QZ q)))

-- | Mary-specific query that can only be answered when the ledger is in the
-- Mary era.
pattern QueryIfCurrentMary
  :: ()
  => CardanoQueryResult c result ~ a
  => Query (ShelleyBlock (MaryEra c)) result
  -> CardanoQuery c a
pattern QueryIfCurrentMary q = QueryIfCurrent (QS (QS (QS (QZ q))))

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
pattern QueryAnytimeByron q = QueryAnytime q (EraIndex (Z (K ())))

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
pattern QueryAnytimeShelley q = QueryAnytime q (EraIndex (S (Z (K ()))))

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
pattern QueryAnytimeAllegra q = QueryAnytime q (EraIndex (S (S (Z (K ())))))

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
pattern QueryAnytimeMary q = QueryAnytime q (EraIndex (S (S (S (Z (K ()))))))

{-# COMPLETE QueryIfCurrentByron
           , QueryIfCurrentShelley
           , QueryIfCurrentAllegra
           , QueryIfCurrentMary
           , QueryAnytimeByron
           , QueryAnytimeShelley
           , QueryAnytimeAllegra
           , QueryAnytimeMary
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
  -> CardanoCodecConfig c
pattern CardanoCodecConfig cfgByron cfgShelley cfgAllegra cfgMary =
    HardForkCodecConfig {
        hardForkCodecConfigPerEra = PerEraCodecConfig
          (  cfgByron
          :* cfgShelley
          :* cfgAllegra
          :* cfgMary
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
  -> CardanoBlockConfig c
pattern CardanoBlockConfig cfgByron cfgShelley cfgAllegra cfgMary =
    HardForkBlockConfig {
        hardForkBlockConfigPerEra = PerEraBlockConfig
          (  cfgByron
          :* cfgShelley
          :* cfgAllegra
          :* cfgMary
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
  -> CardanoStorageConfig c
pattern CardanoStorageConfig cfgByron cfgShelley cfgAllegra cfgMary =
    HardForkStorageConfig {
        hardForkStorageConfigPerEra = PerEraStorageConfig
          (  cfgByron
          :* cfgShelley
          :* cfgAllegra
          :* cfgMary
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
  -> CardanoConsensusConfig c
pattern CardanoConsensusConfig cfgByron cfgShelley cfgAllegra cfgMary <-
    HardForkConsensusConfig {
        hardForkConsensusConfigPerEra = PerEraConsensusConfig
          (  WrapPartialConsensusConfig cfgByron
          :* WrapPartialConsensusConfig cfgShelley
          :* WrapPartialConsensusConfig cfgAllegra
          :* WrapPartialConsensusConfig cfgMary
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
  -> CardanoLedgerConfig c
pattern CardanoLedgerConfig cfgByron cfgShelley cfgAllegra cfgMary <-
    HardForkLedgerConfig {
        hardForkLedgerConfigPerEra = PerEraLedgerConfig
          (  WrapPartialLedgerConfig cfgByron
          :* WrapPartialLedgerConfig cfgShelley
          :* WrapPartialLedgerConfig cfgAllegra
          :* WrapPartialLedgerConfig cfgMary
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
        (TZ (State.Current { currentState = st })))

pattern LedgerStateShelley
  :: LedgerState (ShelleyBlock (ShelleyEra c))
  -> CardanoLedgerState c
pattern LedgerStateShelley st <-
    HardForkLedgerState
      (State.HardForkState
        (TS _ (TZ (State.Current { currentState = st }))))

pattern LedgerStateAllegra
  :: LedgerState (ShelleyBlock (AllegraEra c))
  -> CardanoLedgerState c
pattern LedgerStateAllegra st <-
    HardForkLedgerState
      (State.HardForkState
        (TS _ (TS _ (TZ (State.Current { currentState = st })))))

pattern LedgerStateMary
  :: LedgerState (ShelleyBlock (MaryEra c))
  -> CardanoLedgerState c
pattern LedgerStateMary st <-
    HardForkLedgerState
      (State.HardForkState
        (TS _ (TS _ (TS _ (TZ (State.Current { currentState = st }))))))

{-# COMPLETE LedgerStateByron
           , LedgerStateShelley
           , LedgerStateAllegra
           , LedgerStateMary #-}

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
      (TZ (State.Current { currentState = WrapChainDepState st }))

pattern ChainDepStateShelley
  :: ChainDepState (BlockProtocol (ShelleyBlock (ShelleyEra c)))
  -> CardanoChainDepState c
pattern ChainDepStateShelley st <-
    State.HardForkState
      (TS _ (TZ (State.Current { currentState = WrapChainDepState st })))

pattern ChainDepStateAllegra
  :: ChainDepState (BlockProtocol (ShelleyBlock (AllegraEra c)))
  -> CardanoChainDepState c
pattern ChainDepStateAllegra st <-
    State.HardForkState
      (TS _ (TS _ (TZ (State.Current { currentState = WrapChainDepState st }))))

pattern ChainDepStateMary
  :: ChainDepState (BlockProtocol (ShelleyBlock (MaryEra c)))
  -> CardanoChainDepState c
pattern ChainDepStateMary st <-
    State.HardForkState
      (TS _ (TS _ (TS _ (TZ (State.Current { currentState = WrapChainDepState st })))))

{-# COMPLETE ChainDepStateByron
           , ChainDepStateShelley
           , ChainDepStateAllegra
           , ChainDepStateMary #-}
