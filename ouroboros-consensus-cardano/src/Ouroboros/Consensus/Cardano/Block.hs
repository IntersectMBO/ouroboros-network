{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE DeriveGeneric            #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE GADTs                    #-}
{-# LANGUAGE PatternSynonyms          #-}
{-# LANGUAGE ViewPatterns             #-}
module Ouroboros.Consensus.Cardano.Block (
    -- * Eras
    CardanoEras
    -- * Block
  , CardanoBlock
    -- Note: by exporting the pattern synonyms as part of the matching data
    -- type (instead of as separate patterns), we get better exhaustiveness
    -- checks from GHC. But GHC expects a data type, not a type family, that's
    -- why we sometimes mention the data type of the instance in these exports
    -- instead of the abstract type family.
  , HardForkBlock (BlockByron, BlockShelley)
    -- * Headers
  , CardanoHeader
  , Header (HeaderByron, HeaderShelley)
    -- * Generalised transactions
  , CardanoGenTx
  , GenTx (GenTxByron, GenTxShelley)
  , CardanoGenTxId
  , TxId (GenTxIdByron, GenTxIdShelley)
  , CardanoApplyTxErr
  , HardForkApplyTxErr (ApplyTxErrByron, ApplyTxErrShelley, ApplyTxErrWrongEra)
    -- * LedgerError
  , CardanoLedgerError
  , HardForkLedgerError (
        LedgerErrorByron
      , LedgerErrorShelley
      , LedgerErrorWrongEra
      )
    -- * OtherEnvelopeError
  , CardanoOtherHeaderEnvelopeError
  , HardForkEnvelopeErr (
        OtherHeaderEnvelopeErrorByron
      , OtherHeaderEnvelopeErrorShelley
      , OtherHeaderEnvelopeErrorWrongEra
      )
    -- * TipInfo
  , CardanoTipInfo
  , OneEraTipInfo (TipInfoByron, TipInfoShelley)
    -- * Query
  , CardanoQuery
  , Query (
        QueryIfCurrentByron
      , QueryIfCurrentShelley
      , QueryAnytimeByron
      , QueryAnytimeShelley
      , QueryHardFork
     )
  , CardanoQueryResult
  , Either (QueryResultSuccess, QueryResultEraMismatch)
    -- * CodecConfig
  , CardanoCodecConfig
  , CodecConfig (CardanoCodecConfig)
    -- * BlockConfig
  , CardanoBlockConfig
  , BlockConfig (CardanoBlockConfig)
    -- * ConsensusConfig
  , CardanoConsensusConfig
  , ConsensusConfig (CardanoConsensusConfig)
    -- * LedgerConfig
  , CardanoLedgerConfig
  , HardForkLedgerConfig (CardanoLedgerConfig)
    -- * LedgerState
  , CardanoLedgerState
  , LedgerState (LedgerStateByron, LedgerStateShelley)
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
import           Ouroboros.Consensus.TypeFamilyWrappers

import           Ouroboros.Consensus.HardFork.Combinator
import           Ouroboros.Consensus.HardFork.Combinator.AcrossEras
import qualified Ouroboros.Consensus.HardFork.Combinator.State as State
import qualified Ouroboros.Consensus.HardFork.Combinator.Util.Telescope as Telescope

import           Ouroboros.Consensus.Byron.Ledger.Block (ByronBlock)

import           Ouroboros.Consensus.Shelley.Ledger (ShelleyBlock)

{-------------------------------------------------------------------------------
  The eras of the Cardano bock chain
-------------------------------------------------------------------------------}

-- | First we had the Byron era and then the Shelley era.
--
-- We parameterise over the crypto used in the Shelley era: @sc@.
--
-- TODO: parameterise ByronBlock over crypto too
type CardanoEras sc = '[ByronBlock, ShelleyBlock sc]

{-------------------------------------------------------------------------------
  The block type of the Cardano block chain
-------------------------------------------------------------------------------}

-- | /The/ Cardano block.
--
-- Thanks to the pattern synonyms, you can treat this as a sum type with
-- constructors 'BlockByron' and 'BlockShelley'.
--
-- > toEither :: CardanoBlock sc -> Either ByronBlock (ShelleyBlock sc)
-- > toEither (BlockByron   b) = Left b
-- > toEither (BlockShelley s) = Right s
--
type CardanoBlock sc = HardForkBlock (CardanoEras sc)

pattern BlockByron :: ByronBlock -> CardanoBlock sc
pattern BlockByron b = HardForkBlock (OneEraBlock (Z (I b)))

pattern BlockShelley :: ShelleyBlock sc -> CardanoBlock sc
pattern BlockShelley b = HardForkBlock (OneEraBlock (S (Z (I b))))

{-# COMPLETE BlockByron, BlockShelley #-}

{-------------------------------------------------------------------------------
  Headers
-------------------------------------------------------------------------------}

-- | The Cardano header.
--
-- Thanks to the pattern synonyms, you can treat this as a sum type with
-- constructors 'HeaderByron' and 'HeaderShelley'.
type CardanoHeader sc = Header (CardanoBlock sc)

pattern HeaderByron :: Header ByronBlock -> CardanoHeader sc
pattern HeaderByron h = HardForkHeader (OneEraHeader (Z h))

pattern HeaderShelley :: Header (ShelleyBlock sc) -> CardanoHeader sc
pattern HeaderShelley h = HardForkHeader (OneEraHeader (S (Z h)))

{-# COMPLETE HeaderByron, HeaderShelley #-}

{-------------------------------------------------------------------------------
  Generalised transactions
-------------------------------------------------------------------------------}

-- | The Cardano transaction.
--
-- Thanks to the pattern synonyms, you can treat this as a sum type with
-- constructors 'GenTxByron' and 'GenTxShelley'.
type CardanoGenTx sc = GenTx (CardanoBlock sc)

pattern GenTxByron :: GenTx ByronBlock -> CardanoGenTx sc
pattern GenTxByron tx = HardForkGenTx (OneEraGenTx (Z tx))

pattern GenTxShelley :: GenTx (ShelleyBlock sc) -> CardanoGenTx sc
pattern GenTxShelley tx = HardForkGenTx (OneEraGenTx (S (Z tx)))

{-# COMPLETE GenTxByron, GenTxShelley #-}

-- | The ID of a Cardano transaction.
--
-- Thanks to the pattern synonyms, you can treat this as a sum type with
-- constructors 'GenTxIdByron' and 'GenTxIdShelley'.
type CardanoGenTxId sc = GenTxId (CardanoBlock sc)

pattern GenTxIdByron :: GenTxId ByronBlock -> CardanoGenTxId sc
pattern GenTxIdByron txid =
    HardForkGenTxId (OneEraGenTxId (Z (WrapGenTxId txid)))

pattern GenTxIdShelley :: GenTxId (ShelleyBlock sc) -> CardanoGenTxId sc
pattern GenTxIdShelley txid =
    HardForkGenTxId (OneEraGenTxId (S (Z (WrapGenTxId txid))))

{-# COMPLETE GenTxIdByron, GenTxIdShelley #-}

-- | An error resulting from applying a 'CardanoGenTx' to the ledger.
--
-- Thanks to the pattern synonyms, you can treat this as a sum type with
-- constructors 'ApplyTxByronErr', 'ApplyTxErrShelley', and
-- 'ApplyTxErrWrongEra'.
--
-- > toText :: CardanoApplyTxErr sc -> Text
-- > toText (ApplyTxErrByron b) = byronApplyTxErrToText b
-- > toText (ApplyTxErrShelley s) = shelleyApplyTxErrToText s
-- > toText (ApplyTxErrWrongEra eraMismatch) =
-- >   "Transaction from the " <> otherEraName eraMismatch <>
-- >   " era applied to a ledger from the " <>
-- >   ledgerEraName eraMismatch <> " era"
--
type CardanoApplyTxErr sc = HardForkApplyTxErr (CardanoEras sc)

pattern ApplyTxErrByron :: ApplyTxErr ByronBlock -> CardanoApplyTxErr sc
pattern ApplyTxErrByron err =
    HardForkApplyTxErrFromEra (OneEraApplyTxErr (Z (WrapApplyTxErr err)))

pattern ApplyTxErrShelley :: ApplyTxErr (ShelleyBlock sc) -> CardanoApplyTxErr sc
pattern ApplyTxErrShelley err =
    HardForkApplyTxErrFromEra (OneEraApplyTxErr (S (Z (WrapApplyTxErr err))))

pattern ApplyTxErrWrongEra :: EraMismatch -> CardanoApplyTxErr sc
pattern ApplyTxErrWrongEra eraMismatch <-
    HardForkApplyTxErrWrongEra (mkEraMismatch -> eraMismatch)

{-# COMPLETE ApplyTxErrByron, ApplyTxErrShelley, ApplyTxErrWrongEra #-}

{-------------------------------------------------------------------------------
  LedgerError
-------------------------------------------------------------------------------}

-- | An error resulting from applying a 'CardanoBlock' to the ledger.
--
-- Thanks to the pattern synonyms, you can treat this as a sum type with
-- constructors 'LedgerErrorByron', 'LedgerErrorShelley', and
-- 'LedgerErrorWrongEra'.
--
-- > toText :: CardanoLedgerError sc -> Text
-- > toText (LedgerErrorByron b) = byronLedgerErrorToText b
-- > toText (LedgerErrorShelley s) = shelleyLedgerErrorToText s
-- > toText (LedgerErrorWrongEra eraMismatch) =
-- >   "Block from the " <> otherEraName eraMismatch <>
-- >   " era applied to a ledger from the " <>
-- >   ledgerEraName eraMismatch <> " era"
--
type CardanoLedgerError sc = HardForkLedgerError (CardanoEras sc)

pattern LedgerErrorByron :: LedgerError ByronBlock -> CardanoLedgerError sc
pattern LedgerErrorByron err =
    HardForkLedgerErrorFromEra (OneEraLedgerError (Z (WrapLedgerErr err)))

pattern LedgerErrorShelley :: LedgerError (ShelleyBlock sc)
                           -> CardanoLedgerError sc
pattern LedgerErrorShelley err =
    HardForkLedgerErrorFromEra
      (OneEraLedgerError (S (Z (WrapLedgerErr err))))

pattern LedgerErrorWrongEra :: EraMismatch -> CardanoLedgerError sc
pattern LedgerErrorWrongEra eraMismatch <-
    HardForkLedgerErrorWrongEra (mkEraMismatch -> eraMismatch)

{-# COMPLETE LedgerErrorByron, LedgerErrorShelley, LedgerErrorWrongEra #-}

{-------------------------------------------------------------------------------
  OtherEnvelopeError
-------------------------------------------------------------------------------}

-- | An error resulting from validating a 'CardanoHeader'.
--
-- Thanks to the pattern synonyms, you can treat this as a sum type with
-- constructors 'OtherHeaderEnvelopeErrorByron',
-- 'OtherHeaderEnvelopeErrorShelley', and 'OtherHeaderEnvelopeErrorWrongEra'.
type CardanoOtherHeaderEnvelopeError sc = HardForkEnvelopeErr (CardanoEras sc)

pattern OtherHeaderEnvelopeErrorByron
  :: OtherHeaderEnvelopeError ByronBlock
  -> CardanoOtherHeaderEnvelopeError sc
pattern OtherHeaderEnvelopeErrorByron err =
    HardForkEnvelopeErrFromEra
      (OneEraEnvelopeErr (Z (WrapEnvelopeErr err)))

pattern OtherHeaderEnvelopeErrorShelley
  :: OtherHeaderEnvelopeError (ShelleyBlock sc)
  -> CardanoOtherHeaderEnvelopeError sc
pattern OtherHeaderEnvelopeErrorShelley err =
    HardForkEnvelopeErrFromEra (OneEraEnvelopeErr (S (Z (WrapEnvelopeErr err))))

pattern OtherHeaderEnvelopeErrorWrongEra
  :: EraMismatch
  -> CardanoOtherHeaderEnvelopeError sc
pattern OtherHeaderEnvelopeErrorWrongEra eraMismatch <-
    HardForkEnvelopeErrWrongEra (mkEraMismatch -> eraMismatch)

{-# COMPLETE OtherHeaderEnvelopeErrorByron
           , OtherHeaderEnvelopeErrorShelley
           , OtherHeaderEnvelopeErrorWrongEra #-}

{-------------------------------------------------------------------------------
  TipInfo
-------------------------------------------------------------------------------}

-- | The 'TipInfo' of the Cardano chain.
--
-- Thanks to the pattern synonyms, you can treat this as a sum type with
-- constructors 'TipInfoByron' and 'TipInfoShelley'.
type CardanoTipInfo sc = OneEraTipInfo (CardanoEras sc)

pattern TipInfoByron :: TipInfo ByronBlock -> CardanoTipInfo sc
pattern TipInfoByron ti = OneEraTipInfo (Z (WrapTipInfo ti))

pattern TipInfoShelley :: TipInfo (ShelleyBlock sc) -> CardanoTipInfo sc
pattern TipInfoShelley ti = OneEraTipInfo (S (Z (WrapTipInfo ti)))

{-# COMPLETE TipInfoByron, TipInfoShelley #-}

{-------------------------------------------------------------------------------
  Query
-------------------------------------------------------------------------------}

-- | The 'Query' of Cardano chain.
--
-- Thanks to the pattern synonyms, you can treat this as a sum type with
-- constructors 'QueryIfCurrentByron', 'QueryIfCurrentShelley',
-- 'QueryAnytimeByron', 'QueryAnytimeShelley', and 'QueryHardFork'.
type CardanoQuery sc = Query (CardanoBlock sc)

-- | Byron-specific query that can only be answered when the ledger in the
-- Byron era.
pattern QueryIfCurrentByron
  :: Query ByronBlock result
  -> CardanoQuery sc (CardanoQueryResult sc result)
pattern QueryIfCurrentByron q = QueryIfCurrent (QZ q)

-- | Shelley-specific query that can only be answered when the ledger in the
-- Shelley era.
pattern QueryIfCurrentShelley
  :: Query (ShelleyBlock sc) result
  -> CardanoQuery sc (CardanoQueryResult sc result)
pattern QueryIfCurrentShelley q = QueryIfCurrent (QS (QZ q))

-- | Query about the Byron era that can be answered anytime, i.e.,
-- independent from where the tip of the ledger is.
--
-- For example, to ask for the start of the Byron era (whether the tip of
-- the ledger is in the Byron or Shelley era), use:
--
-- > QueryAnytimeByron EraStart
--
pattern QueryAnytimeByron
  :: QueryAnytime result
  -> CardanoQuery sc result
pattern QueryAnytimeByron q = QueryAnytime q (EraIndex (Z (K ())))

-- | Query about the Shelley era that can be answered anytime, i.e.,
-- independent from where the tip of the ledger is.
--
-- For example, to ask for the start of the Shelley era (whether the tip of
-- the ledger is in the Byron or Shelley era), use:
--
-- > QueryAnytimeShelley EraStart
--
pattern QueryAnytimeShelley
  :: QueryAnytime result
  -> CardanoQuery sc result
pattern QueryAnytimeShelley q = QueryAnytime q (EraIndex (S (Z (K ()))))

{-# COMPLETE QueryIfCurrentByron
           , QueryIfCurrentShelley
           , QueryAnytimeByron
           , QueryAnytimeShelley
           , QueryHardFork #-}

-- | The result of a 'CardanoQuery'
--
-- Thanks to the pattern synonyms, you can treat this as a sum type with
-- constructors 'QueryResultSuccess' and 'QueryResultEraMismatch'.
type CardanoQueryResult sc = HardForkQueryResult (CardanoEras sc)

pattern QueryResultSuccess :: result -> CardanoQueryResult sc result
pattern QueryResultSuccess result = Right result

-- | A query from a different era than the ledger's era was sent.
pattern QueryResultEraMismatch :: EraMismatch -> CardanoQueryResult sc result
pattern QueryResultEraMismatch eraMismatch <- Left (mkEraMismatch -> eraMismatch)

{-# COMPLETE QueryResultSuccess, QueryResultEraMismatch #-}

{-------------------------------------------------------------------------------
  CodecConfig
-------------------------------------------------------------------------------}

-- | The 'CodecConfig' for 'CardanoBlock'.
--
-- Thanks to the pattern synonyms, you can treat this as the product of
-- the Byron and Shelley 'CodecConfig'.
type CardanoCodecConfig sc = CodecConfig (CardanoBlock sc)

pattern CardanoCodecConfig
  :: CodecConfig ByronBlock
  -> CodecConfig (ShelleyBlock sc)
  -> CardanoCodecConfig sc
pattern CardanoCodecConfig cfgByron cfgShelley =
    HardForkCodecConfig (PerEraCodecConfig (cfgByron :* cfgShelley :* Nil))

{-# COMPLETE CardanoCodecConfig #-}

{-------------------------------------------------------------------------------
  BlockConfig
-------------------------------------------------------------------------------}

-- | The 'BlockConfig' for 'CardanoBlock'.
--
-- Thanks to the pattern synonyms, you can treat this as the product of
-- the Byron and Shelley 'BlockConfig'.
type CardanoBlockConfig sc = BlockConfig (CardanoBlock sc)

pattern CardanoBlockConfig
  :: BlockConfig ByronBlock
  -> BlockConfig (ShelleyBlock sc)
  -> CardanoBlockConfig sc
pattern CardanoBlockConfig cfgByron cfgShelley =
    HardForkBlockConfig (PerEraBlockConfig (cfgByron :* cfgShelley :* Nil))

{-# COMPLETE CardanoBlockConfig #-}

{-------------------------------------------------------------------------------
  ConsensusConfig
-------------------------------------------------------------------------------}

-- | The 'ConsensusConfig' for 'CardanoBlock'.
--
-- Thanks to the pattern synonyms, you can treat this as the product of the
-- Byron and Shelley 'PartialConsensusConfig'.
--
-- NOTE: not 'ConsensusConfig', but 'PartialConsensusConfig'.
type CardanoConsensusConfig sc =
  ConsensusConfig (HardForkProtocol (CardanoEras sc))

pattern CardanoConsensusConfig
  :: PartialConsensusConfig (BlockProtocol ByronBlock)
  -> PartialConsensusConfig (BlockProtocol (ShelleyBlock sc))
  -> CardanoConsensusConfig sc
pattern CardanoConsensusConfig cfgByron cfgShelley <-
    HardForkConsensusConfig {
        hardForkConsensusConfigPerEra = PerEraConsensusConfig
          (   WrapPartialConsensusConfig cfgByron
           :* WrapPartialConsensusConfig cfgShelley
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
-- Byron and Shelley 'PartialLedgerConfig'.
--
-- NOTE: not 'LedgerConfig', but 'PartialLedgerConfig'.
type CardanoLedgerConfig sc = HardForkLedgerConfig (CardanoEras sc)

pattern CardanoLedgerConfig
  :: PartialLedgerConfig ByronBlock
  -> PartialLedgerConfig (ShelleyBlock sc)
  -> CardanoLedgerConfig sc
pattern CardanoLedgerConfig cfgByron cfgShelley <-
    HardForkLedgerConfig {
        hardForkLedgerConfigPerEra = PerEraLedgerConfig
          (   WrapPartialLedgerConfig cfgByron
           :* WrapPartialLedgerConfig cfgShelley
           :* Nil
          )
      }

{-# COMPLETE CardanoLedgerConfig #-}

{-------------------------------------------------------------------------------
  LedgerState
-------------------------------------------------------------------------------}

-- | The 'LedgerState' for 'CardanoBlock'.
--
-- Thanks to the pattern synonyms, you can inspect the current era's
-- 'LedgerState' using the 'LedgerStateByron' and 'LedgerStateShelley'
-- constructors.
--
-- NOTE: the 'CardanoLedgerState' contains more than just the current era's
-- 'LedgerState'. We don't give access to those internal details through the
-- pattern synonyms. This is also the reason the pattern synonyms are not
-- bidirectional.
type CardanoLedgerState sc = LedgerState (CardanoBlock sc)

pattern LedgerStateByron
  :: LedgerState ByronBlock
  -> CardanoLedgerState sc
pattern LedgerStateByron st <-
    HardForkLedgerState
      (State.HardForkState
        (Telescope.TZ (State.Current { currentState = st })))

pattern LedgerStateShelley
  :: LedgerState (ShelleyBlock sc)
  -> CardanoLedgerState sc
pattern LedgerStateShelley st <-
    HardForkLedgerState
      (State.HardForkState
        (Telescope.TS _ (Telescope.TZ (State.Current { currentState = st }))))
