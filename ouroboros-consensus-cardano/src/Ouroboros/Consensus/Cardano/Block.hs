{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE DeriveGeneric            #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE GADTs                    #-}
{-# LANGUAGE PatternSynonyms          #-}
{-# LANGUAGE ViewPatterns             #-}
module Ouroboros.Consensus.Cardano.Block (
    -- * Eras
    ShelleyEra
  , CardanoEras
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

import qualified Shelley.Spec.Ledger.BaseTypes as Era (Shelley)

import           Ouroboros.Consensus.Shelley.Ledger (ShelleyBlock)


{-------------------------------------------------------------------------------
  The eras of the Cardano bock chain
-------------------------------------------------------------------------------}

-- | The Shelley ledger and block type itself is parameterised by an era
-- parameter, which is in its turn parameterised by the crypto used.
type ShelleyEra c = Era.Shelley c

-- | First we had the Byron era and then the Shelley era.
--
-- We parameterise over the crypto used in the Shelley era: @c@.
--
-- TODO: parameterise ByronBlock over crypto too
type CardanoEras c = '[ByronBlock, ShelleyBlock (ShelleyEra c)]

{-------------------------------------------------------------------------------
  The block type of the Cardano block chain
-------------------------------------------------------------------------------}

-- | /The/ Cardano block.
--
-- Thanks to the pattern synonyms, you can treat this as a sum type with
-- constructors 'BlockByron' and 'BlockShelley'.
--
-- > toEither :: CardanoBlock c -> Either ByronBlock (ShelleyBlock (ShelleyEra c))
-- > toEither (BlockByron   b) = Left b
-- > toEither (BlockShelley s) = Right s
--
type CardanoBlock c = HardForkBlock (CardanoEras c)

pattern BlockByron :: ByronBlock -> CardanoBlock c
pattern BlockByron b = HardForkBlock (OneEraBlock (Z (I b)))

pattern BlockShelley :: ShelleyBlock (ShelleyEra c) -> CardanoBlock c
pattern BlockShelley b = HardForkBlock (OneEraBlock (S (Z (I b))))

{-# COMPLETE BlockByron, BlockShelley #-}

{-------------------------------------------------------------------------------
  Headers
-------------------------------------------------------------------------------}

-- | The Cardano header.
--
-- Thanks to the pattern synonyms, you can treat this as a sum type with
-- constructors 'HeaderByron' and 'HeaderShelley'.
type CardanoHeader c = Header (CardanoBlock c)

pattern HeaderByron :: Header ByronBlock -> CardanoHeader c
pattern HeaderByron h = HardForkHeader (OneEraHeader (Z h))

pattern HeaderShelley ::
     Header (ShelleyBlock (ShelleyEra c))
  -> CardanoHeader c
pattern HeaderShelley h = HardForkHeader (OneEraHeader (S (Z h)))

{-# COMPLETE HeaderByron, HeaderShelley #-}

{-------------------------------------------------------------------------------
  Generalised transactions
-------------------------------------------------------------------------------}

-- | The Cardano transaction.
--
-- Thanks to the pattern synonyms, you can treat this as a sum type with
-- constructors 'GenTxByron' and 'GenTxShelley'.
type CardanoGenTx c = GenTx (CardanoBlock c)

pattern GenTxByron :: GenTx ByronBlock -> CardanoGenTx c
pattern GenTxByron tx = HardForkGenTx (OneEraGenTx (Z tx))

pattern GenTxShelley :: GenTx (ShelleyBlock (ShelleyEra c)) -> CardanoGenTx c
pattern GenTxShelley tx = HardForkGenTx (OneEraGenTx (S (Z tx)))

{-# COMPLETE GenTxByron, GenTxShelley #-}

-- | The ID of a Cardano transaction.
--
-- Thanks to the pattern synonyms, you can treat this as a sum type with
-- constructors 'GenTxIdByron' and 'GenTxIdShelley'.
type CardanoGenTxId c = GenTxId (CardanoBlock c)

pattern GenTxIdByron :: GenTxId ByronBlock -> CardanoGenTxId c
pattern GenTxIdByron txid =
    HardForkGenTxId (OneEraGenTxId (Z (WrapGenTxId txid)))

pattern GenTxIdShelley ::
     GenTxId (ShelleyBlock (ShelleyEra c))
  -> CardanoGenTxId c
pattern GenTxIdShelley txid =
    HardForkGenTxId (OneEraGenTxId (S (Z (WrapGenTxId txid))))

{-# COMPLETE GenTxIdByron, GenTxIdShelley #-}

-- | An error resulting from applying a 'CardanoGenTx' to the ledger.
--
-- Thanks to the pattern synonyms, you can treat this as a sum type with
-- constructors 'ApplyTxByronErr', 'ApplyTxErrShelley', and
-- 'ApplyTxErrWrongEra'.
--
-- > toText :: CardanoApplyTxErr c -> Text
-- > toText (ApplyTxErrByron b) = byronApplyTxErrToText b
-- > toText (ApplyTxErrShelley s) = shelleyApplyTxErrToText s
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

pattern ApplyTxErrWrongEra :: EraMismatch -> CardanoApplyTxErr c
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
-- > toText :: CardanoLedgerError c -> Text
-- > toText (LedgerErrorByron b) = byronLedgerErrorToText b
-- > toText (LedgerErrorShelley s) = shelleyLedgerErrorToText s
-- > toText (LedgerErrorWrongEra eraMismatch) =
-- >   "Block from the " <> otherEraName eraMismatch <>
-- >   " era applied to a ledger from the " <>
-- >   ledgerEraName eraMismatch <> " era"
--
type CardanoLedgerError c = HardForkLedgerError (CardanoEras c)

pattern LedgerErrorByron :: LedgerError ByronBlock -> CardanoLedgerError c
pattern LedgerErrorByron err =
    HardForkLedgerErrorFromEra (OneEraLedgerError (Z (WrapLedgerErr err)))

pattern LedgerErrorShelley :: LedgerError (ShelleyBlock (ShelleyEra c))
                           -> CardanoLedgerError c
pattern LedgerErrorShelley err =
    HardForkLedgerErrorFromEra
      (OneEraLedgerError (S (Z (WrapLedgerErr err))))

pattern LedgerErrorWrongEra :: EraMismatch -> CardanoLedgerError c
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

pattern OtherHeaderEnvelopeErrorWrongEra
  :: EraMismatch
  -> CardanoOtherHeaderEnvelopeError c
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
type CardanoTipInfo c = OneEraTipInfo (CardanoEras c)

pattern TipInfoByron :: TipInfo ByronBlock -> CardanoTipInfo c
pattern TipInfoByron ti = OneEraTipInfo (Z (WrapTipInfo ti))

pattern TipInfoShelley ::
     TipInfo (ShelleyBlock (ShelleyEra c))
  -> CardanoTipInfo c
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
type CardanoQuery c = Query (CardanoBlock c)

-- | Byron-specific query that can only be answered when the ledger in the
-- Byron era.
pattern QueryIfCurrentByron
  :: ()
  => CardanoQueryResult c result ~ a
  => Query ByronBlock result
  -> CardanoQuery c a
pattern QueryIfCurrentByron q = QueryIfCurrent (QZ q)

-- | Shelley-specific query that can only be answered when the ledger in the
-- Shelley era.
pattern QueryIfCurrentShelley
  :: ()
  => CardanoQueryResult c result ~ a
  => Query (ShelleyBlock (ShelleyEra c)) result
  -> CardanoQuery c a
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
  -> CardanoQuery c result
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
  -> CardanoQuery c result
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
-- the Byron and Shelley 'CodecConfig'.
type CardanoCodecConfig c = CodecConfig (CardanoBlock c)

pattern CardanoCodecConfig
  :: CodecConfig ByronBlock
  -> CodecConfig (ShelleyBlock (ShelleyEra c))
  -> CardanoCodecConfig c
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
type CardanoBlockConfig c = BlockConfig (CardanoBlock c)

pattern CardanoBlockConfig
  :: BlockConfig ByronBlock
  -> BlockConfig (ShelleyBlock (ShelleyEra c))
  -> CardanoBlockConfig c
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
type CardanoConsensusConfig c =
  ConsensusConfig (HardForkProtocol (CardanoEras c))

pattern CardanoConsensusConfig
  :: PartialConsensusConfig (BlockProtocol ByronBlock)
  -> PartialConsensusConfig (BlockProtocol (ShelleyBlock (ShelleyEra c)))
  -> CardanoConsensusConfig c
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
type CardanoLedgerConfig c = HardForkLedgerConfig (CardanoEras c)

pattern CardanoLedgerConfig
  :: PartialLedgerConfig ByronBlock
  -> PartialLedgerConfig (ShelleyBlock (ShelleyEra c))
  -> CardanoLedgerConfig c
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
type CardanoLedgerState c = LedgerState (CardanoBlock c)

pattern LedgerStateByron
  :: LedgerState ByronBlock
  -> CardanoLedgerState c
pattern LedgerStateByron st <-
    HardForkLedgerState
      (State.HardForkState
        (Telescope.TZ (State.Current { currentState = st })))

pattern LedgerStateShelley
  :: LedgerState (ShelleyBlock (ShelleyEra c))
  -> CardanoLedgerState c
pattern LedgerStateShelley st <-
    HardForkLedgerState
      (State.HardForkState
        (Telescope.TS _ (Telescope.TZ (State.Current { currentState = st }))))
