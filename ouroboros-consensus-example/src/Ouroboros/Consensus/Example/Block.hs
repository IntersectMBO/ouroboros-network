{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE DeriveGeneric            #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE GADTs                    #-}
{-# LANGUAGE PatternSynonyms          #-}
{-# LANGUAGE ViewPatterns             #-}
module Ouroboros.Consensus.Example.Block (
    -- * Eras
    -- XXX
    ExampleEras
  , module Ouroboros.Consensus.Example.Eras
  , ShelleyBasedExampleEras
    -- * Block
  , ExampleBlock
    -- Note: by exporting the pattern synonyms as part of the matching data
    -- type (instead of as separate patterns), we get better exhaustiveness
    -- checks from GHC. But GHC expects a data type, not a type family, that's
    -- why we sometimes mention the data type of the instance in these exports
    -- instead of the abstract type family.
  , HardForkBlock (BlockShelley, BlockExample)
    -- * Headers
  , ExampleHeader
  , Header (HeaderShelley, HeaderExample)
    -- * Generalised transactions
  , ExampleApplyTxErr
  , ExampleGenTx
  , ExampleGenTxId
  , GenTx (GenTxShelley, GenTxExample)
  , HardForkApplyTxErr (ApplyTxErrShelley, ApplyTxErrExample, ApplyTxErrWrongEra)
  , TxId (GenTxIdShelley, GenTxIdExample)
    -- * LedgerError
  , ExampleLedgerError
  , HardForkLedgerError (LedgerErrorShelley, LedgerErrorExample, LedgerErrorWrongEra)
    -- * OtherEnvelopeError
  , ExampleOtherHeaderEnvelopeError
  , HardForkEnvelopeErr (OtherHeaderEnvelopeErrorShelley, OtherHeaderEnvelopeErrorExample, OtherHeaderEnvelopeErrorWrongEra)
    -- * TipInfo
  , ExampleTipInfo
  , OneEraTipInfo (TipInfoShelley, TipInfoExample)
    -- * Query
  , Either (QueryResultSuccess, QueryResultEraMismatch)
  , ExampleQuery
  , ExampleQueryResult
  , Query (QueryIfCurrentShelley, QueryIfCurrentExample, QueryAnytimeShelley, QueryAnytimeExample, QueryHardFork)
    -- * CodecConfig
  , CodecConfig (ExampleCodecConfig)
  , ExampleCodecConfig
    -- * BlockConfig
  , BlockConfig (ExampleBlockConfig)
  , ExampleBlockConfig
    -- * StorageConfig
  , ExampleStorageConfig
  , StorageConfig (ExampleStorageConfig)
    -- * ConsensusConfig
  , ConsensusConfig (ExampleConsensusConfig)
  , ExampleConsensusConfig
    -- * LedgerConfig
  , ExampleLedgerConfig
  , HardForkLedgerConfig (ExampleLedgerConfig)
    -- * LedgerState
  , ExampleLedgerState
  , LedgerState (LedgerStateShelley, LedgerStateExample)
    -- * ChainDepState
  , ExampleChainDepState
  , HardForkState (ChainDepStateShelley, ChainDepStateExample)
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

import           Ouroboros.Consensus.Example.Eras
import           Ouroboros.Consensus.Shelley.Ledger (ShelleyBlock)

{-------------------------------------------------------------------------------
  The eras of the Example block chain
-------------------------------------------------------------------------------}

-- | The eras in the Example blockchain.
--
-- We parameterise over the crypto: @c@.
type ExampleEras c =
  '[ ShelleyBlock (ShelleyEra c)
   , ShelleyBlock (ExampleEra c)
   ]

type ShelleyBasedExampleEras c =
  '[ ShelleyEra c
   , ExampleEra c
   ]

{-------------------------------------------------------------------------------
  The block type of the Example block chain
-------------------------------------------------------------------------------}

-- | /The/ Example block.
--
-- Thanks to the pattern synonyms, you can treat this as a sum type with
-- constructors 'BlockShelley' and 'BlockExample'.
--
-- > f :: ExampleBlock c -> _
-- > f (BlockShelley s) = _
-- > f (BlockExample a) = _
--
type ExampleBlock c = HardForkBlock (ExampleEras c)

pattern BlockShelley :: ShelleyBlock (ShelleyEra c) -> ExampleBlock c
pattern BlockShelley b = HardForkBlock (OneEraBlock (Z (I b)))

pattern BlockExample :: ShelleyBlock (ExampleEra c) -> ExampleBlock c
pattern BlockExample b = HardForkBlock (OneEraBlock (S (Z (I b))))

{-# COMPLETE BlockShelley, BlockExample #-}

{-------------------------------------------------------------------------------
  Headers
-------------------------------------------------------------------------------}

-- | The Example header.
type ExampleHeader c = Header (ExampleBlock c)

pattern HeaderShelley ::
     Header (ShelleyBlock (ShelleyEra c))
  -> ExampleHeader c
pattern HeaderShelley h = HardForkHeader (OneEraHeader (Z h))

pattern HeaderExample ::
     Header (ShelleyBlock (ExampleEra c))
  -> ExampleHeader c
pattern HeaderExample h = HardForkHeader (OneEraHeader (S (Z h)))

{-# COMPLETE HeaderShelley, HeaderExample #-}

{-------------------------------------------------------------------------------
  Generalised transactions
-------------------------------------------------------------------------------}

-- | The Example transaction.
type ExampleGenTx c = GenTx (ExampleBlock c)

pattern GenTxShelley :: GenTx (ShelleyBlock (ShelleyEra c)) -> ExampleGenTx c
pattern GenTxShelley tx = HardForkGenTx (OneEraGenTx (Z tx))

pattern GenTxExample :: GenTx (ShelleyBlock (ExampleEra c)) -> ExampleGenTx c
pattern GenTxExample tx = HardForkGenTx (OneEraGenTx (S (Z tx)))

{-# COMPLETE GenTxShelley, GenTxExample #-}

-- | The ID of an Example transaction.
type ExampleGenTxId c = GenTxId (ExampleBlock c)

pattern GenTxIdShelley ::
     GenTxId (ShelleyBlock (ShelleyEra c))
  -> ExampleGenTxId c
pattern GenTxIdShelley txid =
    HardForkGenTxId (OneEraGenTxId (Z (WrapGenTxId txid)))

pattern GenTxIdExample ::
     GenTxId (ShelleyBlock (ExampleEra c))
  -> ExampleGenTxId c
pattern GenTxIdExample txid =
    HardForkGenTxId (OneEraGenTxId (S (Z (WrapGenTxId txid))))

{-# COMPLETE GenTxIdShelley, GenTxIdExample #-}

-- | An error resulting from applying a 'ExampleGenTx' to the ledger.
--
-- Thanks to the pattern synonyms, you can treat this as a sum type with
-- constructors 'ApplyTxErrShelley', 'ApplyTxErrExample', and
-- 'ApplyTxErrWrongEra'.
--
-- > toText :: ExampleApplyTxErr c -> Text
-- > toText (ApplyTxErrShelley s) = shelleyApplyTxErrToText s
-- > tlText (ApplyTxErrExample a) = exampleApplyTxErrToText a
-- > toText (ApplyTxErrWrongEra eraMismatch) =
-- >   "Transaction from the " <> otherEraName eraMismatch <>
-- >   " era applied to a ledger from the " <>
-- >   ledgerEraName eraMismatch <> " era"
--
type ExampleApplyTxErr c = HardForkApplyTxErr (ExampleEras c)

pattern ApplyTxErrShelley ::
     ApplyTxErr (ShelleyBlock (ShelleyEra c))
  -> ExampleApplyTxErr c
pattern ApplyTxErrShelley err =
    HardForkApplyTxErrFromEra (OneEraApplyTxErr (Z (WrapApplyTxErr err)))

pattern ApplyTxErrExample ::
     ApplyTxErr (ShelleyBlock (ExampleEra c))
  -> ExampleApplyTxErr c
pattern ApplyTxErrExample err =
    HardForkApplyTxErrFromEra (OneEraApplyTxErr (S (Z (WrapApplyTxErr err))))

pattern ApplyTxErrWrongEra :: EraMismatch -> ExampleApplyTxErr c
pattern ApplyTxErrWrongEra eraMismatch <-
    HardForkApplyTxErrWrongEra (mkEraMismatch -> eraMismatch)

{-# COMPLETE ApplyTxErrShelley
           , ApplyTxErrExample
           , ApplyTxErrWrongEra #-}

{-------------------------------------------------------------------------------
  LedgerError
-------------------------------------------------------------------------------}

-- | An error resulting from applying a 'ExampleBlock' to the ledger.
--
-- Thanks to the pattern synonyms, you can treat this as a sum type with
-- constructors 'LedgerErrorShelley', 'LedgerErrorExample', and
-- 'LedgerErrorWrongEra'.
--
-- > toText :: ExampleLedgerError c -> Text
-- > toText (LedgerErrorShelley s) = shelleyLedgerErrorToText s
-- > toText (LedgerErrorExample a) = allegraLedgerErrorToText a
-- > toText (LedgerErrorWrongEra eraMismatch) =
-- >   "Block from the " <> otherEraName eraMismatch <>
-- >   " era applied to a ledger from the " <>
-- >   ledgerEraName eraMismatch <> " era"
--
type ExampleLedgerError c = HardForkLedgerError (ExampleEras c)

pattern LedgerErrorShelley ::
     LedgerError (ShelleyBlock (ShelleyEra c))
  -> ExampleLedgerError c
pattern LedgerErrorShelley err =
    HardForkLedgerErrorFromEra
      (OneEraLedgerError (Z (WrapLedgerErr err)))

pattern LedgerErrorExample ::
     LedgerError (ShelleyBlock (ExampleEra c))
  -> ExampleLedgerError c
pattern LedgerErrorExample err =
    HardForkLedgerErrorFromEra
      (OneEraLedgerError (S (Z (WrapLedgerErr err))))

pattern LedgerErrorWrongEra :: EraMismatch -> ExampleLedgerError c
pattern LedgerErrorWrongEra eraMismatch <-
    HardForkLedgerErrorWrongEra (mkEraMismatch -> eraMismatch)

{-# COMPLETE LedgerErrorShelley
           , LedgerErrorExample
           , LedgerErrorWrongEra #-}

{-------------------------------------------------------------------------------
  OtherEnvelopeError
-------------------------------------------------------------------------------}

-- | An error resulting from validating a 'ExampleHeader'.
type ExampleOtherHeaderEnvelopeError c = HardForkEnvelopeErr (ExampleEras c)

pattern OtherHeaderEnvelopeErrorShelley
  :: OtherHeaderEnvelopeError (ShelleyBlock (ShelleyEra c))
  -> ExampleOtherHeaderEnvelopeError c
pattern OtherHeaderEnvelopeErrorShelley err =
    HardForkEnvelopeErrFromEra (OneEraEnvelopeErr (Z (WrapEnvelopeErr err)))

pattern OtherHeaderEnvelopeErrorExample
  :: OtherHeaderEnvelopeError (ShelleyBlock (ExampleEra c))
  -> ExampleOtherHeaderEnvelopeError c
pattern OtherHeaderEnvelopeErrorExample err =
    HardForkEnvelopeErrFromEra (OneEraEnvelopeErr (S (Z (WrapEnvelopeErr err))))

pattern OtherHeaderEnvelopeErrorWrongEra
  :: EraMismatch
  -> ExampleOtherHeaderEnvelopeError c
pattern OtherHeaderEnvelopeErrorWrongEra eraMismatch <-
    HardForkEnvelopeErrWrongEra (mkEraMismatch -> eraMismatch)

{-# COMPLETE OtherHeaderEnvelopeErrorShelley
           , OtherHeaderEnvelopeErrorExample
           , OtherHeaderEnvelopeErrorWrongEra #-}

{-------------------------------------------------------------------------------
  TipInfo
-------------------------------------------------------------------------------}

-- | The 'TipInfo' of the Example chain.
type ExampleTipInfo c = OneEraTipInfo (ExampleEras c)

pattern TipInfoShelley ::
     TipInfo (ShelleyBlock (ShelleyEra c))
  -> ExampleTipInfo c
pattern TipInfoShelley ti = OneEraTipInfo (Z (WrapTipInfo ti))

pattern TipInfoExample ::
     TipInfo (ShelleyBlock (ExampleEra c))
  -> ExampleTipInfo c
pattern TipInfoExample ti = OneEraTipInfo (S (Z (WrapTipInfo ti)))

{-# COMPLETE TipInfoShelley, TipInfoExample #-}

{-------------------------------------------------------------------------------
  Query
-------------------------------------------------------------------------------}

-- | The 'Query' of Example chain.
type ExampleQuery c = Query (ExampleBlock c)

-- | Shelley-specific query that can only be answered when the ledger is in the
-- Shelley era.
pattern QueryIfCurrentShelley
  :: ()
  => ExampleQueryResult c result ~ a
  => Query (ShelleyBlock (ShelleyEra c)) result
  -> ExampleQuery c a
pattern QueryIfCurrentShelley q = QueryIfCurrent (QZ q)

-- | Example-specific query that can only be answered when the ledger is in the
-- Example era.
pattern QueryIfCurrentExample
  :: ()
  => ExampleQueryResult c result ~ a
  => Query (ShelleyBlock (ExampleEra c)) result
  -> ExampleQuery c a
pattern QueryIfCurrentExample q = QueryIfCurrent (QS (QZ q))

-- | Query about the Shelley era that can be answered anytime, i.e.,
-- independent from where the tip of the ledger is.
--
-- For example, to ask for the start of the Shelley era (whether the tip of the
-- ledger is in the Shelley, Example, ... era), use:
--
-- > QueryAnytimeShelley EraStart
--
pattern QueryAnytimeShelley
  :: QueryAnytime result
  -> ExampleQuery c result
pattern QueryAnytimeShelley q = QueryAnytime q (EraIndex (Z (K ())))

-- | Query about the Shelley era that can be answered anytime, i.e.,
-- independent from where the tip of the ledger is.
--
-- For example, to ask for the start of the Shelley era (whether the tip of the
-- ledger is in the Shelley, Example, ... era), use:
--
-- > QueryAnytimeShelley EraStart
--
pattern QueryAnytimeExample
  :: QueryAnytime result
  -> ExampleQuery c result
pattern QueryAnytimeExample q = QueryAnytime q (EraIndex (S (Z (K ()))))

{-# COMPLETE QueryIfCurrentShelley
           , QueryIfCurrentExample
           , QueryAnytimeShelley
           , QueryAnytimeExample
           , QueryHardFork #-}

-- | The result of a 'ExampleQuery'
--
-- Thanks to the pattern synonyms, you can treat this as a sum type with
-- constructors 'QueryResultSuccess' and 'QueryResultEraMismatch'.
type ExampleQueryResult c = HardForkQueryResult (ExampleEras c)

pattern QueryResultSuccess :: result -> ExampleQueryResult c result
pattern QueryResultSuccess result = Right result

-- | A query from a different era than the ledger's era was sent.
pattern QueryResultEraMismatch :: EraMismatch -> ExampleQueryResult c result
pattern QueryResultEraMismatch eraMismatch <- Left (mkEraMismatch -> eraMismatch)

{-# COMPLETE QueryResultSuccess, QueryResultEraMismatch #-}

{-------------------------------------------------------------------------------
  CodecConfig
-------------------------------------------------------------------------------}

-- | The 'CodecConfig' for 'ExampleBlock'.
--
-- Thanks to the pattern synonyms, you can treat this as the product of
-- the Shelley, Example ... 'CodecConfig's.
type ExampleCodecConfig c = CodecConfig (ExampleBlock c)

pattern ExampleCodecConfig
  :: CodecConfig (ShelleyBlock (ShelleyEra c))
  -> CodecConfig (ShelleyBlock (ExampleEra c))
  -> ExampleCodecConfig c
pattern ExampleCodecConfig cfgShelley cfgExample =
    HardForkCodecConfig {
        hardForkCodecConfigPerEra = PerEraCodecConfig
          (  cfgShelley
          :* cfgExample
          :* Nil
          )
      }

{-# COMPLETE ExampleCodecConfig #-}

{-------------------------------------------------------------------------------
  BlockConfig
-------------------------------------------------------------------------------}

-- | The 'BlockConfig' for 'ExampleBlock'.
--
-- Thanks to the pattern synonyms, you can treat this as the product of
-- the Shelley, Example, ... 'BlockConfig's.
type ExampleBlockConfig c = BlockConfig (ExampleBlock c)

pattern ExampleBlockConfig
  :: BlockConfig (ShelleyBlock (ShelleyEra c))
  -> BlockConfig (ShelleyBlock (ExampleEra c))
  -> ExampleBlockConfig c
pattern ExampleBlockConfig cfgShelley cfgExample =
    HardForkBlockConfig {
        hardForkBlockConfigPerEra = PerEraBlockConfig
          (  cfgShelley
          :* cfgExample
          :* Nil
          )
      }

{-# COMPLETE ExampleBlockConfig #-}

{-------------------------------------------------------------------------------
  StorageConfig
-------------------------------------------------------------------------------}

-- | The 'StorageConfig' for 'ExampleBlock'.
--
-- Thanks to the pattern synonyms, you can treat this as the product of
-- the Shelley, Example, ... 'StorageConfig's.
type ExampleStorageConfig c = StorageConfig (ExampleBlock c)

pattern ExampleStorageConfig
  :: StorageConfig (ShelleyBlock (ShelleyEra c))
  -> StorageConfig (ShelleyBlock (ExampleEra c))
  -> ExampleStorageConfig c
pattern ExampleStorageConfig cfgShelley cfgExample =
    HardForkStorageConfig {
        hardForkStorageConfigPerEra = PerEraStorageConfig
          (  cfgShelley
          :* cfgExample
          :* Nil
          )
      }

{-# COMPLETE ExampleStorageConfig #-}

{-------------------------------------------------------------------------------
  ConsensusConfig
-------------------------------------------------------------------------------}

-- | The 'ConsensusConfig' for 'ExampleBlock'.
--
-- Thanks to the pattern synonyms, you can treat this as the product of the
-- Shelley, Example, ... 'PartialConsensusConfig's.
--
-- NOTE: not 'ConsensusConfig', but 'PartialConsensusConfig'.
type ExampleConsensusConfig c =
  ConsensusConfig (HardForkProtocol (ExampleEras c))

pattern ExampleConsensusConfig
  :: PartialConsensusConfig (BlockProtocol (ShelleyBlock (ShelleyEra c)))
  -> PartialConsensusConfig (BlockProtocol (ShelleyBlock (ExampleEra c)))
  -> ExampleConsensusConfig c
pattern ExampleConsensusConfig cfgShelley cfgExample <-
    HardForkConsensusConfig {
        hardForkConsensusConfigPerEra = PerEraConsensusConfig
          (  WrapPartialConsensusConfig cfgShelley
          :* WrapPartialConsensusConfig cfgExample
          :* Nil
          )
      }

{-# COMPLETE ExampleConsensusConfig #-}

{-------------------------------------------------------------------------------
  LedgerConfig
-------------------------------------------------------------------------------}

-- | The 'LedgerConfig' for 'ExampleBlock'.
--
-- Thanks to the pattern synonyms, you can treat this as the product of the
-- Shelley, Example, ... 'PartialLedgerConfig's.
--
-- NOTE: not 'LedgerConfig', but 'PartialLedgerConfig'.
type ExampleLedgerConfig c = HardForkLedgerConfig (ExampleEras c)

pattern ExampleLedgerConfig
  :: PartialLedgerConfig (ShelleyBlock (ShelleyEra c))
  -> PartialLedgerConfig (ShelleyBlock (ExampleEra c))
  -> ExampleLedgerConfig c
pattern ExampleLedgerConfig cfgShelley cfgExample <-
    HardForkLedgerConfig {
        hardForkLedgerConfigPerEra = PerEraLedgerConfig
          ( WrapPartialLedgerConfig cfgShelley
          :* WrapPartialLedgerConfig cfgExample
          :* Nil
          )
      }

{-# COMPLETE ExampleLedgerConfig #-}

{-------------------------------------------------------------------------------
  LedgerState
-------------------------------------------------------------------------------}

-- | The 'LedgerState' for 'ExampleBlock'.
--
-- NOTE: the 'ExampleLedgerState' contains more than just the current era's
-- 'LedgerState'. We don't give access to those internal details through the
-- pattern synonyms. This is also the reason the pattern synonyms are not
-- bidirectional.
type ExampleLedgerState c = LedgerState (ExampleBlock c)

pattern LedgerStateShelley
  :: LedgerState (ShelleyBlock (ShelleyEra c))
  -> ExampleLedgerState c
pattern LedgerStateShelley st <-
    HardForkLedgerState
      (State.HardForkState
        (TZ (State.Current { currentState = st })))

pattern LedgerStateExample
  :: LedgerState (ShelleyBlock (ExampleEra c))
  -> ExampleLedgerState c
pattern LedgerStateExample st <-
    HardForkLedgerState
      (State.HardForkState
        (TS _ (TZ (State.Current { currentState = st }))))

{-# COMPLETE LedgerStateShelley
           , LedgerStateExample #-}

{-------------------------------------------------------------------------------
  ChainDepState
-------------------------------------------------------------------------------}

-- | The 'ChainDepState' for 'ExampleBlock'.
--
-- NOTE: the 'ExampleChainDepState' contains more than just the current era's
-- 'ChainDepState'. We don't give access to those internal details through the
-- pattern synonyms. This is also the reason the pattern synonyms are not
-- bidirectional.
type ExampleChainDepState c = HardForkChainDepState (ExampleEras c)

pattern ChainDepStateShelley
  :: ChainDepState (BlockProtocol (ShelleyBlock (ShelleyEra c)))
  -> ExampleChainDepState c
pattern ChainDepStateShelley st <-
    State.HardForkState
      (TZ (State.Current { currentState = WrapChainDepState st }))

pattern ChainDepStateExample
  :: ChainDepState (BlockProtocol (ShelleyBlock (ExampleEra c)))
  -> ExampleChainDepState c
pattern ChainDepStateExample st <-
    State.HardForkState
      (TS _ (TZ (State.Current { currentState = WrapChainDepState st })))

{-# COMPLETE ChainDepStateShelley
           , ChainDepStateExample #-}
