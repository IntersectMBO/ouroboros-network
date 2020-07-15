{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE PatternSynonyms      #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns         #-}

{-# OPTIONS_GHC -Wno-orphans -Wwarn #-}

module Ouroboros.Consensus.HardFork.Combinator.Degenerate (
    -- * Pattern synonyms
    HardForkBlock (DegenBlock)
  , Header (DegenHeader)
  , GenTx (DegenGenTx)
  , TxId (DegenGenTxId)
  , HardForkApplyTxErr (DegenApplyTxErr)
--  , HardForkLedgerError (DegenLedgerError)
  , HardForkEnvelopeErr (DegenOtherHeaderEnvelopeError)
  , OneEraTipInfo (DegenTipInfo)
  , Query (DegenQuery)
  , Either (DegenQueryResult)
  , CodecConfig (DegenCodecConfig)
  , BlockConfig (DegenBlockConfig)
--  , ConsensusConfig (DegenConsensusConfig)
--  , HardForkLedgerConfig (DegenLedgerConfig)
  , LedgerState (DegenLedgerState)
  ) where

import           Control.Tracer
import           Data.SOP.Strict

import           Ouroboros.Consensus.Block.Abstract
import           Ouroboros.Consensus.Config.SupportsNode
import           Ouroboros.Consensus.HeaderValidation
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Ledger.SupportsMempool
import           Ouroboros.Consensus.Node.NetworkProtocolVersion
import           Ouroboros.Consensus.Node.Run
import           Ouroboros.Consensus.TypeFamilyWrappers

import           Ouroboros.Consensus.HardFork.Combinator.Abstract.NoHardForks
import           Ouroboros.Consensus.HardFork.Combinator.AcrossEras
import           Ouroboros.Consensus.HardFork.Combinator.Basics
import           Ouroboros.Consensus.HardFork.Combinator.Forge ()
import           Ouroboros.Consensus.HardFork.Combinator.Ledger
import           Ouroboros.Consensus.HardFork.Combinator.Ledger.CommonProtocolParams
                     ()
import           Ouroboros.Consensus.HardFork.Combinator.Ledger.Query ()
import           Ouroboros.Consensus.HardFork.Combinator.Ledger.Query
import           Ouroboros.Consensus.HardFork.Combinator.Mempool ()
import           Ouroboros.Consensus.HardFork.Combinator.Mempool
import           Ouroboros.Consensus.HardFork.Combinator.Node ()
import           Ouroboros.Consensus.HardFork.Combinator.Serialisation.Common
import           Ouroboros.Consensus.HardFork.Combinator.Serialisation.SerialiseDisk
                     ()
import           Ouroboros.Consensus.HardFork.Combinator.Serialisation.SerialiseNodeToClient
                     ()
import           Ouroboros.Consensus.HardFork.Combinator.Serialisation.SerialiseNodeToNode
                     ()
import           Ouroboros.Consensus.HardFork.Combinator.Unary

instance ( SupportedNetworkProtocolVersion (HardForkBlock '[b])
         , SerialiseHFC '[b]
         , ConfigSupportsNode b
         , RunNode b
         , NoHardForks b
         ) => RunNode (HardForkBlock '[b]) where

  nodeImmDbChunkInfo cfg                 = nodeImmDbChunkInfo (project cfg)
  nodeCheckIntegrity cfg (DegenBlock  b) = nodeCheckIntegrity (project cfg) b
  nodeGetBinaryBlockInfo (DegenBlock  b) = nodeGetBinaryBlockInfo           b
  nodeBlockFetchSize     (DegenHeader h) = nodeBlockFetchSize               h

  nodeInitChainDB cfg = nodeInitChainDB (project cfg) . contramap DegenBlock

{-------------------------------------------------------------------------------
  Simple patterns
-------------------------------------------------------------------------------}

{-# COMPLETE DegenBlock                    #-}
{-# COMPLETE DegenHeader                   #-}
{-# COMPLETE DegenGenTx                    #-}
{-# COMPLETE DegenGenTxId                  #-}
{-# COMPLETE DegenApplyTxErr               #-}
{-# COMPLETE DegenOtherHeaderEnvelopeError #-}
{-# COMPLETE DegenTipInfo                  #-}
{-# COMPLETE DegenQuery                    #-}
{-# COMPLETE DegenQueryResult              #-}
{-# COMPLETE DegenCodecConfig              #-}
{-# COMPLETE DegenBlockConfig              #-}
{-# COMPLETE DegenLedgerState              #-}

pattern DegenBlock ::
     forall b. NoHardForks b
  => b
  -> HardForkBlock '[b]
pattern DegenBlock x <- (project' (Proxy @(I b)) -> x)
  where
    DegenBlock x = inject' (Proxy @(I b)) x

pattern DegenHeader ::
     NoHardForks b
  => Header b
  -> Header (HardForkBlock '[b])
pattern DegenHeader x <- (project -> x)
  where
    DegenHeader x = inject x

pattern DegenGenTx ::
     NoHardForks b
  => GenTx b
  -> GenTx (HardForkBlock '[b])
pattern DegenGenTx x <- (project -> x)
  where
    DegenGenTx x = inject x

pattern DegenGenTxId ::
     forall b. NoHardForks b
  => GenTxId b
  -> GenTxId (HardForkBlock '[b])
pattern DegenGenTxId x <- (project' (Proxy @(WrapGenTxId b)) -> x)
  where
    DegenGenTxId x = inject' (Proxy @(WrapGenTxId b)) x

pattern DegenApplyTxErr ::
     forall b. NoHardForks b
  => ApplyTxErr b
  -> HardForkApplyTxErr '[b] -- ApplyTxErr (HardForkBlock '[b])
pattern DegenApplyTxErr x <- (project' (Proxy @(WrapApplyTxErr b)) -> x)
  where
    DegenApplyTxErr x = inject' (Proxy @(WrapApplyTxErr b)) x

-- TODO: Ledger error

pattern DegenOtherHeaderEnvelopeError ::
     forall b. NoHardForks b
  => OtherHeaderEnvelopeError b
  -> HardForkEnvelopeErr '[b] -- OtherHeaderEnvelopeError (HardForkBlock '[b])
pattern DegenOtherHeaderEnvelopeError x <- (project' (Proxy @(WrapEnvelopeErr b)) -> x)
  where
    DegenOtherHeaderEnvelopeError x = inject' (Proxy @(WrapEnvelopeErr b)) x

pattern DegenTipInfo ::
     forall b. NoHardForks b
  => TipInfo b
  -> OneEraTipInfo '[b] -- TipInfo (HardForkBlock '[b])
pattern DegenTipInfo x <- (project' (Proxy @(WrapTipInfo b)) -> x)
  where
    DegenTipInfo x = inject' (Proxy @(WrapTipInfo b)) x

pattern DegenQuery
  :: ()
  => HardForkQueryResult '[b] result ~ a
  => Query b result
  -> Query (HardForkBlock '[b]) a
pattern DegenQuery x <- (projQuery' -> ProjHardForkQuery x)
  where
    DegenQuery x = injQuery x

pattern DegenQueryResult ::
     result
  -> HardForkQueryResult '[b] result
pattern DegenQueryResult x <- (projQueryResult -> x)
  where
    DegenQueryResult x = injQueryResult x

pattern DegenCodecConfig
  :: NoHardForks b
  => CodecConfig b
  -> CodecConfig (HardForkBlock '[b])
pattern DegenCodecConfig x <- (project -> x)
  where
    DegenCodecConfig x = inject x

pattern DegenBlockConfig
  :: NoHardForks b
  => BlockConfig b
  -> BlockConfig (HardForkBlock '[b])
pattern DegenBlockConfig x <- (project -> x)
  where
    DegenBlockConfig x = inject x

-- consensus config?
-- ledger config?

pattern DegenLedgerState
  :: NoHardForks b
  => LedgerState b
  -> LedgerState (HardForkBlock '[b])
pattern DegenLedgerState x <- (project -> x)
  where
    DegenLedgerState x = inject x



{-
type CardanoConsensusConfig sc =
  ConsensusConfig (HardForkProtocol '[b])

pattern CardanoConsensusConfig
  :: PartialConsensusConfig (BlockProtocol b)
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
-- | The 'LedgerConfig' for 'CardanoBlock'.
--
-- Thanks to the pattern synonyms, you can treat this as the product of the
-- Byron and Shelley 'PartialLedgerConfig'.
--
-- NOTE: not 'LedgerConfig', but 'PartialLedgerConfig'.
type CardanoLedgerConfig sc = HardForkLedgerConfig '[b]

pattern CardanoLedgerConfig
  :: PartialLedgerConfig b
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


-}

{-
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
type CardanoLedgerError sc = HardForkLedgerError '[b]

pattern LedgerErrorByron :: LedgerError b -> CardanoLedgerError sc
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
-}
