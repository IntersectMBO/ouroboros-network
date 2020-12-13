{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE PatternSynonyms      #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns         #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Ouroboros.Consensus.HardFork.Combinator.Degenerate (
    -- * Pattern synonyms
    HardForkBlock (DegenBlock)
  , Header (DegenHeader)
  , GenTx (DegenGenTx)
  , TxId (DegenGenTxId)
  , HardForkApplyTxErr (DegenApplyTxErr)
  , HardForkLedgerError (DegenLedgerError)
  , HardForkEnvelopeErr (DegenOtherHeaderEnvelopeError)
  , OneEraTipInfo (DegenTipInfo)
  , Query (DegenQuery)
  , Either (DegenQueryResult)
  , CodecConfig (DegenCodecConfig)
  , BlockConfig (DegenBlockConfig)
  , ConsensusConfig (DegenConsensusConfig)
  , HardForkLedgerConfig (DegenLedgerConfig)
  , TopLevelConfig (DegenTopLevelConfig)
  , LedgerState (DegenLedgerState)
  ) where

import           Data.SOP.Strict

import           Ouroboros.Consensus.Block.Abstract
import           Ouroboros.Consensus.Config
import           Ouroboros.Consensus.HeaderValidation
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Ledger.SupportsMempool
import           Ouroboros.Consensus.TypeFamilyWrappers

import           Ouroboros.Consensus.HardFork.Combinator.Abstract.NoHardForks
import           Ouroboros.Consensus.HardFork.Combinator.AcrossEras
import           Ouroboros.Consensus.HardFork.Combinator.Basics
import           Ouroboros.Consensus.HardFork.Combinator.Embed.Unary
import           Ouroboros.Consensus.HardFork.Combinator.Ledger
import           Ouroboros.Consensus.HardFork.Combinator.Ledger.CommonProtocolParams ()
import           Ouroboros.Consensus.HardFork.Combinator.Ledger.Query
import           Ouroboros.Consensus.HardFork.Combinator.Mempool
import           Ouroboros.Consensus.HardFork.Combinator.Node ()
import           Ouroboros.Consensus.HardFork.Combinator.PartialConfig
import           Ouroboros.Consensus.HardFork.Combinator.Serialisation.SerialiseDisk ()
import           Ouroboros.Consensus.HardFork.Combinator.Serialisation.SerialiseNodeToClient ()
import           Ouroboros.Consensus.HardFork.Combinator.Serialisation.SerialiseNodeToNode ()

{-------------------------------------------------------------------------------
  Simple patterns
-------------------------------------------------------------------------------}

{-# COMPLETE DegenApplyTxErr               #-}
{-# COMPLETE DegenBlock                    #-}
{-# COMPLETE DegenBlockConfig              #-}
{-# COMPLETE DegenCodecConfig              #-}
{-# COMPLETE DegenGenTx                    #-}
{-# COMPLETE DegenGenTxId                  #-}
{-# COMPLETE DegenHeader                   #-}
{-# COMPLETE DegenLedgerError              #-}
{-# COMPLETE DegenLedgerState              #-}
{-# COMPLETE DegenOtherHeaderEnvelopeError #-}
{-# COMPLETE DegenQuery                    #-}
{-# COMPLETE DegenQueryResult              #-}
{-# COMPLETE DegenTipInfo                  #-}

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

pattern DegenLedgerError ::
     forall b. NoHardForks b
  => LedgerError b
  -> HardForkLedgerError '[b] -- LedgerError (HardForkBlock '[b])
pattern DegenLedgerError x <- (project' (Proxy @(WrapLedgerErr b)) -> x)
  where
    DegenLedgerError x = inject' (Proxy @(WrapLedgerErr b)) x

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

pattern DegenQuery ::
     ()
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

pattern DegenCodecConfig ::
     NoHardForks b
  => CodecConfig b
  -> CodecConfig (HardForkBlock '[b])
pattern DegenCodecConfig x <- (project -> x)
  where
    DegenCodecConfig x = inject x

pattern DegenBlockConfig ::
     NoHardForks b
  => BlockConfig b
  -> BlockConfig (HardForkBlock '[b])
pattern DegenBlockConfig x <- (project -> x)
  where
    DegenBlockConfig x = inject x

pattern DegenLedgerState ::
     NoHardForks b
  => LedgerState b
  -> LedgerState (HardForkBlock '[b])
pattern DegenLedgerState x <- (project -> x)
  where
    DegenLedgerState x = inject x

{-------------------------------------------------------------------------------
  Dealing with the config

  NOTE: The pattern synonyms for 'ConsensusConfig' and 'LedgerConfig'
  give you a /partial/ config. The pattern synonym for the 'TopLevelConfig'
  /does/ give you a full config.
-------------------------------------------------------------------------------}

{-# COMPLETE DegenConsensusConfig #-}
{-# COMPLETE DegenLedgerConfig    #-}
{-# COMPLETE DegenTopLevelConfig  #-}

pattern DegenConsensusConfig ::
     PartialConsensusConfig (BlockProtocol b)
  -> ConsensusConfig (BlockProtocol (HardForkBlock '[b]))
pattern DegenConsensusConfig x <-
    HardForkConsensusConfig {
        hardForkConsensusConfigPerEra = PerEraConsensusConfig
          (   WrapPartialConsensusConfig x
           :* Nil
          )
      }

pattern DegenLedgerConfig ::
     PartialLedgerConfig b
  -> HardForkLedgerConfig '[b] -- LedgerConfig (HardForkBlock '[b])
pattern DegenLedgerConfig x <-
    HardForkLedgerConfig {
        hardForkLedgerConfigPerEra = PerEraLedgerConfig
          (   WrapPartialLedgerConfig x
           :* Nil
          )
      }

pattern DegenTopLevelConfig ::
     NoHardForks b
  => TopLevelConfig b
  -> TopLevelConfig (HardForkBlock '[b])
pattern DegenTopLevelConfig x <- (project -> x)
  where
    DegenTopLevelConfig x = inject x
