{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeApplications           #-}

-- | Newtype wrappers around type families
module Ouroboros.Consensus.HardFork.Combinator.SingleEra.Wrappers (
    -- * Block based
    WrapTipInfo(..)
  , WrapEnvelopeErr(..)
  , WrapPartialLedgerConfig(..)
  , WrapLedgerErr(..)
  , WrapGenTxId(..)
  , WrapApplyTxErr(..)
  , WrapForgeState(..)
    -- * Protocol based
  , WrapConsensusState(..)
  , WrapPartialConsensusConfig(..)
  , WrapChainSelConfig(..)
  , WrapIsLeader(..)
  , WrapValidationErr(..)
  , WrapValidateView(..)
  , WrapSelectView(..)
    -- * EraInfo
  , LedgerEraInfo(..)
    -- * Convenience
  , singleEraParams'
  , singleEraTransition'
  ) where

import           Data.Proxy

import           Cardano.Prelude (NoUnexpectedThunks)
import           Cardano.Slotting.Slot

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.HardFork.History (EraParams)
import           Ouroboros.Consensus.HeaderValidation
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Mempool.API
import           Ouroboros.Consensus.Protocol.Abstract

import           Ouroboros.Consensus.HardFork.Combinator.Abstract
import           Ouroboros.Consensus.HardFork.Combinator.PartialConfig
import           Ouroboros.Consensus.HardFork.Combinator.SingleEra.Info

{-------------------------------------------------------------------------------
  Newtype wrappers around type families

  We need these because type families cannot be partially applied.
-------------------------------------------------------------------------------}

newtype WrapTipInfo             blk = WrapTipInfo             { unwrapTipInfo             :: TipInfo                  blk }
newtype WrapEnvelopeErr         blk = WrapEnvelopeErr         { unwrapEnvelopeErr         :: OtherHeaderEnvelopeError blk }
newtype WrapPartialLedgerConfig blk = WrapPartialLedgerConfig { unwrapPartialLedgerConfig :: PartialLedgerConfig      blk }
newtype WrapLedgerErr           blk = WrapLedgerErr           { unwrapLedgerError         :: LedgerError              blk }
newtype WrapGenTxId             blk = WrapGenTxId             { unwrapGenTxId             :: GenTxId                  blk }
newtype WrapApplyTxErr          blk = WrapApplyTxErr          { unwrapApplyTxErr          :: ApplyTxErr               blk }
newtype WrapForgeState          blk = WrapForgeState          { unwrapForgeState          :: ForgeState               blk }

{-------------------------------------------------------------------------------
  Consensus based
-------------------------------------------------------------------------------}

newtype WrapConsensusState         blk = WrapConsensusState         { unwrapConsensusState         :: ConsensusState         (BlockProtocol blk) }
newtype WrapPartialConsensusConfig blk = WrapPartialConsensusConfig { unwrapPartialConsensusConfig :: PartialConsensusConfig (BlockProtocol blk) }
newtype WrapChainSelConfig         blk = WrapChainSelConfig         { unwrapChainSelConfig         :: ChainSelConfig         (BlockProtocol blk) }
newtype WrapIsLeader               blk = WrapIsLeader               { unwrapIsLeader               :: IsLeader               (BlockProtocol blk) }
newtype WrapValidationErr          blk = WrapValidationErr          { unwrapValidationErr          :: ValidationErr          (BlockProtocol blk) }
newtype WrapValidateView           blk = WrapValidateView           { unwrapValidateView           :: ValidateView           (BlockProtocol blk) }
newtype WrapSelectView             blk = WrapSelectView             { unwrapSelectView             :: SelectView             (BlockProtocol blk) }

{-------------------------------------------------------------------------------
  Convenience functions
-------------------------------------------------------------------------------}

singleEraParams' :: forall blk. SingleEraBlock blk
                 => WrapPartialLedgerConfig blk -> EraParams
singleEraParams' = singleEraParams (Proxy @blk) . unwrapPartialLedgerConfig

singleEraTransition' :: SingleEraBlock blk
                     => WrapPartialLedgerConfig blk -> LedgerState blk -> Maybe EpochNo
singleEraTransition' = singleEraTransition . unwrapPartialLedgerConfig

{-------------------------------------------------------------------------------
  Instances
-------------------------------------------------------------------------------}

deriving newtype instance SingleEraBlock blk => Eq                 (WrapTipInfo blk)
deriving newtype instance SingleEraBlock blk => Show               (WrapTipInfo blk)
deriving newtype instance SingleEraBlock blk => NoUnexpectedThunks (WrapTipInfo blk)

deriving newtype instance SingleEraBlock blk => Eq                 (WrapEnvelopeErr blk)
deriving newtype instance SingleEraBlock blk => Show               (WrapEnvelopeErr blk)
deriving newtype instance SingleEraBlock blk => NoUnexpectedThunks (WrapEnvelopeErr blk)

deriving newtype instance SingleEraBlock blk => Eq                 (WrapGenTxId blk)
deriving newtype instance SingleEraBlock blk => Ord                (WrapGenTxId blk)
deriving newtype instance SingleEraBlock blk => NoUnexpectedThunks (WrapGenTxId blk)

deriving newtype instance SingleEraBlock blk => Eq                 (WrapLedgerErr   blk)
deriving newtype instance SingleEraBlock blk => Show               (WrapLedgerErr   blk)
deriving newtype instance SingleEraBlock blk => NoUnexpectedThunks (WrapLedgerErr   blk)

deriving newtype instance SingleEraBlock blk => Eq                 (WrapConsensusState blk)
deriving newtype instance SingleEraBlock blk => Show               (WrapConsensusState blk)
deriving newtype instance SingleEraBlock blk => NoUnexpectedThunks (WrapConsensusState blk)

deriving newtype instance SingleEraBlock blk => Eq                 (WrapValidationErr blk)
deriving newtype instance SingleEraBlock blk => Show               (WrapValidationErr blk)
deriving newtype instance SingleEraBlock blk => NoUnexpectedThunks (WrapValidationErr blk)

deriving newtype instance SingleEraBlock blk => NoUnexpectedThunks (WrapPartialConsensusConfig blk)
deriving newtype instance SingleEraBlock blk => NoUnexpectedThunks (WrapChainSelConfig         blk)
deriving newtype instance SingleEraBlock blk => NoUnexpectedThunks (WrapPartialLedgerConfig    blk)
deriving newtype instance SingleEraBlock blk => NoUnexpectedThunks (WrapForgeState             blk)

{-------------------------------------------------------------------------------
  Instances primarily for the benefit of tests
-------------------------------------------------------------------------------}

deriving newtype instance SingleEraBlock blk => Show (WrapGenTxId    blk)
deriving newtype instance SingleEraBlock blk => Show (WrapApplyTxErr blk)
