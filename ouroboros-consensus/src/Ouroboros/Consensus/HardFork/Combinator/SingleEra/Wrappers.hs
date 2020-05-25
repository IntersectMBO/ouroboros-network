{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeApplications           #-}

-- | Newtype wrappers around type families
module Ouroboros.Consensus.HardFork.Combinator.SingleEra.Wrappers (
    -- * Block based
    SingleEraTipInfo(..)
  , SingleEraEnvelopeErr(..)
  , SingleEraLedgerConfig(..)
  , SingleEraLedgerError(..)
  , SingleEraGenTxId(..)
  , SingleEraApplyTxErr(..)
  , SingleEraForgeState(..)
    -- * Protocol based
  , SingleEraConsensusState(..)
  , SingleEraConsensusConfig(..)
  , SingleEraChainSelConfig(..)
  , SingleEraIsLeader(..)
  , SingleEraValidationErr(..)
  , SingleEraValidateView(..)
  , SingleEraSelectView(..)
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

newtype SingleEraTipInfo      blk = SingleEraTipInfo      { getSingleEraTipInfo      :: TipInfo                  blk }
newtype SingleEraEnvelopeErr  blk = SingleEraEnvelopeErr  { getSingleEraEnvelopeErr  :: OtherHeaderEnvelopeError blk }
newtype SingleEraLedgerConfig blk = SingleEraLedgerConfig { getSingleEraLedgerConfig :: PartialLedgerConfig      blk }
newtype SingleEraLedgerError  blk = SingleEraLedgerError  { getSingleEraLedgerError  :: LedgerError              blk }
newtype SingleEraGenTxId      blk = SingleEraGenTxId      { getSingleEraGenTxId      :: GenTxId                  blk }
newtype SingleEraApplyTxErr   blk = SingleEraApplyTxErr   { getSingleEraApplyTxErr   :: ApplyTxErr               blk }
newtype SingleEraForgeState   blk = SingleEraForgeState   { getSingleEraForgeState   :: ForgeState               blk }

{-------------------------------------------------------------------------------
  Consensus based
-------------------------------------------------------------------------------}

newtype SingleEraConsensusState  blk = SingleEraConsensusState  { getSingleEraConsensusState  :: ConsensusState         (BlockProtocol blk) }
newtype SingleEraConsensusConfig blk = SingleEraConsensusConfig { getSingleEraConsensusConfig :: PartialConsensusConfig (BlockProtocol blk) }
newtype SingleEraChainSelConfig  blk = SingleEraChainSelConfig  { getSingleEraChainSelConfig  :: ChainSelConfig         (BlockProtocol blk) }
newtype SingleEraIsLeader        blk = SingleEraIsLeader        { getSingleEraIsLeader        :: IsLeader               (BlockProtocol blk) }
newtype SingleEraValidationErr   blk = SingleEraValidationErr   { getSingleEraValidationErr   :: ValidationErr          (BlockProtocol blk) }
newtype SingleEraValidateView    blk = SingleEraValidateView    { getSingleEraValidateView    :: ValidateView           (BlockProtocol blk) }
newtype SingleEraSelectView      blk = SingleEraSelectView      { getSingleEraSelectView      :: SelectView             (BlockProtocol blk) }

{-------------------------------------------------------------------------------
  Convenience functions
-------------------------------------------------------------------------------}

singleEraParams' :: forall blk. SingleEraBlock blk
                 => SingleEraLedgerConfig blk -> EraParams
singleEraParams' = singleEraParams (Proxy @blk) . getSingleEraLedgerConfig

singleEraTransition' :: SingleEraBlock blk
                     => SingleEraLedgerConfig blk -> LedgerState blk -> Maybe EpochNo
singleEraTransition' = singleEraTransition . getSingleEraLedgerConfig

{-------------------------------------------------------------------------------
  Instances
-------------------------------------------------------------------------------}

deriving newtype instance SingleEraBlock blk => Eq                 (SingleEraTipInfo blk)
deriving newtype instance SingleEraBlock blk => Show               (SingleEraTipInfo blk)
deriving newtype instance SingleEraBlock blk => NoUnexpectedThunks (SingleEraTipInfo blk)

deriving newtype instance SingleEraBlock blk => Eq                 (SingleEraEnvelopeErr blk)
deriving newtype instance SingleEraBlock blk => Show               (SingleEraEnvelopeErr blk)
deriving newtype instance SingleEraBlock blk => NoUnexpectedThunks (SingleEraEnvelopeErr blk)

deriving newtype instance SingleEraBlock blk => Eq                 (SingleEraGenTxId blk)
deriving newtype instance SingleEraBlock blk => Ord                (SingleEraGenTxId blk)
deriving newtype instance SingleEraBlock blk => NoUnexpectedThunks (SingleEraGenTxId blk)

deriving newtype instance SingleEraBlock blk => Eq                 (SingleEraLedgerError blk)
deriving newtype instance SingleEraBlock blk => Show               (SingleEraLedgerError blk)
deriving newtype instance SingleEraBlock blk => NoUnexpectedThunks (SingleEraLedgerError blk)

deriving newtype instance SingleEraBlock blk => Eq                 (SingleEraConsensusState blk)
deriving newtype instance SingleEraBlock blk => Show               (SingleEraConsensusState blk)
deriving newtype instance SingleEraBlock blk => NoUnexpectedThunks (SingleEraConsensusState blk)

deriving newtype instance SingleEraBlock blk => Eq                 (SingleEraValidationErr blk)
deriving newtype instance SingleEraBlock blk => Show               (SingleEraValidationErr blk)
deriving newtype instance SingleEraBlock blk => NoUnexpectedThunks (SingleEraValidationErr blk)

deriving newtype instance SingleEraBlock blk => NoUnexpectedThunks (SingleEraConsensusConfig blk)
deriving newtype instance SingleEraBlock blk => NoUnexpectedThunks (SingleEraChainSelConfig  blk)
deriving newtype instance SingleEraBlock blk => NoUnexpectedThunks (SingleEraLedgerConfig    blk)
deriving newtype instance SingleEraBlock blk => NoUnexpectedThunks (SingleEraForgeState      blk)

{-------------------------------------------------------------------------------
  Instances primarily for the benefit of tests
-------------------------------------------------------------------------------}

deriving newtype instance SingleEraBlock blk => Show (SingleEraGenTxId    blk)
deriving newtype instance SingleEraBlock blk => Show (SingleEraApplyTxErr blk)
