{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE UndecidableInstances       #-}

-- | Newtypes around type families so that they can be partially applied
module Ouroboros.Consensus.TypeFamilyWrappers (
    -- * Block based
    WrapTipInfo(..)
  , WrapEnvelopeErr(..)
  , WrapLedgerErr(..)
  , WrapGenTxId(..)
  , WrapApplyTxErr(..)
  , WrapForgeState(..)
    -- * Protocol based
  , WrapConsensusState(..)
  , WrapChainSelConfig(..)
  , WrapIsLeader(..)
  , WrapValidationErr(..)
  , WrapValidateView(..)
  , WrapSelectView(..)
  ) where

import           Cardano.Prelude (NoUnexpectedThunks)

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.HeaderValidation
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Mempool.API
import           Ouroboros.Consensus.Protocol.Abstract

{-------------------------------------------------------------------------------
  Block based
-------------------------------------------------------------------------------}

newtype WrapTipInfo      blk = WrapTipInfo      { getWrapTipInfo      :: TipInfo                  blk }
newtype WrapEnvelopeErr  blk = WrapEnvelopeErr  { getWrapEnvelopeErr  :: OtherHeaderEnvelopeError blk }
newtype WrapLedgerErr    blk = WrapLedgerError  { getWrapLedgerErr    :: LedgerErr                blk }
newtype WrapGenTxId      blk = WrapGenTxId      { getWrapGenTxId      :: GenTxId                  blk }
newtype WrapApplyTxErr   blk = WrapApplyTxErr   { getWrapApplyTxErr   :: ApplyTxErr               blk }
newtype WrapForgeState   blk = WrapForgeState   { getWrapForgeState   :: ForgeState               blk }

{-------------------------------------------------------------------------------
  Consensus based
-------------------------------------------------------------------------------}

newtype WrapConsensusState  blk = WrapConsensusState  { getWrapConsensusState  :: ConsensusState         (BlockProtocol blk) }
newtype WrapChainSelConfig  blk = WrapChainSelConfig  { getWrapChainSelConfig  :: ChainSelConfig         (BlockProtocol blk) }
newtype WrapIsLeader        blk = WrapIsLeader        { getWrapIsLeader        :: IsLeader               (BlockProtocol blk) }
newtype WrapValidationErr   blk = WrapValidationErr   { getWrapValidationErr   :: ValidationErr          (BlockProtocol blk) }
newtype WrapValidateView    blk = WrapValidateView    { getWrapValidateView    :: ValidateView           (BlockProtocol blk) }
newtype WrapSelectView      blk = WrapSelectView      { getWrapSelectView      :: SelectView             (BlockProtocol blk) }

{-------------------------------------------------------------------------------
  Instances
-------------------------------------------------------------------------------}

deriving instance Eq                 (TipInfo blk)                  => Eq                 (WrapTipInfo blk)
deriving instance Show               (TipInfo blk)                  => Show               (WrapTipInfo blk)
deriving instance NoUnexpectedThunks (TipInfo blk)                  => NoUnexpectedThunks (WrapTipInfo blk)

deriving instance Eq                 (OtherHeaderEnvelopeError blk) => Eq                 (WrapEnvelopeErr blk)
deriving instance Show               (OtherHeaderEnvelopeError blk) => Show               (WrapEnvelopeErr blk)
deriving instance NoUnexpectedThunks (OtherHeaderEnvelopeError blk) => NoUnexpectedThunks (WrapEnvelopeErr blk)

deriving instance Eq                 (GenTxId blk)                  => Eq                 (WrapGenTxId blk)
deriving instance Ord                (GenTxId blk)                  => Ord                (WrapGenTxId blk)
deriving instance Show               (GenTxId blk)                  => Show               (WrapGenTxId blk)
deriving instance NoUnexpectedThunks (GenTxId blk)                  => NoUnexpectedThunks (WrapGenTxId blk)

deriving instance Eq                 (LedgerErr blk)                => Eq                 (WrapLedgerErr blk)
deriving instance Show               (LedgerErr blk)                => Show               (WrapLedgerErr blk)
deriving instance NoUnexpectedThunks (LedgerErr blk)                => NoUnexpectedThunks (WrapLedgerErr blk)

deriving instance NoUnexpectedThunks (ForgeState blk)               => NoUnexpectedThunks (WrapForgeState blk)

deriving instance Show               (ApplyTxErr blk)               => Show               (WrapApplyTxErr blk)

{-------------------------------------------------------------------------------
  .. consensus based
-------------------------------------------------------------------------------}

deriving instance Eq                 (ConsensusState (BlockProtocol blk)) => Eq                 (WrapConsensusState blk)
deriving instance Show               (ConsensusState (BlockProtocol blk)) => Show               (WrapConsensusState blk)
deriving instance NoUnexpectedThunks (ConsensusState (BlockProtocol blk)) => NoUnexpectedThunks (WrapConsensusState blk)

deriving instance Eq                 (ValidationErr  (BlockProtocol blk)) => Eq                 (WrapValidationErr blk)
deriving instance Show               (ValidationErr  (BlockProtocol blk)) => Show               (WrapValidationErr blk)
deriving instance NoUnexpectedThunks (ValidationErr  (BlockProtocol blk)) => NoUnexpectedThunks (WrapValidationErr blk)

deriving instance NoUnexpectedThunks (ChainSelConfig (BlockProtocol blk)) => NoUnexpectedThunks (WrapChainSelConfig blk)
