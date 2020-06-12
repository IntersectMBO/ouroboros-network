{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE UndecidableInstances       #-}

-- | Newtypes around type families so that they can be partially applied
module Ouroboros.Consensus.TypeFamilyWrappers (
    -- * Block based
    WrapApplyTxErr(..)
  , WrapEnvelopeErr(..)
  , WrapForgeState(..)
  , WrapGenTxId(..)
  , WrapHeaderHash(..)
  , WrapLedgerConfig(..)
  , WrapLedgerErr(..)
  , WrapTipInfo(..)
    -- * Protocol based
  , WrapCanBeLeader(..)
  , WrapCannotLead(..)
  , WrapChainSelConfig(..)
  , WrapConsensusConfig(..)
  , WrapConsensusState(..)
  , WrapIsLeader(..)
  , WrapLeaderCheck(..)
  , WrapLedgerView(..)
  , WrapSelectView(..)
  , WrapValidateView(..)
  , WrapValidationErr(..)
    -- * Versioning
  , WrapNodeToNodeVersion(..)
  , WrapNodeToClientVersion(..)
  ) where

import           Codec.Serialise (Serialise)

import           Cardano.Prelude (NoUnexpectedThunks)

import           Ouroboros.Network.Block (HeaderHash)

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.HeaderValidation
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Ledger.SupportsMempool
import           Ouroboros.Consensus.Node.NetworkProtocolVersion
import           Ouroboros.Consensus.Protocol.Abstract

{-------------------------------------------------------------------------------
  Block based
-------------------------------------------------------------------------------}

newtype WrapApplyTxErr   blk = WrapApplyTxErr   { unwrapApplyTxErr   :: ApplyTxErr               blk }
newtype WrapEnvelopeErr  blk = WrapEnvelopeErr  { unwrapEnvelopeErr  :: OtherHeaderEnvelopeError blk }
newtype WrapForgeState   blk = WrapForgeState   { unwrapForgeState   :: ForgeState               blk }
newtype WrapGenTxId      blk = WrapGenTxId      { unwrapGenTxId      :: GenTxId                  blk }
newtype WrapHeaderHash   blk = WrapHeaderHash   { unwrapHeaderHash   :: HeaderHash               blk }
newtype WrapLedgerConfig blk = WrapLedgerConfig { unwrapLedgerConfig :: LedgerConfig             blk }
newtype WrapLedgerErr    blk = WrapLedgerErr    { unwrapLedgerErr    :: LedgerError              blk }
newtype WrapTipInfo      blk = WrapTipInfo      { unwrapTipInfo      :: TipInfo                  blk }

{-------------------------------------------------------------------------------
  Consensus based
-------------------------------------------------------------------------------}

newtype WrapCanBeLeader     blk = WrapCanBeLeader     { unwrapCanBeLeader     :: CanBeLeader     (BlockProtocol blk) }
newtype WrapCannotLead      blk = WrapCannotLead      { unwrapCannotLead      :: CannotLead      (BlockProtocol blk) }
newtype WrapChainSelConfig  blk = WrapChainSelConfig  { unwrapChainSelConfig  :: ChainSelConfig  (BlockProtocol blk) }
newtype WrapConsensusConfig blk = WrapConsensusConfig { unwrapConsensusConfig :: ConsensusConfig (BlockProtocol blk) }
newtype WrapConsensusState  blk = WrapConsensusState  { unwrapConsensusState  :: ConsensusState  (BlockProtocol blk) }
newtype WrapIsLeader        blk = WrapIsLeader        { unwrapIsLeader        :: IsLeader        (BlockProtocol blk) }
newtype WrapLeaderCheck     blk = WrapLeaderCheck     { unwrapLeaderCheck     :: LeaderCheck     (BlockProtocol blk) }
newtype WrapLedgerView      blk = WrapLedgerView      { unwrapLedgerView      :: LedgerView      (BlockProtocol blk) }
newtype WrapSelectView      blk = WrapSelectView      { unwrapSelectView      :: SelectView      (BlockProtocol blk) }
newtype WrapValidateView    blk = WrapValidateView    { unwrapValidateView    :: ValidateView    (BlockProtocol blk) }
newtype WrapValidationErr   blk = WrapValidationErr   { unwrapValidationErr   :: ValidationErr   (BlockProtocol blk) }

{-------------------------------------------------------------------------------
  Versioning
-------------------------------------------------------------------------------}

newtype WrapNodeToNodeVersion   blk = WrapNodeToNodeVersion   { unwrapNodeToNodeVersion   :: BlockNodeToNodeVersion   blk }
newtype WrapNodeToClientVersion blk = WrapNodeToClientVersion { unwrapNodeToClientVersion :: BlockNodeToClientVersion blk }

{-------------------------------------------------------------------------------
  Instances
-------------------------------------------------------------------------------}

deriving instance Eq (GenTxId                  blk) => Eq (WrapGenTxId     blk)
deriving instance Eq (LedgerError              blk) => Eq (WrapLedgerErr   blk)
deriving instance Eq (OtherHeaderEnvelopeError blk) => Eq (WrapEnvelopeErr blk)
deriving instance Eq (TipInfo                  blk) => Eq (WrapTipInfo     blk)

deriving instance Ord (GenTxId blk) => Ord (WrapGenTxId blk)

deriving instance Show (ApplyTxErr               blk) => Show (WrapApplyTxErr  blk)
deriving instance Show (ForgeState               blk) => Show (WrapForgeState  blk)
deriving instance Show (GenTxId                  blk) => Show (WrapGenTxId     blk)
deriving instance Show (LedgerError              blk) => Show (WrapLedgerErr   blk)
deriving instance Show (OtherHeaderEnvelopeError blk) => Show (WrapEnvelopeErr blk)
deriving instance Show (TipInfo                  blk) => Show (WrapTipInfo     blk)

deriving instance NoUnexpectedThunks (ForgeState               blk) => NoUnexpectedThunks (WrapForgeState  blk)
deriving instance NoUnexpectedThunks (GenTxId                  blk) => NoUnexpectedThunks (WrapGenTxId     blk)
deriving instance NoUnexpectedThunks (LedgerError              blk) => NoUnexpectedThunks (WrapLedgerErr   blk)
deriving instance NoUnexpectedThunks (OtherHeaderEnvelopeError blk) => NoUnexpectedThunks (WrapEnvelopeErr blk)
deriving instance NoUnexpectedThunks (TipInfo                  blk) => NoUnexpectedThunks (WrapTipInfo     blk)

{-------------------------------------------------------------------------------
  .. consensus based
-------------------------------------------------------------------------------}

deriving instance Eq (ConsensusState (BlockProtocol blk)) => Eq (WrapConsensusState blk)
deriving instance Eq (ValidationErr  (BlockProtocol blk)) => Eq (WrapValidationErr  blk)

deriving instance Show (CannotLead     (BlockProtocol blk)) => Show (WrapCannotLead     blk)
deriving instance Show (ConsensusState (BlockProtocol blk)) => Show (WrapConsensusState blk)
deriving instance Show (LedgerView     (BlockProtocol blk)) => Show (WrapLedgerView     blk)
deriving instance Show (SelectView     (BlockProtocol blk)) => Show (WrapSelectView     blk)
deriving instance Show (ValidationErr  (BlockProtocol blk)) => Show (WrapValidationErr  blk)

deriving instance NoUnexpectedThunks (ChainSelConfig (BlockProtocol blk)) => NoUnexpectedThunks (WrapChainSelConfig blk)
deriving instance NoUnexpectedThunks (ConsensusState (BlockProtocol blk)) => NoUnexpectedThunks (WrapConsensusState blk)
deriving instance NoUnexpectedThunks (ValidationErr  (BlockProtocol blk)) => NoUnexpectedThunks (WrapValidationErr  blk)

{-------------------------------------------------------------------------------
  Versioning
-------------------------------------------------------------------------------}

deriving instance Show (BlockNodeToNodeVersion   blk) => Show (WrapNodeToNodeVersion   blk)
deriving instance Show (BlockNodeToClientVersion blk) => Show (WrapNodeToClientVersion blk)

deriving instance Eq (BlockNodeToNodeVersion   blk) => Eq (WrapNodeToNodeVersion   blk)
deriving instance Eq (BlockNodeToClientVersion blk) => Eq (WrapNodeToClientVersion blk)

{-------------------------------------------------------------------------------
  Serialise instances

  These are primarily useful in testing.
-------------------------------------------------------------------------------}

deriving instance Serialise (GenTxId                       blk)  => Serialise (WrapGenTxId        blk)
deriving instance Serialise (ConsensusState (BlockProtocol blk)) => Serialise (WrapConsensusState blk)
deriving instance Serialise (TipInfo                       blk)  => Serialise (WrapTipInfo        blk)
