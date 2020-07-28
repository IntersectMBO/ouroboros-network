{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}

-- | Newtypes around type families so that they can be partially applied
module Ouroboros.Consensus.TypeFamilyWrappers (
    -- * Block based
    WrapApplyTxErr(..)
  , WrapCannotForge(..)
  , WrapEnvelopeErr(..)
  , WrapForgeStateInfo(..)
  , WrapGenTxId(..)
  , WrapHeaderHash(..)
  , WrapLedgerConfig(..)
  , WrapLedgerErr(..)
  , WrapLedgerUpdate(..)
  , WrapLedgerWarning(..)
  , WrapTipInfo(..)
  , WrapFullBlockConfig(..)
    -- * Protocol based
  , WrapCanBeLeader(..)
  , WrapChainDepState(..)
  , WrapChainSelConfig(..)
  , WrapConsensusConfig(..)
  , WrapIsLeader(..)
  , WrapLedgerView(..)
  , WrapSelectView(..)
  , WrapValidateView(..)
  , WrapValidationErr(..)
    -- * Versioning
  , WrapNodeToNodeVersion(..)
  , WrapNodeToClientVersion(..)
    -- * Type family instances
  , Ticked(..)
  ) where

import           Codec.Serialise (Serialise)

import           Cardano.Prelude (NoUnexpectedThunks)

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Config
import           Ouroboros.Consensus.HeaderValidation
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Ledger.Inspect
import           Ouroboros.Consensus.Ledger.SupportsMempool
import           Ouroboros.Consensus.Node.NetworkProtocolVersion
import           Ouroboros.Consensus.Protocol.Abstract

{-------------------------------------------------------------------------------
  Block based
-------------------------------------------------------------------------------}

newtype WrapApplyTxErr      blk = WrapApplyTxErr      { unwrapApplyTxErr      :: ApplyTxErr               blk }
newtype WrapCannotForge     blk = WrapCannotForge     { unwrapCannotForge     :: CannotForge              blk }
newtype WrapEnvelopeErr     blk = WrapEnvelopeErr     { unwrapEnvelopeErr     :: OtherHeaderEnvelopeError blk }
newtype WrapForgeStateInfo  blk = WrapForgeStateInfo  { unwrapForgeStateInfo  :: ForgeStateInfo           blk }
newtype WrapGenTxId         blk = WrapGenTxId         { unwrapGenTxId         :: GenTxId                  blk }
newtype WrapHeaderHash      blk = WrapHeaderHash      { unwrapHeaderHash      :: HeaderHash               blk }
newtype WrapLedgerConfig    blk = WrapLedgerConfig    { unwrapLedgerConfig    :: LedgerConfig             blk }
newtype WrapLedgerErr       blk = WrapLedgerErr       { unwrapLedgerErr       :: LedgerError              blk }
newtype WrapLedgerUpdate    blk = WrapLedgerUpdate    { unwrapLedgerUpdate    :: LedgerUpdate             blk }
newtype WrapLedgerWarning   blk = WrapLedgerWarning   { unwrapLedgerWarning   :: LedgerWarning            blk }
newtype WrapTipInfo         blk = WrapTipInfo         { unwrapTipInfo         :: TipInfo                  blk }
newtype WrapFullBlockConfig blk = WrapFullBlockConfig { unwrapFullBlockConfig :: FullBlockConfig (LedgerState blk) blk }

{-------------------------------------------------------------------------------
  Consensus based
-------------------------------------------------------------------------------}

newtype WrapCanBeLeader     blk = WrapCanBeLeader     { unwrapCanBeLeader     :: CanBeLeader     (BlockProtocol blk) }
newtype WrapChainDepState   blk = WrapChainDepState   { unwrapChainDepState   :: ChainDepState   (BlockProtocol blk) }
newtype WrapChainSelConfig  blk = WrapChainSelConfig  { unwrapChainSelConfig  :: ChainSelConfig  (BlockProtocol blk) }
newtype WrapConsensusConfig blk = WrapConsensusConfig { unwrapConsensusConfig :: ConsensusConfig (BlockProtocol blk) }
newtype WrapIsLeader        blk = WrapIsLeader        { unwrapIsLeader        :: IsLeader        (BlockProtocol blk) }
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

deriving instance Eq (ApplyTxErr               blk) => Eq (WrapApplyTxErr     blk)
deriving instance Eq (ForgeStateInfo           blk) => Eq (WrapForgeStateInfo blk)
deriving instance Eq (GenTxId                  blk) => Eq (WrapGenTxId        blk)
deriving instance Eq (LedgerError              blk) => Eq (WrapLedgerErr      blk)
deriving instance Eq (LedgerUpdate             blk) => Eq (WrapLedgerUpdate   blk)
deriving instance Eq (LedgerWarning            blk) => Eq (WrapLedgerWarning  blk)
deriving instance Eq (OtherHeaderEnvelopeError blk) => Eq (WrapEnvelopeErr    blk)
deriving instance Eq (TipInfo                  blk) => Eq (WrapTipInfo        blk)

deriving instance Ord (GenTxId blk) => Ord (WrapGenTxId blk)

deriving instance Show (ApplyTxErr               blk) => Show (WrapApplyTxErr     blk)
deriving instance Show (CannotForge              blk) => Show (WrapCannotForge    blk)
deriving instance Show (ForgeStateInfo           blk) => Show (WrapForgeStateInfo blk)
deriving instance Show (GenTxId                  blk) => Show (WrapGenTxId        blk)
deriving instance Show (LedgerError              blk) => Show (WrapLedgerErr      blk)
deriving instance Show (LedgerUpdate             blk) => Show (WrapLedgerUpdate   blk)
deriving instance Show (LedgerWarning            blk) => Show (WrapLedgerWarning  blk)
deriving instance Show (OtherHeaderEnvelopeError blk) => Show (WrapEnvelopeErr    blk)
deriving instance Show (TipInfo                  blk) => Show (WrapTipInfo        blk)

deriving instance NoUnexpectedThunks (GenTxId                  blk) => NoUnexpectedThunks (WrapGenTxId         blk)
deriving instance NoUnexpectedThunks (LedgerError              blk) => NoUnexpectedThunks (WrapLedgerErr       blk)
deriving instance NoUnexpectedThunks (OtherHeaderEnvelopeError blk) => NoUnexpectedThunks (WrapEnvelopeErr     blk)
deriving instance NoUnexpectedThunks (TipInfo                  blk) => NoUnexpectedThunks (WrapTipInfo         blk)

{-------------------------------------------------------------------------------
  .. consensus based
-------------------------------------------------------------------------------}

deriving instance Eq (ChainDepState  (BlockProtocol blk)) => Eq (WrapChainDepState  blk)
deriving instance Eq (ChainSelConfig (BlockProtocol blk)) => Eq (WrapChainSelConfig blk)
deriving instance Eq (ValidationErr  (BlockProtocol blk)) => Eq (WrapValidationErr  blk)

deriving instance Show (ChainDepState  (BlockProtocol blk)) => Show (WrapChainDepState  blk)
deriving instance Show (ChainSelConfig (BlockProtocol blk)) => Show (WrapChainSelConfig blk)
deriving instance Show (LedgerView     (BlockProtocol blk)) => Show (WrapLedgerView     blk)
deriving instance Show (SelectView     (BlockProtocol blk)) => Show (WrapSelectView     blk)
deriving instance Show (ValidationErr  (BlockProtocol blk)) => Show (WrapValidationErr  blk)

deriving instance NoUnexpectedThunks (ChainSelConfig (BlockProtocol blk)) => NoUnexpectedThunks (WrapChainSelConfig blk)
deriving instance NoUnexpectedThunks (ChainDepState  (BlockProtocol blk)) => NoUnexpectedThunks (WrapChainDepState  blk)
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
deriving instance Serialise (ChainDepState  (BlockProtocol blk)) => Serialise (WrapChainDepState  blk)
deriving instance Serialise (TipInfo                       blk)  => Serialise (WrapTipInfo        blk)

{-------------------------------------------------------------------------------
  Ticking

  These are just forwarding instances
-------------------------------------------------------------------------------}

newtype instance Ticked (WrapLedgerView blk) = WrapTickedLedgerView {
      unwrapTickedLedgerView :: Ticked (LedgerView (BlockProtocol blk))
    }

newtype instance Ticked (WrapChainDepState blk) = WrapTickedChainDepState {
      unwrapTickedChainDepState :: Ticked (ChainDepState (BlockProtocol blk))
    }
