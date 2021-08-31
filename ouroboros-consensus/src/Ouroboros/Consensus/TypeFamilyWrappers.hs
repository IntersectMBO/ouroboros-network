{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}

-- | Newtypes around type families so that they can be partially applied
module Ouroboros.Consensus.TypeFamilyWrappers (
    -- * Block based
    WrapApplyTxErr (..)
  , WrapCannotForge (..)
  , WrapEnvelopeErr (..)
  , WrapForgeStateInfo (..)
  , WrapForgeStateUpdateError (..)
  , WrapGenTxId (..)
  , WrapHeaderHash (..)
  , WrapLedgerConfig (..)
  , WrapLedgerErr (..)
  , WrapLedgerUpdate (..)
  , WrapLedgerWarning (..)
  , WrapTipInfo (..)
    -- * Protocol based
  , WrapCanBeLeader (..)
  , WrapChainDepState (..)
  , WrapConsensusConfig (..)
  , WrapIsLeader (..)
  , WrapLedgerView (..)
  , WrapSelectView (..)
  , WrapValidateView (..)
  , WrapValidatedGenTx (..)
  , WrapValidationErr (..)
    -- * Versioning
  , WrapNodeToClientVersion (..)
  , WrapNodeToNodeVersion (..)
    -- * Type family instances
  , Ticked (..)
  ) where

import           Codec.Serialise (Serialise)
import           NoThunks.Class (NoThunks)

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.HeaderValidation
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Ledger.Inspect
import           Ouroboros.Consensus.Ledger.SupportsMempool
import           Ouroboros.Consensus.Node.NetworkProtocolVersion
import           Ouroboros.Consensus.Protocol.Abstract

{-------------------------------------------------------------------------------
  Block based
-------------------------------------------------------------------------------}

newtype WrapApplyTxErr            blk = WrapApplyTxErr            { unwrapApplyTxErr            :: ApplyTxErr               blk }
newtype WrapCannotForge           blk = WrapCannotForge           { unwrapCannotForge           :: CannotForge              blk }
newtype WrapEnvelopeErr           blk = WrapEnvelopeErr           { unwrapEnvelopeErr           :: OtherHeaderEnvelopeError blk }
newtype WrapForgeStateInfo        blk = WrapForgeStateInfo        { unwrapForgeStateInfo        :: ForgeStateInfo           blk }
newtype WrapForgeStateUpdateError blk = WrapForgeStateUpdateError { unwrapForgeStateUpdateError :: ForgeStateUpdateError    blk }
newtype WrapGenTxId               blk = WrapGenTxId               { unwrapGenTxId               :: GenTxId                  blk }
newtype WrapHeaderHash            blk = WrapHeaderHash            { unwrapHeaderHash            :: HeaderHash               blk }
newtype WrapLedgerConfig          blk = WrapLedgerConfig          { unwrapLedgerConfig          :: LedgerConfig             blk }
newtype WrapLedgerErr             blk = WrapLedgerErr             { unwrapLedgerErr             :: LedgerError              blk }
newtype WrapLedgerUpdate          blk = WrapLedgerUpdate          { unwrapLedgerUpdate          :: LedgerUpdate             blk }
newtype WrapLedgerWarning         blk = WrapLedgerWarning         { unwrapLedgerWarning         :: LedgerWarning            blk }
newtype WrapTipInfo               blk = WrapTipInfo               { unwrapTipInfo               :: TipInfo                  blk }

-- | A data family wrapper for @'Validated' . 'GenTx'@
--
-- 'Validated' is is data family, so this is an outlier in this module full of
-- type family wrappers. However, the standard functor composition operator @f
-- :.: g@ incurs some type classes instances that are inappropriate when the
-- outer type constructor @f@ is a family and hence non-parametric (eg @'Eq' (f
-- :.: g)@ requires @'Data.Functor.Classes.Eq1' f)). The bespoke composition
-- 'WrapValidatedGenTx' therefore serves much the same purpose as the other
-- wrappers in this module.
newtype WrapValidatedGenTx        blk = WrapValidatedGenTx        { unwrapValidatedGenTx        :: Validated         (GenTx blk)}

{-------------------------------------------------------------------------------
  Consensus based
-------------------------------------------------------------------------------}

newtype WrapCanBeLeader     blk = WrapCanBeLeader     { unwrapCanBeLeader     :: CanBeLeader     (BlockProtocol blk) }
newtype WrapChainDepState   blk = WrapChainDepState   { unwrapChainDepState   :: ChainDepState   (BlockProtocol blk) }
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

deriving instance Eq (ApplyTxErr               blk ) => Eq (WrapApplyTxErr     blk)
deriving instance Eq (GenTxId                  blk ) => Eq (WrapGenTxId        blk)
deriving instance Eq (LedgerError              blk ) => Eq (WrapLedgerErr      blk)
deriving instance Eq (LedgerUpdate             blk ) => Eq (WrapLedgerUpdate   blk)
deriving instance Eq (LedgerWarning            blk ) => Eq (WrapLedgerWarning  blk)
deriving instance Eq (OtherHeaderEnvelopeError blk ) => Eq (WrapEnvelopeErr    blk)
deriving instance Eq (TipInfo                  blk ) => Eq (WrapTipInfo        blk)
deriving instance Eq (Validated (GenTx         blk)) => Eq (WrapValidatedGenTx blk)

deriving instance Ord (GenTxId blk) => Ord (WrapGenTxId blk)

deriving instance Show (ApplyTxErr               blk ) => Show (WrapApplyTxErr            blk)
deriving instance Show (CannotForge              blk ) => Show (WrapCannotForge           blk)
deriving instance Show (ForgeStateInfo           blk ) => Show (WrapForgeStateInfo        blk)
deriving instance Show (ForgeStateUpdateError    blk ) => Show (WrapForgeStateUpdateError blk)
deriving instance Show (GenTxId                  blk ) => Show (WrapGenTxId               blk)
deriving instance Show (LedgerError              blk ) => Show (WrapLedgerErr             blk)
deriving instance Show (LedgerUpdate             blk ) => Show (WrapLedgerUpdate          blk)
deriving instance Show (LedgerWarning            blk ) => Show (WrapLedgerWarning         blk)
deriving instance Show (OtherHeaderEnvelopeError blk ) => Show (WrapEnvelopeErr           blk)
deriving instance Show (TipInfo                  blk ) => Show (WrapTipInfo               blk)
deriving instance Show (Validated (GenTx         blk)) => Show (WrapValidatedGenTx blk)

deriving instance NoThunks (GenTxId                  blk ) => NoThunks (WrapGenTxId         blk)
deriving instance NoThunks (LedgerError              blk ) => NoThunks (WrapLedgerErr       blk)
deriving instance NoThunks (OtherHeaderEnvelopeError blk ) => NoThunks (WrapEnvelopeErr     blk)
deriving instance NoThunks (TipInfo                  blk ) => NoThunks (WrapTipInfo         blk)
deriving instance NoThunks (Validated (GenTx         blk)) => NoThunks (WrapValidatedGenTx  blk)

{-------------------------------------------------------------------------------
  .. consensus based
-------------------------------------------------------------------------------}

deriving instance Eq (ChainDepState (BlockProtocol blk)) => Eq (WrapChainDepState blk)
deriving instance Eq (SelectView    (BlockProtocol blk)) => Eq (WrapSelectView    blk)
deriving instance Eq (ValidationErr (BlockProtocol blk)) => Eq (WrapValidationErr blk)

deriving instance Ord (SelectView (BlockProtocol blk)) => Ord (WrapSelectView blk)

deriving instance Show (ChainDepState (BlockProtocol blk)) => Show (WrapChainDepState blk)
deriving instance Show (LedgerView    (BlockProtocol blk)) => Show (WrapLedgerView    blk)
deriving instance Show (SelectView    (BlockProtocol blk)) => Show (WrapSelectView    blk)
deriving instance Show (ValidationErr (BlockProtocol blk)) => Show (WrapValidationErr blk)

deriving instance NoThunks (ChainDepState (BlockProtocol blk)) => NoThunks (WrapChainDepState blk)
deriving instance NoThunks (ValidationErr (BlockProtocol blk)) => NoThunks (WrapValidationErr blk)

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

deriving instance Serialise (GenTxId                      blk)  => Serialise (WrapGenTxId       blk)
deriving instance Serialise (ChainDepState (BlockProtocol blk)) => Serialise (WrapChainDepState blk)
deriving instance Serialise (TipInfo                      blk)  => Serialise (WrapTipInfo       blk)

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
