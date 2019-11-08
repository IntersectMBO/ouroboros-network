{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE UndecidableInstances #-}

module Ouroboros.Consensus.Ledger.Extended (
    -- * Extended ledger state
    BlockPreviouslyApplied(..)
  , ExtLedgerState(..)
  , ExtValidationError(..)
  , applyExtLedgerState
  , foldExtLedgerState
    -- * Serialisation
  , encodeExtLedgerState
  , decodeExtLedgerState
  ) where

import           Codec.CBOR.Decoding (Decoder)
import           Codec.CBOR.Encoding (Encoding)
import           Control.Monad.Except
import           Data.Proxy
import           Data.Typeable
import           GHC.Generics (Generic)
import           GHC.Stack

import           Cardano.Prelude (NoUnexpectedThunks (..))

import           Ouroboros.Network.Block (blockSlot)

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Protocol.Abstract
import           Ouroboros.Consensus.Util (repeatedlyM)

{-------------------------------------------------------------------------------
  Extended ledger state
-------------------------------------------------------------------------------}

-- | Extended ledger state
--
-- This is the combination of the ouroboros state and the ledger state proper.
data ExtLedgerState blk = ExtLedgerState {
      ledgerState         :: !(LedgerState blk)
    , ouroborosChainState :: !(ChainState (BlockProtocol blk))
    }
  deriving (Generic)

data ExtValidationError blk =
    ExtValidationErrorLedger (LedgerError blk)
  | ExtValidationErrorOuroboros (ValidationErr (BlockProtocol blk))

deriving instance ProtocolLedgerView blk => Show (ExtLedgerState     blk)
deriving instance ProtocolLedgerView blk => Show (ExtValidationError blk)

-- | We override 'showTypeOf' to show the type of the block
--
-- This makes debugging a bit easier, as the block gets used to resolve all
-- kinds of type families.
instance ProtocolLedgerView blk => NoUnexpectedThunks (ExtLedgerState blk) where
  showTypeOf _ = show $ typeRep (Proxy @(ExtLedgerState blk))

deriving instance (ProtocolLedgerView blk, Eq (ChainState (BlockProtocol blk)))
               => Eq (ExtLedgerState blk)

data BlockPreviouslyApplied =
    BlockPreviouslyApplied
  -- ^ The block has been previously applied and validated against the given
  -- ledger state and no block validations should be performed.
  | BlockNotPreviouslyApplied
  -- ^ The block has not been previously applied to the given ledger state and
  -- all block validations should be performed.

-- | Update the extended ledger state
--
-- Updating the extended state happens in 3 steps:
--
-- * We call 'applyChainTick' to process any changes that happen at epoch
--   boundaries.
-- * We call 'applyLedgerBlock' to process the block itself; this looks at both
--   the header and the body of the block, but /only/ involves the ledger,
--   not the consensus chain state.
-- * Finally, we pass the updated ledger view to then update the consensus
--   chain state.
--
-- Note: for Byron, this currently deviates from the spec. We apply scheduled
-- updates /before/ checking the signature, but the spec does this the other
-- way around. This means that in the spec delegation updates scheduled for
-- slot @n@ are really only in effect at slot @n+1@.
-- See <https://github.com/input-output-hk/cardano-ledger-specs/issues/1007>
applyExtLedgerState :: ( UpdateLedger blk
                       , ProtocolLedgerView blk
                       , HasCallStack
                       )
                    => BlockPreviouslyApplied
                    -> NodeConfig (BlockProtocol blk)
                    -> blk
                    -> ExtLedgerState blk
                    -> Except (ExtValidationError blk) (ExtLedgerState blk)
applyExtLedgerState prevApplied cfg blk ExtLedgerState{..} = do
    ledgerState'         <- withExcept ExtValidationErrorLedger $
                              applyChainTick
                                (ledgerConfigView cfg)
                                (blockSlot blk)
                                ledgerState
    ledgerState''        <- case prevApplied of
                              BlockNotPreviouslyApplied ->
                                withExcept ExtValidationErrorLedger $
                                  applyLedgerBlock
                                    (ledgerConfigView cfg)
                                    blk
                                    ledgerState'
                              BlockPreviouslyApplied -> pure $
                                reapplyLedgerBlock
                                  (ledgerConfigView cfg)
                                  blk
                                  ledgerState'
    ouroborosChainState' <- withExcept ExtValidationErrorOuroboros $
                              applyChainState
                                cfg
                                (protocolLedgerView cfg ledgerState'')
                                (getHeader blk)
                                ouroborosChainState
    return $ ExtLedgerState ledgerState'' ouroborosChainState'

foldExtLedgerState :: (ProtocolLedgerView blk, HasCallStack)
                   => BlockPreviouslyApplied
                   -> NodeConfig (BlockProtocol blk)
                   -> [blk] -- ^ Blocks to apply, oldest first
                   -> ExtLedgerState blk
                   -> Except (ExtValidationError blk) (ExtLedgerState blk)
foldExtLedgerState prevApplied = repeatedlyM . (applyExtLedgerState prevApplied)

{-------------------------------------------------------------------------------
  Serialisation
-------------------------------------------------------------------------------}

encodeExtLedgerState :: (LedgerState blk -> Encoding)
                     -> (ChainState (BlockProtocol blk) -> Encoding)
                     -> ExtLedgerState blk -> Encoding
encodeExtLedgerState encodeLedger
                     encodeChainState
                     ExtLedgerState{..} = mconcat [
      encodeLedger     ledgerState
    , encodeChainState ouroborosChainState
    ]

decodeExtLedgerState :: (forall s. Decoder s (LedgerState blk))
                     -> (forall s. Decoder s (ChainState (BlockProtocol blk)))
                     -> forall s. Decoder s (ExtLedgerState blk)
decodeExtLedgerState decodeLedger decodeChainState = do
    ledgerState         <- decodeLedger
    ouroborosChainState <- decodeChainState
    return ExtLedgerState{..}
