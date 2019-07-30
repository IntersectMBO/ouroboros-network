{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE UndecidableInstances #-}

module Ouroboros.Consensus.Ledger.Extended (
    -- * Extended ledger state
    ExtLedgerState(..)
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
import           GHC.Stack

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

data ExtValidationError blk =
    ExtValidationErrorLedger (LedgerError blk)
  | ExtValidationErrorOuroboros (ValidationErr (BlockProtocol blk))

deriving instance ProtocolLedgerView blk => Show (ExtLedgerState     blk)
deriving instance ProtocolLedgerView blk => Show (ExtValidationError blk)

deriving instance (ProtocolLedgerView blk, Eq (ChainState (BlockProtocol blk)))
               => Eq (ExtLedgerState blk)

applyExtLedgerState :: ( UpdateLedger blk
                       , ProtocolLedgerView blk
                       , HasCallStack
                       )
                    => NodeConfig (BlockProtocol blk)
                    -> blk
                    -> ExtLedgerState blk
                    -> Except (ExtValidationError blk) (ExtLedgerState blk)
applyExtLedgerState cfg blk ExtLedgerState{..} = do
    ledgerState'         <- withExcept ExtValidationErrorLedger $
                              applyChainTick
                                (ledgerConfigView cfg)
                                (blockSlot blk)
                                ledgerState
    ledgerState''        <- withExcept ExtValidationErrorLedger $
                              applyLedgerBlock
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
                   => NodeConfig (BlockProtocol blk)
                   -> [blk] -- ^ Blocks to apply, oldest first
                   -> ExtLedgerState blk
                   -> Except (ExtValidationError blk) (ExtLedgerState blk)
foldExtLedgerState = repeatedlyM . applyExtLedgerState

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
