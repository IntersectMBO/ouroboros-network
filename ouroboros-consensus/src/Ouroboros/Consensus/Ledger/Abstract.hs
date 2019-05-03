{-# LANGUAGE DeriveFunctor           #-}
{-# LANGUAGE FlexibleContexts        #-}
{-# LANGUAGE KindSignatures          #-}
{-# LANGUAGE MultiParamTypeClasses   #-}
{-# LANGUAGE RankNTypes              #-}
{-# LANGUAGE RecordWildCards         #-}
{-# LANGUAGE StandaloneDeriving      #-}
{-# LANGUAGE TypeFamilies            #-}
{-# LANGUAGE UndecidableInstances    #-}
{-# LANGUAGE UndecidableSuperClasses #-}

-- | Interface to the ledger layer
module Ouroboros.Consensus.Ledger.Abstract (
    SlotBounded(sbLower, sbUpper)
  , slotUnbounded
  , atSlot
  , slotBounded
    -- * Interaction with the ledger layer
  , UpdateLedger(..)
  , BlockProtocol
  , ProtocolLedgerView(..)
  , LedgerConfigView(..)
    -- * Extended ledger state
  , ExtLedgerState(..)
  , ExtValidationError(..)
  , applyExtLedgerState
  , foldExtLedgerState
  , chainExtLedgerState
  , verifyChain
    -- * Serialisation
  , encodeExtLedgerState
  , decodeExtLedgerState
  ) where

import           Codec.CBOR.Decoding (Decoder)
import           Codec.CBOR.Encoding (Encoding)
import           Control.Monad.Except

import           Ouroboros.Consensus.Protocol.Abstract
import           Ouroboros.Consensus.Util (repeatedlyM)
import           Ouroboros.Network.Block (HasHeader (..), SlotNo)
import           Ouroboros.Network.Chain (Chain, toOldestFirst)

-- | An item bounded to be valid within particular slots
data SlotBounded a = SlotBounded
  { sbLower   :: !SlotNo
  , sbUpper   :: !SlotNo
  , sbContent :: !a
  } deriving (Eq, Functor, Show)

-- | Construct a slot bounded item.
--
--   We choose not to validate that the slot bounds are reasonable here.
slotBounded :: SlotNo -> SlotNo -> a -> SlotBounded a
slotBounded = SlotBounded

slotUnbounded :: a -> SlotBounded a
slotUnbounded = SlotBounded minBound maxBound

atSlot :: SlotNo -> SlotBounded a -> Maybe a
atSlot slot sb =
  if (slot <= sbUpper sb && slot >= sbLower sb)
  then Just $ sbContent sb
  else Nothing

{-------------------------------------------------------------------------------
  Interaction with the ledger layer
-------------------------------------------------------------------------------}

-- | Interaction with the ledger layer
class ( Show (LedgerState b)
      , Show (LedgerError b)
      ) => UpdateLedger (b :: *) where
  data family LedgerState b :: *
  data family LedgerError b :: *

  data family LedgerConfig b :: *

  -- | Apply a block header to the ledger state.
  applyLedgerHeader :: LedgerConfig b
                    -> b
                    -> LedgerState b
                    -> Except (LedgerError b) (LedgerState b)

  -- | Apply a block to the ledger state
  -- w3
  -- TODO: We need to support rollback, so this probably won't be a pure
  -- function but rather something that lives in a monad with some actions
  -- that we can compute a "running diff" so that we can go back in time.
  applyLedgerBlock :: LedgerConfig b
                   -> b
                   -> LedgerState b
                   -> Except (LedgerError b) (LedgerState b)


-- | Link blocks to their unique protocol
type family BlockProtocol b :: *

-- | Link protocol to ledger
class ( OuroborosTag (BlockProtocol b)
      , UpdateLedger b
      , HasHeader b
      , SupportedBlock (BlockProtocol b) b
      ) => ProtocolLedgerView b where
  protocolLedgerView :: NodeConfig (BlockProtocol b)
                     -> LedgerState b
                     -> LedgerView (BlockProtocol b)

  -- | Get a ledger view for a specific slot.
  --
  -- This may return Nothing if it's not possible to extract the ledger view for
  -- the specified slot.
  --
  -- Implementers of this function may provide a ledger view which is valid for
  -- more than the slot which was requested, in which case they may indicate
  -- this via the bounds in 'SlotBounded'.
  anachronisticProtocolLedgerView
    :: NodeConfig (BlockProtocol b)
    -> LedgerState b
    -> SlotNo -- ^ Slot for which you would like a ledger view
    -> Maybe (SlotBounded (LedgerView (BlockProtocol b)))

-- | Extract the ledger environment from the node config
class ( UpdateLedger b
      , OuroborosTag (BlockProtocol b)
      ) => LedgerConfigView b where
  ledgerConfigView :: NodeConfig (BlockProtocol b)
                   -> LedgerConfig b

{-------------------------------------------------------------------------------
  Extended ledger state
-------------------------------------------------------------------------------}

-- | Extended ledger state
--
-- This is the combination of the ouroboros state and the ledger state proper.
data ExtLedgerState b = ExtLedgerState {
      ledgerState         :: LedgerState b
    , ouroborosChainState :: ChainState (BlockProtocol b)
    }

deriving instance ProtocolLedgerView b => Show (ExtLedgerState b)

data ExtValidationError b =
    ExtValidationErrorLedger (LedgerError b)
  | ExtValidationErrorOuroboros (ValidationErr (BlockProtocol b))
  | ExtValidationErrorEnvelope -- TODO (check back pointers etc)

deriving instance ProtocolLedgerView b => Show (ExtValidationError b)

applyExtLedgerState :: (LedgerConfigView b, ProtocolLedgerView b)
                    => (PreHeader b -> Encoding) -- Serialiser for the preheader
                    -> NodeConfig (BlockProtocol b)
                    -> b
                    -> ExtLedgerState b
                    -> Except (ExtValidationError b) (ExtLedgerState b)
applyExtLedgerState toEnc cfg b ExtLedgerState{..} = do
    ledgerState'         <- withExcept ExtValidationErrorLedger $
                              applyLedgerHeader (ledgerConfigView cfg) b ledgerState
    ouroborosChainState' <- withExcept ExtValidationErrorOuroboros $
                              applyChainState
                                toEnc
                                cfg
                                (protocolLedgerView cfg ledgerState')
                                b
                                ouroborosChainState
    ledgerState''        <- withExcept ExtValidationErrorLedger $
                              applyLedgerBlock (ledgerConfigView cfg) b ledgerState'
    return $ ExtLedgerState ledgerState'' ouroborosChainState'

foldExtLedgerState :: (LedgerConfigView b, ProtocolLedgerView b)
                   => (PreHeader b -> Encoding) -- Serialiser for the preheader
                   -> NodeConfig (BlockProtocol b)
                   -> [b] -- ^ Blocks to apply, oldest first
                   -> ExtLedgerState b
                   -> Except (ExtValidationError b) (ExtLedgerState b)
foldExtLedgerState toEnc = repeatedlyM . applyExtLedgerState toEnc

-- TODO: This should check stuff like backpointers also
chainExtLedgerState :: (LedgerConfigView b, ProtocolLedgerView b)
                    => (PreHeader b -> Encoding) -- Serialiser for the preheader
                    -> NodeConfig (BlockProtocol b)
                    -> Chain b
                    -> ExtLedgerState b
                    -> Except (ExtValidationError b) (ExtLedgerState b)
chainExtLedgerState toEnc cfg = foldExtLedgerState toEnc cfg . toOldestFirst

-- | Validation of an entire chain
verifyChain :: (LedgerConfigView b, ProtocolLedgerView b)
            => (PreHeader b -> Encoding) -- Serialiser for the preheader
            -> NodeConfig (BlockProtocol b)
            -> ExtLedgerState b
            -> Chain b
            -> Bool
verifyChain toEnc cfg initSt c =
    case runExcept (chainExtLedgerState toEnc cfg c initSt) of
      Left  _err -> False
      Right _st' -> True

{-------------------------------------------------------------------------------
  Serialisation
-------------------------------------------------------------------------------}

encodeExtLedgerState :: (LedgerState b -> Encoding)
                     -> (ChainState (BlockProtocol b) -> Encoding)
                     -> ExtLedgerState b -> Encoding
encodeExtLedgerState encodeLedger
                     encodeChainState
                     ExtLedgerState{..} = mconcat [
      encodeLedger     ledgerState
    , encodeChainState ouroborosChainState
    ]

decodeExtLedgerState :: (forall s. Decoder s (LedgerState b))
                     -> (forall s. Decoder s (ChainState (BlockProtocol b)))
                     -> forall s. Decoder s (ExtLedgerState b)
decodeExtLedgerState decodeLedger decodeChainState = do
    ledgerState         <- decodeLedger
    ouroborosChainState <- decodeChainState
    return ExtLedgerState{..}
