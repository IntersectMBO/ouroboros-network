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
    -- * Interaction with the ledger layer
    UpdateLedger(..)
  , BlockProtocol
  , ProtocolLedgerView(..)
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

import           Data.Word (Word64)

import           Ouroboros.Consensus.Protocol.Abstract
import           Ouroboros.Consensus.Util (repeatedlyM)
import           Ouroboros.Network.Block (HasHeader (..))
import           Ouroboros.Network.Chain (Chain, toOldestFirst)

{-------------------------------------------------------------------------------
  Interaction with the ledger layer
-------------------------------------------------------------------------------}

-- | Interaction with the ledger layer
class ( Show (LedgerState b)
      , Show (LedgerError b)
      ) => UpdateLedger (b :: *) where
  data family LedgerState b :: *
  data family LedgerError b :: *
  -- | The 'HeaderState' can be used to verify the headers of blocks using
  -- 'advanceHeader'.
  data family HeaderState b :: *

  -- | Apply a block to the ledger state
  --
  -- TODO: We need to support rollback, so this probably won't be a pure
  -- function but rather something that lives in a monad with some actions
  -- that we can compute a "running diff" so that we can go back in time.
  applyLedgerState :: b
                   -> LedgerState b
                   -> Except (LedgerError b) (LedgerState b)

  -- | Obtain from the given 'LedgerState' a 'HeaderState' corresponding to
  -- some block in the past (relative to the given 'LedgerState').
  getHeaderState :: LedgerState b
                 -> Word64  -- ^ How many blocks in the past, max 2k slots
                 -> HeaderState b

  -- | Validate the given header and return the updated 'HeaderState', or, in
  -- case of an invalid header, a 'LedgerError'.
  --
  -- For Ouroboros Classic, a 'HeaderState' can only be used for a window of
  -- 2k slots forward and 2k slots backwards. So after advancing a
  -- 'HeaderState' beyond the window, a new 'HeaderState' must be obtained
  -- from the 'LedgerState'. Instead of burdening the user with this
  -- responsibility, it is shifted to the __implementors__ of this method:
  -- when the given 'HeaderState' is no longer valid (the user has advanced it
  -- beyond the valid window), it must be ignored and a new 'HeaderState' must
  -- be obtained from the 'LedgerState' and used to validate the header (and
  -- returned).
  advanceHeader :: HasHeader hdr
                => LedgerState b
                -> hdr
                -> HeaderState b
                -> Except (LedgerError b) (HeaderState b)
  -- TODO make hdr a type parameter or a data/type family?



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

applyExtLedgerState :: ProtocolLedgerView b
                    => (PreHeader b -> Encoding) -- Serialiser for the preheader
                    -> NodeConfig (BlockProtocol b)
                    -> b
                    -> ExtLedgerState b
                    -> Except (ExtValidationError b) (ExtLedgerState b)
applyExtLedgerState toEnc cfg b ExtLedgerState{..} = do
    ledgerState'         <- withExcept ExtValidationErrorLedger $
                              applyLedgerState b ledgerState
    ouroborosChainState' <- withExcept ExtValidationErrorOuroboros $
                              applyChainState
                                toEnc
                                cfg
                                (protocolLedgerView cfg ledgerState')
                                b
                                ouroborosChainState
    return $ ExtLedgerState ledgerState' ouroborosChainState'

foldExtLedgerState :: ProtocolLedgerView b
                   => (PreHeader b -> Encoding) -- Serialiser for the preheader
                   -> NodeConfig (BlockProtocol b)
                   -> [b] -- ^ Blocks to apply, oldest first
                   -> ExtLedgerState b
                   -> Except (ExtValidationError b) (ExtLedgerState b)
foldExtLedgerState toEnc = repeatedlyM . applyExtLedgerState toEnc

-- TODO: This should check stuff like backpointers also
chainExtLedgerState :: ProtocolLedgerView b
                    => (PreHeader b -> Encoding) -- Serialiser for the preheader
                    -> NodeConfig (BlockProtocol b)
                    -> Chain b
                    -> ExtLedgerState b
                    -> Except (ExtValidationError b) (ExtLedgerState b)
chainExtLedgerState toEnc cfg = foldExtLedgerState toEnc cfg . toOldestFirst

-- | Validation of an entire chain
verifyChain :: ProtocolLedgerView b
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
