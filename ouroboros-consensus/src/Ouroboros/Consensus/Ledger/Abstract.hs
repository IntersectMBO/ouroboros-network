{-# LANGUAGE DeriveFunctor           #-}
{-# LANGUAGE FlexibleContexts        #-}
{-# LANGUAGE KindSignatures          #-}
{-# LANGUAGE MultiParamTypeClasses   #-}
{-# LANGUAGE RankNTypes              #-}
{-# LANGUAGE RecordWildCards         #-}
{-# LANGUAGE ScopedTypeVariables     #-}
{-# LANGUAGE StandaloneDeriving      #-}
{-# LANGUAGE TypeApplications        #-}
{-# LANGUAGE TypeFamilies            #-}
{-# LANGUAGE UndecidableInstances    #-}
{-# LANGUAGE UndecidableSuperClasses #-}

-- | Interface to the ledger layer
module Ouroboros.Consensus.Ledger.Abstract (
    -- * Interaction with the ledger layer
    UpdateLedger(..)
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
import           GHC.Stack

import           Ouroboros.Consensus.Protocol.Abstract
import           Ouroboros.Consensus.Util (repeatedlyM)
import           Ouroboros.Consensus.Util.SlotBounded (SlotBounded)
import           Ouroboros.Network.Block (HasHeader (..), Point, SlotNo)
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

  -- | Static environment required for the ledger
  data family LedgerConfig b :: *

  -- | Apply a block header to the ledger state.
  --
  -- Used in 'applyExtLedgerState' to update the ledger state in 3 steps:
  --
  -- 1. 'applyLedgerHeader' updates the ledger with information from the header
  -- 2. 'applyChainState' updates the the consensus-specific chain state
  --    This gets passed the updated ledger from step (1) as an argument
  -- 3. 'applyLedgerBlock' updates the ledger with information from the body
  --
  -- TODO: Explain why this ordering is correct and why we need the split;
  -- (3) does not seem to rely on (2), and so we could do (1), (3), (2), and if
  -- that is indeed possible, we could just combine (1) and (3) into a single
  -- step..?
  -- <https://github.com/input-output-hk/ouroboros-network/issues/596>
  applyLedgerHeader :: LedgerConfig b
                    -> b
                    -> LedgerState b
                    -> Except (LedgerError b) (LedgerState b)

  -- | Apply a block to the ledger state
  applyLedgerBlock :: LedgerConfig b
                   -> b
                   -> LedgerState b
                   -> Except (LedgerError b) (LedgerState b)

  -- | Point of the most recently applied block
  --
  -- Should be 'genesisPoint' when no blocks have been applied yet
  ledgerTipPoint :: LedgerState b -> Point b

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

  -- | Get a ledger view for a specific slot
  --
  -- Suppose @k = 4@, i.e., we can roll back 4 blocks
  --
  -- >             /-----------\
  -- >             |           ^
  -- >             v           |
  -- >     --*--*--*--*--*--*--*--
  -- >          |  A           B
  -- >          |
  -- >          \- A'
  --
  -- In other words, this means that we can roll back from point B to point A,
  -- and then roll forward to any block on any fork from A. Note that we can
  -- /not/ roll back to any siblings of A (such as A'), as that would require
  -- us to roll back at least @k + 1@ blocks, which we can't (by definition).
  --
  -- Given a ledger state at point B, we should be able to verify any of the
  -- headers (corresponding to the blocks) at point A or any of its successors
  -- on any fork, up to some maximum distance from A. This distance can be
  -- determined by the ledger, though must be at least @k@: we must be able to
  -- validate any of these past headers, since otherwise we would not be able to
  -- switch to a fork. It is not essential that the maximum distance extends
  -- into the future (@> k@), though it is helpful: it means that in the chain
  -- sync client we can download and validate headers even if they don't fit
  -- directly onto the tip of our chain.
  --
  -- The anachronistic ledger state at point B is precisely the ledger state
  -- that can be used to validate this set of headers. The bounds (in terms of
  -- slots) are a hint about its valid range: how far into the past can we look
  -- (at least @k@) and how far into the future (depending on the maximum
  -- distance supported by the ledger). It is however important to realize that
  -- this is not a full specification: after all, blocks @A@ and @A'@ have the
  -- same slot number, but @A@ can be validated using the anachronistic ledger
  -- view at @B@ whereas @A'@ can not.
  --
  -- Invariant: when calling this function with slot @s@ yields a
  -- 'SlotBounded' @sb@, then @'atSlot' sb@ yields a 'Just'.
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

deriving instance ProtocolLedgerView b => Show (ExtValidationError b)

applyExtLedgerState :: forall b.
                       ( LedgerConfigView b
                       , ProtocolLedgerView b
                       , HasCallStack
                       )
                    => NodeConfig (BlockProtocol b)
                    -> b
                    -> ExtLedgerState b
                    -> Except (ExtValidationError b) (ExtLedgerState b)
applyExtLedgerState cfg b ExtLedgerState{..} = do
    ledgerState'         <- withExcept ExtValidationErrorLedger $
                              applyLedgerHeader (ledgerConfigView cfg) b ledgerState
    ouroborosChainState' <- withExcept ExtValidationErrorOuroboros $
                              applyChainState
                                cfg
                                (protocolLedgerView cfg ledgerState')
                                b
                                ouroborosChainState
    ledgerState''        <- withExcept ExtValidationErrorLedger $
                              applyLedgerBlock (ledgerConfigView cfg) b ledgerState'
    return $ ExtLedgerState ledgerState'' ouroborosChainState'

foldExtLedgerState :: ( LedgerConfigView b
                      , ProtocolLedgerView b
                      , HasCallStack
                      )
                   => NodeConfig (BlockProtocol b)
                   -> [b] -- ^ Blocks to apply, oldest first
                   -> ExtLedgerState b
                   -> Except (ExtValidationError b) (ExtLedgerState b)
foldExtLedgerState = repeatedlyM . applyExtLedgerState

chainExtLedgerState :: ( LedgerConfigView b
                       , ProtocolLedgerView b
                       , HasCallStack
                       )
                    => NodeConfig (BlockProtocol b)
                    -> Chain b
                    -> ExtLedgerState b
                    -> Except (ExtValidationError b) (ExtLedgerState b)
chainExtLedgerState cfg = foldExtLedgerState cfg . toOldestFirst

-- | Validation of an entire chain
verifyChain :: ( LedgerConfigView b
               , ProtocolLedgerView b
               )
            => NodeConfig (BlockProtocol b)
            -> ExtLedgerState b
            -> Chain b
            -> Bool
verifyChain cfg initSt c =
    case runExcept (chainExtLedgerState cfg c initSt) of
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
