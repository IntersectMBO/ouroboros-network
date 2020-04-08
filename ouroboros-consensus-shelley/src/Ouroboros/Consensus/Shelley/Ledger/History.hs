{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}
-- | Store the history of the Shelley ledger view in order to allow time-travel
-- to the recent past.
--
-- Intended for qualified import
module Ouroboros.Consensus.Shelley.Ledger.History (
    LedgerView
  , LedgerViewHistory
  , empty
  , snapOld
  , find
    -- * Serialisation
  , encodeLedgerViewHistory
  , decodeLedgerViewHistory
  ) where

import           Codec.CBOR.Decoding (Decoder)
import           Codec.CBOR.Encoding (Encoding, encodeListLen)
import           Codec.Serialise (decode, encode)
import           Data.Coerce (coerce)
import           Data.Word (Word64)

import           Cardano.Binary (FromCBOR (..), ToCBOR (..), enforceSize)
import           Cardano.Prelude (NoUnexpectedThunks (..))
import           Cardano.Slotting.Slot (SlotNo (..), WithOrigin)

import           Ouroboros.Consensus.Ledger.History (History (..))
import qualified Ouroboros.Consensus.Ledger.History as History
import           Ouroboros.Consensus.Util.CBOR (decodeSeq, encodeSeq)

import           Shelley.Spec.Ledger.API (LedgerView)

import           Ouroboros.Consensus.Shelley.Protocol (Crypto)

{-------------------------------------------------------------------------------
  LedgerView history
-------------------------------------------------------------------------------}

-- | Ledger view history
--
-- Motivation: the ledger state does not give us information about past
-- 'LedgerView's, therefore we track past 'LedgerView' as long as they are
-- required, as rollbacks are limited to @k@ blocks.
--
-- See 'History' for more details.
newtype LedgerViewHistory c = LVH {
      unLedgerViewHistory :: History (LedgerView c)
    }
  deriving stock (Show, Eq)
  deriving newtype (NoUnexpectedThunks)

-- | Empty (genesis) 'LedgerView' history
empty :: LedgerViewHistory c
empty = LVH History.empty

-- | Take a snapshot of the 'LedgerView'
snapOld
  :: forall c.
     Word64        -- ^ Maximum rollback (@k@)
  -> SlotNo        -- ^ Slot number of the block that changed the @LedgerView@
  -> LedgerView c  -- ^ @LedgerView@ /before/ it changed
  -> LedgerViewHistory c
  -> LedgerViewHistory c
snapOld = coerce (History.snapOld @(LedgerView c))

find
  :: forall c.
     WithOrigin SlotNo
  -> LedgerViewHistory c
  -> Maybe (LedgerView c)
find = coerce (History.find @(LedgerView c))

{-------------------------------------------------------------------------------
  Serialisation
-------------------------------------------------------------------------------}

encodeLedgerViewHistory :: Crypto c => LedgerViewHistory c -> Encoding
encodeLedgerViewHistory (LVH History{..}) = mconcat [
      encodeListLen 2
    , encode historyAnchor
    , encodeSeq toCBOR historySnapshots
    ]

decodeLedgerViewHistory :: Crypto c => Decoder s (LedgerViewHistory c)
decodeLedgerViewHistory = do
    enforceSize "LedgerViewHistory" 2
    historyAnchor <- decode
    historySnapshots <- decodeSeq fromCBOR
    return $ LVH History{..}
