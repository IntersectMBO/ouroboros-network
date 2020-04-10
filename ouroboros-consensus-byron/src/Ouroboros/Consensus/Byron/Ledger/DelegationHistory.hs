{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeApplications           #-}

-- | Delegation history
--
-- Intended for qualified import
module Ouroboros.Consensus.Byron.Ledger.DelegationHistory (
    DelegationHistory
  , empty
  , snapOld
  , find
    -- * Serialisation
  , encodeDelegationHistory
  , decodeDelegationHistory
  ) where

import           Codec.CBOR.Decoding (Decoder)
import           Codec.CBOR.Encoding (Encoding, encodeListLen)
import           Codec.Serialise (decode, encode)
import           Data.Coerce (coerce)
import qualified Data.Foldable as Foldable
import qualified Data.Sequence.Strict as Seq

import           Cardano.Binary (enforceSize)

import qualified Cardano.Chain.Common as CC
import qualified Cardano.Chain.Delegation as Delegation
import           Cardano.Prelude (NoUnexpectedThunks)
import           Cardano.Slotting.SlotBounded (Bounds (..), SlotBounded (..))

import           Ouroboros.Network.Block (SlotNo (..))
import           Ouroboros.Network.Point (WithOrigin (..))

import           Ouroboros.Consensus.Ledger.History (History (..))
import qualified Ouroboros.Consensus.Ledger.History as History
import           Ouroboros.Consensus.Protocol.PBFT

import           Ouroboros.Consensus.Byron.Ledger.PBFT
import           Ouroboros.Consensus.Byron.Protocol

{-------------------------------------------------------------------------------
  Delegation history
-------------------------------------------------------------------------------}

-- | Delegation history
--
-- Motivation: the ledger state gives us both the current delegation state
-- ('getDelegationMap') as well any planned future changes to the delegation
-- state ('getScheduledDelegations'). It does not however give us information
-- about past delegation states. This is where the 'DelegationHistory' comes in.
--
-- See 'History' for more details
newtype DelegationHistory = DH (History Delegation.Map)
  deriving stock (Show, Eq)
  deriving newtype (NoUnexpectedThunks)

-- | Empty (genesis) delegation history
empty :: DelegationHistory
empty = DH History.empty

-- | Take a snapshot of the delegation state
snapOld :: CC.BlockCount  -- ^ Maximum rollback (@k@)
        -> SlotNo         -- ^ Slot number of the block that changed delegation
        -> Delegation.Map -- ^ Delegation state /before/ it changed
        -> DelegationHistory -> DelegationHistory
snapOld = coerce (History.snapOld @Delegation.Map)

find :: WithOrigin SlotNo -> DelegationHistory -> Maybe Delegation.Map
find = coerce (History.find @Delegation.Map)

{-------------------------------------------------------------------------------
  Serialisation

  We translate to @PBftLedgerView@ so that we can piggy-back on its @Serialise@
  instance.
-------------------------------------------------------------------------------}

toLedgerViews :: History.Snapshots Delegation.Map
              -> [SlotBounded 'IX (PBftLedgerView PBftByronCrypto)]
toLedgerViews = map (fmap toPBftLedgerView) . Foldable.toList

fromLedgerViews :: [SlotBounded 'IX (PBftLedgerView PBftByronCrypto)]
                -> History.Snapshots Delegation.Map
fromLedgerViews = Seq.fromList . map (fmap fromPBftLedgerView)

encodeDelegationHistory :: DelegationHistory -> Encoding
encodeDelegationHistory (DH History{..}) = mconcat [
      encodeListLen 2
    , encode historyAnchor
    , encode $ toLedgerViews historySnapshots
    ]

decodeDelegationHistory :: Decoder s DelegationHistory
decodeDelegationHistory = do
    enforceSize "DelegationHistory" 2
    historyAnchor    <- decode
    historySnapshots <- fromLedgerViews <$> decode
    return $ DH History{..}
