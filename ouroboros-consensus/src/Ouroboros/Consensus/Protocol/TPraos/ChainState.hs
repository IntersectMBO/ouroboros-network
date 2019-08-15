{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}

-- | Chain state for Transitional Praos
module Ouroboros.Consensus.Protocol.TPraos.ChainState where

import Cardano.Prelude (NoUnexpectedThunks(..))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import GHC.Generics (Generic)
import Ouroboros.Network.Block (SlotNo (..))
import Ouroboros.Network.Point (WithOrigin (..))
import Ouroboros.Consensus.Protocol.TPraos.Util

import qualified STS.Prtcl as STS

-- | Praos chain state.
--
-- The transitional praos implementation itself has a concept of state, given by
-- the STS.State (PRTCL c) type. This chain state, however, doesn't support
-- rewinding.
--
--
data TPraosChainState c = TPraosChainState
  { -- | Anchor
    --
    -- The anchor is the earliest slot to which we can roll back. It should
    -- correspond to the first slot with an entry in the historical state.
    --
    -- We store this for easy computation of whether a rollback is allowed, and
    -- for sanity checking of the state.
    anchor :: WithOrigin SlotNo

    -- | Historical state snapshots.
  , historicalStates :: Map (WithOrigin SlotNo) (STS.State (PRTCL c))
  } deriving (Generic, Show)

instance NoUnexpectedThunks (TPraosChainState c)

-- | Extract the chain state
toPRTCLState :: TPraosChainState c -> STS.State (PRTCL c)
toPRTCLState cs
  | Map.null (historicalStates cs) = error "Empty chain state"
  | otherwise = snd . Map.findMax $ historicalStates cs

-- | Append a new state to the history.
--
-- This does not prune anything from the old history - so it's possible after
-- calling this to have a chain state containing more history than needed.
appendState
  :: STS.State (PRTCL c)
  -> TPraosChainState c
  -> TPraosChainState c
appendState ss st = let
    STS.PrtclState _ _ slot _ _ = ss
  in st
    { historicalStates = Map.insert (At $ convertSlotNo slot) ss (historicalStates st) }

-- | Prune the chain state to a given maximum size
prune
  :: Int -- ^ Size (in terms of number of blocks) to prune the chain state to.
  -> TPraosChainState c
  -> TPraosChainState c
prune toSize cs = let
    hs = historicalStates cs
    oldestIx = Map.size hs - toSize
  in if oldestIx < 0 then cs else
    let (newAnchor, anchoredCS) = Map.elemAt oldestIx hs
        (_, newStates) = Map.split newAnchor hs
    in TPraosChainState
      { anchor = newAnchor
      , historicalStates = Map.insert newAnchor anchoredCS newStates
      }

size :: TPraosChainState c -> Int
size = Map.size . historicalStates

-- | Rewind the state to the specified slot
--
-- The state is rewound to its position at the _end_ of the specified slot (i.e.
-- after any blocks in that slot have been applied)
--
-- Callers of this function should ensure that the slot we are requesting to
-- rewind to contains a block which was previously applies. However, we do not
-- technically require this - if a block has not been applied in this slot, then
-- we simply return the state as it was following the last applied block.
rewind
  :: WithOrigin SlotNo -- ^ Slot to rewind to
  -> TPraosChainState c
  -> Maybe (TPraosChainState c)
rewind toSlot st
  | toSlot < anchor st = Nothing
  | otherwise = Just $ TPraosChainState
      { -- The anchor remains the same when we rewind
        anchor = anchor st
        -- We trim the historical states to only those before the rollback point
      , historicalStates = let
          (older, mcurrent, _ ) = Map.splitLookup toSlot $ historicalStates st
          newStates = maybe older (curry (flip (uncurry Map.insert) older) toSlot) mcurrent
        in newStates
      }
