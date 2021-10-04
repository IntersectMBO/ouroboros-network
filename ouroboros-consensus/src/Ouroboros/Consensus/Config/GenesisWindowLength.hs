{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Ouroboros.Consensus.Config.GenesisWindowLength (
    GenesisWindowLength (..)
  , genesisWindowBounds
  ) where

import           Cardano.Slotting.Slot
import           Data.Word
import           GHC.Generics (Generic)
import           NoThunks.Class (NoThunks)
import           Quiet

-- | The Genesis window length as needed for Ouroboros Genesis.
--
-- The paper <https://eprint.iacr.org/2018/378.pdf Ouroboros Genesis: Composable Proof-of-Stake Blockchains with Dynamic Availability>
-- specifies that when two chains intersect more than @k@ blocks in the past,
-- then the best chain is the one that has more blocks in the window of length
-- @s@.
--
-- After discussing what value should be used for @s@ with the researchers, it
-- was set to @3k/f@.
--
-- The genesis window must be available for all eras. Ouroboros Genesis is a
-- refinement of Praos that aims to solve the problem of nodes joining the
-- network. Therefore it does not make sense to enable the genesis window only
-- for certain eras or periods.
--
-- Although the genesis window length could potentially change among eras,
-- there's no benefit currently for our use case or for any use case that we
-- envision to run on this consensus layer for having a way of varying the
-- length. See the consensus report for more details.
newtype GenesisWindowLength = GenesisWindowLength { genesisWindowLength :: Word64 }
  deriving (Eq, Generic, NoThunks)
  deriving Show via Quiet GenesisWindowLength

-- | Given the slot of the window's anchor and the length of the window return
-- the bounds of the window.
genesisWindowBounds
  :: WithOrigin SlotNo
  -> GenesisWindowLength
  -> (WithOrigin SlotNo, SlotNo)
genesisWindowBounds w (GenesisWindowLength s) =
  (w, withOrigin (SlotNo s) (SlotNo s +) w)
