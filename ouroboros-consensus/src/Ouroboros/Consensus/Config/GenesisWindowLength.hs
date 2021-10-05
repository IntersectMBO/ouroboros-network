{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Ouroboros.Consensus.Config.GenesisWindowLength (
    GenesisWindow (..)
  , GenesisWindowLength (..)
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

-- | A genesis window starts at a given SlotNo (maybe Origin) and always ends
-- necessarily on a SlotNo.
--
-- The lower bound is irrelevant whether or not it is inclusive or exclusive as
-- every candidate will have a block at said point. The upper bound is
-- inclusive.
data GenesisWindow = GenesisWindow {
  gwFrom :: !SlotNo,
  gwTo   :: !SlotNo
  }
-- TODO: Split dominated and non-dominated

-- | Smart constructor for GenesisWindow.
--
-- > > genesisWindowBounds (At 1) (GenesisWindowLength 3)
-- > GenesisWindow 1 4
-- >
-- > > genesisWindowBounds Origin (GenesisWindowLength 3)
-- > GenesisWindow 0 3
--
-- There will always be some point in time known as slot 0, and no blocks can
-- exist before that one. This function will be invoked when there is a fork so
-- if the fork is on a regular block, it won't be called with @Origin@. If
-- there is a fork at Genesis, then we have the following situation:
--
-- > G -- A -- B ......
-- >  \--   -- C
--
-- Then the slot 0 will be somewhere in @(G,A]@, and the window should be
-- anchored at slot 0.
genesisWindowBounds
  :: WithOrigin SlotNo
  -> GenesisWindowLength
  -> GenesisWindow
genesisWindowBounds Origin (GenesisWindowLength s) =
  GenesisWindow 0 (SlotNo s)
genesisWindowBounds (At x) (GenesisWindowLength s) =
  GenesisWindow x $ SlotNo s + x
