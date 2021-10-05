{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Ouroboros.Network.AnchoredFragment.Completeness (
    AnnotatedAnchoredFragment (..)
  , FragmentCompleteness (..)
  , IsFragmentComplete (..)
  , emptyAnnotatedAnchoredFragment
  , isFragmentComplete
  ) where

import           GHC.Generics (Generic)
import           NoThunks.Class (NoThunks)

import           Ouroboros.Network.AnchoredFragment
import           Ouroboros.Network.Block
import           Cardano.Slotting.Slot

-- | BlockFetch and ChainSync operate with 'AnchoredFragment's when dealing with
-- candidate chains. For Ouroboros Genesis, we need to know whether or not the
-- fragment represents the end of the remote chain, to make judgments about
-- whether or not our perceived density is indeed the real density.
--
-- Every fragment returned by ChainSync will be coupled with one of these tags,
-- specifying if the ChainSync client thinks that the fragment is indeed
-- complete. This flag will be used by the prefix selection algorithm to filter
-- appropriate candidates to be provided to BlockFetch.
--
-- Note that completeness of a fragment is a local property, dependent on the
-- current state of the ChainSync protocol, and based on the information
-- provided by the ChainSync server.
data FragmentCompleteness = FragmentComplete | FragmentIncomplete
  deriving (Show, Generic, Eq, NoThunks)

data IsFragmentComplete =
  ServerIsLying
  | IsFragmentComplete !FragmentCompleteness

-- | Return a 'FragmentCompleteness' for the given fragment assuming the
-- provided tip is the one announced by the remote peer.
--
-- This function will return 'ServerIsLying' when the slot of the announced tip
-- is smaller than the slot of the most recent block on the fragment. This
-- should be interpreted in the ChainSync protocol as a misbehave of the server.
isFragmentComplete ::
  ( StandardHash blk
  , HasHeader blk)
  => Tip blk
  -> AnchoredFragment blk
  -> IsFragmentComplete
isFragmentComplete TipGenesis theirFrag =
  if pointHash (anchorPoint theirFrag) == GenesisHash
  then IsFragmentComplete FragmentComplete
  else ServerIsLying
isFragmentComplete (Tip s tipHash _) theirFrag =
  if headSlot theirFrag > At s
  then
    ServerIsLying
  else
  IsFragmentComplete $ case headHash theirFrag of
    GenesisHash -> FragmentIncomplete
    BlockHash theHeadHash
      | theHeadHash == tipHash -> FragmentComplete
      | otherwise              -> FragmentIncomplete

-- | A fragment annotated with its completeness.
data AnnotatedAnchoredFragment blk =
  AnnotatedAnchoredFragment {
      fragment ::     !(AnchoredFragment blk)
    , completeness :: !FragmentCompleteness
    }
  deriving (Generic, NoThunks)

emptyAnnotatedAnchoredFragment :: HasHeader blk => AnnotatedAnchoredFragment blk
emptyAnnotatedAnchoredFragment =
  AnnotatedAnchoredFragment (Empty AnchorGenesis) FragmentComplete
