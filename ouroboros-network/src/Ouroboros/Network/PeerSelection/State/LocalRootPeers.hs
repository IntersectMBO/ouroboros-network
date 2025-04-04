{-# LANGUAGE BangPatterns   #-}
{-# LANGUAGE DerivingVia    #-}
{-# LANGUAGE NamedFieldPuns #-}

module Ouroboros.Network.PeerSelection.State.LocalRootPeers
  ( -- * Types
    LocalRootPeers (..)
  , LocalRootConfig (..)
  , HotValency (..)
  , WarmValency (..)
  , Config
    -- Export constructors for defining tests.
  , invariant
    -- * Basic operations
  , mapPeers
  , empty
  , null
  , size
  , member
  , hotTarget
  , warmTarget
  , fromGroups
  , toGroups
  , toGroupSets
  , toMap
  , keysSet
  , trustableKeysSet
    -- * Special operations
  , clampToLimit
  , clampToTrustable
  , isPeerTrustable
  ) where

import Prelude hiding (null)

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set

import Cardano.Network.PeerSelection.PeerTrustable (PeerTrustable (..))
import Ouroboros.Network.NodeToNode.Version (DiffusionMode)
import Ouroboros.Network.PeerSelection.PeerAdvertise (PeerAdvertise)


---------------------------------------
-- Local root peer set representation
--

data LocalRootConfig extraFlags = LocalRootConfig {
    peerAdvertise :: !PeerAdvertise,
    diffusionMode :: !DiffusionMode,
    extraFlags    :: !extraFlags
  }
  deriving (Show, Eq)

data LocalRootPeers extraFlags peeraddr =
     LocalRootPeers
       -- We use two partial & overlapping representations:

       -- The collection of all the peers, with the associated PeerAdvertise
       -- and extra values
       (Map peeraddr (LocalRootConfig extraFlags))

       -- The groups, but without the associated PeerAdvertise and
       -- PeerTrustable values
       [(HotValency, WarmValency, Set peeraddr)]
  deriving Eq

mapPeers :: (Ord b)
         => (a -> b)
         -> LocalRootPeers extraFlags a
         -> LocalRootPeers extraFlags b
mapPeers f (LocalRootPeers m l) =
  LocalRootPeers (Map.mapKeys f m) (fmap (fmap (Set.map f)) l)

-- | Newtype wrapper representing hot valency value from local root group
-- configuration
--
newtype HotValency = HotValency { getHotValency :: Int }
  deriving (Show, Eq, Ord)
  deriving Num via Int

-- | Newtype wrapper representing warm valency value from local root group
-- configuration
--
newtype WarmValency = WarmValency { getWarmValency :: Int }
  deriving (Show, Eq, Ord)
  deriving Num via Int

-- | Data available from topology file.
--
type Config extraFlags peeraddr =
     [(HotValency, WarmValency, Map peeraddr (LocalRootConfig extraFlags))]


-- It is an abstract type, so the derived Show is unhelpful, e.g. for replaying
-- test cases.
--
instance (Show extraFlags, Show peeraddr, Ord peeraddr)
  => Show (LocalRootPeers extraFlags peeraddr) where
  show lrps = "fromGroups " ++ show (toGroups lrps)

invariant :: Ord peeraddr => LocalRootPeers extraFlags peeraddr -> Bool
invariant (LocalRootPeers m gs) =

    -- The overlapping representations must be consistent
    Set.unions [ g | (_, _, g) <- gs ] == Map.keysSet m

    -- The localRootPeers groups must not overlap with each other
 && Map.size m == sum [ Set.size g | (_, _, g) <- gs ]

    -- Individual group targets must be greater than zero and achievable given
    -- the group sizes.
    --
    -- Also the warm target needs to be greater than or equal to the hot target
 && and [   0 < h
          && getWarmValency w >= getHotValency h
          -- If warm valency is achievable, by monotonicity, hot valency also is
          && getWarmValency w <= Set.size g
       | (h, w, g) <- gs ]


empty :: LocalRootPeers extraFlags peeraddr
empty = LocalRootPeers Map.empty []

null :: LocalRootPeers extraFlags peeraddr -> Bool
null (LocalRootPeers m _) = Map.null m

size :: LocalRootPeers extraFlags peeraddr -> Int
size (LocalRootPeers m _) = Map.size m

member :: Ord peeraddr => peeraddr -> LocalRootPeers extraFlags peeraddr -> Bool
member p (LocalRootPeers m _) = Map.member p m

hotTarget :: LocalRootPeers extraFlags peeraddr -> HotValency
hotTarget (LocalRootPeers _ gs) = sum [ h | (h, _, _) <- gs ]

warmTarget :: LocalRootPeers extraFlags peeraddr -> WarmValency
warmTarget (LocalRootPeers _ gs) = sum [ w | (_, w, _) <- gs ]

toMap :: LocalRootPeers extraFlags peeraddr -> Map peeraddr (LocalRootConfig extraFlags)
toMap (LocalRootPeers m _) = m

keysSet :: LocalRootPeers extraFlags peeraddr -> Set peeraddr
keysSet (LocalRootPeers m _) = Map.keysSet m

toGroupSets :: LocalRootPeers extraFlags peeraddr -> [(HotValency, WarmValency, Set peeraddr)]
toGroupSets (LocalRootPeers _ gs) = gs


-- | The local root peers info has some invariants that are not directly
-- enforced in the types, and the config comes from an external source. Of
-- course it's good to validate that at source, but here we need to not fail
-- if we're given imperfect data.
--
-- So what we do is bash it until it is valid. We don't need to be too careful
-- about how we do it, it's ok to be brutal. We should however make sure we
-- trace a warning about dodgy config.
--
fromGroups :: Ord peeraddr
           => [(HotValency, WarmValency, Map peeraddr (LocalRootConfig extraFlags))]
           -> LocalRootPeers extraFlags peeraddr
fromGroups =
    (\gs -> let m'  = Map.unions [ g | (_, _, g) <- gs ]
                gs' = [ (h, w, Map.keysSet g) | (h, w, g) <- gs ]
             in LocalRootPeers m' gs')
  . establishStructureInvariant Set.empty
  where
    -- The groups must not overlap;
    -- have achievable targets;
    -- Hot targets need to be smaller than or equal to warm targets
    -- and be non-empty.
    establishStructureInvariant !_ [] = []
    establishStructureInvariant !acc ((h, w, g): gs)
      | w' > 0 && h' > 0 = (h', w', g') : establishStructureInvariant acc' gs
      | otherwise        =                  establishStructureInvariant acc' gs
      where
        !g'   = g `Map.withoutKeys` acc
        !w'   = min w (WarmValency (Map.size g'))
        !h'   = HotValency (getHotValency h `min` getWarmValency w')
        !acc' = acc <> Map.keysSet g

-- | Inverse of 'fromGroups', for the subset of inputs to 'fromGroups' that
-- satisfy the invariant.
--
toGroups :: Ord peeraddr
         => LocalRootPeers extraFlags peeraddr
         -> [(HotValency, WarmValency, Map peeraddr (LocalRootConfig extraFlags))]
toGroups (LocalRootPeers m gs) =
    [ (h, w, Map.fromSet (m Map.!) g)
    | (h, w, g) <- gs ]


-- | Limit the size of the root peers collection to fit within given bounds.
--
-- The governor needs to be able to do this to enforce its invariant that:
--
-- > LocalRootPeers.size localRootPeers <= targetNumberOfKnownPeers
--
-- It needs to be able to /establish/ that invariant given arbitrary
-- configuration for local root peers. It makes sense to do it this way rather
-- than just enforce that local root peers config fits the invariant because
-- the invariant depends on both the targets and the local root peers config
-- and these can both vary dynamically and independently.
--
-- It is unlikely in practice that there are so many local root peers
-- configured that it goes over this targets, so it's ok to resolve it pretty
-- arbitrarily. We just take the local roots in left to right order up to the
-- limit. So we have the property that
--
-- > LocalRootPeers.size (LocalRootPeers.clampToLimit sz lrps)
-- >  == min sz (LocalRootPeers.size lrps)
--
clampToLimit :: Ord peeraddr
             => Int -- ^ The limit on the total number of local peers
             -> LocalRootPeers extraFlags peeraddr
             -> LocalRootPeers extraFlags peeraddr
clampToLimit totalLimit (LocalRootPeers m gs0) =
    let gs' = limitTotalSize 0 gs0
        m'  = m `Map.restrictKeys` Set.unions [ g | (_, _, g) <- gs' ]
     in LocalRootPeers m' gs'

  where
    limitTotalSize !_ [] = []
    limitTotalSize !n ((h, w, g) : gs)

        -- No space at all!
      | n == totalLimit
      = []

        -- It fits entirely!
      | let n' = n + Set.size g
      , n' <= totalLimit
      = (h, w, g) : limitTotalSize n' gs

        -- We can fit a bit more if we chop it up!
      | otherwise
      , let !g' = Set.take (totalLimit - n) g
            !w' = min w (WarmValency (Set.size g'))
            !h' = HotValency (getHotValency h `min` getWarmValency w')
      = [(h', w', g')]

clampToTrustable :: Ord peeraddr
                 => LocalRootPeers PeerTrustable peeraddr
                 -> LocalRootPeers PeerTrustable peeraddr
clampToTrustable (LocalRootPeers m gs) =
  let trustedMap = Map.filter (\LocalRootConfig { extraFlags } -> case extraFlags of
                                 IsTrustable    -> True
                                 IsNotTrustable -> False
                              )
                              m
   in LocalRootPeers trustedMap (trustedGroups gs)
  where
    trustedGroups [] = []
    trustedGroups ((h, w, g):gss) =
      let trusted = Map.filter (\LocalRootConfig { extraFlags } -> case extraFlags of
                                  IsTrustable    -> True
                                  IsNotTrustable -> False
                               )
                               m
          trustedSet = Map.keysSet trusted
          trustedGroup = Set.intersection g trustedSet
          w' = min w (WarmValency (Set.size trustedGroup))
          h' = HotValency (getHotValency h `min` getWarmValency w')
       in if Set.null trustedGroup
             then trustedGroups gss
             else (h', w', trustedGroup) : trustedGroups gss

isPeerTrustable :: Ord peeraddr
                => peeraddr
                -> LocalRootPeers PeerTrustable peeraddr
                -> Bool
isPeerTrustable peeraddr lrp =
  case Map.lookup peeraddr (toMap lrp) of
    Just LocalRootConfig { extraFlags = IsTrustable }
      -> True
    _ -> False

trustableKeysSet :: LocalRootPeers PeerTrustable peeraddr
                 -> Set peeraddr
trustableKeysSet (LocalRootPeers m _) =
    Map.keysSet
  . Map.filter (\LocalRootConfig { extraFlags } ->
                 case extraFlags of
                   IsTrustable    -> True
                   IsNotTrustable -> False)
  $ m
