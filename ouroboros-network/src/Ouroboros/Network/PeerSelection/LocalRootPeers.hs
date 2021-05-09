{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}

module Ouroboros.Network.PeerSelection.LocalRootPeers (
    -- * Types
    LocalRootPeers,
    invariant,

    -- * Basic operations
    empty,
    null,
    size,
    target,
    fromGroups,
    toGroups,
    toGroupSets,
    toMap,
    keysSet,

    -- * Special operations
    clampToLimit
  ) where

import           Prelude hiding (null)

import qualified Data.Set as Set
import           Data.Set (Set)
import qualified Data.Map.Strict as Map
import           Data.Map.Strict (Map)

import           Ouroboros.Network.PeerSelection.Types


---------------------------------------
-- Local root peer set representation
--

data LocalRootPeers peeraddr =
     LocalRootPeers
       -- We use two partial & overlapping representations:

       -- The collection of all the peers, with the associated PeerAdvertise
       (Map peeraddr PeerAdvertise)

       -- The groups, but without the associated PeerAdvertise
       [(Int, Set peeraddr)]
  deriving Eq

-- It is an abstract type, so the derived Show is unhelpful, e.g. for replaying
-- test cases.
--
instance (Show peeraddr, Ord peeraddr) => Show (LocalRootPeers peeraddr) where
  show lrps = "fromGroups " ++ show (toGroups lrps)

invariant :: Ord peeraddr => LocalRootPeers peeraddr -> Bool
invariant (LocalRootPeers m gs) =

    -- The overlapping representations must be consistent
    Set.unions [ g | (_, g) <- gs ] == Map.keysSet m

    -- The localRootPeers groups must not overlap with each other
 && Map.size m == sum [ Set.size g | (_, g) <- gs ]

    -- Individual group targets must be greater than zero and achievable given
    -- the group sizes.
 && and [ 0 < t && t <= Set.size g | (t, g) <- gs ]


empty :: LocalRootPeers peeraddr
empty = LocalRootPeers Map.empty []

null :: LocalRootPeers peeraddr -> Bool
null (LocalRootPeers m _) = Map.null m

size :: LocalRootPeers peeraddr -> Int
size (LocalRootPeers m _) = Map.size m

target :: LocalRootPeers peeraddr -> Int
target (LocalRootPeers _ gs) = sum [ t | (t, _) <- gs ]

toMap :: LocalRootPeers peeraddr -> Map peeraddr PeerAdvertise
toMap (LocalRootPeers m _) = m

keysSet :: LocalRootPeers peeraddr -> Set peeraddr
keysSet (LocalRootPeers m _) = Map.keysSet m

toGroupSets :: LocalRootPeers peeraddr -> [(Int, Set peeraddr)]
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
           => [(Int, Map peeraddr PeerAdvertise)]
           -> LocalRootPeers peeraddr
fromGroups =
    (\gs -> let m'  = Map.unions [ g | (_, g) <- gs ]
                gs' = [ (t, Map.keysSet g) | (t, g) <- gs ]
             in LocalRootPeers m' gs')
  . establishStructureInvariant Set.empty
  where
    -- The groups must not overlap; have achievable targets; and be non-empty.
    establishStructureInvariant !_ [] = []
    establishStructureInvariant !acc ((t, g): gs)
      | t' > 0    = (t', g') : establishStructureInvariant acc' gs
      | otherwise =            establishStructureInvariant acc' gs
      where
        !g'   = g `Map.withoutKeys` acc
        !t'   = min t (Map.size g')
        !acc' = acc <> Map.keysSet g

-- | Inverse of 'fromGroups', for the subset of inputs to 'fromGroups' that
-- satisfy the invariant.
--
toGroups :: Ord peeraddr
         => LocalRootPeers peeraddr
         -> [(Int, Map peeraddr PeerAdvertise)]
toGroups (LocalRootPeers m gs) =
    [ (t, Map.fromSet (m Map.!) g)
    | (t, g) <- gs ]


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
             -> LocalRootPeers peeraddr
             -> LocalRootPeers peeraddr
clampToLimit totalLimit (LocalRootPeers m gs0) =
    let gs' = limitTotalSize 0 gs0
        m'  = m `Map.restrictKeys` Set.unions [ g | (_, g) <- gs' ]
     in LocalRootPeers m' gs'

  where
    limitTotalSize !_ [] = []
    limitTotalSize !n ((t, g) : gs)

        -- No space at all!
      | n == totalLimit
      = []

        -- It fits entirely!
      | let n' = n + Set.size g
      , n' <= totalLimit
      = (t, g) : limitTotalSize n' gs

        -- We can fit a bit more if we chop it up!
      | otherwise
      , let !g' = Set.take (totalLimit - n) g
            !t' = min t (Set.size g')
      = (t', g') : []
