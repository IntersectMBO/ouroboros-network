{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Ouroboros.Network.PeerSelection.KnownPeers (
    -- * Types
    KnownPeers,
    KnownPeerInfo(..),
    invariant,

    -- * Basic container operations
    empty,
    size,
    insert,
    delete,
    toMap,

    -- * Special operations
    setCurrentTime,
    minGossipTime,
    setGossipTime,
    availableForGossip,
  ) where

import qualified Data.List as List
import qualified Data.Set as Set
import           Data.Set (Set)
import qualified Data.Map.Strict as Map
import           Data.Map.Strict (Map)
import qualified Data.OrdPSQ as PSQ
import           Data.OrdPSQ (OrdPSQ)
--import           System.Random (RandomGen(..))

import           Control.Monad.Class.MonadTime
import           Control.Exception (assert)

import           Ouroboros.Network.PeerSelection.Types


-------------------------------
-- Known peer set representation
--

data KnownPeerInfo = KnownPeerInfo {

       -- | Should we advertise this peer when other nodes send us gossip requests?
       knownPeerAdvertise :: !PeerAdvertise,

       knownPeerSource    :: !PeerSource
{-
       -- | The current number of consecutive connection attempt failures. This
       -- is reset as soon as there is a successful connection. It is used
       --
       -- This may be used as part of the policy for picking peers to forget.
       --
       knownPeerNumFailures :: !Int,

       -- | The last time there was an established connection with this peer.
       --
       -- This may be used as part of the policy for picking peers to forget.
       --
       knownPeerLastSuccess :: !Time

       knownPeerOther     :: ()
-}
     }
  deriving (Eq, Show)

-- | The set of known peers. To a first approximation it can be thought of as
-- a 'Map' from @peeraddr@ to the 'KnownPeerInfo' for each one.
--
-- It has two special features:
--
--  * It tracks which peers we are permitted to gossip with now, or for peers
--    we cannot gossip with now the time at which we would next be allowed to
--    do so.
--
--  * It tracks the subset of peers that we are happy to publish in reply to
--    gossip requests to our node. It supports random sampling from this set.
--
data KnownPeers peeraddr = KnownPeers {

       -- | All the known peers.
       --
       knownPeersByAddr             :: !(Map peeraddr KnownPeerInfo),

       -- | The subset of known peers that we would be allowed to gossip with
       -- now. This is because we have not gossiped with them recently.
       --
       knownPeersAvailableForGossip :: !(Set peeraddr),

       -- | The subset of known peers that we cannot gossip with now. It keeps
       -- track of the next time we are allowed to gossip with them.
       --
       knownPeersNextGossipTimes    :: !(OrdPSQ peeraddr Time ())
     }
  deriving Show


invariant :: Ord peeraddr => KnownPeers peeraddr -> Bool
invariant KnownPeers{..} =
       knownPeersAvailableForGossip
    <> Set.fromList (PSQ.keys knownPeersNextGossipTimes)
    == Map.keysSet knownPeersByAddr

 && Set.null
      (Set.intersection
         knownPeersAvailableForGossip
        (Set.fromList (PSQ.keys knownPeersNextGossipTimes)))

empty :: KnownPeers peeraddr
empty =
    KnownPeers {
      knownPeersByAddr             = Map.empty,
      knownPeersAvailableForGossip = Set.empty,
      knownPeersNextGossipTimes    = PSQ.empty
    }

-- | /O(1)/
toMap :: KnownPeers peeraddr -> Map peeraddr KnownPeerInfo
toMap = knownPeersByAddr

availableForGossip :: Ord peeraddr => KnownPeers peeraddr -> Map peeraddr KnownPeerInfo
availableForGossip KnownPeers {knownPeersByAddr, knownPeersAvailableForGossip} =
    Map.restrictKeys knownPeersByAddr knownPeersAvailableForGossip

insert :: Ord peeraddr
       => PeerSource
       -> (peeraddr -> PeerAdvertise) -- ^ Usually @const DoAdvertisePeer@
       -> Set peeraddr
       -> KnownPeers peeraddr
       -> KnownPeers peeraddr
insert peersource peeradvertise peeraddrs
       knownPeers@KnownPeers {
         knownPeersByAddr,
         knownPeersAvailableForGossip
       } =
    let knownPeers' = knownPeers {
          knownPeersByAddr =
              knownPeersByAddr
           <> Map.fromSet newPeerInfo peeraddrs,

          knownPeersAvailableForGossip =
              knownPeersAvailableForGossip
           <> Set.filter (`Map.notMember` knownPeersByAddr) peeraddrs
        }
    in assert (invariant knownPeers') knownPeers'
  where
    newPeerInfo peeraddr =
      KnownPeerInfo {
        knownPeerSource    = peersource,
        knownPeerAdvertise = peeradvertise peeraddr
      }

delete :: Ord peeraddr
       => Set peeraddr
       -> KnownPeers peeraddr
       -> KnownPeers peeraddr
delete peeraddrs
       knownPeers@KnownPeers {
         knownPeersByAddr,
         knownPeersAvailableForGossip,
         knownPeersNextGossipTimes
       } =
    knownPeers {
      knownPeersByAddr =
        Map.withoutKeys knownPeersByAddr peeraddrs,

      knownPeersAvailableForGossip =
        Set.difference knownPeersAvailableForGossip peeraddrs,

      knownPeersNextGossipTimes =
        List.foldl' (flip PSQ.delete) knownPeersNextGossipTimes peeraddrs
    }

size :: KnownPeers peeraddr -> Int
size = Map.size . knownPeersByAddr


setCurrentTime :: Ord peeraddr
               => Time
               -> KnownPeers peeraddr
               -> KnownPeers peeraddr
setCurrentTime now knownPeers@KnownPeers { knownPeersNextGossipTimes }
 -- Efficient check for the common case of there being nothing to do:
  | Just (_,t,_,_) <- PSQ.minView knownPeersNextGossipTimes
  , t > now
  = knownPeers

setCurrentTime now knownPeers@KnownPeers {
                     knownPeersAvailableForGossip,
                     knownPeersNextGossipTimes
                   } =
  let knownPeers' =
        knownPeers {
          knownPeersAvailableForGossip = knownPeersAvailableForGossip',
          knownPeersNextGossipTimes    = knownPeersNextGossipTimes'
        }
   in assert (invariant knownPeers') knownPeers'
  where
    (nowAvailable, knownPeersNextGossipTimes') =
      PSQ.atMostView now knownPeersNextGossipTimes

    knownPeersAvailableForGossip' =
         knownPeersAvailableForGossip
      <> Set.fromList [ peeraddr | (peeraddr, _, _) <- nowAvailable ]

-- | The first time that a peer will become available for gossip. If peers are
-- already available for gossip, or there are no known peers at all then the
-- result is @Nothing@.
--
minGossipTime :: Ord peeraddr => KnownPeers peeraddr -> Maybe Time
minGossipTime KnownPeers {
                knownPeersAvailableForGossip,
                knownPeersNextGossipTimes
              }
  | Set.null knownPeersAvailableForGossip
  , Just (_k, t, _, _psq) <- PSQ.minView knownPeersNextGossipTimes
  = Just t

  | otherwise
  = Nothing

setGossipTime :: Ord peeraddr
              => [peeraddr]
              -> Time
              -> KnownPeers peeraddr
              -> KnownPeers peeraddr
setGossipTime peeraddrs time
              knownPeers@KnownPeers {
                knownPeersByAddr,
                knownPeersAvailableForGossip,
                knownPeersNextGossipTimes
              } =
    assert (all (`Map.member` knownPeersByAddr) peeraddrs) $
    let knownPeers' = knownPeers {
          knownPeersAvailableForGossip =
                             knownPeersAvailableForGossip
            `Set.difference` Set.fromList peeraddrs,

          knownPeersNextGossipTimes =
            List.foldl' (\psq peeraddr -> PSQ.insert peeraddr time () psq)
                        knownPeersNextGossipTimes
                        peeraddrs
        }
    in assert (invariant knownPeers') knownPeers'


-- | Select a random subset of the known peers that are available to publish.
--
-- The selection is done in such a way that when the same initial PRNG state is
-- used, the selected set does not significantly vary with small perturbations
-- in the set of published peers.
--
-- The intention of this selection method is that the selection should give
-- approximately the same replies to the same peers over the course of multiple
-- requests from the same peer. This is to deliberately slow the rate at which
-- peers can discover and map out the entire network.
--
{-
samplePublishedPeers :: RandomGen prng
                     => KnownPeers peeraddr
                     -> prng
                     -> Int
                     -> [peeraddr]
samplePublishedPeers _ _ _ = []
-- idea is to generate a sequence of random numbers and map them to locations
-- in a relatively stable way, that's mostly insensitive to additions or
-- deletions
-}

