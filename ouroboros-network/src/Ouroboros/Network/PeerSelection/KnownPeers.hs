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
    -- ** Tracking when we can gossip
    minGossipTime,
    setGossipTime,
    availableForGossip,
    -- ** Tracking when we can (re)connect
    minReconnectTime,
    setReconnectTime,
    availableToReconnect,
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

       knownPeerSource    :: !PeerSource,

       -- | The current number of consecutive connection attempt failures. This
       -- is reset as soon as there is a successful connection.
       --
       -- It is used to implement the exponential backoff strategy and may also
       -- be used by policies to select peers to forget.
       --
       knownPeerFailCount :: !Int
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
       knownPeersNextGossipTimes    :: !(OrdPSQ peeraddr Time ()),

       -- | The subset of known peers that we would be allowed to try to
       -- establish a connection to now. This is because we have not connected
       -- with them before or because any failure backoff time has expired.
       --
       knownPeersAvailableToConnect :: !(Set peeraddr),

       -- | The subset of known peers that we cannot connect to for the moment.
       -- It keeps track of the next time we are allowed to make the next
       -- connection attempt.
       knownPeersNextConnectTimes   :: !(OrdPSQ peeraddr Time ())
     }
  deriving Show


invariant :: Ord peeraddr => KnownPeers peeraddr -> Bool
invariant KnownPeers{..} =
       -- The combo of the gossip set + psq = the whole set of peers
       knownPeersAvailableForGossip
    <> Set.fromList (PSQ.keys knownPeersNextGossipTimes)
    == Map.keysSet knownPeersByAddr

       -- The gossip set and psq do not overlap
 && Set.null
      (Set.intersection
         knownPeersAvailableForGossip
        (Set.fromList (PSQ.keys knownPeersNextGossipTimes)))

       -- The combo of the connect set + psq = the whole set of peers
 &&    knownPeersAvailableToConnect
    <> Set.fromList (PSQ.keys knownPeersNextConnectTimes)
    == Map.keysSet knownPeersByAddr

       -- The connect set and psq do not overlap
 && Set.null
      (Set.intersection
         knownPeersAvailableToConnect
        (Set.fromList (PSQ.keys knownPeersNextConnectTimes)))


-------------------------------
-- Basic container operations
--

empty :: KnownPeers peeraddr
empty =
    KnownPeers {
      knownPeersByAddr             = Map.empty,
      knownPeersAvailableForGossip = Set.empty,
      knownPeersNextGossipTimes    = PSQ.empty,
      knownPeersAvailableToConnect = Set.empty,
      knownPeersNextConnectTimes   = PSQ.empty
    }

size :: KnownPeers peeraddr -> Int
size = Map.size . knownPeersByAddr

-- | /O(1)/
toMap :: KnownPeers peeraddr -> Map peeraddr KnownPeerInfo
toMap = knownPeersByAddr

insert :: Ord peeraddr
       => PeerSource
       -> (peeraddr -> PeerAdvertise) -- ^ Usually @const DoAdvertisePeer@
       -> Set peeraddr
       -> KnownPeers peeraddr
       -> KnownPeers peeraddr
insert peersource peeradvertise peeraddrs
       knownPeers@KnownPeers {
         knownPeersByAddr,
         knownPeersAvailableForGossip,
         knownPeersAvailableToConnect
       } =
    let knownPeers' = knownPeers {
          knownPeersByAddr =
              let (<+>) = Map.unionWith mergePeerInfo in
              knownPeersByAddr
          <+> Map.fromSet newPeerInfo peeraddrs,

          -- The sets tracking peers ready for gossip or to connect to need to
          -- be updated with any /fresh/ peers, but any already present are
          -- ignored since they are either already in these sets or they are in
          -- the corresponding PSQs, for which we also preserve existing info.
          knownPeersAvailableForGossip =
              knownPeersAvailableForGossip
           <> Set.filter (`Map.notMember` knownPeersByAddr) peeraddrs,

          knownPeersAvailableToConnect =
              knownPeersAvailableToConnect
           <> Set.filter (`Map.notMember` knownPeersByAddr) peeraddrs
        }
    in assert (invariant knownPeers') knownPeers'
  where
    newPeerInfo peeraddr =
      KnownPeerInfo {
        knownPeerSource    = peersource,
        knownPeerAdvertise = peeradvertise peeraddr,
        knownPeerFailCount = 0
      }
    mergePeerInfo KnownPeerInfo {knownPeerFailCount}
                  KnownPeerInfo {knownPeerSource, knownPeerAdvertise} =
      KnownPeerInfo {
        knownPeerSource,
        knownPeerAdvertise,
        knownPeerFailCount
      }

delete :: Ord peeraddr
       => Set peeraddr
       -> KnownPeers peeraddr
       -> KnownPeers peeraddr
delete peeraddrs
       knownPeers@KnownPeers {
         knownPeersByAddr,
         knownPeersAvailableForGossip,
         knownPeersNextGossipTimes,
         knownPeersAvailableToConnect,
         knownPeersNextConnectTimes
       } =
    knownPeers {
      knownPeersByAddr =
        Map.withoutKeys knownPeersByAddr peeraddrs,

      knownPeersAvailableForGossip =
        Set.difference knownPeersAvailableForGossip peeraddrs,

      knownPeersNextGossipTimes =
        List.foldl' (flip PSQ.delete) knownPeersNextGossipTimes peeraddrs,

      knownPeersAvailableToConnect =
        Set.difference knownPeersAvailableToConnect peeraddrs,

      knownPeersNextConnectTimes =
        List.foldl' (flip PSQ.delete) knownPeersNextConnectTimes peeraddrs
    }


-------------------------------
-- Special operations
--

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
                     knownPeersNextGossipTimes,
                     knownPeersAvailableToConnect,
                     knownPeersNextConnectTimes
                   } =
  let knownPeers' =
        knownPeers {
          knownPeersAvailableForGossip = knownPeersAvailableForGossip',
          knownPeersNextGossipTimes    = knownPeersNextGossipTimes',
          knownPeersAvailableToConnect = knownPeersAvailableToConnect',
          knownPeersNextConnectTimes   = knownPeersNextConnectTimes'
        }
   in assert (invariant knownPeers') knownPeers'
  where
    (nowAvailableForGossip, knownPeersNextGossipTimes') =
      PSQ.atMostView now knownPeersNextGossipTimes

    knownPeersAvailableForGossip' =
         knownPeersAvailableForGossip
      <> Set.fromList [ peeraddr | (peeraddr, _, _) <- nowAvailableForGossip ]

    (nowAvailableToConnect, knownPeersNextConnectTimes') =
      PSQ.atMostView now knownPeersNextConnectTimes

    knownPeersAvailableToConnect' =
         knownPeersAvailableToConnect
      <> Set.fromList [ peeraddr | (peeraddr, _, _) <- nowAvailableToConnect ]


-------------------------------
-- Tracking when we can gossip
--

availableForGossip :: Ord peeraddr
                   => KnownPeers peeraddr
                   -> Map peeraddr KnownPeerInfo
availableForGossip KnownPeers {
                     knownPeersByAddr,
                     knownPeersAvailableForGossip
                   } =
    knownPeersByAddr `Map.restrictKeys` knownPeersAvailableForGossip

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
              => Set peeraddr
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
            Set.\\ peeraddrs,

          knownPeersNextGossipTimes =
            List.foldl' (\psq peeraddr -> PSQ.insert peeraddr time () psq)
                        knownPeersNextGossipTimes
                        peeraddrs
        }
    in assert (invariant knownPeers') knownPeers'


-----------------------------------
-- Tracking when we can reconnect
--

availableToReconnect :: Ord peeraddr
                     => KnownPeers peeraddr
                     -> Map peeraddr KnownPeerInfo
availableToReconnect KnownPeers {
                       knownPeersByAddr,
                       knownPeersAvailableToConnect
                     } =
    knownPeersByAddr `Map.restrictKeys` knownPeersAvailableToConnect


minReconnectTime :: Ord peeraddr => KnownPeers peeraddr -> Maybe Time
minReconnectTime KnownPeers {
                   knownPeersAvailableToConnect,
                   knownPeersNextConnectTimes
                 }
  | Set.null knownPeersAvailableToConnect
  , Just (_k, t, _, _psq) <- PSQ.minView knownPeersNextConnectTimes
  = Just t

  | otherwise
  = Nothing


setReconnectTime :: Ord peeraddr
                 => Set peeraddr
                 -> Time
                 -> KnownPeers peeraddr
                 -> KnownPeers peeraddr
setReconnectTime peeraddrs time
                 knownPeers@KnownPeers {
                   knownPeersByAddr,
                   knownPeersAvailableToConnect,
                   knownPeersNextConnectTimes
                 } =
    assert (all (`Map.member` knownPeersByAddr) peeraddrs) $
    let knownPeers' = knownPeers {
          knownPeersAvailableForGossip =
                   knownPeersAvailableToConnect
            Set.\\ peeraddrs,

          knownPeersNextGossipTimes =
            List.foldl' (\psq peeraddr -> PSQ.insert peeraddr time () psq)
                        knownPeersNextConnectTimes
                        peeraddrs
        }
    in assert (invariant knownPeers') knownPeers'


---------------------------------
-- Selecting peers to advertise
--

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
sampleAdvertisedPeers :: RandomGen prng
                      => KnownPeers peeraddr
                      -> prng
                      -> Int
                      -> [peeraddr]
sampleAdvertisedPeers _ _ _ = []
-- idea is to generate a sequence of random numbers and map them to locations
-- in a relatively stable way, that's mostly insensitive to additions or
-- deletions
-}

