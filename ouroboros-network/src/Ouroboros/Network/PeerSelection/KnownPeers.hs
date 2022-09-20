{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Ouroboros.Network.PeerSelection.KnownPeers
  ( -- * Types
    KnownPeers
  , invariant
    -- * Basic container operations
  , empty
  , size
  , insert
  , delete
  , toSet
  , member
    -- * Special operations
  , setCurrentTime
  , incrementFailCount
  , resetFailCount
  , lookupFailCount
  , lookupTepidFlag
  , setTepidFlag
  , clearTepidFlag
    -- ** Tracking when we can gossip
  , minGossipTime
  , setGossipTime
  , availableForGossip
    -- ** Tracking when we can (re)connect
  , minConnectTime
  , setConnectTimes
  , availableToConnect
  ) where

import qualified Data.List as List
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.OrdPSQ (OrdPSQ)
import qualified Data.OrdPSQ as PSQ
import           Data.Semigroup (Min (..))
import           Data.Set (Set)
import qualified Data.Set as Set
--import           System.Random (RandomGen(..))

import           Control.Exception (assert)
import           Control.Monad.Class.MonadTime

import           Ouroboros.Network.PeerSelection.LedgerPeers (IsLedgerPeer (..))
import           Ouroboros.Network.PeerSelection.PeerAdvertise.Type
                     (PeerAdvertise)
import           Ouroboros.Network.PeerSelection.PeerSharing.Type (PeerSharing,
                     combinePeerInformation)


-------------------------------
-- Known peer set representation
--

-- | The set of known peers. To a first approximation it can be thought of as
-- a 'Set' of @peeraddr@.
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
       allPeers           :: !(Map peeraddr KnownPeerInfo),

       -- | The subset of known peers that we would be allowed to gossip with
       -- now. This is because we have not gossiped with them recently.
       --
       availableForGossip :: !(Set peeraddr),

       -- | The subset of known peers that we cannot gossip with now. It keeps
       -- track of the next time we are allowed to gossip with them.
       --
       nextGossipTimes    :: !(OrdPSQ peeraddr Time ()),

       -- | The subset of known peers that we would be allowed to try to
       -- establish a connection to now. This is because we have not connected
       -- with them before or because any failure backoff time has expired.
       --
       availableToConnect :: !(Set peeraddr),

       -- | The subset of known peers that we cannot connect to for the moment.
       -- It keeps track of the next time we are allowed to make the next
       -- connection attempt.
       nextConnectTimes   :: !(OrdPSQ peeraddr Time ())
     }
  deriving Show

data KnownPeerInfo = KnownPeerInfo {
       -- | The current number of consecutive connection attempt failures. This
       -- is reset as soon as there is a successful connection.
       --
       -- It is used to implement the exponential backoff strategy and may also
       -- be used by policies to select peers to forget.
       --
       knownPeerFailCount :: !Int,

       -- | Indicates if the peer was hot but then got demoted.
       --
       -- It is set on the hot to warm promotion and reset on cold to warm,
       -- thus it can be present for warm or cold peers.  It's purpose is to
       -- provide information to demotion policies.
       --
       -- It is also used as useful information for the Peer Selection Governor
       -- when deciding which peers to share when Peer Sharing.
       --
       knownPeerTepid     :: !Bool,

       -- | Indicates current remote Peer Willingness information.
       --
       -- If a connection with this address hasn't been established we won't
       -- have any information about this particular flag
       --
       -- It is used by the Peer Sharing logic to decide if we should share/ask
       -- about/to this peer's address to others.
       knownPeerSharing   :: !PeerSharing,

       -- | Indicates current local Peer Willingness information.
       --
       -- If this address didn't come from a local configuration then this
       -- value is set to 'DoAdvertise' by default.
       --
       -- It is used by the Peer Sharing logic to decide if we should share
       -- about this peer's address to others.
       knownPeerAdvertise :: !PeerAdvertise,

       -- | Indicates if peer came from ledger.
       --
       -- It is used so we can filter out the ledger Peers from a Peer Sharing
       -- reply, since ledger peers are not particularly what one is looking for
       -- in a Peer Sharing reply.
       --
       knownLedgerPeer    :: !IsLedgerPeer
     }
  deriving (Eq, Show)


invariant :: Ord peeraddr => KnownPeers peeraddr -> Bool
invariant KnownPeers{..} =
       -- The combo of the gossip set + psq = the whole set of peers
       availableForGossip
    <> Set.fromList (PSQ.keys nextGossipTimes)
    == Map.keysSet allPeers

       -- The gossip set and psq do not overlap
 && Set.null
      (Set.intersection
         availableForGossip
        (Set.fromList (PSQ.keys nextGossipTimes)))

       -- The combo of the connect set + psq = the whole set of peers
 &&    availableToConnect
    <> Set.fromList (PSQ.keys nextConnectTimes)
    == Map.keysSet allPeers

       -- The connect set and psq do not overlap
 && Set.null
      (Set.intersection
         availableToConnect
        (Set.fromList (PSQ.keys nextConnectTimes)))


-------------------------------
-- Basic container operations
--

empty :: KnownPeers peeraddr
empty =
    KnownPeers {
      allPeers           = Map.empty,
      availableForGossip = Set.empty,
      nextGossipTimes    = PSQ.empty,
      availableToConnect = Set.empty,
      nextConnectTimes   = PSQ.empty
    }

size :: KnownPeers peeraddr -> Int
size = Map.size . allPeers

-- | /O(n)/
toSet :: KnownPeers peeraddr -> Set peeraddr
toSet = Map.keysSet . allPeers

member :: Ord peeraddr
       => peeraddr
       -> KnownPeers peeraddr
       -> Bool
member peeraddr KnownPeers {allPeers} =
    peeraddr `Map.member` allPeers

insert :: Ord peeraddr
       => Map peeraddr (PeerSharing, PeerAdvertise, IsLedgerPeer)
       -> KnownPeers peeraddr
       -> KnownPeers peeraddr
insert peeraddrs
       knownPeers@KnownPeers {
         allPeers,
         availableForGossip,
         availableToConnect
       } =
    let allPeersAddrs = Map.keysSet peeraddrs
        knownPeers' = knownPeers {
          allPeers =
              let (<+>) = Map.unionWith mergePeerInfo in
              allPeers
          <+> Map.map newPeerInfo peeraddrs,

          -- The sets tracking peers ready for gossip or to connect to need to
          -- be updated with any /fresh/ peers, but any already present are
          -- ignored since they are either already in these sets or they are in
          -- the corresponding PSQs, for which we also preserve existing info.
          availableForGossip =
              availableForGossip
           <> Set.filter (`Map.notMember` allPeers) allPeersAddrs,

          availableToConnect =
              availableToConnect
           <> Set.filter (`Map.notMember` allPeers) allPeersAddrs
        }
    in assert (invariant knownPeers') knownPeers'
  where
    newPeerInfo (ps, pa, lp) =
      KnownPeerInfo {
        knownPeerFailCount = 0
      , knownPeerTepid     = False
      , knownPeerSharing   = ps
      , knownPeerAdvertise = pa
      , knownLedgerPeer    = lp
      }
    mergePeerInfo old new =
      KnownPeerInfo {
        knownPeerFailCount = knownPeerFailCount old
      , knownPeerTepid     = knownPeerTepid old
      -- It might be the case we are updating a peer's particular willingness
      -- flags or we just learned this peer comes from ledger.
      , knownPeerSharing   = combinePeerInformation (knownPeerSharing new)
                                                    (knownPeerAdvertise new)
      , knownPeerAdvertise = knownPeerAdvertise new
      -- Preserve Ledger Peer information if the peer is ledger.
      , knownLedgerPeer    = case (knownLedgerPeer old) of
                               IsLedgerPeer    -> IsLedgerPeer
                               IsNotLedgerPeer -> knownLedgerPeer new
      }

delete :: Ord peeraddr
       => Set peeraddr
       -> KnownPeers peeraddr
       -> KnownPeers peeraddr
delete peeraddrs
       knownPeers@KnownPeers {
         allPeers,
         availableForGossip,
         nextGossipTimes,
         availableToConnect,
         nextConnectTimes
       } =
    knownPeers {
      allPeers =
        Map.withoutKeys allPeers peeraddrs,

      availableForGossip =
        Set.difference availableForGossip peeraddrs,

      nextGossipTimes =
        List.foldl' (flip PSQ.delete) nextGossipTimes peeraddrs,

      availableToConnect =
        Set.difference availableToConnect peeraddrs,

      nextConnectTimes =
        List.foldl' (flip PSQ.delete) nextConnectTimes peeraddrs
    }


-------------------------------
-- Special operations
--

setCurrentTime :: Ord peeraddr
               => Time
               -> KnownPeers peeraddr
               -> KnownPeers peeraddr
setCurrentTime now knownPeers@KnownPeers { nextGossipTimes, nextConnectTimes }
 -- Efficient check for the common case of there being nothing to do:
  | Just (Min t) <- (f <$> PSQ.minView nextGossipTimes)
                 <> (f <$> PSQ.minView nextConnectTimes)
  , t > now
  = knownPeers
  where
    f (_,t,_,_) = Min t

setCurrentTime now knownPeers@KnownPeers {
                     availableForGossip,
                     nextGossipTimes,
                     availableToConnect,
                     nextConnectTimes
                   } =
  let knownPeers' =
        knownPeers {
          availableForGossip = availableForGossip',
          nextGossipTimes    = nextGossipTimes',
          availableToConnect = availableToConnect',
          nextConnectTimes   = nextConnectTimes'
        }
   in assert (invariant knownPeers') knownPeers'
  where
    (nowAvailableForGossip, nextGossipTimes') =
      PSQ.atMostView now nextGossipTimes

    availableForGossip' =
         availableForGossip
      <> Set.fromList [ peeraddr | (peeraddr, _, _) <- nowAvailableForGossip ]

    (nowAvailableToConnect, nextConnectTimes') =
      PSQ.atMostView now nextConnectTimes

    availableToConnect' =
         availableToConnect
      <> Set.fromList [ peeraddr | (peeraddr, _, _) <- nowAvailableToConnect ]


incrementFailCount :: Ord peeraddr
                   => peeraddr
                   -> KnownPeers peeraddr
                   -> (Int, KnownPeers peeraddr)
incrementFailCount peeraddr knownPeers@KnownPeers{allPeers} =
    assert (peeraddr `Map.member` allPeers) $
    let allPeers' = Map.update (Just . incr) peeraddr allPeers
    in ( -- since the `peeraddr` is assumed to be part of `allPeers` the `Map.!`
         -- is safe
         knownPeerFailCount (allPeers' Map.! peeraddr)
       , knownPeers { allPeers = allPeers' }
       )
  where
    incr kpi = kpi { knownPeerFailCount = knownPeerFailCount kpi + 1 }


resetFailCount :: Ord peeraddr
               => peeraddr
               -> KnownPeers peeraddr
               -> KnownPeers peeraddr
resetFailCount peeraddr knownPeers@KnownPeers{allPeers} =
    assert (peeraddr `Map.member` allPeers) $
    knownPeers { allPeers = Map.update (\kpi  -> Just kpi { knownPeerFailCount = 0 })
                              peeraddr allPeers
               }

lookupFailCount :: Ord peeraddr
                => peeraddr
                -> KnownPeers peeraddr
                -> Maybe Int
lookupFailCount peeraddr KnownPeers{allPeers} =
    knownPeerFailCount <$> Map.lookup peeraddr allPeers


lookupTepidFlag :: Ord peeraddr
                => peeraddr
                -> KnownPeers peeraddr
                -> Maybe Bool
lookupTepidFlag peeraddr KnownPeers{allPeers} =
    knownPeerTepid <$> Map.lookup peeraddr allPeers

setTepidFlag' :: Ord peeraddr
             => Bool
             -> peeraddr
             -> KnownPeers peeraddr
             -> KnownPeers peeraddr
setTepidFlag' val peeraddr knownPeers@KnownPeers{allPeers} =
    assert (peeraddr `Map.member` allPeers) $
    knownPeers { allPeers = Map.update (\kpi  -> Just kpi { knownPeerTepid = val })
                              peeraddr allPeers
               }

clearTepidFlag :: Ord peeraddr
             => peeraddr
             -> KnownPeers peeraddr
             -> KnownPeers peeraddr
clearTepidFlag = setTepidFlag' False

setTepidFlag :: Ord peeraddr
             => peeraddr
             -> KnownPeers peeraddr
             -> KnownPeers peeraddr
setTepidFlag = setTepidFlag' True

-------------------------------
-- Tracking when we can gossip
--

-- | The first time that a peer will become available for gossip. If peers are
-- already available for gossip, or there are no known peers at all then the
-- result is @Nothing@.
--
minGossipTime :: Ord peeraddr => KnownPeers peeraddr -> Maybe Time
minGossipTime KnownPeers {
                availableForGossip,
                nextGossipTimes
              }
  | Set.null availableForGossip
  , Just (_k, t, _, _psq) <- PSQ.minView nextGossipTimes
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
                allPeers,
                availableForGossip,
                nextGossipTimes
              } =
    assert (all (`Map.member` allPeers) peeraddrs) $
    let knownPeers' = knownPeers {
          availableForGossip =
                   availableForGossip
            Set.\\ peeraddrs,

          nextGossipTimes =
            List.foldl' (\psq peeraddr -> PSQ.insert peeraddr time () psq)
                        nextGossipTimes
                        peeraddrs
        }
    in assert (invariant knownPeers') knownPeers'


-----------------------------------
-- Tracking when we can (re)connect
--

minConnectTime :: Ord peeraddr
               => KnownPeers peeraddr
               -> Maybe Time
minConnectTime KnownPeers { nextConnectTimes }
  | Just (_k, t, _, _psq) <- PSQ.minView nextConnectTimes
  = Just t

  | otherwise
  = Nothing


setConnectTimes :: Ord peeraddr
                => Map peeraddr Time --TODO: make this a single entry
                -> KnownPeers peeraddr
                -> KnownPeers peeraddr
setConnectTimes times
                knownPeers@KnownPeers {
                  allPeers,
                  availableToConnect,
                  nextConnectTimes
                } =
    assert (all (`Map.member` allPeers) (Map.keysSet times)) $
    let knownPeers' = knownPeers {
          availableToConnect =
                   availableToConnect
            Set.\\ Map.keysSet times,

          nextConnectTimes =
            Map.foldlWithKey' (\psq peeraddr time -> PSQ.insert peeraddr time () psq)
                              nextConnectTimes
                              times
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
