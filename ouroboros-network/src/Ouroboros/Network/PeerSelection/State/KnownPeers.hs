{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Ouroboros.Network.PeerSelection.State.KnownPeers
  ( -- * Types
    KnownPeers
  , KnownPeerInfo (..)
  , invariant
  , allPeers
    -- * KnownPeerInfo operations
  , alterKnownPeerInfo
    -- * Basic container operations
  , empty
  , size
  , insert
  , alter
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
  , setSuccessfulConnectionFlag
    -- ** Tracking when we can (re)connect
  , minConnectTime
  , setConnectTimes
  , availableToConnect
    -- ** Selecting peers to ask
  , canPeerShareRequest
  , getPeerSharingRequestPeers
    -- * Selecting peers to share
  , canSharePeers
  , getPeerSharingResponsePeers
    -- ** Filtering ledger peers
  , isKnownLedgerPeer
  ) where

import qualified Data.List as List
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.OrdPSQ (OrdPSQ)
import qualified Data.OrdPSQ as PSQ
import           Data.Set (Set)
import qualified Data.Set as Set
--import           System.Random (RandomGen(..))

import           Control.Exception (assert)
import           Control.Monad.Class.MonadTime.SI

import           Data.Maybe (fromMaybe)
import           Ouroboros.Network.PeerSelection.LedgerPeers (IsLedgerPeer (..))
import           Ouroboros.Network.PeerSelection.PeerAdvertise
                     (PeerAdvertise (..))
import           Ouroboros.Network.PeerSelection.PeerSharing (PeerSharing (..))


-------------------------------
-- Known peer set representation
--

-- | The set of known peers. To a first approximation it can be thought of as
-- a 'Set' of @peeraddr@.
--
-- It has one special feature:
--
--  * It tracks the subset of peers that we are happy to publish in reply to
--    peer share requests to our node. It supports random sampling from this set.
--
data KnownPeers peeraddr = KnownPeers {

       -- | All the known peers.
       --
       allPeers           :: !(Map peeraddr KnownPeerInfo),

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
  deriving (Eq, Show)

data KnownPeerInfo = KnownPeerInfo {
       -- | The current number of consecutive connection attempt failures. This
       -- is reset as soon as there is a successful connection.
       --
       -- It is used to implement the exponential backoff strategy and may also
       -- be used by policies to select peers to forget.
       --
       knownPeerFailCount        :: !Int,

       -- | Indicates if the peer was hot but then got demoted.
       --
       -- It is set on the hot to warm promotion and reset on cold to warm,
       -- thus it can be present for warm or cold peers.  It's purpose is to
       -- provide information to demotion policies.
       --
       -- It is also used as useful information for the Peer Selection Governor
       -- when deciding which peers to share when Peer Sharing.
       --
       knownPeerTepid            :: !Bool,

       -- | Indicates current remote Peer Willingness information.
       --
       -- If a connection with this address hasn't been established we won't
       -- have any information about this particular flag
       --
       -- It is used by the Peer Sharing logic to decide if we should share/ask
       -- about/to this peer's address to others.
       knownPeerSharing          :: !PeerSharing,

       -- | Indicates current local Peer Willingness information.
       --
       -- If this address didn't come from a local configuration then this
       -- value is set to 'DoAdvertise' by default.
       --
       -- It is used by the Peer Sharing logic to decide if we should share
       -- about this peer's address to others.
       knownPeerAdvertise        :: !PeerAdvertise,

       -- | Indicates if peer came from ledger.
       --
       -- It is used so we can filter out the ledger Peers from a Peer Sharing
       -- reply, since ledger peers are not particularly what one is looking for
       -- in a Peer Sharing reply.
       --
       knownLedgerPeer           :: !IsLedgerPeer,

       -- | Indicates if the node managed to connect to the peer at some point
       -- in time.
       --
       -- This differs from the tepid flag in a way that this flag will be
       -- set/enabled if we established a successful connection with this
       -- peer. It won't be unset after this.
       --
       knownSuccessfulConnection :: !Bool
     }
  deriving (Eq, Show)


invariant :: Ord peeraddr => KnownPeers peeraddr -> Bool
invariant KnownPeers{..} =
       -- The combo of the connect set + psq = the whole set of peers
       availableToConnect
    <> Set.fromList (PSQ.keys nextConnectTimes)
    == Map.keysSet allPeers

       -- The connect set and psq do not overlap
 && Set.null
      (Set.intersection
         availableToConnect
        (Set.fromList (PSQ.keys nextConnectTimes)))


-------------------------------
-- KnownPeerInfo manipulation
--

alterKnownPeerInfo
  :: (Maybe PeerSharing, Maybe PeerAdvertise, Maybe IsLedgerPeer)
  -> Maybe KnownPeerInfo
  -> Maybe KnownPeerInfo
alterKnownPeerInfo (peerSharing, peerAdvertise, ledgerPeers) peerLookupResult =
  case peerLookupResult of
    Nothing -> Just $
      KnownPeerInfo {
        knownPeerFailCount = 0
      , knownPeerTepid     = False
      , knownPeerSharing   = fromMaybe PeerSharingDisabled peerSharing
      , knownPeerAdvertise = fromMaybe DoNotAdvertisePeer peerAdvertise
      , knownLedgerPeer    = fromMaybe IsNotLedgerPeer ledgerPeers
      , knownSuccessfulConnection = False
      }
    Just kpi -> Just $
      kpi {
        knownPeerSharing   = fromMaybe (knownPeerSharing kpi) peerSharing
      , knownPeerAdvertise = fromMaybe (knownPeerAdvertise kpi) peerAdvertise
      , knownLedgerPeer    = fromMaybe (knownLedgerPeer kpi) ledgerPeers
      }

-------------------------------
-- Basic container operations
--

empty :: KnownPeers peeraddr
empty =
    KnownPeers {
      allPeers           = Map.empty,
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

-- | This inserts a map of peers with its respective peer sharing, peer
-- advertise and ledger flags into the known peers set.
--
-- Please note that if in the map there's an entry for a peer already present
-- in the known peers set, then its values will only be overwritten if they
-- are a 'Just'. Otherwise the current information will be preserved. On the
-- other hand if there's an entry for a peer that isn't a member of the known
-- peer set, the 'Nothing' values will default to 'NoPeerSharing',
-- 'DoNotAdvertisePeer' and 'IsNotLedgerPeer', respectively, unless a 'Just'
-- value is used.
--
insert :: Ord peeraddr
       => Map peeraddr (Maybe PeerSharing, Maybe PeerAdvertise, Maybe IsLedgerPeer)
       -> KnownPeers peeraddr
       -> KnownPeers peeraddr
insert peeraddrs
       knownPeers@KnownPeers {
         allPeers,
         availableToConnect
       } =
    let allPeersAddrs = Map.keysSet peeraddrs
        knownPeers' = knownPeers {
          allPeers = Map.foldlWithKey' (\m peer v -> Map.alter (alterKnownPeerInfo v) peer m)
                                       allPeers
                                       peeraddrs,
          availableToConnect =
              availableToConnect
           <> Set.filter (`Map.notMember` allPeers) allPeersAddrs
        }
    in assert (invariant knownPeers') knownPeers'

alter :: Ord peeraddr
      => (Maybe KnownPeerInfo -> Maybe KnownPeerInfo)
      -> Set peeraddr
      -> KnownPeers peeraddr
      -> KnownPeers peeraddr
alter f ks knownPeers@KnownPeers {
            allPeers = allPeers
          , availableToConnect = availableToConnect
          , nextConnectTimes
          } =
  let newAllPeers =
        Set.foldl' (\acc k -> Map.alter f k acc)
                   allPeers
                   ks
      deletedPeers =
        Set.filter (`Map.notMember` newAllPeers) ks
      newAvailableToConnect =
        (availableToConnect <> ks)
        `Set.difference`
        deletedPeers
      newNextConnectTimes =
        Set.foldl' (flip PSQ.delete) nextConnectTimes ks
   in knownPeers {
        allPeers           = newAllPeers
      , availableToConnect = newAvailableToConnect
      , nextConnectTimes   = newNextConnectTimes
      }

delete :: Ord peeraddr
       => Set peeraddr
       -> KnownPeers peeraddr
       -> KnownPeers peeraddr
delete peeraddrs
       knownPeers@KnownPeers {
         allPeers,
         availableToConnect,
         nextConnectTimes
       } =
    knownPeers {
      allPeers =
        Map.withoutKeys allPeers peeraddrs,

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
setCurrentTime now knownPeers@KnownPeers {
                     availableToConnect,
                     nextConnectTimes
                   } =
  let knownPeers' =
        knownPeers {
          availableToConnect = availableToConnect',
          nextConnectTimes   = nextConnectTimes'
        }
   in assert (invariant knownPeers') knownPeers'
  where
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

setSuccessfulConnectionFlag :: Ord peeraddr
                             => peeraddr
                             -> KnownPeers peeraddr
                             -> KnownPeers peeraddr
setSuccessfulConnectionFlag peeraddr knownPeers@KnownPeers{allPeers} =
    assert (peeraddr `Map.member` allPeers) $
    knownPeers { allPeers = Map.update (\kpi  -> Just kpi { knownSuccessfulConnection = True })
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

-----------------------------------
-- Tracking when we can (re)connect
--

minConnectTime :: Ord peeraddr
               => KnownPeers peeraddr
               -> (peeraddr -> Bool)
               -- ^ a predicate which describes the peers to take into account
               -> Maybe Time
minConnectTime KnownPeers { nextConnectTimes } fn =
    go nextConnectTimes
  where
    go psq = case PSQ.minView psq of
      Just (k, t, _, psq') | fn k      -> Just t
                           | otherwise -> go psq'
      Nothing                          -> Nothing


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
-- Selecting peers to ask
--

-- Only make Peer Share requests to peers which wish to participate in
-- PeerSharing, i.e. have non-'NoPeerSharing' 'PeerSharing' values.
--
canPeerShareRequest :: Ord peeraddr => peeraddr -> KnownPeers peeraddr -> Bool
canPeerShareRequest pa KnownPeers { allPeers } =
  case Map.lookup pa allPeers of
    Just KnownPeerInfo
          { knownPeerSharing = PeerSharingEnabled
          } -> True
    _     -> False

-- Only share peers which are allowed to be advertised, i.e. have
-- 'DoAdvertisePeer' 'PeerAdvertise' values.
--
canSharePeers :: Ord peeraddr => peeraddr -> KnownPeers peeraddr -> Bool
canSharePeers pa KnownPeers { allPeers } =
  case Map.lookup pa allPeers of
    Just KnownPeerInfo
          { knownPeerAdvertise        = DoAdvertisePeer
          , knownSuccessfulConnection = True
          } -> True
    _     -> False

-- | Filter peers available for Peer Sharing requests, according to their
-- 'PeerSharing' information
--
getPeerSharingRequestPeers :: Ord peeraddr
                           => Set peeraddr
                           -> KnownPeers peeraddr
                           -> Set peeraddr
getPeerSharingRequestPeers availableForPeerShare knownPeers =
  Set.filter (`canPeerShareRequest` knownPeers) availableForPeerShare

-- | Filter peers available for Peer Sharing replies, according to their
-- 'PeerAdvertise' information
--
getPeerSharingResponsePeers :: KnownPeers peeraddr
                            -> Set peeraddr
getPeerSharingResponsePeers knownPeers =
    Map.keysSet
  $ Map.filter (\case
                  KnownPeerInfo
                    { knownPeerAdvertise        = DoAdvertisePeer
                    , knownSuccessfulConnection = True
                    } -> True
                  _   -> False
               )
  $ allPeers knownPeers


---------------------------------
-- Filter ledger peers
--

-- | Checks the KnownPeers Set for known ledger peers.
--
-- This is used in Peer Selection Governor to filter out the known-to-te ledger
-- peers from the share result set.
--
isKnownLedgerPeer :: Ord peeraddr => peeraddr -> KnownPeers peeraddr -> Bool
isKnownLedgerPeer peeraddr KnownPeers { allPeers } =
  case Map.lookup peeraddr allPeers of
    Just KnownPeerInfo { knownLedgerPeer } ->
      case knownLedgerPeer of
        IsLedgerPeer    -> True
        IsNotLedgerPeer -> False
    Nothing             -> False
