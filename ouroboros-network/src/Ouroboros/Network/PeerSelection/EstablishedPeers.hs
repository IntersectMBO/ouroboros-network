{-# LANGUAGE DeriveFunctor  #-}
{-# LANGUAGE NamedFieldPuns #-}

{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Ouroboros.Network.PeerSelection.EstablishedPeers
  ( EstablishedPeers
  , establishedReady
  , establishedStatus
  , empty
  , toMap

  , size

  , member

  , insert
  , delete
  , deletePeers

  , updateStatus
  , updateStatuses

  , setCurrentTime
  , minActivateTime
  , setActivateTime

  , invariant
  ) where

import           Prelude

import           Data.Foldable (foldl')
import qualified Data.Map.Strict as Map
import           Data.Map.Strict (Map)
import           Data.OrdPSQ (OrdPSQ)
import qualified Data.OrdPSQ as PSQ
import qualified Data.Set as Set
import           Data.Set (Set)

import           Control.Monad.Class.MonadTime
import           Control.Exception (assert)

import           Ouroboros.Network.PeerSelection.Types


data EstablishedPeers peeraddr peerconn = EstablishedPeers {
    -- | Peers which are either ready do become active or are active.
    --
    establishedReady  :: !(Map peeraddr peerconn),

    -- | 'PeerStatus' of all established peers.
    establishedStatus :: !(Map peeraddr PeerStatus),

    -- | Peers which are not ready to become active.
    nextActivateTimes :: !(OrdPSQ peeraddr Time peerconn)
  }
  deriving (Show, Functor)


empty :: EstablishedPeers peeraddr perconn
empty = EstablishedPeers Map.empty Map.empty PSQ.empty


invariant :: Ord peeraddr
          => EstablishedPeers peeraddr peerconn
          -> Bool
invariant EstablishedPeers { establishedReady,
                             establishedStatus,
                             nextActivateTimes } =
     -- established connections and nextActivateTimes do not intersect
     Set.null
       (Set.intersection
         (Map.keysSet establishedReady)
         (Set.fromList (PSQ.keys nextActivateTimes)))

     -- established connection + nextActivate times has the same keys as
     -- established status
  &&    Map.keysSet establishedReady
     <> (Set.fromList (PSQ.keys nextActivateTimes))
     == Map.keysSet establishedStatus

     -- there are only warm peers in 'nextActiveTimes'
  && all (== PeerWarm)
         (Map.filterWithKey
           (\peeraddr _ -> PSQ.member peeraddr nextActivateTimes)
           establishedStatus)


-- | Map of all established connections.
--
-- Complexity: /O(n*log n)/
--
toMap :: Ord peeraddr
      => EstablishedPeers peeraddr peerconn
      -> Map peeraddr peerconn
toMap EstablishedPeers { establishedReady, nextActivateTimes } =
       establishedReady
    <> Map.fromList [ (peeraddr, peerconn)
                    | (peeraddr, _, peerconn) <- PSQ.toList nextActivateTimes ]


size :: EstablishedPeers peeraddr peerconn -> Int
size = Map.size . establishedStatus


member :: Ord peeraddr => peeraddr -> EstablishedPeers peeraddr peerconn -> Bool
member peeraddr = Map.member peeraddr . establishedStatus


-- | Insert a peer into 'EstablishedPeers'.
--
insert :: Ord peeraddr
       => peeraddr
       -> peerconn
       -> EstablishedPeers peeraddr peerconn
       -> EstablishedPeers peeraddr peerconn
insert peeraddr peerconn ep@EstablishedPeers { establishedReady, establishedStatus } =
  ep { establishedReady  = Map.insert peeraddr peerconn establishedReady,
       establishedStatus = Map.insert peeraddr PeerWarm establishedStatus }

updateStatus :: Ord peeraddr
             => peeraddr
             -> PeerStatus
             -- ^ keys must be a subset of keys of 'establishedStatus' map
             -> EstablishedPeers peeraddr peerconn
             -> EstablishedPeers peeraddr peerconn
updateStatus peeraddr peerStatus ep@EstablishedPeers { establishedStatus } =
    assert (Map.member peeraddr establishedStatus) $
    ep { establishedStatus = Map.insert peeraddr peerStatus establishedStatus }

-- | Update 'establishedStatus' map.
--
updateStatuses :: Ord peeraddr
               => Map peeraddr PeerStatus
               -- ^ keys must be a subset of keys of 'establishedStatus' map
               -> EstablishedPeers peeraddr peerconn
               -> EstablishedPeers peeraddr peerconn
updateStatuses newStatuses ep@EstablishedPeers { establishedStatus } =
    assert (Map.isSubmapOfBy (\_ _ -> True) newStatuses establishedStatus) $
    ep { establishedStatus = newStatuses <> establishedStatus }


delete :: Ord peeraddr
       => peeraddr
       -> EstablishedPeers peeraddr peerconn
       -> EstablishedPeers peeraddr peerconn
delete peeraddr es@EstablishedPeers { establishedReady,
                                            establishedStatus,
                                            nextActivateTimes } =
    es { establishedReady  = Map.delete peeraddr establishedReady,
         establishedStatus = Map.delete peeraddr establishedStatus,
         nextActivateTimes = PSQ.delete peeraddr nextActivateTimes }



-- | Bulk delete of peers from 'EstablishedPeers.
--
deletePeers :: Ord peeraddr
            => Set peeraddr
            -> EstablishedPeers peeraddr peerconn
            -> EstablishedPeers peeraddr peerconn
deletePeers peeraddrs es@EstablishedPeers { establishedReady,
                                            establishedStatus,
                                            nextActivateTimes } =
    es { establishedReady  = foldl' (flip Map.delete) establishedReady  peeraddrs,
         establishedStatus = foldl' (flip Map.delete) establishedStatus peeraddrs,
         nextActivateTimes = foldl' (flip PSQ.delete) nextActivateTimes peeraddrs }


--
-- Time managment
--

setCurrentTime :: Ord peeraddr
               => Time
               -> EstablishedPeers peeraddr peerconn
               -> EstablishedPeers peeraddr peerconn
setCurrentTime now ep@EstablishedPeers { establishedReady,
                                         nextActivateTimes } =
    let ep' = ep { establishedReady =
                        Map.fromList [ (peeraddr, peerconn) 
                                     | (peeraddr, _, peerconn) <- nowAvailableToActivate ]
                     <> establishedReady,
                   nextActivateTimes = nextActivateTimes' }
    in assert (invariant ep') ep'
  where
    (nowAvailableToActivate, nextActivateTimes') =
      PSQ.atMostView now nextActivateTimes


minActivateTime :: Ord peeraddr
                => EstablishedPeers peeraddr peerconn
                -> Maybe Time
minActivateTime EstablishedPeers {
                   establishedReady,
                   nextActivateTimes
                 }
  | Map.null establishedReady
  , Just (_k, t, _, _psq) <- PSQ.minView nextActivateTimes
  = Just t

  | otherwise
  = Nothing


setActivateTime :: Ord peeraddr
                => Set peeraddr
                -> Time
                -> EstablishedPeers peeraddr peerconn
                -> EstablishedPeers peeraddr peerconn
setActivateTime peeraddrs _time ep | Set.null peeraddrs = ep
setActivateTime peeraddrs time  ep =
    let ep' = foldl'
                (\peers@EstablishedPeers { establishedReady  = ready,
                                           nextActivateTimes = times }
                  peeraddr ->

                  -- 'alterF' allows to delete and return the value
                  case Map.alterF (\x -> (x, Nothing)) peeraddr ready of
                    (Just peerconn, ready') ->
                      peers { establishedReady  = ready',
                              nextActivateTimes = PSQ.insert peeraddr time peerconn times }

                    (Nothing, _ready') ->
                      error "EstablishedPeers.setActivateTime: invariant violation")
                ep
                peeraddrs
    in   assert (all (not . (`Map.member` establishedReady ep')) peeraddrs)
       . assert (invariant ep')
       $ ep'
