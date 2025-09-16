{-# LANGUAGE DeriveFunctor   #-}
{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE RecordWildCards #-}

{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Ouroboros.Network.PeerSelection.State.EstablishedPeers
  ( EstablishedPeers
  , empty
  , toMap
  , toSet
  , readyPeers
  , size
  , sizeReady
  , member
  , insert
  , delete
  , deletePeers
    -- * Special operations
  , setCurrentTime
  , setActivateTimes
    -- ** Tracking when we can (re)activate
  , minActivateTime
    -- ** Tracking when we can peer share
  , minPeerShareTime
  , setPeerShareTime
  , availableForPeerShare
  , nextPeerShareTimes
  , invariant
  ) where

import Prelude

import Data.List qualified as List
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.OrdPSQ (OrdPSQ)
import Data.OrdPSQ qualified as PSQ
import Data.Semigroup (Min (..))
import Data.Set (Set)
import Data.Set qualified as Set

import Control.Exception (assert)
import Control.Monad.Class.MonadTime.SI


-------------------------------
-- Established peer set representation
--

-- | The set of established peers. To a first approximation it can be thought of
-- as a 'Set' of @peeraddr@.
--
-- It has one special feature:
--
--  * It tracks which peers we are permitted to ask for peers now, or for peers
--    we cannot issue share requests with now the time at which we would next be
--    allowed to do so.
--
data EstablishedPeers peeraddr peerconn = EstablishedPeers {
    -- | Peers which are either ready to become active or are active.
    --
    allPeers              :: !(Map peeraddr peerconn),

    -- | The subset of established peers that we would be allowed to peer share
    -- with now. This is because we have not peer shared with them recently.
    --
    -- NOTE that this is the set of available peers one would be able to perform
    -- peer sharing _now_, it doesn't mean they are 100% eligible. This will
    -- depend on other factors like the peer's 'PeerSharing' value.
    --
    availableForPeerShare :: !(Set peeraddr),

    -- | The subset of established peers that we cannot peer share with now. It
    -- keeps track of the next time we are allowed to peer share with them.
    --
    nextPeerShareTimes    :: !(OrdPSQ peeraddr Time ()),


    -- | Peers which are not ready to become active.
    nextActivateTimes     :: !(OrdPSQ peeraddr Time ())
  }
  deriving (Show, Functor)


empty :: EstablishedPeers peeraddr perconn
empty = EstablishedPeers {
      allPeers              = Map.empty,
      availableForPeerShare = Set.empty,
      nextPeerShareTimes    = PSQ.empty,
      nextActivateTimes     = PSQ.empty
    }


invariant :: Ord peeraddr
          => EstablishedPeers peeraddr peerconn
          -> Bool
invariant EstablishedPeers {..} =
       -- The combo of the peer share set + psq = a subset of all peers
 (  availableForPeerShare
    <> Set.fromList (PSQ.keys nextPeerShareTimes)
 )
    `Set.isSubsetOf` Map.keysSet allPeers
       -- The peer share set and psq do not overlap
 && Set.null
      (Set.intersection
         availableForPeerShare
        (Set.fromList (PSQ.keys nextPeerShareTimes)))
     -- nextActivateTimes is a subset of allPeers
 &&  Set.fromList (PSQ.keys nextActivateTimes)
     `Set.isSubsetOf`
     Map.keysSet allPeers


-- | /O(1)/
toMap :: EstablishedPeers peeraddr peerconn -> Map peeraddr peerconn
toMap = allPeers

-- | /O(n)/
toSet :: EstablishedPeers peeraddr peerconn -> Set peeraddr
toSet = Map.keysSet . allPeers


-- | Map of established peers that are either active or ready to be promoted
-- to active.
--
-- /O(n log m), for n not-ready peers, and m established peers/
--
readyPeers :: Ord peeraddr
           => EstablishedPeers peeraddr peerconn
           -> Set peeraddr
readyPeers EstablishedPeers { allPeers, nextActivateTimes } =
    PSQ.fold'
      (\peeraddr _ _ -> Set.delete peeraddr)
      (Map.keysSet allPeers)
      nextActivateTimes


-- | The number of established peers. The size of 'allPeers'
--
-- /O(1)/
--
size :: EstablishedPeers peeraddr peerconn -> Int
size EstablishedPeers { allPeers } = Map.size allPeers


-- | The number of ready peers. The size of 'readyPeers'
--
-- /O(1)/
--
sizeReady :: EstablishedPeers peeraddr peerconn -> Int
sizeReady EstablishedPeers { allPeers, nextActivateTimes } =
    Map.size allPeers - PSQ.size nextActivateTimes


member :: Ord peeraddr => peeraddr -> EstablishedPeers peeraddr peerconn -> Bool
member peeraddr = Map.member peeraddr . allPeers


-- | Insert a peer into 'EstablishedPeers'.
--
insert :: Ord peeraddr
       => peeraddr
       -> peerconn
       -> Maybe Time -- ^ When to first peershare with peer, Nothing means never
       -> EstablishedPeers peeraddr peerconn
       -> EstablishedPeers peeraddr peerconn
insert peeraddr peerconn peerShareAt_m ep@EstablishedPeers { allPeers } =
   -- Ask newly established peers for its peers after the specified delay.
   let ep' = ep { allPeers = Map.insert peeraddr peerconn allPeers } in
   case peerShareAt_m of
        Nothing          -> ep'
        Just peerShareAt ->
          setPeerShareTime (Set.singleton peeraddr) peerShareAt $ ep'

delete :: Ord peeraddr
       => peeraddr
       -> EstablishedPeers peeraddr peerconn
       -> EstablishedPeers peeraddr peerconn
delete peeraddr es@EstablishedPeers { allPeers
                                    , availableForPeerShare
                                    , nextPeerShareTimes
                                    , nextActivateTimes
                                    } =
    es { allPeers              = Map.delete peeraddr allPeers,
         availableForPeerShare = Set.delete peeraddr availableForPeerShare,
         nextPeerShareTimes    = PSQ.delete peeraddr nextPeerShareTimes,
         nextActivateTimes     = PSQ.delete peeraddr nextActivateTimes
       }



-- | Bulk delete of peers from 'EstablishedPeers.
--
deletePeers :: Ord peeraddr
            => Set peeraddr
            -> EstablishedPeers peeraddr peerconn
            -> EstablishedPeers peeraddr peerconn
deletePeers peeraddrs es@EstablishedPeers { allPeers,
                                            availableForPeerShare,
                                            nextPeerShareTimes,
                                            nextActivateTimes
                                          } =
    es { allPeers              = Map.withoutKeys allPeers peeraddrs,
         availableForPeerShare = Set.difference availableForPeerShare peeraddrs,
         nextPeerShareTimes    =
           List.foldl' (flip PSQ.delete) nextPeerShareTimes peeraddrs,
         nextActivateTimes     =
           List.foldl' (flip PSQ.delete) nextActivateTimes peeraddrs
       }


--
-- Time managment
--

setCurrentTime :: Ord peeraddr
               => Time
               -> EstablishedPeers peeraddr peerconn
               -> EstablishedPeers peeraddr peerconn
setCurrentTime now ep@EstablishedPeers { nextPeerShareTimes
                                       , nextActivateTimes
                                       }
 -- Efficient check for the common case of there being nothing to do:
    | Just (Min t) <- (f <$> PSQ.minView nextPeerShareTimes)
                   <> (f <$> PSQ.minView nextActivateTimes)
    , t > now
    = ep
  where
    f (_,t,_,_) = Min t

setCurrentTime now ep@EstablishedPeers { nextPeerShareTimes
                                       , availableForPeerShare
                                       , nextActivateTimes
                                       } =
  let ep' = ep { nextPeerShareTimes    = nextPeerShareTimes'
               , availableForPeerShare = availableForPeerShare'
               , nextActivateTimes     = nextActivateTimes'
               }
    in assert (invariant ep') ep'
  where
    (nowAvailableForPeerShare, nextPeerShareTimes') =
      PSQ.atMostView now nextPeerShareTimes

    availableForPeerShare' =
         availableForPeerShare
      <> Set.fromList [ peeraddr | (peeraddr, _, _) <- nowAvailableForPeerShare ]

    (_, nextActivateTimes') = PSQ.atMostView now nextActivateTimes


-- | Find smallest activation time for a peer belonging to a given set.
--
minActivateTime :: Ord peeraddr
                => EstablishedPeers peeraddr peerconn
                -> (peeraddr -> Bool)
                -- ^ a predicate which describes the peers to take into
                -- account
                -> Maybe Time
minActivateTime EstablishedPeers { nextActivateTimes } fn = go nextActivateTimes
  where
    go psq = case PSQ.minView psq of
      Just (k, t, _, psq') | fn k      -> Just t
                           | otherwise -> go psq'
      Nothing                          -> Nothing


setActivateTimes :: Ord peeraddr
                 => Map peeraddr Time
                 -> EstablishedPeers peeraddr peerconn
                 -> EstablishedPeers peeraddr peerconn
setActivateTimes times ep | Map.null times = ep
setActivateTimes times ep@EstablishedPeers { nextActivateTimes } =
    let ep' = ep { nextActivateTimes =
                     Map.foldlWithKey'
                       (\psq peeraddr time ->
                             snd $
                             PSQ.alter (\case
                                           Nothing         -> ((), Just (time, ()))
                                           Just (time', _) -> ((), Just (time `max` time', ()))
                                       )
                                       peeraddr psq)
                       nextActivateTimes
                       times
                 }
    in   assert (all (not . (`Set.member` readyPeers ep')) (Map.keys times))
       . assert (invariant ep')
       $ ep'

-------------------------------
-- Tracking when we can peer share
--

-- | The first time that a peer will become available for peer sharing. If
-- peers are already available for peer share, or there are no peers at all
-- then the result is @Nothing@.
--
minPeerShareTime :: Ord peeraddr
                 => EstablishedPeers peeraddr peercon
                 -> Maybe Time
minPeerShareTime EstablishedPeers { availableForPeerShare,
                                    nextPeerShareTimes
                                  }
  | Set.null availableForPeerShare
  , Just (_k, t, _, _psq) <- PSQ.minView nextPeerShareTimes
  = Just t

  | otherwise
  = Nothing

setPeerShareTime :: Ord peeraddr
                 => Set peeraddr
                 -> Time
                 -> EstablishedPeers peeraddr peercon
                 -> EstablishedPeers peeraddr peercon
setPeerShareTime peeraddrs time
                 ep@EstablishedPeers {
                   allPeers,
                   availableForPeerShare,
                   nextPeerShareTimes
                 } =
    assert (all (`Map.member` allPeers) peeraddrs) $
    let ep' = ep {
          availableForPeerShare =
                   availableForPeerShare
            Set.\\ peeraddrs,

          nextPeerShareTimes =
            List.foldl' (\psq peeraddr -> PSQ.insert peeraddr time () psq)
                        nextPeerShareTimes
                        peeraddrs
        }
    in assert (invariant ep') ep'

