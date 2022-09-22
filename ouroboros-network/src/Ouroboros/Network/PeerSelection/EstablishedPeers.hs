{-# LANGUAGE DeriveFunctor   #-}
{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE RecordWildCards #-}

{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Ouroboros.Network.PeerSelection.EstablishedPeers
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
    -- ** Tracking when we can gossip
  , minGossipTime
  , setGossipTime
  , availableForGossip
  , invariant
  ) where

import           Prelude

import qualified Data.List as List
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.OrdPSQ (OrdPSQ)
import qualified Data.OrdPSQ as PSQ
import           Data.Semigroup (Min (..))
import           Data.Set (Set)
import qualified Data.Set as Set

import           Control.Exception (assert)
import           Control.Monad.Class.MonadTime


-------------------------------
-- Established peer set representation
--

-- | The set of established peers. To a first approximation it can be thought of
-- as a 'Set' of @peeraddr@.
--
-- It has one special feature:
--
--  * It tracks which peers we are permitted to gossip with now, or for peers
--    we cannot gossip with now the time at which we would next be allowed to
--    do so.
--
data EstablishedPeers peeraddr peerconn = EstablishedPeers {
    -- | Peers which are either ready to become active or are active.
    --
    allPeers           :: !(Map peeraddr peerconn),

    -- | The subset of established peers that we would be allowed to gossip with
    -- now. This is because we have not gossiped with them recently.
    --
    -- NOTE that this is the set of available peers one would be able to perform
    -- peer sharing _now_, it doesn't mean they are 100% eligible. This will
    -- depend on other factors like the peer's 'PeerSharing' value.
    --
    availableForGossip :: !(Set peeraddr),

    -- | The subset of established peers that we cannot gossip with now. It
    -- keeps track of the next time we are allowed to gossip with them.
    --
    nextGossipTimes    :: !(OrdPSQ peeraddr Time ()),


    -- | Peers which are not ready to become active.
    nextActivateTimes  :: !(OrdPSQ peeraddr Time ())
  }
  deriving (Show, Functor)


empty :: EstablishedPeers peeraddr perconn
empty = EstablishedPeers {
      allPeers           = Map.empty,
      availableForGossip = Set.empty,
      nextGossipTimes    = PSQ.empty,
      nextActivateTimes  = PSQ.empty
    }


invariant :: Ord peeraddr
          => EstablishedPeers peeraddr peerconn
          -> Bool
invariant EstablishedPeers {..} =
       -- The combo of the gossip set + psq = the whole set of peers
       availableForGossip
    <> Set.fromList (PSQ.keys nextGossipTimes)
    == Map.keysSet allPeers
       -- The gossip set and psq do not overlap
 && Set.null
      (Set.intersection
         availableForGossip
        (Set.fromList (PSQ.keys nextGossipTimes)))
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
       -> EstablishedPeers peeraddr peerconn
       -> EstablishedPeers peeraddr peerconn
insert peeraddr peerconn ep@EstablishedPeers { allPeers, availableForGossip } =
   ep { allPeers = Map.insert peeraddr peerconn allPeers,

        -- The sets tracking peers ready for gossip need to be updated with any
        -- /fresh/ peers, but any already present are ignored since they are
        -- either already in these sets or they are in the corresponding PSQs,
        -- for which we also preserve existing info.
        availableForGossip =
          if Map.member peeraddr allPeers
             then availableForGossip
             else Set.insert peeraddr availableForGossip
      }

delete :: Ord peeraddr
       => peeraddr
       -> EstablishedPeers peeraddr peerconn
       -> EstablishedPeers peeraddr peerconn
delete peeraddr es@EstablishedPeers { allPeers
                                    , availableForGossip
                                    , nextGossipTimes
                                    , nextActivateTimes
                                    } =
    es { allPeers           = Map.delete peeraddr allPeers,
         availableForGossip = Set.delete peeraddr availableForGossip,
         nextGossipTimes    = PSQ.delete peeraddr nextGossipTimes,
         nextActivateTimes  = PSQ.delete peeraddr nextActivateTimes
       }



-- | Bulk delete of peers from 'EstablishedPeers.
--
deletePeers :: Ord peeraddr
            => Set peeraddr
            -> EstablishedPeers peeraddr peerconn
            -> EstablishedPeers peeraddr peerconn
deletePeers peeraddrs es@EstablishedPeers { allPeers
                                          , availableForGossip
                                          , nextGossipTimes
                                          , nextActivateTimes
                                          } =
    es { allPeers           = Map.withoutKeys allPeers peeraddrs,
         availableForGossip = Set.difference availableForGossip peeraddrs,
         nextGossipTimes    =
           List.foldl' (flip PSQ.delete) nextGossipTimes peeraddrs,
         nextActivateTimes  =
           List.foldl' (flip PSQ.delete) nextActivateTimes peeraddrs
       }


--
-- Time managment
--

setCurrentTime :: Ord peeraddr
               => Time
               -> EstablishedPeers peeraddr peerconn
               -> EstablishedPeers peeraddr peerconn
setCurrentTime now ep@EstablishedPeers { nextGossipTimes
                                       , nextActivateTimes
                                       }
 -- Efficient check for the common case of there being nothing to do:
  | Just (Min t) <- (f <$> PSQ.minView nextGossipTimes)
                 <> (f <$> PSQ.minView nextActivateTimes)
  , t > now
  = ep
  where
    f (_,t,_,_) = Min t

setCurrentTime now ep@EstablishedPeers { nextGossipTimes
                                       , availableForGossip
                                       , nextActivateTimes
                                       } =
  let ep' = ep { nextGossipTimes   = nextGossipTimes'
               , availableForGossip = availableForGossip'
               , nextActivateTimes = nextActivateTimes'
               }
    in assert (invariant ep') ep'
  where
    (nowAvailableForGossip, nextGossipTimes') =
      PSQ.atMostView now nextGossipTimes

    availableForGossip' =
         availableForGossip
      <> Set.fromList [ peeraddr | (peeraddr, _, _) <- nowAvailableForGossip ]

    (_, nextActivateTimes') = PSQ.atMostView now nextActivateTimes


minActivateTime :: Ord peeraddr
                => EstablishedPeers peeraddr peerconn
                -> Maybe Time
minActivateTime EstablishedPeers { nextActivateTimes }
  | Just (_k, t, _, _psq) <- PSQ.minView nextActivateTimes
  = Just t

  | otherwise
  = Nothing


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
-- Tracking when we can gossip
--

-- | The first time that a peer will become available for gossip. If peers are
-- already available for gossip, or there are no peers at all then the
-- result is @Nothing@.
--
minGossipTime :: Ord peeraddr => EstablishedPeers peeraddr peercon -> Maybe Time
minGossipTime EstablishedPeers {
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
                 -> EstablishedPeers peeraddr peercon
                 -> EstablishedPeers peeraddr peercon
setGossipTime peeraddrs time
                 ep@EstablishedPeers {
                   allPeers,
                   availableForGossip,
                   nextGossipTimes
                 } =
    assert (all (`Map.member` allPeers) peeraddrs) $
    let ep' = ep {
          availableForGossip =
                   availableForGossip
            Set.\\ peeraddrs,

          nextGossipTimes =
            List.foldl' (\psq peeraddr -> PSQ.insert peeraddr time () psq)
                        nextGossipTimes
                        peeraddrs
        }
    in assert (invariant ep') ep'

