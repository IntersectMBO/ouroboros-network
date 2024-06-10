{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

-- | This module contains the part of the block fetch decisions process that is
-- specific to the deadline mode.
module Ouroboros.Network.BlockFetch.Decision.Deadline
  ( -- * Deciding what to fetch
    fetchDecisionsDeadline
    -- ** Components of the decision-making process
  , prioritisePeerChains
  , fetchRequestDecisions
  ) where

import Data.Set qualified as Set

import Data.Function (on)
import Data.Hashable
import Data.List (foldl', groupBy, sortBy, transpose)
import Data.Maybe (mapMaybe)
import Data.Set (Set)

import Control.Monad (guard)

import Ouroboros.Network.AnchoredFragment (AnchoredFragment)
import Ouroboros.Network.AnchoredFragment qualified as AF
import Ouroboros.Network.Block
import Ouroboros.Network.Point (withOriginToMaybe)

import Ouroboros.Network.BlockFetch.ClientState (FetchRequest (..),
           PeerFetchInFlight (..), PeerFetchStatus (..))
import Ouroboros.Network.BlockFetch.ConsensusInterface (FetchMode (..))
import Ouroboros.Network.BlockFetch.DeltaQ (PeerGSV (..), SizeInBytes, calculatePeerFetchInFlightLimits,
           comparePeerGSV, comparePeerGSV', estimateExpectedResponseDuration,
           estimateResponseDeadlineProbability)

import Ouroboros.Network.BlockFetch.Decision.Common

fetchDecisionsDeadline
  :: (Ord peer,
      Hashable peer,
      HasHeader header,
      HeaderHash header ~ HeaderHash block)
  => FetchDecisionPolicy header
  -> AnchoredFragment header
  -> (Point block -> Bool)
  -> MaxSlotNo
  -> [(AnchoredFragment header, PeerInfo header peer extra)]
  -> [(FetchDecision (FetchRequest header), PeerInfo header peer extra)]

fetchDecisionsDeadline fetchDecisionPolicy@FetchDecisionPolicy {
                 plausibleCandidateChain,
                 compareCandidateChains,
                 blockFetchSize,
                 peerSalt
               }
               currentChain
               fetchedBlocks
               fetchedMaxSlotNo =

    -- Finally, make a decision for each (chain, peer) pair.
    fetchRequestDecisions
      fetchDecisionPolicy
      FetchModeDeadline
  . map swizzleSIG

    -- Reorder chains based on consensus policy and network timing data.
  . prioritisePeerChains
      FetchModeDeadline
      peerSalt
      compareCandidateChains
      blockFetchSize
  . map swizzleIG

    -- Filter to keep blocks that are not already in-flight for this peer.
  . filterNotAlreadyInFlightWithPeer'
  . map swizzleI

    -- Filter to keep blocks that have not already been downloaded.
  . filterNotAlreadyFetched'
      fetchedBlocks
      fetchedMaxSlotNo

    -- Select the suffix up to the intersection with the current chain.
  . selectForkSuffixes
      currentChain

    -- First, filter to keep chains the consensus layer tells us are plausible.
  . filterPlausibleCandidates
      plausibleCandidateChain
      currentChain
  where
    -- Data swizzling functions to get the right info into each stage.
    swizzleI   (c, p@(_,     inflight,_,_,      _)) = (c,         inflight,       p)
    swizzleIG  (c, p@(_,     inflight,gsvs,peer,_)) = (c,         inflight, gsvs, peer, p)
    swizzleSIG (c, p@(status,inflight,gsvs,peer,_)) = (c, status, inflight, gsvs, peer, p)

{-
In the example, this leaves us with only the candidate chains: A, B and C, but
still paired up with the various peers.


peer    1         2         3         4                   6         7
      ┆   ┆     ┆   ┆     ┆   ┆     ┆   ┆               ┆   ┆     ┆   ┆
      ├───┤     ├───┤     ├───┤     ├───┤               ├───┤     ├───┤
      │   │     │   │     │   │     │   │               │   │     │   │
      ├───┤     ├───┤     ├───┤     ├───┤               ├───┤     ├───┤
      │   │     │   │     │   │     │   │               │   │     │   │
    ──┼───┼─────┼───┼─────┼───┼─────┼───┼───────────────┼───┼─────┼───┼──
      │   │     │   │     │   │     │   │               │   │     │   │
      └───┘     ├───┤     ├───┤     ├───┤               ├───┤     ├───┤
                │   │     │   │     │   │               │   │     │   │
                └───┘     └───┘     └───┘               └───┘     └───┘
chain   C         A         B         A                   B         A
-}



{-
We define the /chain suffix/ as the suffix of the candidate chain up until (but
not including) where it intersects the current chain.


   current    peer 1    peer 2

    ┆   ┆
    ├───┤
    │  ◀┿━━━━━━━━━━━━━━━━━┓
    ├───┤               ┌─╂─┐
    │   │               │ ◉ │
    ├───┤               ├───┤
    │   │               │   │
    ├───┤               ├───┤
    │  ◀┿━━━━━━━┓       │   │
 ───┴───┴─────┬─╂─┬─────┼───┼───
              │ ◉ │     │   │
              └───┘     ├───┤
                        │ ◉ │
                        └───┘
                C         A

In this example we found that C was a strict extension of the current chain
and chain A was a short fork.

Note that it's possible that we don't find any intersection within the last K
blocks. This means the candidate forks by more than K and so we are not
interested in this candidate at all.
-}

prioritisePeerChains
  :: forall extra header peer.
   ( HasHeader header
   , Hashable peer
   , Ord peer
   )
  => FetchMode
  -> Int
  -> (AnchoredFragment header -> AnchoredFragment header -> Ordering)
  -> (header -> SizeInBytes)
  -> [(FetchDecision (CandidateFragments header), PeerFetchInFlight header,
                                                  PeerGSV,
                                                  peer,
                                                  extra )]
  -> [(FetchDecision [AnchoredFragment header],   extra)]
prioritisePeerChains FetchModeDeadline salt compareCandidateChains blockFetchSize =
    map (\(decision, peer) ->
            (fmap (\(_,_,fragment) -> fragment) decision, peer))
  . concatMap ( concat
              . transpose
              . groupBy (equatingFst
                          (equatingRight
                            ((==) `on` chainHeadPoint)))
              . sortBy  (comparingFst
                          (comparingRight
                            (compare `on` chainHeadPoint)))
              )
  . groupBy (equatingFst
              (equatingRight
                (equatingPair
                   -- compare on probability band first, then preferred chain
                   (==)
                   (equateCandidateChains `on` getChainSuffix)
                 `on`
                   (\(band, chain, _fragments) -> (band, chain)))))
  . sortBy  (descendingOrder
              (comparingFst
                (comparingRight
                  (comparingPair
                     -- compare on probability band first, then preferred chain
                     compare
                     (compareCandidateChains `on` getChainSuffix)
                   `on`
                      (\(band, chain, _fragments) -> (band, chain))))))
  . map annotateProbabilityBand
  . sortBy (\(_,_,a,ap,_) (_,_,b,bp,_) ->
      comparePeerGSV' salt (a,ap) (b,bp))
  where
    annotateProbabilityBand (Left decline, _, _, _, peer) = (Left decline, peer)
    annotateProbabilityBand (Right (chain,fragments), inflight, gsvs, _, peer) =
        (Right (band, chain, fragments), peer)
      where
        band = probabilityBand $
                 estimateResponseDeadlineProbability
                   gsvs
                   (peerFetchBytesInFlight inflight)
                   (totalFetchSize blockFetchSize fragments)
                   deadline

    deadline = 2 -- seconds -- TODO: get this from external info

    equateCandidateChains chain1 chain2
      | EQ <- compareCandidateChains chain1 chain2 = True
      | otherwise                                  = False

    chainHeadPoint (_,ChainSuffix c,_) = AF.headPoint c

prioritisePeerChains FetchModeBulkSync salt compareCandidateChains blockFetchSize =
    map (\(decision, peer) ->
            (fmap (\(_, _, fragment) -> fragment) decision, peer))
  . sortBy (comparingFst
             (comparingRight
               (comparingPair
                  -- compare on preferred chain first, then duration
                  (compareCandidateChains `on` getChainSuffix)
                  compare
                `on`
                  (\(duration, chain, _fragments) -> (chain, duration)))))
  . map annotateDuration
  . sortBy (\(_,_,a,ap,_) (_,_,b,bp,_) ->
      comparePeerGSV' salt (a,ap) (b,bp))
  where
    annotateDuration (Left decline, _, _, _, peer) = (Left decline, peer)
    annotateDuration (Right (chain,fragments), inflight, gsvs, _, peer) =
        (Right (duration, chain, fragments), peer)
      where
        -- TODO: consider if we should put this into bands rather than just
        -- taking the full value.
        duration = estimateExpectedResponseDuration
                     gsvs
                     (peerFetchBytesInFlight inflight)
                     (totalFetchSize blockFetchSize fragments)

totalFetchSize :: (header -> SizeInBytes)
               -> [AnchoredFragment header]
               -> SizeInBytes
totalFetchSize blockFetchSize fragments =
  sum [ blockFetchSize header
      | fragment <- fragments
      , header   <- AF.toOldestFirst fragment ]

type Comparing a = a -> a -> Ordering
type Equating  a = a -> a -> Bool

descendingOrder :: Comparing a -> Comparing a
descendingOrder cmp = flip cmp

comparingPair :: Comparing a -> Comparing b -> Comparing (a, b)
comparingPair cmpA cmpB (a1, b1) (a2, b2) = cmpA a1 a2 <> cmpB b1 b2

equatingPair :: Equating a -> Equating b -> Equating (a, b)
equatingPair eqA eqB (a1, b1) (a2, b2) = eqA a1 a2 && eqB b1 b2

comparingEither :: Comparing a -> Comparing b -> Comparing (Either a b)
comparingEither _ _    (Left  _) (Right _) = LT
comparingEither cmpA _ (Left  x) (Left  y) = cmpA x y
comparingEither _ cmpB (Right x) (Right y) = cmpB x y
comparingEither _ _    (Right _) (Left  _) = GT

equatingEither :: Equating a -> Equating b -> Equating (Either a b)
equatingEither _ _   (Left  _) (Right _) = False
equatingEither eqA _ (Left  x) (Left  y) = eqA x y
equatingEither _ eqB (Right x) (Right y) = eqB x y
equatingEither _ _   (Right _) (Left  _) = False

comparingFst :: Comparing a -> Comparing (a, b)
comparingFst cmp = cmp `on` fst

equatingFst :: Equating a -> Equating (a, b)
equatingFst eq = eq `on` fst

comparingRight :: Comparing b -> Comparing (Either a b)
comparingRight = comparingEither mempty

equatingRight :: Equating b -> Equating (Either a b)
equatingRight = equatingEither (\_ _ -> True)

-- | Given the probability of the download completing within the deadline,
-- classify that into one of three broad bands: high, medium and low.
--
-- The bands are
--
-- * high:    98% -- 100%
-- * medium:  75% --  98%
-- * low:      0% --  75%
--
probabilityBand :: Double -> ProbabilityBand
probabilityBand p
  | p > 0.98  = ProbabilityHigh
  | p > 0.75  = ProbabilityModerate
  | otherwise = ProbabilityLow
 -- TODO: for hysteresis, increase probability if we're already using this peer

data ProbabilityBand = ProbabilityLow
                     | ProbabilityModerate
                     | ProbabilityHigh
  deriving (Eq, Ord, Show)


{-
In the second phase we walk over the prioritised fetch suffixes for each peer
and make a decision about whether we should initiate any new fetch requests.

This decision is based on a number of factors:

 * Is the fetch suffix empty? If so, there's nothing to do.
 * Do we already have block fetch requests in flight with this peer?
 * If so are we under the maximum number of in-flight blocks for this peer?
 * Is this peer still performing within expectations or has it missed any soft
   time outs?
 * Has the peer missed any hard timeouts or otherwise been disconnected.
 * Are we at our soft or hard limit of the number of peers we are prepared to
   fetch blocks from concurrently?

We look at each peer chain fetch suffix one by one. Of course decisions we
make earlier can affect decisions later, in particular the number of peers we
fetch from concurrently can increase if we fetch from a new peer, and we must
obviously take that into account when considering later peer chains.
-}


fetchRequestDecisions
  :: forall extra header peer.
      ( Hashable peer
      , HasHeader header
      , Ord peer
      )
  => FetchDecisionPolicy header
  -> FetchMode
  -> [( FetchDecision [AnchoredFragment header]
      , PeerFetchStatus header
      , PeerFetchInFlight header
      , PeerGSV
      , peer
      , extra)]
  -> [(FetchDecision (FetchRequest header), extra)]
fetchRequestDecisions fetchDecisionPolicy fetchMode chains =
    go nConcurrentFetchPeers0 Set.empty NoMaxSlotNo chains
  where
    go :: Word
       -> Set (Point header)
       -> MaxSlotNo
       -> [(Either FetchDecline [AnchoredFragment header],
            PeerFetchStatus header, PeerFetchInFlight header, PeerGSV, peer, extra)]
       -> [(FetchDecision (FetchRequest header), extra)]
    go !_ !_ !_ [] = []
    go !nConcurrentFetchPeers !blocksFetchedThisRound !maxSlotNoFetchedThisRound
       ((mchainfragments, status, inflight, gsvs, peer, extra) : cps) =

        (decision, extra)
      : go nConcurrentFetchPeers' blocksFetchedThisRound'
           maxSlotNoFetchedThisRound' cps
      where
        decision = fetchRequestDecision
                     fetchDecisionPolicy
                     fetchMode
                     -- Permit the preferred peers to by pass any concurrency limits.
                     (if elem peer nPreferedPeers then 0
                                                  else nConcurrentFetchPeers)
                     (calculatePeerFetchInFlightLimits gsvs)
                     inflight
                     status
                     mchainfragments'

        mchainfragments' =
          case fetchMode of
            FetchModeDeadline -> mchainfragments
            FetchModeBulkSync -> do
                chainfragments <- mchainfragments
                let fragments =
                      concatMap (filterWithMaxSlotNo
                                   notFetchedThisRound
                                   maxSlotNoFetchedThisRound)
                                chainfragments
                guard (not (null fragments)) ?! FetchDeclineInFlightOtherPeer
                return fragments
              where
                notFetchedThisRound h =
                  blockPoint h `Set.notMember` blocksFetchedThisRound

        nConcurrentFetchPeers'
          -- increment if it was idle, and now will not be
          | peerFetchReqsInFlight inflight == 0
          , Right{} <- decision = nConcurrentFetchPeers + 1
          | otherwise           = nConcurrentFetchPeers

        -- This is only for avoiding duplication between fetch requests in this
        -- round of decisions. Avoiding duplication with blocks that are already
        -- in flight is handled by filterNotAlreadyInFlightWithOtherPeers
        (blocksFetchedThisRound', maxSlotNoFetchedThisRound') =
          case decision of
            Left _                         ->
              (blocksFetchedThisRound, maxSlotNoFetchedThisRound)
            Right (FetchRequest fragments) ->
              (blocksFetchedThisRound `Set.union` blocksFetchedThisDecision,
               maxSlotNoFetchedThisRound `max` maxSlotNoFetchedThisDecision)
              where
                maxSlotNoFetchedThisDecision =
                  foldl' max NoMaxSlotNo $ map MaxSlotNo $
                  mapMaybe (withOriginToMaybe . AF.headSlot) fragments

                blocksFetchedThisDecision =
                  Set.fromList
                    [ blockPoint header
                    | fragment <- fragments
                    , header   <- AF.toOldestFirst fragment ]

    nConcurrentFetchPeers0 = fromIntegral $ Set.size nActivePeers

    -- Set of peers with outstanding bytes.
    nActivePeers :: Set peer
    nActivePeers =
        Set.fromList
      . map snd
      . filter (\(inFlight, _) -> inFlight > 0)
      . map (\(_, _, PeerFetchInFlight{peerFetchReqsInFlight}, _, p, _) ->
                       (peerFetchReqsInFlight, p))
      $ chains

    -- Order the peers based on current PeerGSV. The top performing peers will be
    -- permitted to go active even if we're above the desired maxConcurrentFetchPeers
    -- which will cause us to switch smoothly from a slower to faster peers.
    -- When switching from slow to faster peers we will be over the configured limit, but
    -- PeerGSV is expected to be updated rather infrequently so the set of preferred peers should
    -- be stable during 10s of seconds.
    nPreferedPeers :: [peer]
    nPreferedPeers =
        map snd
      . take (fromIntegral maxConcurrentFetchPeers)
      . sortBy (\a b -> comparePeerGSV nActivePeers (peerSalt fetchDecisionPolicy) a b)
      . map (\(_, _, _, gsv, p, _) -> (gsv, p))
      $ chains

    maxConcurrentFetchPeers :: Word
    maxConcurrentFetchPeers =
      case fetchMode of
           FetchModeBulkSync -> maxConcurrencyBulkSync fetchDecisionPolicy
           FetchModeDeadline -> maxConcurrencyDeadline fetchDecisionPolicy
