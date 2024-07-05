{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

module Ouroboros.Network.BlockFetch.Decision
  ( -- * Deciding what to fetch
    fetchDecisions
  , FetchDecisionPolicy (..)
  , FetchMode (..)
  , PeerInfo
  , FetchDecision
  , FetchDecline (..)
    -- ** Components of the decision-making process
  , filterPlausibleCandidates
  , selectForkSuffixes
  , filterNotAlreadyFetched
  , filterNotAlreadyInFlightWithPeer
  , prioritisePeerChains
  , fetchRequestDecisions
  ) where

import Data.Bifunctor (Bifunctor(..))
import Data.Hashable
import Data.List (singleton)
import Data.Foldable (traverse_)
import Data.Function ((&))
import Control.Monad.Class.MonadTime.SI (MonadMonotonicTime(..), addTime, DiffTime)

import Ouroboros.Network.AnchoredFragment (AnchoredFragment)
import Ouroboros.Network.Block
import Ouroboros.Network.BlockFetch.ClientState (FetchRequest (..), PeersOrder (..), msnoc, mcons)
import Ouroboros.Network.BlockFetch.ConsensusInterface (FetchMode (..), ChainSelStarvation)

import Ouroboros.Network.BlockFetch.Decision.Common (FetchDecisionPolicy (..), PeerInfo, FetchDecision, FetchDecline (..),
                                                     filterPlausibleCandidates, filterNotAlreadyFetched, filterNotAlreadyInFlightWithPeer,
                                                     selectForkSuffixes)
import Ouroboros.Network.BlockFetch.Decision.Deadline (fetchDecisionsDeadline,
                                                      prioritisePeerChains, fetchRequestDecisions)
import Ouroboros.Network.BlockFetch.Decision.BulkSync (fetchDecisionsBulkSync)
import Ouroboros.Network.BlockFetch.ConsensusInterface (ChainSelStarvation(..))

fetchDecisions
  :: forall peer header block m extra.
     (Ord peer,
      Hashable peer,
      HasHeader header,
      HeaderHash header ~ HeaderHash block, MonadMonotonicTime m)
  => FetchDecisionPolicy header
  -> FetchMode
  -> AnchoredFragment header
  -> (Point block -> Bool)
  -> MaxSlotNo
  -> ChainSelStarvation
  -> ( PeersOrder peer
     , PeersOrder peer -> m ()
     , peer -> m ()
     )
  -> [(AnchoredFragment header, PeerInfo header peer extra)]
  -> m [(FetchDecision (FetchRequest header), PeerInfo header peer extra)]

fetchDecisions
  fetchDecisionPolicy
  FetchModeDeadline
  currentChain
  fetchedBlocks
  fetchedMaxSlotNo
  _chainSelStarvation
  _peersOrderHandlers
  candidatesAndPeers
  =
    pure
      $ fetchDecisionsDeadline
        fetchDecisionPolicy
        currentChain
        fetchedBlocks
        fetchedMaxSlotNo
        candidatesAndPeers

fetchDecisions
  fetchDecisionPolicy
  FetchModeBulkSync
  currentChain
  fetchedBlocks
  fetchedMaxSlotNo
  chainSelStarvation
  ( peersOrder0,
    writePeersOrder,
    demoteCSJDynamo
    )
  candidatesAndPeers = do
    peersOrder@PeersOrder {peersOrderCurrent, peersOrderOthers} <-
      -- Align the peers order with the actual peers; this consists in removing
      -- all peers from the peers order that are not in the actual peers list and
      -- adding at the end of the peers order all the actual peers that were not
      -- there before.
      alignPeersOrderWithActualPeers
        peersOrder0
        (map (\(_, (_, _, _, peer, _)) -> peer) candidatesAndPeers)
        -- If the chain selection has been starved recently, that is after the
        -- current peer started (and a grace period), then the current peer is bad.
        -- We push it at the end of the queue and demote it from CSJ dynamo.
        & checkLastChainSelStarvation

    -- Compute the actual block fetch decision. This contains only declines and
    -- at most one request. 'theDecision' is therefore a 'Maybe'.
    let (theDecision, declines) =
          fetchDecisionsBulkSync
            fetchDecisionPolicy
            currentChain
            fetchedBlocks
            fetchedMaxSlotNo
            peersOrder
            candidatesAndPeers

    -- If the peer that is supposed to fetch the block is not the current one in
    -- the peers order, then we have shifted our focus: we make the new peer our
    -- current one and we put back the previous current peer at the beginning of
    -- the queue; not the end, because it has not done anything wrong.
    case theDecision of
      Just (_, (_, _, _, thePeer, _))
        | Just thePeer /= peersOrderCurrent -> do
            peersOrderStart' <- getMonotonicTime
            let peersOrder' =
                  PeersOrder
                    { peersOrderCurrent = Just thePeer,
                      peersOrderStart = peersOrderStart',
                      peersOrderOthers = mcons peersOrderCurrent (filter (/= thePeer) peersOrderOthers)
                    }
            writePeersOrder peersOrder'
      _ -> pure ()

    pure $
      maybe [] (singleton . first Right) theDecision
        ++ map (first Left) declines
    where
      alignPeersOrderWithActualPeers :: PeersOrder peer -> [peer] -> PeersOrder peer
      alignPeersOrderWithActualPeers
        PeersOrder {peersOrderCurrent, peersOrderStart, peersOrderOthers}
        actualPeers =
          let peersOrderCurrent' = case peersOrderCurrent of
                Just peersOrderCurrent_ | peersOrderCurrent_ `elem` actualPeers -> peersOrderCurrent
                _ -> Nothing
              peersOrderOthers' =
                filter (`elem` actualPeers) peersOrderOthers
                  ++ filter (`notElem` peersOrderOthers) actualPeers
           in PeersOrder
                { peersOrderCurrent = peersOrderCurrent',
                  peersOrderOthers = peersOrderOthers',
                  peersOrderStart
                }

      checkLastChainSelStarvation :: PeersOrder peer -> m (PeersOrder peer)
      checkLastChainSelStarvation
        peersOrder@PeersOrder {peersOrderCurrent, peersOrderStart, peersOrderOthers} =
          case chainSelStarvation of
            ChainSelStarvationEndedAt time
              | time < addTime (10 :: DiffTime) peersOrderStart ->
                pure peersOrder
            _ -> do
              let peersOrder' =
                    PeersOrder
                      { peersOrderCurrent = Nothing,
                        peersOrderOthers = msnoc peersOrderOthers peersOrderCurrent,
                        peersOrderStart
                      }
              traverse_ demoteCSJDynamo peersOrderCurrent
              pure peersOrder'
