{-# LANGUAGE DeriveFunctor       #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Ouroboros.Network.PeerSelection.Governor.KnownPeers
  ( belowTarget
  , aboveTarget
  ) where

import           Data.Maybe (fromMaybe)
import           Data.Semigroup (Min(..))
import qualified Data.Set as Set

import           Control.Concurrent.JobPool (Job(..))
import           Control.Monad.Class.MonadAsync
import           Control.Monad.Class.MonadSTM
import           Control.Monad.Class.MonadTime
import           Control.Monad.Class.MonadTimer
import           Control.Exception (Exception(..), SomeException, assert)

import qualified Ouroboros.Network.PeerSelection.EstablishedPeers as EstablishedPeers
import qualified Ouroboros.Network.PeerSelection.KnownPeers as KnownPeers
import qualified Ouroboros.Network.PeerSelection.LocalRootPeers as LocalRootPeers
import           Ouroboros.Network.PeerSelection.Governor.Types


---------------------------
-- Known peers below target
--


-- | If we are below the target of /known peers/ we gossip (if we are above the
-- gossip request threashold).
--
belowTarget :: (MonadAsync m, MonadTimer m, Ord peeraddr)
            => PeerSelectionActions peeraddr peerconn m
            -> MkGuardedDecision peeraddr peerconn m
belowTarget actions
            policy@PeerSelectionPolicy {
              policyMaxInProgressGossipReqs,
              policyPickKnownPeersForGossip,
              policyGossipRetryTime
            }
            st@PeerSelectionState {
              knownPeers,
              inProgressGossipReqs,
              targets = PeerSelectionTargets {
                          targetNumberOfKnownPeers
                        }
            }
    -- Are we under target for number of known peers?
  | numKnownPeers < targetNumberOfKnownPeers

    -- Are we at our limit for number of gossip requests?
  , numGossipReqsPossible > 0

    -- Are there any known peers that we can send a gossip request to?
    -- We can only ask ones where we have not asked them within a certain time.
  , not (Set.null availableForGossip)
  = Guarded Nothing $ do
      selectedForGossip <- pickPeers st
                             policyPickKnownPeersForGossip
                             availableForGossip
                             numGossipReqsPossible
      let numGossipReqs = Set.size selectedForGossip
      return $ \now -> Decision {
        decisionTrace = TraceGossipRequests
                          targetNumberOfKnownPeers
                          numKnownPeers
                          availableForGossip
                          selectedForGossip,
        decisionState = st {
                          inProgressGossipReqs = inProgressGossipReqs
                                               + numGossipReqs,
                          knownPeers = KnownPeers.setGossipTime
                                         selectedForGossip
                                         (addTime policyGossipRetryTime now)
                                         knownPeers
                        },
        decisionJobs  = [jobGossip actions policy
                           (Set.toList selectedForGossip)]
      }

    -- If we could gossip except that there are none currently available
    -- then we return the next wakeup time (if any)
  | numKnownPeers < targetNumberOfKnownPeers
  , numGossipReqsPossible > 0
  , Set.null availableForGossip
  = GuardedSkip (Min <$> KnownPeers.minGossipTime knownPeers)

  | otherwise
  = GuardedSkip Nothing
  where
    numKnownPeers         = KnownPeers.size knownPeers
    numGossipReqsPossible = policyMaxInProgressGossipReqs
                          - inProgressGossipReqs
    availableForGossip    = KnownPeers.availableForGossip knownPeers


jobGossip :: forall m peeraddr peerconn.
             (MonadAsync m, MonadTimer m, Ord peeraddr)
          => PeerSelectionActions peeraddr peerconn m
          -> PeerSelectionPolicy peeraddr m
          -> [peeraddr]
          -> Job () m (Completion m peeraddr peerconn)
jobGossip PeerSelectionActions{requestPeerGossip}
           PeerSelectionPolicy{..} =
    \peers -> Job (jobPhase1 peers) (handler peers) () "gossipPhase1"
  where
    handler :: [peeraddr] -> SomeException -> m (Completion m peeraddr peerconn)
    handler peers e = return $
      Completion $ \st _ ->
      Decision {
        decisionTrace = TraceGossipResults [ (p, Left e) | p <- peers ],
        decisionState = st {
                          inProgressGossipReqs = inProgressGossipReqs st
                                               - length peers
                        },
        decisionJobs  = []
      }

    jobPhase1 :: [peeraddr] -> m (Completion m peeraddr peerconn)
    jobPhase1 peers = do
      -- In the typical case, where most requests return within a short
      -- timeout we want to collect all the responses into a batch and
      -- add them to the known peers set in one go.
      --
      -- So fire them all off in one go:
      gossips <- sequence [ async (requestPeerGossip peer) | peer <- peers ]

      -- First to finish synchronisation between /all/ the gossips completing
      -- or the timeout (with whatever partial results we have at the time)
      results <- waitAllCatchOrTimeout gossips policyGossipBatchWaitTime
      case results of
        Right totalResults -> do
          let peerResults = zip peers totalResults
              newPeers    = [ p | Right ps <- totalResults, p <- ps ]
          return $ Completion $ \st _ -> Decision {
            decisionTrace = TraceGossipResults peerResults,
            decisionState = st {
                              --TODO: also update with the failures
                              knownPeers = KnownPeers.insert
                                             (Set.fromList newPeers)
                                             (knownPeers st),
                              inProgressGossipReqs = inProgressGossipReqs st
                                                   - length peers
                            },
            decisionJobs  = []
          }

        -- But if any don't make the first timeout then they'll be added later
        -- when they do reply or never if we hit the hard timeout.
        Left partialResults -> do

          -- We have to keep track of the relationship between the peer
          -- addresses and the gossip requests, completed and still in progress:
          let peerResults      = [ (p, r)
                                 | (p, Just r)  <- zip peers   partialResults ]
              newPeers         = [  p
                                 | Just (Right ps) <-          partialResults
                                 ,  p <- ps ]
              peersRemaining   = [  p
                                 | (p, Nothing) <- zip peers   partialResults ]
              gossipsRemaining = [  a
                                 | (a, Nothing) <- zip gossips partialResults ]

          return $ Completion $ \st _ -> Decision {
            decisionTrace = TraceGossipResults peerResults,
            decisionState = st {
                              --TODO: also update with the failures
                              knownPeers = KnownPeers.insert
                                             (Set.fromList newPeers)
                                             (knownPeers st),
                              inProgressGossipReqs = inProgressGossipReqs st
                                                   - length peerResults
                            },
            decisionJobs  = [Job (jobPhase2 peersRemaining gossipsRemaining)
                                 (handler peersRemaining)
                                 ()
                                 "gossipPhase2"]
          }

    jobPhase2 :: [peeraddr] -> [Async m [peeraddr]]
              -> m (Completion m peeraddr peerconn)
    jobPhase2 peers gossips = do

      -- Wait again, for all remaining to finish or a timeout.
      results <- waitAllCatchOrTimeout
                      gossips
                      (policyGossipOverallTimeout
                       - policyGossipBatchWaitTime)
      let peerResults =
            case results of
              Right totalResults  -> zip peers totalResults
              Left partialResults -> [ (p, fromMaybe err r)
                                     | (p, r) <- zip peers partialResults ]
                where err = Left (toException AsyncCancelled)

          newPeers =
            case results of
              Right totalResults  -> [ p | Right ps <- totalResults,  p <- ps ]
              Left partialResults -> [ p | Just (Right ps) <- partialResults,  p <- ps ]

          gossipsIncomplete =
            case results of
              Right _totalResults -> []
              Left partialResults ->
                [ a | (a, Nothing) <- zip gossips partialResults ]

      mapM_ cancel gossipsIncomplete

      return $ Completion $ \st _ -> Decision {
        decisionTrace = TraceGossipResults peerResults,
        decisionState = st {
                          --TODO: also update with the failures
                          knownPeers = KnownPeers.insert
                                         (Set.fromList newPeers)
                                         (knownPeers st),
                          inProgressGossipReqs = inProgressGossipReqs st
                                               - length peers
                        },
        decisionJobs  = []
      }


---------------------------
-- Known peers above target
--


-- | If we are above the target of /known peers/ (i.e. /cold/, /warm/ and /hot/
-- combined), we drop some of the /cold peers/ but we protect the
-- 'targetNumberOfRootPeers' (from combined sets of /local/ and /public root/
-- peers). 'policyPickColdPeersToForget' policy is used to pick the peers.
--
aboveTarget :: (MonadSTM m, Ord peeraddr)
            => MkGuardedDecision peeraddr peerconn m
aboveTarget PeerSelectionPolicy {
              policyPickColdPeersToForget
            }
            st@PeerSelectionState {
              localRootPeers,
              publicRootPeers,
              knownPeers,
              establishedPeers,
              inProgressPromoteCold,
              targets = PeerSelectionTargets {
                          targetNumberOfKnownPeers,
                          targetNumberOfRootPeers
                        }
            }
    -- Are we above the target for number of known peers?
  | numKnownPeers > targetNumberOfKnownPeers

    -- Are there any cold peers we could pick to forget?
    -- As a first cheap approximation, check if there are any cold peers.
  , numKnownPeers > numEstablishedPeers

    -- Beyond this it gets more complicated, and it is not clear that there
    -- are any precise cheap checks. So we just do the full calculation.
    -- In particular there can be overlap between cold peers and root peers
    -- and we have constraints on forgetting root peers.
    --
    -- We must never pick local root peers to forget as this would violate
    -- our invariant that the localRootPeers is a subset of the knownPeers.
    --
    -- We also need to avoid picking public root peers if that would put us
    -- below the target for root peers.
    --
  , let numRootPeersCanForget = LocalRootPeers.size localRootPeers
                              + Set.size publicRootPeers
                              - targetNumberOfRootPeers
        availableToForget     = KnownPeers.toSet knownPeers
                                  Set.\\ EstablishedPeers.toSet establishedPeers
                                  Set.\\ LocalRootPeers.keysSet localRootPeers
                                  Set.\\ (if numRootPeersCanForget <= 0
                                            then publicRootPeers else Set.empty)
                                  Set.\\ inProgressPromoteCold

  , not (Set.null availableToForget)
  = Guarded Nothing $ do
      let numOtherPeersToForget         = numKnownPeers
                                        - targetNumberOfKnownPeers
          numPeersToForget
            | numRootPeersCanForget > 0 = min numRootPeersCanForget
                                              numOtherPeersToForget
            | otherwise                 = numOtherPeersToForget
      -- If we /might/ pick a root peer, limit the number to forget so we do
      -- not pick too many root peers. This may cause us to go round several
      -- times but that is ok.
      selectedToForget <- pickPeers st
                            policyPickColdPeersToForget
                            availableToForget
                            numPeersToForget
      return $ \_now ->
        let knownPeers'      = KnownPeers.delete
                                 selectedToForget
                                 knownPeers
            publicRootPeers' = publicRootPeers
                                 Set.\\ selectedToForget
        in assert (Set.isSubsetOf
                     publicRootPeers'
                    (KnownPeers.toSet knownPeers'))

              Decision {
                decisionTrace = TraceForgetColdPeers
                                  targetNumberOfKnownPeers
                                  numKnownPeers
                                  selectedToForget,
                decisionState = st { knownPeers      = knownPeers',
                                     publicRootPeers = publicRootPeers' },
                decisionJobs  = []
              }

  | otherwise
  = GuardedSkip Nothing
  where
    numKnownPeers, numEstablishedPeers :: Int
    numKnownPeers        = KnownPeers.size knownPeers
    numEstablishedPeers  = EstablishedPeers.size establishedPeers


-------------------------------
-- Utils
--

-- | Perform a first-to-finish synchronisation between:
--
-- * /all/ the async actions completing; or
-- * the timeout with whatever partial results we have at the time
--
-- The result list is the same length and order as the asyncs, so the results
-- can be paired up.
--
waitAllCatchOrTimeout :: (MonadAsync m, MonadTimer m)
                      => [Async m a]
                      -> DiffTime
                      -> m (Either [Maybe (Either SomeException a)]
                                   [Either SomeException a])
waitAllCatchOrTimeout as time = do
    t       <- newTimeout time
    results <- atomically $
                         (Right <$> mapM waitCatchSTM as)
                `orElse` (Left  <$> (awaitTimeout t >> mapM pollSTM as))
    case results of
      Right{} -> cancelTimeout t
      _       -> return ()
    return results
