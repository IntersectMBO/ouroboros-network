{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Ouroboros.Network.PeerSelection.Governor.KnownPeers
  ( belowTarget
  , aboveTarget
  ) where

import qualified Data.Map as Map
import           Data.Maybe (fromMaybe)
import           Data.Semigroup (Min (..))
import qualified Data.Set as Set

import           Control.Concurrent.JobPool (Job (..))
import           Control.Exception (Exception (..), SomeException, assert)
import           Control.Monad.Class.MonadAsync
import           Control.Monad.Class.MonadSTM
import           Control.Monad.Class.MonadTime
import           Control.Monad.Class.MonadTimer

import qualified Ouroboros.Network.PeerSelection.EstablishedPeers as EstablishedPeers
import           Ouroboros.Network.PeerSelection.Governor.Types
import qualified Ouroboros.Network.PeerSelection.KnownPeers as KnownPeers
import           Ouroboros.Network.PeerSelection.LedgerPeers (IsLedgerPeer (..))
import qualified Ouroboros.Network.PeerSelection.LocalRootPeers as LocalRootPeers
import           Ouroboros.Network.PeerSelection.PeerAdvertise.Type
                     (PeerAdvertise (..))
import           Ouroboros.Network.PeerSelection.PeerSharing.Type
                     (PeerSharing (..))


---------------------------
-- Known peers below target
--


-- | If we are below the target of /known peers/ we peer share (if we are above
-- the peer share request threashold).
--
belowTarget :: (MonadAsync m, MonadTimer m, Ord peeraddr)
            => PeerSelectionActions peeraddr peerconn m
            -> MkGuardedDecision peeraddr peerconn m
belowTarget actions
            policy@PeerSelectionPolicy {
              policyMaxInProgressPeerShareReqs,
              policyPickKnownPeersForPeerShare,
              policyPeerShareRetryTime
            }
            st@PeerSelectionState {
              knownPeers,
              establishedPeers,
              inProgressPeerShareReqs,
              targets = PeerSelectionTargets {
                          targetNumberOfKnownPeers
                        }
            }
    -- Are we under target for number of known peers?
  | numKnownPeers < targetNumberOfKnownPeers

    -- Are we at our limit for number of peer share requests?
  , numPeerShareReqsPossible > 0

    -- Are there any known peers that we can send a peer share request to?
    -- We can only ask ones where we have not asked them within a certain time.
  , not (Set.null availableForPeerShare)
  = Guarded Nothing $ do
      selectedForPeerShare <- pickPeers st
                             policyPickKnownPeersForPeerShare
                             availableForPeerShare
                             numPeerShareReqsPossible
      let numPeerShareReqs = Set.size selectedForPeerShare
      return $ \now -> Decision {
        decisionTrace = [TracePeerShareRequests
                          targetNumberOfKnownPeers
                          numKnownPeers
                          availableForPeerShare
                          selectedForPeerShare],
        decisionState = st {
                          inProgressPeerShareReqs = inProgressPeerShareReqs
                                                  + numPeerShareReqs,
                          establishedPeers = EstablishedPeers.setPeerShareTime
                                              selectedForPeerShare
                                              (addTime policyPeerShareRetryTime now)
                                              establishedPeers
                        },
        decisionJobs  = [jobPeerShare actions policy
                           (Set.toList selectedForPeerShare)]
      }

    -- If we could peer share except that there are none currently available
    -- then we return the next wakeup time (if any)
  | numKnownPeers < targetNumberOfKnownPeers
  , numPeerShareReqsPossible > 0
  , Set.null availableForPeerShare
  = GuardedSkip (Min <$> EstablishedPeers.minPeerShareTime establishedPeers)

  | otherwise
  = GuardedSkip Nothing
  where
    numKnownPeers            = KnownPeers.size knownPeers
    numPeerShareReqsPossible = policyMaxInProgressPeerShareReqs
                             - inProgressPeerShareReqs
    availableForPeerShare    = EstablishedPeers.availableForPeerShare establishedPeers


jobPeerShare :: forall m peeraddr peerconn.
             (MonadAsync m, MonadTimer m, Ord peeraddr)
             => PeerSelectionActions peeraddr peerconn m
             -> PeerSelectionPolicy peeraddr m
             -> [peeraddr]
             -> Job () m (Completion m peeraddr peerconn)
jobPeerShare PeerSelectionActions{requestPeerShare}
             PeerSelectionPolicy{..} =
    \peers -> Job (jobPhase1 peers) (handler peers) () "peerSharePhase1"
  where
    handler :: [peeraddr] -> SomeException -> m (Completion m peeraddr peerconn)
    handler peers e = return $
      Completion $ \st _ ->
      Decision {
        decisionTrace = [TracePeerShareResults [ (p, Left e) | p <- peers ]],
        decisionState = st {
                          inProgressPeerShareReqs = inProgressPeerShareReqs st
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
      peerShares <- sequence [ async (requestPeerShare peer) | peer <- peers ]

      -- First to finish synchronisation between /all/ the peer share requests
      -- completing or the timeout (with whatever partial results we have at
      -- the time)
      results <- waitAllCatchOrTimeout peerShares policyPeerShareBatchWaitTime
      case results of
        Right totalResults -> do
          let peerResults = zip peers totalResults
              newPeers    = [ p | Right ps <- totalResults, p <- ps ]
          return $ Completion $ \st _ -> Decision {
            decisionTrace = [TracePeerShareResults peerResults],
            decisionState = st { -- TODO: also update with the failures
                                 --
                                 -- TODO: Update logic to check for ledger peers before adding
                                 knownPeers = KnownPeers.insert
                                                (Map.fromList
                                                 $ map (\a -> ( a
                                                              , ( NoPeerSharing
                                                                , DoAdvertisePeer
                                                                , IsNotLedgerPeer))
                                                       )
                                                       newPeers)
                                                (knownPeers st),
                                 inProgressPeerShareReqs = inProgressPeerShareReqs st
                                                         - length peers
                            },
            decisionJobs  = []
          }

        -- But if any don't make the first timeout then they'll be added later
        -- when they do reply or never if we hit the hard timeout.
        Left partialResults -> do

          -- We have to keep track of the relationship between the peer
          -- addresses and the peer share requests, completed and still in progress:
          let peerResults      = [ (p, r)
                                 | (p, Just r)  <- zip peers   partialResults ]
              newPeers         = [  p
                                 | Just (Right ps) <-          partialResults
                                 ,  p <- ps ]
              peersRemaining   = [  p
                                 | (p, Nothing) <- zip peers   partialResults ]
              peerSharesRemaining = [  a
                                    | (a, Nothing) <- zip peerShares partialResults ]

          return $ Completion $ \st _ -> Decision {
            decisionTrace = [TracePeerShareResults peerResults],
            decisionState = st { -- TODO: also update with the failures
                                 --
                                 -- TODO: Update logic to check for ledger peers before adding
                                 knownPeers = KnownPeers.insert
                                                (Map.fromList
                                                 $ map (\a -> ( a
                                                              , ( NoPeerSharing
                                                                , DoAdvertisePeer
                                                                , IsNotLedgerPeer))
                                                       )
                                                       newPeers)
                                                (knownPeers st),
                                 inProgressPeerShareReqs = inProgressPeerShareReqs st
                                                         - length peerResults
                            },
            decisionJobs  = [Job (jobPhase2 peersRemaining peerSharesRemaining)
                                 (handler peersRemaining)
                                 ()
                                 "peerSharePhase2"]
          }

    jobPhase2 :: [peeraddr] -> [Async m [peeraddr]]
              -> m (Completion m peeraddr peerconn)
    jobPhase2 peers peerShares = do

      -- Wait again, for all remaining to finish or a timeout.
      results <- waitAllCatchOrTimeout
                      peerShares
                      (policyPeerShareOverallTimeout
                       - policyPeerShareBatchWaitTime)
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

          peerSharesIncomplete =
            case results of
              Right _totalResults -> []
              Left partialResults ->
                [ a | (a, Nothing) <- zip peerShares partialResults ]

      mapM_ cancel peerSharesIncomplete

      return $ Completion $ \st _ -> Decision {
        decisionTrace = [TracePeerShareResults peerResults],
        decisionState = st { -- TODO: also update with the failures
                             --
                             -- TODO: Update logic to check for ledger peers before adding
                             knownPeers = KnownPeers.insert
                                            (Map.fromList
                                             $ map (\a -> ( a
                                                          , ( NoPeerSharing
                                                            , DoAdvertisePeer
                                                            , IsNotLedgerPeer))
                                                   )
                                                   newPeers)
                                            (knownPeers st),
                             inProgressPeerShareReqs = inProgressPeerShareReqs st
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
                decisionTrace = [TraceForgetColdPeers
                                   targetNumberOfKnownPeers
                                   numKnownPeers
                                   selectedToForget],
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
