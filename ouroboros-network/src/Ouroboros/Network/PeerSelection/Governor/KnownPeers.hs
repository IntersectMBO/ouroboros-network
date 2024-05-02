{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Ouroboros.Network.PeerSelection.Governor.KnownPeers
  ( belowTarget
  , aboveTarget
  ) where

import Data.Hashable
import Data.List (sortBy)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Set qualified as Set
import GHC.Stack (HasCallStack)
import System.Random (random)

import Control.Concurrent.JobPool (Job (..))
import Control.Exception (Exception (..), SomeException, assert)
import Control.Monad.Class.MonadAsync
import Control.Monad.Class.MonadSTM
import Control.Monad.Class.MonadTime.SI
import Control.Monad.Class.MonadTimer.SI

import Ouroboros.Network.Diffusion.Policies qualified as Policies
import Ouroboros.Network.PeerSelection.Bootstrap (requiresBootstrapPeers)
import Ouroboros.Network.PeerSelection.Governor.Types
import Ouroboros.Network.PeerSelection.PeerAdvertise (PeerAdvertise (..))
import Ouroboros.Network.PeerSelection.PeerSharing (PeerSharing (..))
import Ouroboros.Network.PeerSelection.PublicRootPeers qualified as PublicRootPeers
import Ouroboros.Network.PeerSelection.State.EstablishedPeers qualified as EstablishedPeers
import Ouroboros.Network.PeerSelection.State.KnownPeers qualified as KnownPeers
import Ouroboros.Network.PeerSelection.State.LocalRootPeers qualified as LocalRootPeers
import Ouroboros.Network.Protocol.PeerSharing.Type (PeerSharingAmount)


---------------------------
-- Known peers below target
--

-- | If we are below the target of /known peers/ we flip a coin to either get
-- new peers from:
--
-- * inbound connections; or
-- * peer share (if we are above the peer share request threshold).
--
-- It should be noted if the node is in bootstrap mode (i.e. in a sensitive
-- state) then this monitoring action will be disabled.
--
belowTarget
    :: (MonadAsync m, MonadTimer m, Ord peeraddr, Hashable peeraddr)
    => PeerSelectionActions peeraddr peerconn m
    -> Time -- ^ blocked at
    -> Map peeraddr PeerSharing
    -> MkGuardedDecision peeraddr peerconn m
belowTarget actions@PeerSelectionActions { peerSharing }
            blockedAt
            inboundPeers
            policy@PeerSelectionPolicy {
              policyMaxInProgressPeerShareReqs,
              policyPickKnownPeersForPeerShare,
              policyPickInboundPeers,
              policyPeerShareRetryTime
            }
            st@PeerSelectionState {
              knownPeers,
              establishedPeers,
              inProgressPeerShareReqs,
              inProgressDemoteToCold,
              inboundPeersRetryTime,
              targets = PeerSelectionTargets {
                          targetNumberOfKnownPeers
                        },
              ledgerStateJudgement,
              bootstrapPeersFlag,
              stdGen
            }
    --
    -- Light peer sharing
    --

  | PeerSharingEnabled <- peerSharing
    -- Are we under target for number of known peers?
  , numKnownPeers < targetNumberOfKnownPeers

    -- There are no peer share requests in-flight.
  , inProgressPeerShareReqs <= 0

    -- No inbound peers should be used when the node is using bootstrap peers.
  , not (requiresBootstrapPeers bootstrapPeersFlag ledgerStateJudgement)

  , blockedAt >= inboundPeersRetryTime

    -- Use inbound peers either if it won the coin flip or if there are no
    -- available peers to do peer sharing.
  , useInboundPeers || Set.null availableForPeerShare

  , let availablePeers = Map.keysSet inboundPeers
                  Set.\\ KnownPeers.toSet knownPeers
  , not (Set.null availablePeers)

  = Guarded Nothing $ do
      selected <- pickUnknownPeers
                  st
                  policyPickInboundPeers
                  availablePeers
                  (Policies.maxInboundPeers `min` (targetNumberOfKnownPeers - numKnownPeers))
      let selectedMap = inboundPeers `Map.restrictKeys` selected
      return $ \now -> Decision {
          decisionTrace = [TracePickInboundPeers
                            targetNumberOfKnownPeers
                            numKnownPeers
                            selectedMap
                            availablePeers
                          ],
          decisionState = st { knownPeers = KnownPeers.setSuccessfulConnectionFlag selected
                                          $ KnownPeers.insert
                                              (Map.map (\ps -> (Just ps, Just DoAdvertisePeer)) selectedMap)
                                              knownPeers,
                               inboundPeersRetryTime = Policies.inboundPeersRetryDelay `addTime` now,
                               stdGen = stdGen'

                             },
          decisionJobs = []
        }

    --
    -- Peer sharing
    --

  | PeerSharingEnabled <- peerSharing
    -- Are we under target for number of known peers?
  , numKnownPeers < targetNumberOfKnownPeers

    -- Are we at our limit for number of peer share requests?
  , numPeerShareReqsPossible > 0

    -- Are there any known peers that we can send a peer share request to?
    -- We can only ask ones where we have not asked them within a certain time.
  , not (Set.null availableForPeerShare)

    -- No peer share requests should be issued when the node is using bootstrap
    -- peers.
  , not (requiresBootstrapPeers bootstrapPeersFlag ledgerStateJudgement)

  = Guarded Nothing $ do
      -- Max selected should be <= numPeerShareReqsPossible
      selectedForPeerShare <- pickPeers st
                              policyPickKnownPeersForPeerShare
                              availableForPeerShare
                              numPeerShareReqsPossible

      let -- Should be <= numPeerShareReqsPossible
          numPeerShareReqs = Set.size selectedForPeerShare
          objective        = targetNumberOfKnownPeers - numKnownPeers
          -- Split current peer target objective across all peer sharing
          -- candidates. If the objective is smaller than the number of
          -- peer share requests available, ask for at 1 peer to each.
          --
          -- This is to increase diversity.
          numPeersToReq :: PeerSharingAmount
          !numPeersToReq = fromIntegral
                         $ min 255 (max 8 (objective `div` numPeerShareReqs))
          (salt, stdGen'') = random stdGen'

      return $ \now -> Decision {
        decisionTrace = [TracePeerShareRequests
                          targetNumberOfKnownPeers
                          numKnownPeers
                          numPeersToReq
                          availableForPeerShare
                          selectedForPeerShare],
        decisionState = st {
                          inProgressPeerShareReqs = inProgressPeerShareReqs
                                                  + numPeerShareReqs,
                          establishedPeers = EstablishedPeers.setPeerShareTime
                                              selectedForPeerShare
                                              (addTime policyPeerShareRetryTime now)
                                              establishedPeers,
                          stdGen = stdGen''
                        },
        decisionJobs  =
          [jobPeerShare actions policy objective salt numPeersToReq
             (Set.toList selectedForPeerShare)]
      }

    -- If we could peer share except that there are none currently available
    -- then we return the next wakeup time (if any)
  | numKnownPeers < targetNumberOfKnownPeers
  , numPeerShareReqsPossible > 0
  , Set.null availableForPeerShare
  = GuardedSkip $ EstablishedPeers.minPeerShareTime establishedPeers

  | otherwise
  = GuardedSkip Nothing
  where
    (useInboundPeers, stdGen') = random stdGen
    PeerSelectionCounters {
        numberOfKnownPeers = numKnownPeers
      }
      =
      peerSelectionStateToCounters st
    numPeerShareReqsPossible = policyMaxInProgressPeerShareReqs
                             - inProgressPeerShareReqs
    -- Only peer which permit peersharing are available
    availableForPeerShare    = EstablishedPeers.availableForPeerShare establishedPeers
                             Set.\\ inProgressDemoteToCold

---------------------------
-- Peer sharing job
--


-- | The peer sharing job is run in two stages. The expected path is for all
-- peer sharing request to return within a short timeout. The second phase is
-- with a longer timeout for all still outstanding requests.
--
-- The result from each phase is filtered. Already known peers and big ledger
-- peers are removed before adding them to known peers. Big ledger peers are
-- popular so they don't need to be shared through peer sharing. However ledger
-- peers belonging to smaller pools shouldn't be discarded. Smaller pools could
-- use extra upstream peers and we spread out the load in the network.
--
-- If we ask for more peers than needed a random subset of the peers in the filtered result
-- is used.
jobPeerShare :: forall m peeraddr peerconn.
                (MonadAsync m, MonadTimer m, Ord peeraddr, Hashable peeraddr)
             => PeerSelectionActions peeraddr peerconn m
             -> PeerSelectionPolicy peeraddr m
             -> Int
             -> Int
             -> PeerSharingAmount
             -> [peeraddr]
             -> Job () m (Completion m peeraddr peerconn)
jobPeerShare PeerSelectionActions{requestPeerShare}
             PeerSelectionPolicy{..} salt maxAmount =
    \amount peers -> Job (jobPhase1 amount peers) (handler peers) () "peerSharePhase1"
  where
    -- Return n random peers from a list of peers.
    --
    -- Every jobPeerShare will be called with a new random salt.
    -- This means that even if presented with the same list peers their ordering
    -- will be unpredictable.
    takeNPeers :: Int -> [peeraddr] -> [peeraddr]
    takeNPeers n addrs = take n $
      sortBy (\a b -> compare (hashWithSalt salt a) (hashWithSalt salt b))
      addrs

    handler :: [peeraddr] -> SomeException -> m (Completion m peeraddr peerconn)
    handler peers e = return $
      Completion $ \st _ ->
      Decision { decisionTrace = [TracePeerShareResults [ (p, Left e) | p <- peers ]],
                 decisionState =
                  st { inProgressPeerShareReqs = inProgressPeerShareReqs st
                                               - length peers
                     },
                 decisionJobs = []
               }

    jobPhase1 :: PeerSharingAmount -> [peeraddr] -> m (Completion m peeraddr peerconn)
    jobPhase1 amount peers = do
      -- In the typical case, where most requests return within a short
      -- timeout we want to collect all the responses into a batch and
      -- add them to the known peers set in one go.
      --
      -- So fire them all off in one go:
      peerShares <- sequence [ async (requestPeerShare amount peer)
                             | peer <- peers ]

      -- First to finish synchronisation between /all/ the peer share requests
      -- completing or the timeout (with whatever partial results we have at
      -- the time)
      results <- waitAllCatchOrTimeout peerShares policyPeerShareBatchWaitTime
      case results of
        Right totalResults ->
          return $ Completion $ \st _ ->
           let peerResults = zip peers totalResults
               newPeers    = takeNPeers maxAmount $
                                 [ p | Right (PeerSharingResult ps) <- totalResults
                                 , p <- ps
                                 , not (KnownPeers.member p (knownPeers st))
                                 , Set.notMember p (PublicRootPeers.getBigLedgerPeers (publicRootPeers st))]
            in Decision { decisionTrace = [ TracePeerShareResults peerResults
                                          , TracePeerShareResultsFiltered newPeers
                                          ]
                        , decisionState =
                           st { -- TODO: also update with the failures
                                knownPeers = KnownPeers.alter
                                              (\x -> case x of
                                                Nothing ->
                                                  KnownPeers.alterKnownPeerInfo
                                                    (Nothing, Just DoAdvertisePeer)
                                                    x
                                                Just _ ->
                                                  KnownPeers.alterKnownPeerInfo
                                                    (Nothing, Nothing)
                                                    x
                                              )
                                              (Set.fromList newPeers)
                                              (knownPeers st),
                                inProgressPeerShareReqs = inProgressPeerShareReqs st
                                                        - length peers
                           }
                        , decisionJobs  = []
                        }

        -- But if any don't make the first timeout then they'll be added later
        -- when they do reply or never if we hit the hard timeout.
        Left partialResults -> do

          -- We have to keep track of the relationship between the peer
          -- addresses and the peer share requests, completed and still in progress:
          let peerResults      = [ (p, r)
                                 | (p, Just r)  <- zip peers partialResults ]
              peersRemaining   = [  p
                                 | (p, Nothing) <- zip peers partialResults ]
              peerSharesRemaining = [  a
                                    | (a, Nothing) <- zip peerShares partialResults ]

          return $ Completion $ \st _ ->
            let newPeers = takeNPeers maxAmount $
                               [ p | Just (Right (PeerSharingResult ps)) <- partialResults
                               , p <- ps
                               , not (KnownPeers.member p (knownPeers st))
                               , Set.notMember p (PublicRootPeers.getBigLedgerPeers (publicRootPeers st))]
             in Decision { decisionTrace = [ TracePeerShareResults peerResults
                                           , TracePeerShareResultsFiltered newPeers
                                           ]
                         , decisionState =
                            st { -- TODO: also update with the failures
                                 knownPeers = KnownPeers.alter
                                               (\x -> case x of
                                                 Nothing ->
                                                   KnownPeers.alterKnownPeerInfo
                                                     (Nothing, Just DoAdvertisePeer)
                                                     x
                                                 Just _ ->
                                                   KnownPeers.alterKnownPeerInfo
                                                     (Nothing, Nothing)
                                                     x
                                               )
                                               (Set.fromList newPeers)
                                               (knownPeers st),
                                 inProgressPeerShareReqs = inProgressPeerShareReqs st
                                                         - length peerResults
                               }
                         , decisionJobs  = [Job (jobPhase2 (maxAmount - length newPeers) peersRemaining
                                                 peerSharesRemaining)
                                                (handler peersRemaining)
                                                ()
                                                "peerSharePhase2"]
                         }

    jobPhase2 :: Int -> [peeraddr] -> [Async m (PeerSharingResult peeraddr)]
              -> m (Completion m peeraddr peerconn)
    jobPhase2 maxRemaining peers peerShares = do

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

          peerSharesIncomplete =
            case results of
              Right _totalResults -> []
              Left partialResults ->
                [ a | (a, Nothing) <- zip peerShares partialResults ]

      mapM_ cancel peerSharesIncomplete

      return $ Completion $ \st _ ->
        let newPeers = takeNPeers maxRemaining $
              case results of
                Right totalResults  -> [ p | Right (PeerSharingResult ps) <- totalResults
                                           , p <- ps
                                           , not (KnownPeers.member p (knownPeers st))
                                           , Set.notMember p (PublicRootPeers.getBigLedgerPeers (publicRootPeers st))]
                Left partialResults -> [ p | Just (Right (PeerSharingResult ps)) <- partialResults
                                           , p <- ps
                                           , not (KnownPeers.member p (knownPeers st))
                                           , Set.notMember p (PublicRootPeers.getBigLedgerPeers (publicRootPeers st))]

         in Decision { decisionTrace = [ TracePeerShareResults peerResults
                                       , TracePeerShareResultsFiltered newPeers
                                       ]
                     , decisionState =
                        st { -- TODO: also update with the failures
                             knownPeers = KnownPeers.alter
                                           (\x -> case x of
                                             Nothing ->
                                               KnownPeers.alterKnownPeerInfo
                                                 (Nothing, Just DoAdvertisePeer)
                                                 x
                                             Just _ ->
                                               KnownPeers.alterKnownPeerInfo
                                                 (Nothing, Nothing)
                                                 x
                                           )
                                           (Set.fromList newPeers)
                                           (knownPeers st),
                             inProgressPeerShareReqs = inProgressPeerShareReqs st
                                                     - length peers
                           }
                     , decisionJobs  = []
                     }


---------------------------
-- Known peers above target
--


-- | If we are above the target of /known peers/ (i.e. /cold/, /warm/ and /hot/
-- combined), we drop some of the /cold peers/ but we protect the
-- 'targetNumberOfRootPeers' (from combined sets of /local/ and /public root/
-- peers). 'policyPickColdPeersToForget' policy is used to pick the peers.
--
aboveTarget :: (MonadSTM m, Ord peeraddr, HasCallStack)
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
                              + PublicRootPeers.size publicRootPeers
                              - targetNumberOfRootPeers
        availableToForget     = KnownPeers.toSet knownPeers
                                  Set.\\ EstablishedPeers.toSet establishedPeers
                                  Set.\\ LocalRootPeers.keysSet localRootPeers
                                  Set.\\ (if numRootPeersCanForget <= 0
                                            then PublicRootPeers.toSet publicRootPeers
                                            else Set.empty)
                                  Set.\\ inProgressPromoteCold
                                  Set.\\ bigLedgerPeersSet

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
                                `PublicRootPeers.difference` selectedToForget
        in assert (Set.isSubsetOf
                     (PublicRootPeers.toSet publicRootPeers')
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
    bigLedgerPeersSet = PublicRootPeers.getBigLedgerPeers publicRootPeers

    PeerSelectionCounters {
        numberOfKnownPeers       = numKnownPeers,
        numberOfEstablishedPeers = numEstablishedPeers
      }
      =
      peerSelectionStateToCounters st


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
    (readTimeout, cancelTimeout) <- registerDelayCancellable time
    results <- atomically $
                         (Right <$> mapM waitCatchSTM as)
                `orElse` (Left  <$> (readTimeout >>= \case TimeoutPending -> retry
                                                           _              -> mapM pollSTM as))
    case results of
      Right{} -> cancelTimeout
      _       -> return ()
    return results
