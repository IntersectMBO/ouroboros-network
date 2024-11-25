{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Ouroboros.Network.PeerSelection.Governor.BigLedgerPeers
  ( belowTarget
  , aboveTarget
  ) where

import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import GHC.Stack (HasCallStack)

import Control.Applicative (Alternative)
import Control.Concurrent.JobPool (Job (..))
import Control.Exception (SomeException)
import Control.Monad.Class.MonadSTM
import Control.Monad.Class.MonadTime.SI

import Ouroboros.Network.PeerSelection.Governor.Types
import Ouroboros.Network.PeerSelection.LedgerPeers (LedgerPeersKind (..))
import Ouroboros.Network.PeerSelection.PeerAdvertise (PeerAdvertise (..))
import Ouroboros.Network.PeerSelection.PublicRootPeers (PublicRootPeers)
import Ouroboros.Network.PeerSelection.PublicRootPeers qualified as PublicRootPeers
import Ouroboros.Network.PeerSelection.State.KnownPeers qualified as KnownPeers
import Ouroboros.Network.PeerSelection.State.LocalRootPeers qualified as LocalRootPeers
import Ouroboros.Network.PeerSelection.Types (PublicExtraPeersActions (..))


belowTarget :: (MonadSTM m, Ord peeraddr, Semigroup extraPeers)
            => (extraState -> Bool)
            -> PeerSelectionActions extraState extraActions extraPeers extraFlags extraAPI extraCounters peeraddr peerconn m
            -> Time
            -> PeerSelectionState extraState extraFlags extraPeers peeraddr peerconn
            -> Guarded (STM m) (TimedDecision m extraState extraFlags extraPeers peeraddr peerconn)
belowTarget enableAction
            actions@PeerSelectionActions {
              extraPeersActions = PublicExtraPeersActions {
                extraPeersToSet
              },
              extraStateToExtraCounters
            }
            blockedAt
            st@PeerSelectionState {
              bigLedgerPeerRetryTime,
              inProgressBigLedgerPeersReq,
              targets = PeerSelectionTargets {
                          targetNumberOfKnownBigLedgerPeers
                        },
              extraState
            }
    | enableAction extraState

      -- Do we need more big ledger peers?
    , maxExtraBigLedgerPeers > 0

    , not inProgressBigLedgerPeersReq

    , blockedAt >= bigLedgerPeerRetryTime
    = Guarded Nothing $
        return $ \_now -> Decision {
          decisionTrace = [TraceBigLedgerPeersRequest
                             targetNumberOfKnownBigLedgerPeers
                             numBigLedgerPeers],
          decisionState = st { inProgressBigLedgerPeersReq = True },
          decisionJobs  = [jobReqBigLedgerPeers actions maxExtraBigLedgerPeers]
        }

    | otherwise
    = GuardedSkip Nothing
  where
    PeerSelectionCounters {
        numberOfKnownBigLedgerPeers = numBigLedgerPeers
      }
      =
      peerSelectionStateToCounters extraPeersToSet extraStateToExtraCounters st

    maxExtraBigLedgerPeers = targetNumberOfKnownBigLedgerPeers
                           - numBigLedgerPeers


jobReqBigLedgerPeers :: forall m extraActions extraState extraFlags extraAPI extraPeers extraCounters peeraddr peerconn.
                        (MonadSTM m, Ord peeraddr, Semigroup extraPeers)
                     => PeerSelectionActions extraState extraActions extraPeers extraFlags extraAPI extraCounters peeraddr peerconn m
                     -> Int
                     -> Job () m (Completion m extraState extraFlags extraPeers peeraddr peerconn)
jobReqBigLedgerPeers PeerSelectionActions {
                       extraPeersActions = PublicExtraPeersActions {
                         extraPeersToSet,
                         differenceExtraPeers,
                         nullExtraPeers
                       },
                       requestPublicRootPeers
                     }
                     numExtraAllowed =
    Job job (return . handler) () "reqBigLedgerPeers"
  where
    handler :: SomeException -> Completion m extraState extraFlags extraPeers peeraddr peerconn
    handler e =
      Completion $ \st now ->
      -- This is a failure, so move the backoff counter one in the failure
      -- direction (negative) and schedule the next retry time accordingly.
      -- We use an exponential backoff strategy. The max retry time of 2^8
      -- seconds is just over 4 minutes.
      let bigLedgerPeerBackoffs'      :: Int
          bigLedgerPeerBackoffs'      = (bigLedgerPeerBackoffs st `min` 0) - 1

          bigLedgerPeerRetryDiffTime' :: DiffTime
          bigLedgerPeerRetryDiffTime' = 2 ^ (abs bigLedgerPeerBackoffs' `min` 8)

          bigLedgerPeerRetryTime'     :: Time
          bigLedgerPeerRetryTime'     = addTime bigLedgerPeerRetryDiffTime' now
       in Decision {
            decisionTrace = [TraceBigLedgerPeersFailure
                               e
                               bigLedgerPeerBackoffs'
                               bigLedgerPeerRetryDiffTime'],
            decisionState = st {
                              inProgressBigLedgerPeersReq = False,
                              bigLedgerPeerBackoffs       = bigLedgerPeerBackoffs',
                              bigLedgerPeerRetryTime      = bigLedgerPeerRetryTime'
                            },
            decisionJobs  = []
          }

    job :: m (Completion m extraState extraFlags extraPeers peeraddr peerconn)
    job = do
      (results, ttl) <- requestPublicRootPeers BigLedgerPeers numExtraAllowed
      return $ Completion $ \st now ->
        let -- We make sure the set of big ledger peers disjoint from the sum
            -- of local, public and ledger peers.
            newPeers :: PublicRootPeers extraPeers peeraddr
            newPeers =
              PublicRootPeers.difference differenceExtraPeers
                results
                (   LocalRootPeers.keysSet (localRootPeers st)
                 <> PublicRootPeers.toSet extraPeersToSet (publicRootPeers st))

            newPeersSet      = PublicRootPeers.toSet extraPeersToSet newPeers
            publicRootPeers' =
              PublicRootPeers.mergeG extraPeersToSet
                (publicRootPeers st) newPeers

            knownPeers'
                     = KnownPeers.insert
                         (Map.fromSet (\_ -> ( Nothing
                                               -- the peer sharing flag will be
                                               -- updated once we negotiate
                                               -- the connection
                                             , Just DoNotAdvertisePeer
                                             ))
                                      newPeersSet)
                         (knownPeers st)

            -- We got a successful response to our request, but if we're still
            -- below target we're going to want to try again at some point.
            -- If we made progress towards our target then we will retry at the
            -- suggested ttl. But if we did not make progress then we want to
            -- follow an exponential backoff strategy. The max retry time of 2^8
            -- seconds is just over four minutes.
            bigLedgerPeerBackoffs' :: Int
            bigLedgerPeerBackoffs'
              | PublicRootPeers.null nullExtraPeers newPeers = (bigLedgerPeerBackoffs st `max` 0) + 1
              | otherwise = 0

            bigLedgerPeerRetryDiffTime :: DiffTime
            bigLedgerPeerRetryDiffTime
              | bigLedgerPeerBackoffs' == 0
                          = ttl
              | otherwise = 2^(bigLedgerPeerBackoffs' `min` 8)

            bigLedgerPeerRetryTime :: Time
            bigLedgerPeerRetryTime = bigLedgerPeerRetryDiffTime `addTime` now

         in Decision {
               decisionTrace = [TraceBigLedgerPeersResults
                                 newPeersSet
                                 bigLedgerPeerBackoffs'
                                 bigLedgerPeerRetryDiffTime],
               decisionState = st {
                                 publicRootPeers             = publicRootPeers',
                                 knownPeers                  = knownPeers',
                                 bigLedgerPeerBackoffs       = bigLedgerPeerBackoffs',
                                 bigLedgerPeerRetryTime      = bigLedgerPeerRetryTime,
                                 inProgressBigLedgerPeersReq = False
                               },
               decisionJobs  = []
             }


aboveTarget :: forall m extraState extraActions extraFlags extraAPI extraPeers extraCounters peeraddr peerconn.
               (Alternative (STM m), MonadSTM m, Ord peeraddr, HasCallStack)
                 => PeerSelectionActions extraState extraActions extraPeers extraFlags extraAPI extraCounters peeraddr peerconn m
                 -> MkGuardedDecision extraState extraFlags extraPeers peeraddr peerconn m
aboveTarget PeerSelectionActions {
              extraPeersActions = PublicExtraPeersActions {
                extraPeersToSet,
                differenceExtraPeers,
                memberExtraPeers
              }
              , extraStateToExtraCounters
            }
            PeerSelectionPolicy {policyPickColdPeersToForget}
            st@PeerSelectionState {
                 publicRootPeers,
                 knownPeers,
                 inProgressPromoteCold,
                 targets = PeerSelectionTargets {
                             targetNumberOfKnownBigLedgerPeers
                           }
               }
    -- Are we above the target for number of known big ledger peers
    | numKnownBigLedgerPeers > targetNumberOfKnownBigLedgerPeers

    -- Are there any cold big ledger peers we could pick to forget?
    , numKnownBigLedgerPeers > numEstablishedBigLedgerPeers

    , let availableToForget :: Set peeraddr
          availableToForget = bigLedgerPeersSet
                                Set.\\ establishedBigLedgerPeers
                                Set.\\ inProgressPromoteCold

    , not (Set.null availableToForget)
    = Guarded Nothing $ do
        let numPeersCanForget = numKnownBigLedgerPeers
                              - targetNumberOfKnownBigLedgerPeers
        selectedToForget <- pickPeers memberExtraPeers st
                                      policyPickColdPeersToForget
                                      availableToForget
                                      numPeersCanForget
        return $ \_now ->
          let knownPeers'     = KnownPeers.delete selectedToForget knownPeers
              publicRootPeers' =
                PublicRootPeers.difference differenceExtraPeers
                  publicRootPeers selectedToForget
          in Decision {
               decisionTrace = [TraceForgetBigLedgerPeers
                                  targetNumberOfKnownBigLedgerPeers
                                  numKnownBigLedgerPeers
                                  selectedToForget
                               ],
               decisionState = st { knownPeers      = knownPeers',
                                    publicRootPeers = publicRootPeers'
                                  },
               decisionJobs  = []
             }

    | otherwise
    = GuardedSkip Nothing
  where
    bigLedgerPeersSet = PublicRootPeers.getBigLedgerPeers publicRootPeers

    PeerSelectionView {
        viewKnownBigLedgerPeers       = (_, numKnownBigLedgerPeers),
        viewEstablishedBigLedgerPeers = (establishedBigLedgerPeers, numEstablishedBigLedgerPeers)
      } = peerSelectionStateToView extraPeersToSet extraStateToExtraCounters st
