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

import Ouroboros.Network.PeerSelection.Bootstrap (requiresBootstrapPeers)
import Ouroboros.Network.PeerSelection.Governor.Types
import Ouroboros.Network.PeerSelection.LedgerPeers (LedgerPeersKind (..))
import Ouroboros.Network.PeerSelection.PeerAdvertise (PeerAdvertise (..))
import Ouroboros.Network.PeerSelection.PublicRootPeers (PublicRootPeers)
import Ouroboros.Network.PeerSelection.PublicRootPeers qualified as PublicRootPeers
import Ouroboros.Network.PeerSelection.State.EstablishedPeers qualified as EstablishedPeers
import Ouroboros.Network.PeerSelection.State.KnownPeers qualified as KnownPeers
import Ouroboros.Network.PeerSelection.State.LocalRootPeers qualified as LocalRootPeers


belowTarget :: (MonadSTM m, Ord peeraddr)
            => PeerSelectionActions peeraddr peerconn m
            -> Time
            -> PeerSelectionState peeraddr peerconn
            -> Guarded (STM m) (TimedDecision m peeraddr peerconn)
belowTarget actions
            blockedAt
            st@PeerSelectionState {
              bigLedgerPeerRetryTime,
              inProgressBigLedgerPeersReq,
              targets = PeerSelectionTargets {
                          targetNumberOfKnownBigLedgerPeers
                        },
              ledgerStateJudgement,
              bootstrapPeersFlag
            }
      -- Are we in a sensitive state? We shouldn't attempt to fetch ledger peers
      -- in a sensitive state since we only want to connect to trustable peers.
    | not (requiresBootstrapPeers bootstrapPeersFlag ledgerStateJudgement)

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
      peerSelectionStateToCounters st

    maxExtraBigLedgerPeers = targetNumberOfKnownBigLedgerPeers
                           - numBigLedgerPeers


jobReqBigLedgerPeers :: forall m peeraddr peerconn.
                        (MonadSTM m, Ord peeraddr)
                     => PeerSelectionActions peeraddr peerconn m
                     -> Int
                     -> Job () m (Completion m peeraddr peerconn)
jobReqBigLedgerPeers PeerSelectionActions{ requestPublicRootPeers }
                     numExtraAllowed =
    Job job (return . handler) () "reqBigLedgerPeers"
  where
    handler :: SomeException -> Completion m peeraddr peerconn
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
                              inProgressPublicRootsReq = False,
                              publicRootBackoffs  = bigLedgerPeerBackoffs',
                              publicRootRetryTime = bigLedgerPeerRetryTime'
                            },
            decisionJobs  = []
          }

    job :: m (Completion m peeraddr peerconn)
    job = do
      (results, ttl) <- requestPublicRootPeers BigLedgerPeers numExtraAllowed
      return $ Completion $ \st now ->
        let -- We make sure the set of big ledger peers disjoint from the sum
            -- of local, public and ledger peers.
            newPeers :: PublicRootPeers peeraddr
            newPeers = results
                       `PublicRootPeers.difference`
                       (   LocalRootPeers.keysSet (localRootPeers st)
                        <> PublicRootPeers.toSet (publicRootPeers st))

            newPeersSet      = PublicRootPeers.toSet newPeers
            publicRootPeers' = publicRootPeers st <> newPeers

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
              | PublicRootPeers.null newPeers = (bigLedgerPeerBackoffs st `max` 0) + 1
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


aboveTarget :: forall m peeraddr peerconn.
               (Alternative (STM m), MonadSTM m, Ord peeraddr, HasCallStack)
            => MkGuardedDecision peeraddr peerconn m
aboveTarget PeerSelectionPolicy {policyPickColdPeersToForget}
            st@PeerSelectionState {
                 publicRootPeers,
                 knownPeers,
                 establishedPeers,
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
        selectedToForget <- pickPeers st
                                      policyPickColdPeersToForget
                                      availableToForget
                                      numPeersCanForget
        return $ \_now ->
          let knownPeers'     = KnownPeers.delete selectedToForget knownPeers
              publicRootPeers' = PublicRootPeers.difference publicRootPeers selectedToForget
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

    PeerSelectionCounters {
        numberOfKnownBigLedgerPeers = numKnownBigLedgerPeers
      }
      =
      peerSelectionStateToCounters st

    establishedBigLedgerPeers :: Set peeraddr
    establishedBigLedgerPeers = EstablishedPeers.toSet establishedPeers
                                `Set.intersection`
                                bigLedgerPeersSet

    -- TODO: we should compute this with `PeerSelectionCounters`, but we also
    -- need to return the `establishedBigLedgerPeers` set.
    numEstablishedBigLedgerPeers :: Int
    numEstablishedBigLedgerPeers = Set.size establishedBigLedgerPeers
