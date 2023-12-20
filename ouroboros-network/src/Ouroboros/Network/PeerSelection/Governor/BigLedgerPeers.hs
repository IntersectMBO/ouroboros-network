{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Ouroboros.Network.PeerSelection.Governor.BigLedgerPeers
  ( belowTarget
  , aboveTarget
  ) where

import qualified Data.Map.Strict as Map
import           Data.Set (Set)
import qualified Data.Set as Set

import           Control.Applicative (Alternative)
import           Control.Concurrent.JobPool (Job (..))
import           Control.Exception (SomeException)
import           Control.Monad.Class.MonadSTM
import           Control.Monad.Class.MonadTime.SI

import           Ouroboros.Network.PeerSelection.Governor.Types
import           Ouroboros.Network.PeerSelection.LedgerPeers (IsLedgerPeer (..))
import           Ouroboros.Network.PeerSelection.PeerAdvertise
                     (PeerAdvertise (..))
import qualified Ouroboros.Network.PeerSelection.State.EstablishedPeers as EstablishedPeers
import qualified Ouroboros.Network.PeerSelection.State.KnownPeers as KnownPeers
import qualified Ouroboros.Network.PeerSelection.State.LocalRootPeers as LocalRootPeers


belowTarget :: (MonadSTM m, Ord peeraddr)
            => PeerSelectionActions peeraddr peerconn m
            -> Time
            -> PeerSelectionState peeraddr peerconn
            -> Guarded (STM m) (TimedDecision m peeraddr peerconn)
belowTarget actions
            blockedAt
            st@PeerSelectionState {
              bigLedgerPeers,
              bigLedgerPeerRetryTime,
              inProgressBigLedgerPeersReq,
              targets = PeerSelectionTargets {
                          targetNumberOfKnownBigLedgerPeers
                        }
            }
      -- Do we need more big ledger peers?
    | maxExtraBigLedgerPeers > 0

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
    numBigLedgerPeers      = Set.size bigLedgerPeers
    maxExtraBigLedgerPeers = targetNumberOfKnownBigLedgerPeers
                           - numBigLedgerPeers


jobReqBigLedgerPeers :: forall m peeraddr peerconn.
                        (MonadSTM m, Ord peeraddr)
                     => PeerSelectionActions peeraddr peerconn m
                     -> Int
                     -> Job () m (Completion m peeraddr peerconn)
jobReqBigLedgerPeers PeerSelectionActions{ requestBigLedgerPeers }
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
      (results, ttl) <- requestBigLedgerPeers numExtraAllowed
      return $ Completion $ \st now ->
        let -- We make sure the set of big ledger peers disjoint from the sum
            -- of local, public and ledger peers.
            newPeers :: Set peeraddr
            newPeers = results Set.\\ LocalRootPeers.keysSet (localRootPeers st)
                               Set.\\ publicRootPeers st

            bigLedgerPeers' = bigLedgerPeers st `Set.union` newPeers

            knownPeers'
                     = KnownPeers.insert
                         (Map.fromSet (\_ -> ( Nothing
                                               -- the peer sharing flag will be
                                               -- updated once we negotiate
                                               -- the connection
                                             , Just DoNotAdvertisePeer
                                             , Just IsLedgerPeer
                                             ))
                                      newPeers)
                         (knownPeers st)

            -- We got a successful response to our request, but if we're still
            -- below target we're going to want to try again at some point.
            -- If we made progress towards our target then we will retry at the
            -- suggested ttl. But if we did not make progress then we want to
            -- follow an exponential backoff strategy. The max retry time of 2^8
            -- seconds is just over four minutes.
            bigLedgerPeerBackoffs' :: Int
            bigLedgerPeerBackoffs'
              | Set.null newPeers = (bigLedgerPeerBackoffs st `max` 0) + 1
              | otherwise         = 0

            bigLedgerPeerRetryDiffTime :: DiffTime
            bigLedgerPeerRetryDiffTime
              | bigLedgerPeerBackoffs' == 0
                          = ttl
              | otherwise = 2^(bigLedgerPeerBackoffs' `min` 8)

            bigLedgerPeerRetryTime :: Time
            bigLedgerPeerRetryTime = bigLedgerPeerRetryDiffTime `addTime` now

         in Decision {
               decisionTrace = [TraceBigLedgerPeersResults
                                 newPeers
                                 bigLedgerPeerBackoffs'
                                 bigLedgerPeerRetryDiffTime],
               decisionState = st {
                                 bigLedgerPeers              = bigLedgerPeers',
                                 knownPeers                  = knownPeers',
                                 bigLedgerPeerBackoffs       = bigLedgerPeerBackoffs',
                                 bigLedgerPeerRetryTime      = bigLedgerPeerRetryTime,
                                 inProgressBigLedgerPeersReq = False
                               },
               decisionJobs  = []
             }


aboveTarget :: forall m peeraddr peerconn.
               (Alternative (STM m), MonadSTM m, Ord peeraddr)
            => MkGuardedDecision peeraddr peerconn m
aboveTarget PeerSelectionPolicy {policyPickColdPeersToForget}
            st@PeerSelectionState {
                 bigLedgerPeers,
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
          availableToForget = bigLedgerPeers
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
              bigLedgerPeers' = bigLedgerPeers Set.\\ selectedToForget
          in Decision {
               decisionTrace = [TraceForgetBigLedgerPeers
                                  targetNumberOfKnownBigLedgerPeers
                                  numKnownBigLedgerPeers
                                  selectedToForget
                               ],
               decisionState = st { knownPeers     = knownPeers',
                                    bigLedgerPeers = bigLedgerPeers'
                                  },
               decisionJobs  = []
             }

    | otherwise
    = GuardedSkip Nothing
  where
    numKnownBigLedgerPeers :: Int
    numKnownBigLedgerPeers = Set.size bigLedgerPeers

    establishedBigLedgerPeers :: Set peeraddr
    establishedBigLedgerPeers = EstablishedPeers.toSet establishedPeers
                                `Set.intersection`
                                bigLedgerPeers

    numEstablishedBigLedgerPeers :: Int
    numEstablishedBigLedgerPeers = Set.size establishedBigLedgerPeers
