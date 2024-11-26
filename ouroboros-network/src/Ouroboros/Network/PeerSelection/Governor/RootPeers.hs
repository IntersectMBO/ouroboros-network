{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}

module Ouroboros.Network.PeerSelection.Governor.RootPeers (belowTarget) where

import Data.Map.Strict qualified as Map
import Data.Set qualified as Set

import Control.Concurrent.JobPool (Job (..))
import Control.Exception (SomeException, assert)
import Control.Monad.Class.MonadSTM
import Control.Monad.Class.MonadTime.SI

import Ouroboros.Network.PeerSelection.Governor.Types
import Ouroboros.Network.PeerSelection.LedgerPeers (LedgerPeersKind (..))
import Ouroboros.Network.PeerSelection.PublicRootPeers qualified as PublicRootPeers
import Ouroboros.Network.PeerSelection.State.KnownPeers qualified as KnownPeers
import Ouroboros.Network.PeerSelection.State.LocalRootPeers qualified as LocalRootPeers
import Ouroboros.Network.PeerSelection.Types (PublicExtraPeersActions (..))


--------------------------
-- Root peers below target
--

belowTarget :: (MonadSTM m, Ord peeraddr, Semigroup extraPeers)
            => PeerSelectionActions extraState extraActions extraPeers extraFlags extraAPI extraCounters peeraddr peerconn m
            -> Time
            -> PeerSelectionState extraState extraFlags extraPeers peeraddr peerconn
            -> Guarded (STM m) (TimedDecision m extraState extraFlags extraPeers peeraddr peerconn)
belowTarget actions@PeerSelectionActions {
              extraPeersActions = PublicExtraPeersActions {
                extraPeersToSet
              },
              extraStateToExtraCounters
            }
            blockedAt
            st@PeerSelectionState {
              publicRootRetryTime,
              inProgressPublicRootsReq,
              targets = PeerSelectionTargets {
                          targetNumberOfRootPeers
                        }
            }
    -- Are we under target for number of root peers?
  | maxExtraRootPeers > 0

    -- Are we already requesting more root peers?
  , not inProgressPublicRootsReq

    -- We limit how frequently we make requests, are we allowed to do it yet?
  , blockedAt >= publicRootRetryTime
  = Guarded Nothing $
      return $ \_now -> Decision {
        decisionTrace = [TracePublicRootsRequest
                           targetNumberOfRootPeers
                           numRootPeers],
        decisionState = st { inProgressPublicRootsReq = True },
        decisionJobs  = [jobReqPublicRootPeers actions maxExtraRootPeers]
      }

    -- If we would be able to do the request except for the time, return the
    -- next retry time.
  | maxExtraRootPeers > 0
  , not inProgressPublicRootsReq
  = GuardedSkip (Just publicRootRetryTime)

  | otherwise
  = GuardedSkip Nothing
  where
    PeerSelectionCounters {
        numberOfRootPeers = numRootPeers
      }
      =
      peerSelectionStateToCounters extraPeersToSet extraStateToExtraCounters st

    maxExtraRootPeers = targetNumberOfRootPeers - numRootPeers


jobReqPublicRootPeers :: forall m extraActions extraState extraFlags extraPeers extraAPI extraCounters peeraddr peerconn.
                         (MonadSTM m, Ord peeraddr, Semigroup extraPeers)
                      => PeerSelectionActions extraState extraActions extraPeers extraFlags extraAPI extraCounters peeraddr peerconn m
                      -> Int
                      -> Job () m (Completion m extraState extraFlags extraPeers peeraddr peerconn)
jobReqPublicRootPeers PeerSelectionActions{ requestPublicRootPeers
                                          , extraPeersActions = PublicExtraPeersActions {
                                              differenceExtraPeers
                                            , extraPeersToSet
                                            , nullExtraPeers
                                            , toAdvertise
                                          }
                                          }
                      numExtraAllowed =
    Job job (return . handler) () "reqPublicRootPeers"
  where
    handler :: SomeException -> Completion m extraState extraFlags extraPeers peeraddr peerconn
    handler e =
      Completion $ \st now ->
      -- This is a failure, so move the backoff counter one in the failure
      -- direction (negative) and schedule the next retry time accordingly.
      -- We use an exponential backoff strategy. The max retry time of 2^8
      -- seconds is about 4 minutes.
      let publicRootBackoffs'      :: Int
          publicRootBackoffs'      = (publicRootBackoffs st `min` 0) - 1

          publicRootRetryDiffTime' :: DiffTime
          publicRootRetryDiffTime' = 2 ^ (abs publicRootBackoffs' `min` 8)

          publicRootRetryTime'     :: Time
          publicRootRetryTime'     = addTime publicRootRetryDiffTime' now
       in Decision {
            decisionTrace = [TracePublicRootsFailure
                               e
                               publicRootBackoffs'
                               publicRootRetryDiffTime'],
            decisionState = st {
                              inProgressPublicRootsReq = False,
                              publicRootBackoffs  = publicRootBackoffs',
                              publicRootRetryTime = publicRootRetryTime'
                            },
            decisionJobs  = []
          }

    job :: m (Completion m extraState extraFlags extraPeers peeraddr peerconn)
    job = do
      (results, ttl) <- requestPublicRootPeers AllLedgerPeers numExtraAllowed
      return $ Completion $ \st now ->
        let newPeers =
              PublicRootPeers.difference differenceExtraPeers
                (PublicRootPeers.difference differenceExtraPeers
                   results (LocalRootPeers.keysSet (localRootPeers st)))
                (PublicRootPeers.toSet extraPeersToSet (publicRootPeers st))
            publicRootPeers'  = PublicRootPeers.mergeG extraPeersToSet (publicRootPeers st) newPeers
            extraPeers = PublicRootPeers.getExtraPeers publicRootPeers'
            ledgerPeers       = PublicRootPeers.toAllLedgerPeerSet publicRootPeers'
            -- Add extra peers
            knownPeers' = KnownPeers.insert
                            -- When we don't know about the PeerSharing information
                            -- we default to PeerSharingDisabled. I.e. we only pass
                            -- a Just value if we want a different value than
                            -- the the default one.
                            ( Map.fromList
                            . map (\(p, pa) -> (p, (Nothing, Just pa)))
                            $ Map.assocs (toAdvertise extraPeers)
                            )
                            (knownPeers st)

            -- Add ledger peers
            knownPeers'' = KnownPeers.insert
                            -- When we don't know about the PeerSharing information
                            -- we default to NoPeerSharing. I.e. we only pass
                            -- a Just value if we want a different value than
                            -- the the default one.
                            ( Map.fromList
                            . map (,(Nothing, Nothing))
                            $ Set.toList ledgerPeers
                            )
                            knownPeers'

            -- We got a successful response to our request, but if we're still
            -- below target we're going to want to try again at some point.
            -- If we made progress towards our target then we will retry at the
            -- suggested ttl. But if we did not make progress then we want to
            -- follow an exponential backoff strategy. The max retry time of 2^8
            -- seconds is about 4 minutes.
            publicRootBackoffs' :: Int
            publicRootBackoffs'
              | PublicRootPeers.null nullExtraPeers newPeers = (publicRootBackoffs st `max` 0) + 1
              | otherwise                     = 0

            publicRootRetryDiffTime :: DiffTime
            publicRootRetryDiffTime
              | publicRootBackoffs' == 0
                          = min 60 ttl -- don't let days long dns timeout kreep in here.
              | otherwise = 2^(publicRootBackoffs' `min` 8)

            publicRootRetryTime :: Time
            publicRootRetryTime = addTime publicRootRetryDiffTime now

         in assert (Set.isSubsetOf
                     (PublicRootPeers.toSet extraPeersToSet publicRootPeers')
                     (KnownPeers.toSet knownPeers''))

             Decision {
                decisionTrace = [TracePublicRootsResults
                                  newPeers
                                  publicRootBackoffs'
                                  publicRootRetryDiffTime],
                decisionState = st {
                                  publicRootPeers     = publicRootPeers',
                                  knownPeers          = knownPeers'',
                                  publicRootBackoffs  = publicRootBackoffs',
                                  publicRootRetryTime = publicRootRetryTime,
                                  inProgressPublicRootsReq = False
                                },
                decisionJobs  = []
              }
