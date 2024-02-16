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

import Ouroboros.Network.PeerSelection.Bootstrap (requiresBootstrapPeers)
import Ouroboros.Network.PeerSelection.Governor.Types
import Ouroboros.Network.PeerSelection.LedgerPeers (LedgerPeersKind (..))
import Ouroboros.Network.PeerSelection.PublicRootPeers qualified as PublicRootPeers
import Ouroboros.Network.PeerSelection.State.KnownPeers qualified as KnownPeers
import Ouroboros.Network.PeerSelection.State.LocalRootPeers qualified as LocalRootPeers


--------------------------
-- Root peers below target
--

belowTarget :: (MonadSTM m, Ord peeraddr)
            => PeerSelectionActions peeraddr peerconn m
            -> Time
            -> PeerSelectionState peeraddr peerconn
            -> Guarded (STM m) (TimedDecision m peeraddr peerconn)
belowTarget actions
            blockedAt
            st@PeerSelectionState {
              localRootPeers,
              publicRootPeers,
              publicRootRetryTime,
              inProgressPublicRootsReq,
              ledgerStateJudgement,
              bootstrapPeersFlag,
              targets = PeerSelectionTargets {
                          targetNumberOfRootPeers,
                          targetNumberOfBootstrapPeers
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
                           target
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
    numRootPeers      = LocalRootPeers.size localRootPeers
                      + PublicRootPeers.size publicRootPeers
    target | requiresBootstrapPeers
               bootstrapPeersFlag
               ledgerStateJudgement = targetNumberOfBootstrapPeers
           | otherwise = targetNumberOfRootPeers
    maxExtraRootPeers = target - numRootPeers


jobReqPublicRootPeers :: forall m peeraddr peerconn.
                         (MonadSTM m, Ord peeraddr)
                      => PeerSelectionActions peeraddr peerconn m
                      -> Int
                      -> Job () m (Completion m peeraddr peerconn)
jobReqPublicRootPeers PeerSelectionActions{ requestPublicRootPeers
                                          }
                      numExtraAllowed =
    Job job (return . handler) () "reqPublicRootPeers"
  where
    handler :: SomeException -> Completion m peeraddr peerconn
    handler e =
      Completion $ \st now ->
      -- This is a failure, so move the backoff counter one in the failure
      -- direction (negative) and schedule the next retry time accordingly.
      -- We use an exponential backoff strategy. The max retry time of 2^12
      -- seconds is just over an hour.
      let publicRootBackoffs'      :: Int
          publicRootBackoffs'      = (publicRootBackoffs st `min` 0) - 1

          publicRootRetryDiffTime' :: DiffTime
          publicRootRetryDiffTime' = 2 ^ (abs publicRootBackoffs' `min` 12)

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

    job :: m (Completion m peeraddr peerconn)
    job = do
      (results, ttl) <- requestPublicRootPeers AllLedgerPeers numExtraAllowed
      return $ Completion $ \st now ->
        let newPeers = results `PublicRootPeers.difference` LocalRootPeers.keysSet (localRootPeers st)
                               `PublicRootPeers.difference` PublicRootPeers.toSet (publicRootPeers st)
            publicRootPeers'  = publicRootPeers st <> newPeers
            publicConfigPeers = PublicRootPeers.getPublicConfigPeers publicRootPeers'
            bootstrapPeers    = PublicRootPeers.getBootstrapPeers publicRootPeers'
            ledgerPeers       = PublicRootPeers.toAllLedgerPeerSet publicRootPeers'
            -- Add bootstrapPeers peers
            knownPeers' = KnownPeers.insert
                            -- When we don't know about the PeerSharing information
                            -- we default to NoPeerSharing. I.e. we only pass
                            -- a Just value if we want a different value than
                            -- the the default one.
                            ( Map.fromList
                            . map (\(p, pa) -> (p, (Nothing, Just pa)))
                            $ Map.assocs publicConfigPeers
                            )
                            (knownPeers st)

            -- Add all other peers
            knownPeers'' = KnownPeers.insert
                            -- When we don't know about the PeerSharing information
                            -- we default to NoPeerSharing. I.e. we only pass
                            -- a Just value if we want a different value than
                            -- the the default one.
                            ( Map.fromList
                            . map (,(Nothing, Nothing))
                            $ Set.toList (bootstrapPeers <> ledgerPeers)
                            )
                            knownPeers'

            -- We got a successful response to our request, but if we're still
            -- below target we're going to want to try again at some point.
            -- If we made progress towards our target then we will retry at the
            -- suggested ttl. But if we did not make progress then we want to
            -- follow an exponential backoff strategy. The max retry time of 2^12
            -- seconds is just over an hour.
            publicRootBackoffs' :: Int
            publicRootBackoffs'
              | PublicRootPeers.null newPeers = (publicRootBackoffs st `max` 0) + 1
              | otherwise                     = 0

            publicRootRetryDiffTime :: DiffTime
            publicRootRetryDiffTime
              | publicRootBackoffs' == 0
                          = ttl
              | otherwise = 2^(publicRootBackoffs' `min` 12)

            publicRootRetryTime :: Time
            publicRootRetryTime = addTime publicRootRetryDiffTime now

         in assert (Set.isSubsetOf
                     (PublicRootPeers.toSet publicRootPeers')
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
