{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Ouroboros.Network.PeerSelection.Governor.RootPeers 
  ( belowTarget
  ) where

import           Data.Semigroup (Min(..))
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import           Control.Concurrent.JobPool (Job(..))
import           Control.Monad.Class.MonadSTM
import           Control.Monad.Class.MonadTime
import           Control.Exception (SomeException, assert)

import           Ouroboros.Network.PeerSelection.Types
import           Ouroboros.Network.PeerSelection.KnownPeers (KnownPeerInfo (..))
import qualified Ouroboros.Network.PeerSelection.KnownPeers as KnownPeers
import           Ouroboros.Network.PeerSelection.Governor.Types


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
        decisionTrace = TracePublicRootsRequest
                          targetNumberOfRootPeers
                          numRootPeers,
        decisionState = st { inProgressPublicRootsReq = True },
        decisionJobs  = [jobReqPublicRootPeers actions maxExtraRootPeers]
      }

    -- If we would be able to do the request except for the time, return the
    -- next retry time.
  | maxExtraRootPeers > 0
  , not inProgressPublicRootsReq
  = GuardedSkip (Just (Min publicRootRetryTime))

  | otherwise
  = GuardedSkip Nothing
  where
    numRootPeers      = Map.size localRootPeers + Set.size publicRootPeers
    maxExtraRootPeers = targetNumberOfRootPeers - numRootPeers


jobReqPublicRootPeers :: forall m peeraddr peerconn.
                         (Monad m, Ord peeraddr)
                      => PeerSelectionActions peeraddr peerconn m
                      -> Int
                      -> Job m (Completion m peeraddr peerconn)
jobReqPublicRootPeers PeerSelectionActions{requestPublicRootPeers}
                   numExtraAllowed =
    Job job handler "reqPublicRootPeers"
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
            decisionTrace = TracePublicRootsFailure
                              e
                              publicRootBackoffs'
                              publicRootRetryDiffTime',
            decisionState = st {
                              inProgressPublicRootsReq = False,
                              publicRootBackoffs  = publicRootBackoffs',
                              publicRootRetryTime = publicRootRetryTime'
                            },
            decisionJobs  = []
          }

    job :: m (Completion m peeraddr peerconn)
    job = do
      (results, ttl) <- requestPublicRootPeers numExtraAllowed
      return $ Completion $ \st now ->
        let newPeers         = results Set.\\ Map.keysSet (localRootPeers st)
                                       Set.\\ publicRootPeers st
            publicRootPeers' = publicRootPeers st <> newPeers
            knownPeers'      = KnownPeers.insert
                                 PeerSourcePublicRoot
                                 (const DoAdvertisePeer)
                                 newPeers
                                 (knownPeers st)

            -- We got a successful response to our request, but if we're still
            -- below target we're going to want to try again at some point.
            -- If we made progress towards our target then we will retry at the
            -- suggested ttl. But if we did not make progress then we want to
            -- follow an exponential backoff strategy. The max retry time of 2^12
            -- seconds is just over an hour.
            publicRootBackoffs' :: Int
            publicRootBackoffs'
              | Set.null newPeers = (publicRootBackoffs st `max` 0) + 1
              | otherwise         = 0

            publicRootRetryDiffTime :: DiffTime
            publicRootRetryDiffTime
              | publicRootBackoffs' == 0
                          = ttl
              | otherwise = 2^(publicRootBackoffs' `min` 12)

            publicRootRetryTime :: Time
            publicRootRetryTime = addTime publicRootRetryDiffTime now

         in assert
             (Map.isSubmapOfBy (\_ KnownPeerInfo {knownPeerSource} ->
                        knownPeerSource == PeerSourcePublicRoot
                     || knownPeerSource == PeerSourceLocalRoot)
                  (Map.fromSet (const ()) publicRootPeers')
                  (KnownPeers.toMap knownPeers'))

             Decision {
                decisionTrace = TracePublicRootsResults
                                  newPeers
                                  publicRootBackoffs'
                                  publicRootRetryDiffTime,
                decisionState = st {
                                  publicRootPeers     = publicRootPeers',
                                  knownPeers          = knownPeers',
                                  publicRootBackoffs  = publicRootBackoffs',
                                  publicRootRetryTime = publicRootRetryTime,
                                  inProgressPublicRootsReq = False
                                },
                decisionJobs  = []
              }
