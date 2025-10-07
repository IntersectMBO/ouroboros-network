{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PackageImports      #-}

module DMQ.Diffusion.NodeKernel
  ( NodeKernel (..)
  , withNodeKernel
  ) where

import Control.Concurrent.Class.MonadMVar
import Control.Concurrent.Class.MonadSTM.Strict
import Control.Monad.Class.MonadAsync
import Control.Monad.Class.MonadThrow
import Control.Monad.Class.MonadTime.SI
import Control.Monad.Class.MonadTimer.SI
import "contra-tracer" Control.Tracer (Tracer, nullTracer)

import Data.Function (on)
import Data.Functor.Contravariant ((>$<))
import Data.Hashable
import Data.Sequence (Seq)
import Data.Sequence qualified as Seq
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Time.Clock.POSIX (POSIXTime)
import Data.Time.Clock.POSIX qualified as Time
import Data.Void (Void)
import System.Random (StdGen)
import System.Random qualified as Random

import Cardano.KESAgent.KES.Crypto (Crypto (..))

import Ouroboros.Network.BlockFetch (FetchClientRegistry,
           newFetchClientRegistry)
import Ouroboros.Network.ConnectionId (ConnectionId (..))
import Ouroboros.Network.PeerSelection.Governor.Types
           (makePublicPeerSelectionStateVar)
import Ouroboros.Network.PeerSharing (PeerSharingAPI, PeerSharingRegistry,
           newPeerSharingAPI, newPeerSharingRegistry,
           ps_POLICY_PEER_SHARE_MAX_PEERS, ps_POLICY_PEER_SHARE_STICKY_TIME)
import Ouroboros.Network.TxSubmission.Inbound.V2
import Ouroboros.Network.TxSubmission.Mempool.Simple (Mempool (..),
           MempoolSeq (..))
import Ouroboros.Network.TxSubmission.Mempool.Simple qualified as Mempool

import DMQ.Configuration
import DMQ.Protocol.SigSubmission.Type (Sig (sigExpiresAt, sigId), SigId)
import DMQ.Tracer


data NodeKernel crypto ntnAddr m =
  NodeKernel {
    -- | The fetch client registry, used for the keep alive clients.
    fetchClientRegistry :: !(FetchClientRegistry (ConnectionId ntnAddr) () () m)

    -- | Read the current peer sharing registry, used for interacting with
    -- the PeerSharing protocol
  , peerSharingRegistry :: !(PeerSharingRegistry ntnAddr m)
  , peerSharingAPI      :: !(PeerSharingAPI ntnAddr StdGen m)
  , mempool             :: !(Mempool m SigId (Sig crypto))
  , sigChannelVar       :: !(TxChannelsVar m ntnAddr SigId (Sig crypto))
  , sigMempoolSem       :: !(TxMempoolSem m)
  , sigSharedTxStateVar :: !(SharedTxStateVar m ntnAddr SigId (Sig crypto))
  }

newNodeKernel :: ( MonadLabelledSTM m
                 , MonadMVar m
                 , Ord ntnAddr
                 )
              => StdGen
              -> m (NodeKernel crypto ntnAddr m)
newNodeKernel rng = do
  publicPeerSelectionStateVar <- makePublicPeerSelectionStateVar

  fetchClientRegistry <- newFetchClientRegistry
  peerSharingRegistry <- newPeerSharingRegistry

  mempool <- Mempool.empty
  sigChannelVar <- newTxChannelsVar
  sigMempoolSem <- newTxMempoolSem
  let (rng', rng'') = Random.split rng
  sigSharedTxStateVar <- newSharedTxStateVar rng'

  peerSharingAPI <-
    newPeerSharingAPI
      publicPeerSelectionStateVar
      rng''
      ps_POLICY_PEER_SHARE_STICKY_TIME
      ps_POLICY_PEER_SHARE_MAX_PEERS

  pure NodeKernel { fetchClientRegistry
                  , peerSharingRegistry
                  , peerSharingAPI
                  , mempool
                  , sigChannelVar
                  , sigMempoolSem
                  , sigSharedTxStateVar
                  }


withNodeKernel :: forall crypto ntnAddr m a.
                  ( Crypto crypto
                  , MonadAsync       m
                  , MonadFork        m
                  , MonadDelay       m
                  , MonadLabelledSTM m
                  , MonadMask        m
                  , MonadMVar        m
                  , MonadTime        m
                  , Ord ntnAddr
                  , Show ntnAddr
                  , Hashable ntnAddr
                  )
               => Tracer m WithEventType 
               -> Configuration
               -> StdGen
               -> (NodeKernel crypto ntnAddr m -> m a)
               -- ^ as soon as the callback exits the `mempoolWorker` and all
               -- decision logic threads will be killed
               -> m a
withNodeKernel tracer
               _
               rng k = do
  nodeKernel@NodeKernel { mempool,
                          sigChannelVar,
                          sigSharedTxStateVar
                        }
    <- newNodeKernel rng
  withAsync (mempoolWorker mempool)
          $ \mempoolThread ->
    withAsync (decisionLogicThreads
                (WithEventType (DMQ "SigSubmission.Logic") >$< tracer)
                nullTracer
                defaultSigDecisionPolicy
                sigChannelVar
                sigSharedTxStateVar)
            $ \sigLogicThread
      -> link mempoolThread
      >> link sigLogicThread
      >> k nodeKernel


mempoolWorker :: forall crypto m.
                 ( MonadDelay m
                 , MonadSTM   m
                 , MonadTime  m
                 )
              => Mempool m SigId (Sig crypto)
              -> m Void
mempoolWorker (Mempool v) = loop
  where
    loop = do
      now <- getCurrentPOSIXTime
      rt <- atomically $ do
        MempoolSeq { mempoolSeq, mempoolSet } <- readTVar v
        let mempoolSeq' :: Seq (Sig crypto)
            mempoolSet', expiredSet' :: Set SigId

            (resumeTime, expiredSet', mempoolSeq') =
              foldr (\sig (rt, expiredSet, sigs) ->
                      if sigExpiresAt sig <= now
                      then ( rt
                           , sigId sig `Set.insert` expiredSet
                           , sigs
                           )
                      else ( rt `min` sigExpiresAt sig
                           , expiredSet
                           , sig Seq.<| sigs
                           )
                    )
                    (now, Set.empty, Seq.empty)
                    mempoolSeq

            mempoolSet' = mempoolSet `Set.difference` expiredSet'

        writeTVar v MempoolSeq { mempoolSet = mempoolSet',
                                 mempoolSeq = mempoolSeq' }
        return resumeTime

      now' <- getCurrentPOSIXTime
      threadDelay $ rt `diffPOSIXTime` now' `max` _MEMPOOL_WORKER_MIN_DELAY

      loop



_MEMPOOL_WORKER_MIN_DELAY :: DiffTime
_MEMPOOL_WORKER_MIN_DELAY = 0.05


--
-- POSIXTime utils
--


getCurrentPOSIXTime :: MonadTime m
                    => m POSIXTime
getCurrentPOSIXTime = Time.utcTimeToPOSIXSeconds <$> getCurrentTime


diffPOSIXTime :: POSIXTime -> POSIXTime -> DiffTime
diffPOSIXTime = on diffTime (Time . posixTimeToDiffTime)
  where
    posixTimeToDiffTime :: POSIXTime -> DiffTime
    posixTimeToDiffTime = realToFrac
