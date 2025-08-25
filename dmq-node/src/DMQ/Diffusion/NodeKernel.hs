{-# LANGUAGE DataKinds #-}

module DMQ.Diffusion.NodeKernel
  ( NodeKernel (..)
  , withNodeKernel
  , PoolValidationCtx (..)
  , StakePools (..)
  ) where

import Control.Concurrent.Class.MonadMVar
import Control.Concurrent.Class.MonadSTM.Strict
import Control.Monad.Class.MonadAsync
import Control.Monad.Class.MonadThrow
import Control.Monad.Class.MonadTime.SI
import Control.Monad.Class.MonadTimer.SI
import Control.Tracer

import Data.Function (on)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Sequence (Seq)
import Data.Sequence qualified as Seq
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Time.Clock.POSIX (POSIXTime)
import Data.Time.Clock.POSIX qualified as Time
import Data.Void (Void)
import System.Random (StdGen)
import System.Random qualified as Random

import Cardano.Ledger.Shelley.API
import Ouroboros.Consensus.Shelley.Ledger.Query
import Ouroboros.Network.BlockFetch (FetchClientRegistry,
           newFetchClientRegistry)
import Ouroboros.Network.ConnectionId (ConnectionId (..))
import Ouroboros.Network.PeerSelection.Governor.Types
           (makePublicPeerSelectionStateVar)
import Ouroboros.Network.PeerSharing (PeerSharingAPI, PeerSharingRegistry,
           newPeerSharingAPI, newPeerSharingRegistry,
           ps_POLICY_PEER_SHARE_MAX_PEERS, ps_POLICY_PEER_SHARE_STICKY_TIME)
import Ouroboros.Network.TxSubmission.Inbound.V2.Registry
import Ouroboros.Network.TxSubmission.Mempool.Simple (Mempool (..),
           MempoolSeq (..))
import Ouroboros.Network.TxSubmission.Mempool.Simple qualified as Mempool

import DMQ.Protocol.SigSubmission.Type (Sig (sigExpiresAt, sigId), SigId)


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
  , stakePools          :: !(StakePools m)
  , nextEpochVar        :: !(StrictTVar m (Maybe UTCTime))
  }

-- | Cardano pool id's are hashes of the cold verification key
--
type PoolId = KeyHash StakePool

data StakePools m = StakePools {
    -- | contains map of cardano pool stake snapshot obtained
    -- via local state query client
    stakePoolsVar     :: StrictTVar m (Map PoolId StakeSnapshot)
    -- | acquires validation context for signature validation
  , poolValidationCtx :: m PoolValidationCtx
  }

data PoolValidationCtx =
  DMQPoolValidationCtx !UTCTime -- ^ time of context acquisition
                       !(Maybe UTCTime) -- ^ UTC time of next epoch boundary
                       !(Map PoolId StakeSnapshot) -- ^ for signature validation

newNodeKernel :: ( MonadLabelledSTM m
                 , MonadMVar m
                 , MonadTime m
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
  nextEpochVar <- newTVarIO Nothing
  stakePoolsVar <- newTVarIO Map.empty
  let poolValidationCtx = do
        (nextEpochBoundary, stakePools) <-
          atomically $ (,) <$> readTVar nextEpochVar <*> readTVar stakePoolsVar
        now <- getCurrentTime
        return $ DMQPoolValidationCtx now nextEpochBoundary stakePools

      stakePools = StakePools { stakePoolsVar, poolValidationCtx }

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
                  , nextEpochVar
                  , stakePools
                  }


withNodeKernel :: forall crypto ntnAddr m a.
                  ( MonadAsync       m
                  , MonadFork        m
                  , MonadDelay       m
                  , MonadLabelledSTM m
                  , MonadMask        m
                  , MonadMVar        m
                  , MonadTime        m
                  , Ord ntnAddr
                  )
               => StdGen
               -> (NodeKernel crypto ntnAddr m -> m a)
               -- ^ as soon as the callback exits the `mempoolWorker` will be
               -- killed
               -> m a
withNodeKernel rng k = do
  nodeKernel@NodeKernel { mempool } <- newNodeKernel rng
  withAsync (mempoolWorker mempool)
    $ \thread -> link thread
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
