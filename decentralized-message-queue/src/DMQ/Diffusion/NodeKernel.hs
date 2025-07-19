{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}

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

import Data.Function (on)
import Data.Sequence qualified as Seq
import Data.Time.Clock.POSIX (POSIXTime)
import Data.Time.Clock.POSIX qualified as Time
import Data.Void (Void)
import System.Random (StdGen)
import System.Random qualified as Random

import Ouroboros.Network.BlockFetch (FetchClientRegistry,
           newFetchClientRegistry)
import Ouroboros.Network.ConnectionId (ConnectionId (..))
import Ouroboros.Network.PeerSelection.Governor.Types
           (makePublicPeerSelectionStateVar)
import Ouroboros.Network.PeerSharing (PeerSharingAPI, PeerSharingRegistry,
           newPeerSharingAPI, newPeerSharingRegistry,
           ps_POLICY_PEER_SHARE_MAX_PEERS, ps_POLICY_PEER_SHARE_STICKY_TIME)
import Ouroboros.Network.TxSubmission.Inbound.V2.Registry
import Ouroboros.Network.TxSubmission.Mempool.Simple (Mempool (..))
import Ouroboros.Network.TxSubmission.Mempool.Simple qualified as Mempool

import DMQ.Protocol.SigSubmission.Type (Sig (..), SigId, SigTTL (..))


data NodeKernel ntnAddr m =
  NodeKernel {
    -- | The fetch client registry, used for the keep alive clients.
    fetchClientRegistry :: FetchClientRegistry (ConnectionId ntnAddr) () () m

    -- | Read the current peer sharing registry, used for interacting with
    -- the PeerSharing protocol
  , peerSharingRegistry :: PeerSharingRegistry ntnAddr m
  , peerSharingAPI      :: PeerSharingAPI ntnAddr StdGen m
  , mempool             :: Mempool m Sig
  , sigChannelVar       :: TxChannelsVar m ntnAddr SigId Sig
  , sigMempoolSem       :: TxMempoolSem m
  , sigSharedTxStateVar :: SharedTxStateVar m ntnAddr SigId Sig
  }

newNodeKernel :: ( MonadLabelledSTM m
                 , MonadMVar m
                 , Ord ntnAddr
                 )
              => StdGen
              -> m (NodeKernel ntnAddr m)
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


withNodeKernel :: ( MonadAsync       m
                  , MonadFork        m
                  , MonadDelay       m
                  , MonadLabelledSTM m
                  , MonadMask        m
                  , MonadMVar        m
                  , MonadTime        m
                  , Ord ntnAddr
                  )
               => StdGen
               -> (NodeKernel ntnAddr m -> m a)
               -- ^ as soon as the callback exits the `mempoolWorker` will be
               -- killed
               -> m a
withNodeKernel rng k = do
  nodeKernel@NodeKernel { mempool } <- newNodeKernel rng
  withAsync (mempoolWorker mempool)
    $ \thread -> link thread
              >> k nodeKernel


mempoolWorker :: ( MonadDelay m
                 , MonadSTM   m
                 , MonadTime  m
                 )
              => Mempool m Sig
              -> m Void
mempoolWorker (Mempool v) = loop
  where
    loop = do
      now <- getCurrentPOSIXTime
      rt <- atomically $ do
        (sigs :: Seq.Seq Sig) <- readTVar v
        let sigs' :: Seq.Seq Sig
            (resumeTime, sigs') =
              foldr (\a (rt, as) -> if getSigTTL (sigTTL a) <= now
                                    then (rt, as)
                                    else (rt `min` getSigTTL (sigTTL a), a Seq.<| as))
                    (now, Seq.empty)
                    sigs
        writeTVar v sigs'
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
