{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns   #-}

module Ouroboros.Network.PeerSharing
  ( PeerSharingController
  , PeerSharingRegistry (..)
  , newPeerSharingRegistry
  , bracketPeerSharingClient
  , peerSharingClient
  , peerSharingServer
  , requestPeers
  , PeerSharingAPI (..)
  , newPeerSharingAPI
    -- * Constants
  , ps_POLICY_PEER_SHARE_STICKY_TIME
  , ps_POLICY_PEER_SHARE_MAX_PEERS
  ) where

import Control.Applicative (Alternative)
import Control.Concurrent.Class.MonadMVar (MVar, MonadMVar (putMVar),
           newEmptyMVar, takeMVar)
import Control.Concurrent.Class.MonadSTM.Strict
import Control.Monad.Class.MonadThrow (MonadThrow, bracket)
import Control.Monad.Class.MonadTime.SI
import Data.Hashable (Hashable (..))
import Data.List (sortBy)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Monoid.Synchronisation (FirstToFinish (..), runFirstToFinish)
import Data.Set qualified as Set
import Ouroboros.Network.ControlMessage (ControlMessage (..), ControlMessageSTM)
import Ouroboros.Network.PeerSelection.Governor.Types (PublicPeerSelectionState,
           availableToShare, emptyPublicPeerSelectionState)
import Ouroboros.Network.Protocol.PeerSharing.Client (PeerSharingClient (..))
import Ouroboros.Network.Protocol.PeerSharing.Server (PeerSharingServer (..))
import Ouroboros.Network.Protocol.PeerSharing.Type (PeerSharingAmount)
import System.Random

-- | Request and Result queue for the peer sharing client implementation.
--
-- Although Peer Sharing is a request-response protocol we can not run it as
-- one, i.e. starting and terminating the protocol on demand since protocol
-- termination as a different semantics. We have to keep the client and server
-- protocol sides running and only issue the requests on demand.
--
-- A workaround to this is to implement the client side with the help of a
-- PeerSharingController which contains two queues: request and result.
-- The client side will be waiting to receive a 'PeerSharingAmount' from the
-- request queue and as soon as it gets something it will send a
-- 'SendMsgShareRequest' and wait for a response before writing it to the
-- result queue.
--
newtype PeerSharingController peer m = PeerSharingController {
  -- | Depth 1 mailbox that contains a locally scoped result queue
    requestQueue :: StrictTMVar m (PeerSharingAmount, MVar m [peer])
  }

requestPeers :: (MonadMVar m, MonadSTM m)
             => PeerSharingController peer m -> PeerSharingAmount -> m [peer]
requestPeers (PeerSharingController requestQueue) amount = do
   res <- newEmptyMVar
   atomically $ putTMVar requestQueue (amount, res)
   takeMVar res

-- | Peer Sharing Registry is a registry that stores a 'PeerSharingController'
-- for every peer that we connect to.
--
-- 'bracketPeerSharingClient' should be used.
newtype PeerSharingRegistry peer m = PeerSharingRegistry {
    getPeerSharingRegistry :: StrictTVar m (Map peer (PeerSharingController peer m))
  }

newPeerSharingRegistry :: (MonadSTM m, Ord peer)
                       => m (PeerSharingRegistry peer m)
newPeerSharingRegistry = PeerSharingRegistry <$> newTVarIO mempty

bracketPeerSharingClient :: (Ord peer, MonadSTM m, MonadThrow m)
                         => PeerSharingRegistry peer m
                         -> peer
                         -> (PeerSharingController peer m -> m a)
                         -> m a
bracketPeerSharingClient (PeerSharingRegistry registry) peer k = do
  -- Create new PeerSharingController
  newPSController <- PeerSharingController <$> newEmptyTMVarIO
  -- Add peer to registry with fresh controller. Call continuation with new
  -- controller. If something goes wrong, unregister peer.
  bracket (atomically (modifyTVar registry (Map.insert peer newPSController)))
          (\_ -> atomically (modifyTVar registry (Map.delete peer)))
          (\_ -> k newPSController)

peerSharingClient :: ( Alternative (STM m)
                     , MonadMVar m
                     , MonadSTM m
                     )
                  => ControlMessageSTM m
                  -> PeerSharingController peer m
                  -> m (PeerSharingClient peer m ())
peerSharingClient controlMessageSTM
                  psc@PeerSharingController { requestQueue } = do

  mbTerminated <- atomically
                $ runFirstToFinish
                $ FirstToFinish (Just <$> takeTMVar requestQueue)
               <> FirstToFinish (do controlMessage <- controlMessageSTM
                                    case controlMessage of
                                         Terminate -> return Nothing
                                         _         -> retry
                                )
  case mbTerminated of
    Nothing       -> return
                   $ SendMsgDone (return ())
    Just (amount, resultQueue) -> return $
      SendMsgShareRequest amount $ \result -> do
        putMVar resultQueue result
        peerSharingClient controlMessageSTM psc

peerSharingServer :: ( MonadSTM m
                     , MonadMonotonicTime m
                     , Hashable peer
                     , RandomGen s
                     )
                  => PeerSharingAPI peer s m
                  -> PeerSharingServer peer m
peerSharingServer peerSharingAPI =
  PeerSharingServer
    { recvMsgShareRequest = \amount -> (,) <$> computePeerSharingPeers peerSharingAPI amount
                                           <*> return (peerSharingServer peerSharingAPI)
    }

--
-- Utility Function
--

-- | PeerSharingAPI needed to compute the peers to be shared.
--
data PeerSharingAPI addr s m =
  PeerSharingAPI { psPublicPeerSelectionStateVar :: StrictTVar m (PublicPeerSelectionState addr)
                 , psGenVar                      :: StrictTVar m s
                 , psReSaltAtVar                 :: StrictTVar m Time
                 , psPolicyPeerShareStickyTime   :: !DiffTime
                 -- ^ Amount of time between changes to the salt used to pick peers to
                 -- gossip about.
                 , psPolicyPeerShareMaxPeers     :: !PeerSharingAmount
                 -- ^ Maximum number of peers to respond with in a single request
                 }

-- | Amount of time between changes to the salt used to pick peers to
-- gossip about.
--
ps_POLICY_PEER_SHARE_STICKY_TIME :: DiffTime
ps_POLICY_PEER_SHARE_STICKY_TIME = 823  -- seconds

-- | Maximum number of peers to respond with in a single request
--
ps_POLICY_PEER_SHARE_MAX_PEERS :: PeerSharingAmount
ps_POLICY_PEER_SHARE_MAX_PEERS = 10

-- | Create a new PeerSharingAPI
--
newPeerSharingAPI :: ( MonadSTM m
                     , Ord addr
                     )
                  => s
                  -> DiffTime
                  -> PeerSharingAmount
                  -> m (PeerSharingAPI addr s m)
newPeerSharingAPI rng policyPeerShareStickyTime policyPeerShareMaxPeers = do
  publicPeerSelectionStateVar <- newTVarIO emptyPublicPeerSelectionState
  genVar <- newTVarIO rng
  reSaltAtVar <- newTVarIO (Time 0)
  return $
    PeerSharingAPI { psPublicPeerSelectionStateVar = publicPeerSelectionStateVar,
                     psGenVar                      = genVar,
                     psReSaltAtVar                 = reSaltAtVar,
                     psPolicyPeerShareStickyTime   = policyPeerShareStickyTime,
                     psPolicyPeerShareMaxPeers     = policyPeerShareMaxPeers
                   }

-- | Select a random subset of the known peers that are available to publish through peersharing.
-- The list of peers will change after `policyPeerShareStickyTime` seconds.
-- The list of peers shared does at most change by the number of peers added or removed.
-- That is a newly added or removed peer can at most lead to one new peer beeing shared.
--
computePeerSharingPeers :: (  MonadSTM m
                           , MonadMonotonicTime m
                           , Hashable ntnAddr
                           , RandomGen s
                           )
                        => PeerSharingAPI ntnAddr s m
                        -> PeerSharingAmount
                        -> m [ntnAddr]
computePeerSharingPeers PeerSharingAPI{ psPublicPeerSelectionStateVar,
                                        psPolicyPeerShareStickyTime,
                                        psPolicyPeerShareMaxPeers,
                                        psReSaltAtVar,
                                        psGenVar
                                      } amount = do
  now <- getMonotonicTime
  publicState <- readTVarIO psPublicPeerSelectionStateVar
  salt <- atomically $ do
    reSaltAt <- readTVar psReSaltAtVar
    if reSaltAt <= now
       then do
         writeTVar psReSaltAtVar $ addTime psPolicyPeerShareStickyTime now
         stateTVar psGenVar random
       else do
         gen <- readTVar psGenVar
         return $ fst $ random gen

  let availableToShareSet = availableToShare publicState
      randomList = take (fromIntegral psPolicyPeerShareMaxPeers
                                      `min`
                                      fromIntegral amount)
                 $ sortBy (\a b -> compare (hashWithSalt salt a) (hashWithSalt salt b))
                 $ Set.elems availableToShareSet
  if null availableToShareSet
     then return []
     else return randomList
