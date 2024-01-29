{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns   #-}

module Ouroboros.Network.PeerSharing where

import Control.Applicative (Alternative)
import Control.Concurrent.Class.MonadMVar (MVar, MonadMVar (putMVar))
import Control.Concurrent.Class.MonadSTM.Strict (MonadSTM, STM, StrictTMVar,
           StrictTVar, atomically, modifyTVar, newEmptyTMVarIO, newTVarIO,
           retry, takeTMVar)
import Control.Monad.Class.MonadThrow (MonadThrow, bracket)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Monoid.Synchronisation (FirstToFinish (..), runFirstToFinish)
import Ouroboros.Network.ControlMessage (ControlMessage (..), ControlMessageSTM)
import Ouroboros.Network.Protocol.PeerSharing.Client (PeerSharingClient (..))
import Ouroboros.Network.Protocol.PeerSharing.Server (PeerSharingServer (..))
import Ouroboros.Network.Protocol.PeerSharing.Type (PeerSharingAmount)

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

peerSharingServer :: Monad m
                  => (PeerSharingAmount -> m [peer])
                  -> PeerSharingServer peer m
peerSharingServer computePeersToShare =
  PeerSharingServer
    { recvMsgShareRequest = \amount -> (,) <$> computePeersToShare amount
                                          <*> return (peerSharingServer computePeersToShare)
    }
