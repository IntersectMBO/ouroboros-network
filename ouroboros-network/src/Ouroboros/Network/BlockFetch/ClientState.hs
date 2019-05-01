{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE BangPatterns               #-}

module Ouroboros.Network.BlockFetch.ClientState (
    FetchClientStateVars(..),
    newFetchClientStateVars,
    readFetchClientState,
    PeerFetchStatus(..),
    PeerFetchInFlight(..),
    initialPeerFetchInFlight,
    FetchRequest(..),
    setFetchRequest,
    acceptFetchRequest,
    completeBlockDownload,
    completeFetchBatch,
  ) where

import qualified Data.Set as Set
import           Data.Set (Set)
import           Data.Semigroup (Semigroup, Last(..))

import           Control.Monad (when)
import           Control.Monad.Class.MonadSTM

import           Ouroboros.Network.Block (Point, blockPoint, HasHeader)
import qualified Ouroboros.Network.ChainFragment as CF
import           Ouroboros.Network.ChainFragment (ChainFragment)
import           Ouroboros.Network.BlockFetch.DeltaQ
                   ( PeerFetchInFlightLimits(..)
                   , calculatePeerFetchInFlightLimits
                   , SizeInBytes, PeerGSV )

-- | A set of variables shared between the block fetch logic thread and each
-- thread executing the client side of the block fetch protocol. That is, these
-- are the shared variables per peer. The 'FetchClientRegistry' contains the
-- mapping of these for all peers.
--
-- The variables are used for communicating from the protocol thread to the
-- decision making thread the status of things with that peer. And in the other
-- direction one shared variable is for providing new fetch requests.
--
data FetchClientStateVars m header =
     FetchClientStateVars {

       -- | The current status of communication with the peer. It is written
       -- by the protocol thread and monitored and read by the decision logic
       -- thread. Changes in this state trigger re-evaluation of fetch
       -- decisions.
       --
       fetchClientStatusVar   :: TVar m (PeerFetchStatus header),

       -- | The current number of requests in-flight and the amount of data
       -- in-flight with the peer. It is written by the protocol thread and
       -- read by the decision logic thread. This is used in fetch decisions
       -- but changes here do not trigger re-evaluation of fetch decisions.
       --
       fetchClientInFlightVar :: TVar m (PeerFetchInFlight header),

       -- | The shared variable used to communicate fetch requests to the thread
       -- running the block fetch protocol. Fetch requests are posted by the
       -- decision logic thread. The protocol thread accepts the requests and
       -- acts on them, updating the in-flight stats. While this is a 'TMVar',
       -- it is not used as a one-place queue: the requests can be updated
       -- before being accepted.
       --
       fetchClientRequestVar  :: TFetchRequestVar m header
     }

newFetchClientStateVars :: MonadSTM m => STM m (FetchClientStateVars m header)
newFetchClientStateVars = do
    fetchClientInFlightVar <- newTVar initialPeerFetchInFlight
    fetchClientStatusVar   <- newTVar (PeerFetchStatusReady Set.empty)
    fetchClientRequestVar  <- newTFetchRequestVar
    return FetchClientStateVars {..}

readFetchClientState :: MonadSTM m
                     => FetchClientStateVars m header
                     -> STM m (PeerFetchStatus header,
                               PeerFetchInFlight header,
                               FetchClientStateVars m header)
readFetchClientState vars@FetchClientStateVars{..} =
    (,,) <$> readTVar fetchClientStatusVar
         <*> readTVar fetchClientInFlightVar
         <*> pure vars

-- | The status of the block fetch communication with a peer. This is maintained
-- by fetch protocol threads and used in the block fetch decision making logic.
-- Changes in this status trigger re-evaluation of fetch decisions.
--
data PeerFetchStatus header =
       -- | Communication with the peer has failed. This is a temporary status
       -- that may occur during the process of shutting down the thread that
       -- runs the block fetch protocol. The peer will promptly be removed from
       -- the peer registry and so will not be considered at all.
       --
       PeerFetchStatusShutdown

       -- | The peer is in a potentially-temporary state in which it has not
       -- responded to us within a certain expected time limit. This is not
       -- a hard protocol timeout where the whole connection will be abandoned,
       -- it is simply a reply that has taken longer than expected. This status
       -- is used to trigger re-evaluating which peer to ask for blocks from,
       -- so that we can swiftly ask other peers for blocks if one unexpectedly
       -- responds too slowly
       --
       -- Peers in this state may later return to normal states if communication
       -- resumes, or they may eventually hit a hard timeout and fail.
       --
     | PeerFetchStatusAberrant

       -- | Communication with the peer is in a normal state, and the peer is
       -- considered too busy to accept new requests. Changing from this state
       -- to the ready state is used to trigger re-evaluating fetch decisions
       -- and may eventually result in new fetch requests. This state is used
       -- as part of a policy to batch new requests: instead of switching to
       -- the ready state the moment there is tiny bit of capacity available,
       -- the state is changed once the capacity reaches a certain threshold.
       --
     | PeerFetchStatusBusy

       -- | Communication with the peer is in a normal state, and the peer is
       -- considered ready to accept new requests.
       --
     | PeerFetchStatusReady (Set (Point header))
  deriving (Eq, Show)


-- | The number of requests in-flight and the amount of data in-flight with a
-- peer. This is maintained by fetch protocol threads and used in the block
-- fetch decision making logic.
--
data PeerFetchInFlight header = PeerFetchInFlight {
       -- | The number of block fetch requests that are currently in-flight.
       -- This is the number of /requests/ not the number of blocks. Each
       -- request is for a range of blocks.
       --
       -- We track this because there is a fixed maximum number of outstanding
       -- requests that the protocol allows.
       --
       peerFetchReqsInFlight :: !Word,

       -- | The sum of the byte count of blocks expected from all in-flight
       -- fetch requests. This is a close approximation of the amount of data
       -- we expect to receive, assuming no failures.
       --
       -- We track this because we pipeline fetch requests and we want to keep
       -- some but not too much data in flight at once.
       --
       peerFetchBytesInFlight :: !SizeInBytes,

       -- | The points for the set of blocks that are currently in-flight.
       -- Note that since requests are for ranges of blocks this does not
       -- correspond to the number of requests in flight.
       --
       -- We track this because as part of the decision for which blocks to
       -- fetch from which peers we take into account what blocks are already
       -- in-flight with peers.
       --
       peerFetchBlocksInFlight :: Set (Point header)
     }
  deriving (Eq, Show)

initialPeerFetchInFlight :: PeerFetchInFlight header
initialPeerFetchInFlight =
    PeerFetchInFlight {
      peerFetchReqsInFlight   = 0,
      peerFetchBytesInFlight  = 0,
      peerFetchBlocksInFlight = Set.empty
    }

addHeadersInFlight :: HasHeader header
                   => (header -> SizeInBytes)
                   -> FetchRequest header
                   -> PeerFetchInFlight header
                   -> PeerFetchInFlight header
addHeadersInFlight blockFetchSize (FetchRequest fragments) inflight =
    PeerFetchInFlight {
      peerFetchReqsInFlight   = peerFetchReqsInFlight inflight
                              + fromIntegral (length fragments),

      peerFetchBytesInFlight  = peerFetchBytesInFlight inflight
                              + sum [ blockFetchSize header
                                    | fragment <- fragments
                                    , header   <- CF.toOldestFirst fragment ],

      peerFetchBlocksInFlight = peerFetchBlocksInFlight inflight
                    `Set.union` Set.fromList
                                  [ blockPoint header
                                  | fragment <- fragments
                                  , header   <- CF.toOldestFirst fragment ]
    }

deleteHeaderInFlight :: HasHeader header
                     => (header -> SizeInBytes)
                     -> header
                     -> PeerFetchInFlight header
                     -> PeerFetchInFlight header
deleteHeaderInFlight blockFetchSize header inflight =
    inflight {
      peerFetchBytesInFlight  = peerFetchBytesInFlight inflight
                              - blockFetchSize header,

      peerFetchBlocksInFlight = blockPoint header
                   `Set.delete` peerFetchBlocksInFlight inflight
      --TODO: can assert here that we don't go negative, and the
      -- block we're deleting was in fact there.
    }


newtype FetchRequest header =
        FetchRequest { fetchRequestFragments :: [ChainFragment header] }
  deriving Show


setFetchRequest :: MonadSTM m
                => FetchClientStateVars m header
                -> FetchRequest header
                -> PeerGSV
                -> m ()
setFetchRequest FetchClientStateVars{fetchClientRequestVar} request gsvs =
    atomically (writeTFetchRequestVar fetchClientRequestVar request gsvs)

acceptFetchRequest :: (MonadSTM m, HasHeader header)
                   => (header -> SizeInBytes)
                   -> FetchClientStateVars m header
                   -> m ( FetchRequest header
                        , PeerGSV
                        , PeerFetchInFlight header
                        , PeerFetchInFlightLimits
                        , PeerFetchStatus header )

acceptFetchRequest blockFetchSize FetchClientStateVars {..} =
    atomically $ do
      (request, gsvs) <- takeTFetchRequestVar fetchClientRequestVar
      let inflightlimits = calculatePeerFetchInFlightLimits gsvs
      --TODO: if recalculating the limits here is expensive we can pass them
      -- along with the fetch request and the gsvs

      inflight <- readTVar fetchClientInFlightVar
      let !inflight' = addHeadersInFlight blockFetchSize request inflight
      writeTVar fetchClientInFlightVar inflight'

      -- Set our status to busy if we've got over the high watermark.
      let currentStatus'
           | peerFetchBytesInFlight inflight'
             >= inFlightBytesHighWatermark inflightlimits
           = PeerFetchStatusBusy
           | otherwise
           = PeerFetchStatusReady (peerFetchBlocksInFlight inflight')
      -- Only update the variable if it changed, to avoid spurious wakeups.
      currentStatus <- readTVar fetchClientStatusVar
      when (currentStatus' /= currentStatus) $
        writeTVar fetchClientStatusVar currentStatus'

      --TODO: think about status aberrant

      return (request, gsvs, inflight', inflightlimits, currentStatus')

completeBlockDownload :: (MonadSTM m, HasHeader header)
                      => (header -> SizeInBytes)
                      -> PeerFetchInFlightLimits
                      -> header
                      -> FetchClientStateVars m header
                      -> m (PeerFetchInFlight header, PeerFetchStatus header)

completeBlockDownload blockFetchSize inflightlimits
                      header FetchClientStateVars {..} =
    atomically $ do
      inflight <- readTVar fetchClientInFlightVar
      let !inflight' = deleteHeaderInFlight blockFetchSize header inflight
      writeTVar fetchClientInFlightVar inflight'

      -- Set our status to ready if we're under the low watermark.
      let currentStatus'
            | peerFetchBytesInFlight inflight'
              <= inFlightBytesLowWatermark inflightlimits
            = PeerFetchStatusReady (peerFetchBlocksInFlight inflight')
            | otherwise
            = PeerFetchStatusBusy
      -- Only update the variable if it changed, to avoid spurious wakeups.
      currentStatus <- readTVar fetchClientStatusVar
      when (currentStatus' /= currentStatus) $
        writeTVar fetchClientStatusVar currentStatus'

    -- TODO: when do we reset the status from PeerFetchStatusAberrant
    -- to PeerFetchStatusReady/Busy?

      return (inflight', currentStatus')

completeFetchBatch :: MonadSTM m
                   => FetchClientStateVars m header
                   -> m ()
completeFetchBatch FetchClientStateVars {fetchClientInFlightVar} =
    atomically $ modifyTVar' fetchClientInFlightVar $ \inflight ->
      inflight {
        peerFetchReqsInFlight = peerFetchReqsInFlight inflight - 1
      }

--
-- STM TFetchRequestVar
--

-- | The 'TFetchRequestVar' is a 'TMergeVar' for communicating the
-- 'FetchRequest's from the logic thread to a fetch client thread.
--
-- The pattern is that the logic thread determines a current request and this
-- is written to the var with 'writeTMergeVar'. The fetch client thread uses
-- 'takeTMergeVar', which blocks until a value is available. On the other hand,
-- 'writeTMergeVar' never blocks, if a value is already present then it
-- overwrites it. This makes sense for the fetch requests because if a fetch
-- client has not accepted the request yet then we can replace it with the
-- request based on the more recent state.
--
type TFetchRequestVar m header =
       TMergeVar m (Last (FetchRequest header, PeerGSV))

newTFetchRequestVar :: MonadSTM m => STM m (TFetchRequestVar m header)
newTFetchRequestVar = newTMergeVar

writeTFetchRequestVar :: MonadSTM m
                      => TFetchRequestVar m header
                      -> FetchRequest header
                      -> PeerGSV
                      -> STM m ()
writeTFetchRequestVar v r g = writeTMergeVar v (Last (r, g))

takeTFetchRequestVar :: MonadSTM m
                     => TFetchRequestVar m header
                     -> STM m (FetchRequest header, PeerGSV)
takeTFetchRequestVar v = getLast <$> takeTMergeVar v


--
-- STM TMergeVar mini-abstraction
--

-- | The 'TMergeVar' is like a 'TMVar' in that we take it, leaving it empty.
-- Unlike an ordinary 'TMVar' with a blocking \'put\' operation, it has a
-- non-blocking combiing write operation: if a value is already present then
-- the values are combined using the 'Semigroup' operator.
--
-- This is used much like a 'TMVar' as a one-place queue between threads but
-- with the property that we can \"improve\" the current value (if any).
--
newtype TMergeVar m a = TMergeVar (TMVar m a)

newTMergeVar :: MonadSTM m => STM m (TMergeVar m a)
newTMergeVar = TMergeVar <$> newEmptyTMVar

writeTMergeVar :: (MonadSTM m, Semigroup a) => TMergeVar m a -> a -> STM m ()
writeTMergeVar (TMergeVar v) x = do
    mx0 <- tryTakeTMVar v
    case mx0 of
      Nothing -> putTMVar v x
      Just x0 -> putTMVar v x' where !x' = x0 <> x

takeTMergeVar :: MonadSTM m => TMergeVar m a -> STM m a
takeTMergeVar (TMergeVar v) = takeTMVar v

