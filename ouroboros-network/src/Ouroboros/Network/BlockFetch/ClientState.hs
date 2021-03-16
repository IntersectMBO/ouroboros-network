{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE BangPatterns               #-}

module Ouroboros.Network.BlockFetch.ClientState (
    FetchClientContext(..),
    FetchClientPolicy(..),
    FetchClientStateVars(..),
    newFetchClientStateVars,
    readFetchClientState,
    PeerFetchStatus(..),
    IsIdle(..),
    PeerFetchInFlight(..),
    initialPeerFetchInFlight,
    FetchRequest(..),
    addNewFetchRequest,
    acknowledgeFetchRequest,
    startedFetchBatch,
    completeBlockDownload,
    completeFetchBatch,
    rejectedFetchBatch,
    TraceFetchClientState(..),
    TraceLabelPeer(..),
    ChainRange(..),
    -- * Ancillary
    FromConsensus(..),
  ) where

import           Data.List (foldl')
import           Data.Maybe (mapMaybe)
import qualified Data.Set as Set
import           Data.Set (Set)
import           Data.Semigroup (Last(..))

import           Control.Monad (when)
import           Control.Monad.Class.MonadSTM.Strict
import           Control.Monad.Class.MonadTime
import           Control.Exception (assert)
import           Control.Tracer (Tracer, traceWith)

import           Ouroboros.Network.Mux (ControlMessageSTM, timeoutWithControlMessage)
import           Ouroboros.Network.AnchoredFragment (AnchoredFragment)
import qualified Ouroboros.Network.AnchoredFragment as AF
import           Ouroboros.Network.Block
                   ( HasHeader, MaxSlotNo (..), Point, blockPoint )
import           Ouroboros.Network.Protocol.BlockFetch.Type (ChainRange(..))
import           Ouroboros.Network.BlockFetch.DeltaQ
                   ( PeerFetchInFlightLimits(..)
                   , calculatePeerFetchInFlightLimits
                   , SizeInBytes, PeerGSV )
import           Ouroboros.Network.Point (withOriginToMaybe)

-- | The context that is passed into the block fetch protocol client when it
-- is started.
--
data FetchClientContext header block m =
     FetchClientContext {
       fetchClientCtxTracer    :: Tracer m (TraceFetchClientState header),
       fetchClientCtxPolicy    :: FetchClientPolicy header block m,
       fetchClientCtxStateVars :: FetchClientStateVars m header
     }


-- | The policy used by the fetch clients. It is set by the central block fetch
-- logic, and passed to them via the 'FetchClientRegistry'.
--
data FetchClientPolicy header block m =
     FetchClientPolicy {
       blockFetchSize     :: header -> SizeInBytes,
       blockMatchesHeader :: header -> block -> Bool,
       addFetchedBlock    :: Point block -> block -> m (),
       blockForgeUTCTime  :: FromConsensus block -> STM m UTCTime
     }

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
       fetchClientStatusVar   :: StrictTVar m (PeerFetchStatus header),

       -- | The current number of requests in-flight and the amount of data
       -- in-flight with the peer. It is written by the protocol thread and
       -- read by the decision logic thread. This is used in fetch decisions
       -- but changes here do not trigger re-evaluation of fetch decisions.
       --
       fetchClientInFlightVar :: StrictTVar m (PeerFetchInFlight header),

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
    fetchClientStatusVar   <- newTVar (PeerFetchStatusReady Set.empty IsIdle)
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
       -- The 'Set' is the blocks in flight.
     | PeerFetchStatusReady (Set (Point header)) IsIdle
  deriving (Eq, Show)

-- | Whether this mini protocol instance is in the @Idle@ State
--
data IsIdle = IsIdle | IsNotIdle
  deriving (Eq, Show)

idleIf :: Bool -> IsIdle
idleIf b = if b then IsIdle else IsNotIdle

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
       peerFetchBlocksInFlight :: Set (Point header),

       -- | The maximum slot of a block that /has ever been/ in flight for
       -- this peer.
       --
       -- We track this to more efficiently remove blocks that are already
       -- in-flight from the candidate fragments: blocks with a slot number
       -- higher than this one do not have to be filtered out.
       peerFetchMaxSlotNo  :: !MaxSlotNo
     }
  deriving (Eq, Show)

initialPeerFetchInFlight :: PeerFetchInFlight header
initialPeerFetchInFlight =
    PeerFetchInFlight {
      peerFetchReqsInFlight   = 0,
      peerFetchBytesInFlight  = 0,
      peerFetchBlocksInFlight = Set.empty,
      peerFetchMaxSlotNo      = NoMaxSlotNo
    }

-- | Update the 'PeerFetchInFlight' in-flight tracking numbers.
--
-- Note that it takes both the existing \"old\" request, the \"added\" request
-- and resulting \"merged\" request. The relationship between the three is
-- @old <> added = merged@.
--
addHeadersInFlight :: HasHeader header
                   => (header -> SizeInBytes)
                   -> Maybe (FetchRequest header) -- ^ The old request (if any).
                   -> FetchRequest header         -- ^ The added request.
                   -> FetchRequest header         -- ^ The merged request.
                   -> PeerFetchInFlight header
                   -> PeerFetchInFlight header
addHeadersInFlight blockFetchSize oldReq addedReq mergedReq inflight =

    -- This assertion checks the pre-condition 'addNewFetchRequest' that all
    -- requested blocks are new. This is true irrespective of fetch-request
    -- command merging.
    assert (and [ blockPoint header `Set.notMember` peerFetchBlocksInFlight inflight
                | fragment <- fetchRequestFragments addedReq
                , header   <- AF.toOldestFirst fragment ]) $

    PeerFetchInFlight {

      -- Fetch request merging makes the update of the number of in-flight
      -- requests rather subtle. See the 'FetchRequest' semigroup instance
      -- documentation for details. The upshot is that we have to look at the
      -- /difference/ in the number of fragments for the old request
      -- (if any) and merged request.
      peerFetchReqsInFlight   = peerFetchReqsInFlight inflight
                              +         numFetchReqs mergedReq
                              - maybe 0 numFetchReqs oldReq,

      -- For the bytes and blocks in flight however we can rely on the
      -- pre-condition that is asserted above.
      peerFetchBytesInFlight  = peerFetchBytesInFlight inflight
                              + sum [ blockFetchSize header
                                    | fragment <- fetchRequestFragments addedReq
                                    , header   <- AF.toOldestFirst fragment ],

      peerFetchBlocksInFlight = peerFetchBlocksInFlight inflight
                    `Set.union` Set.fromList
                                  [ blockPoint header
                                  | fragment <- fetchRequestFragments addedReq
                                  , header   <- AF.toOldestFirst fragment ],

      peerFetchMaxSlotNo      = peerFetchMaxSlotNo inflight
                          `max` fetchRequestMaxSlotNo addedReq
    }
  where
    numFetchReqs :: FetchRequest header -> Word
    numFetchReqs = fromIntegral . length . fetchRequestFragments

deleteHeaderInFlight :: HasHeader header
                     => (header -> SizeInBytes)
                     -> header
                     -> PeerFetchInFlight header
                     -> PeerFetchInFlight header
deleteHeaderInFlight blockFetchSize header inflight =
    assert (peerFetchBytesInFlight inflight >= blockFetchSize header) $
    assert (blockPoint header `Set.member` peerFetchBlocksInFlight inflight) $
    inflight {
      peerFetchBytesInFlight  = peerFetchBytesInFlight inflight
                              - blockFetchSize header,

      peerFetchBlocksInFlight = blockPoint header
                   `Set.delete` peerFetchBlocksInFlight inflight
    }

deleteHeadersInFlight :: HasHeader header
                      => (header -> SizeInBytes)
                      -> [header]
                      -> PeerFetchInFlight header
                      -> PeerFetchInFlight header
deleteHeadersInFlight blockFetchSize headers inflight =
    -- Reusing 'deleteHeaderInFlight' rather than a direct impl still
    -- gives us O(n log m) which is fine
    foldl' (flip (deleteHeaderInFlight blockFetchSize)) inflight headers


newtype FetchRequest header =
        FetchRequest { fetchRequestFragments :: [AnchoredFragment header] }
  deriving Show

-- | We sometimes have the opportunity to merge fetch request fragments to
-- reduce the number of separate range request messages that we send. We send
-- one message per fragment. It is better to send fewer requests for bigger
-- ranges, rather than lots of requests for small ranges.
--
-- We never expect fetch requests to overlap (ie have blocks in common) but we
-- do expect a common case that requests will \"touch\" so that two ranges
-- could be merged into a single contiguous range.
--
-- This semigroup instance implements this merging when possible, otherwise the
-- two lists of fragments are just appended.
--
-- A consequence of merging and sending fewer request messages is that tracking
-- the number of requests in-flight a bit more subtle. To track this accurately
-- we have to look at the /old request/ as well a the updated request after any
-- merging. We meed to account for the /difference/ in the number of fragments
-- in the existing request (if any) and in new request.
--
instance HasHeader header => Semigroup (FetchRequest header) where
  FetchRequest afs@(_:_) <> FetchRequest bfs@(_:_)
    | Just f <- AF.join (last afs) (head bfs)
    = FetchRequest (init afs ++ f : tail bfs)

  FetchRequest afs <> FetchRequest bfs
    = FetchRequest (afs ++ bfs)

fetchRequestMaxSlotNo :: HasHeader header => FetchRequest header -> MaxSlotNo
fetchRequestMaxSlotNo (FetchRequest afs) =
    foldl' max NoMaxSlotNo $ map MaxSlotNo $
      mapMaybe (withOriginToMaybe . AF.headSlot) afs

-- | Tracing types for the various events that change the state
-- (i.e. 'FetchClientStateVars') for a block fetch client.
--
-- Note that while these are all state changes, the 'AddedFetchRequest' occurs
-- in the decision thread while the other state changes occur in the block
-- fetch client threads.
--
data TraceFetchClientState header =

       -- | The block fetch decision thread has added a new fetch instruction
       -- consisting of one or more individual request ranges.
       --
       AddedFetchRequest
         (FetchRequest header)
         (PeerFetchInFlight header)
          PeerFetchInFlightLimits
         (PeerFetchStatus header)

       -- | Mark the point when the fetch client picks up the request added
       -- by the block fetch decision thread. Note that this event can happen
       -- fewer times than the 'AddedFetchRequest' due to fetch request merging.
       --
     | AcknowledgedFetchRequest
         (FetchRequest header)

       -- | Mark the start of receiving a streaming batch of blocks. This will
       -- be followed by one or more 'CompletedBlockFetch' and a final
       -- 'CompletedFetchBatch'.
       --
     | StartedFetchBatch
         (ChainRange (Point header))
         (PeerFetchInFlight header)
          PeerFetchInFlightLimits
         (PeerFetchStatus header)

       -- | Mark the completion of of receiving a single block within a
       -- streaming batch of blocks.
       --
     | CompletedBlockFetch
         (Point header)
         (PeerFetchInFlight header)
          PeerFetchInFlightLimits
         (PeerFetchStatus header)
         NominalDiffTime

       -- | Mark the successful end of receiving a streaming batch of blocks
       --
     | CompletedFetchBatch
         (ChainRange (Point header))
         (PeerFetchInFlight header)
          PeerFetchInFlightLimits
         (PeerFetchStatus header)

       -- | If the other peer rejects our request then we have this event
       -- instead of 'StartedFetchBatch' and 'CompletedFetchBatch'.
       --
     | RejectedFetchBatch
         (ChainRange (Point header))
         (PeerFetchInFlight header)
          PeerFetchInFlightLimits
         (PeerFetchStatus header)

        -- | The client is terminating.  Log the number of outstanding
        -- requests.
        --
      | ClientTerminating Int
  deriving Show

-- | A peer label for use in 'Tracer's. This annotates tracer output as being
-- associated with a given peer identifier.
--
data TraceLabelPeer peerid a = TraceLabelPeer peerid a
  deriving (Eq, Functor, Show)


-- | Add a new fetch request for a single peer. This is used by the fetch
-- decision logic thread to add new fetch requests.
--
-- We have as a pre-condition that all requested blocks are new, i.e. none
-- should appear in the existing 'peerFetchBlocksInFlight'. This is a
-- relatively easy precondition to satisfy since the decision logic can filter
-- its requests based on this in-flight blocks state, and this operation is the
-- only operation that grows the in-flight blocks, and is only used by the
-- fetch decision logic thread.
--
addNewFetchRequest :: (MonadSTM m, HasHeader header)
                   => Tracer m (TraceFetchClientState header)
                   -> (header -> SizeInBytes)
                   -> FetchRequest header
                   -> PeerGSV
                   -> FetchClientStateVars m header
                   -> m (PeerFetchStatus header)
addNewFetchRequest tracer blockFetchSize addedReq gsvs
                   FetchClientStateVars{
                     fetchClientRequestVar,
                     fetchClientInFlightVar,
                     fetchClientStatusVar
                   } = do
    (inflight', currentStatus') <- atomically $ do

      -- Add a new fetch request, or extend or merge with the existing
      -- unacknowledged one.
      --
      -- Fetch request merging makes the update of the in-flight stats subtle.
      -- See the 'FetchRequest' semigroup instance documentation for details.
      -- The upshot is that our in-flight stats update is based on the existing
      -- \"old\" request (if any), the \"added\" one and the resulting
      -- \"merged\" one.
      --
      oldReq    <- peekTFetchRequestVar fetchClientRequestVar
      mergedReq <- writeTFetchRequestVar fetchClientRequestVar
                                           addedReq gsvs inflightlimits

      -- Update our in-flight stats
      inflight <- readTVar fetchClientInFlightVar
      let !inflight' = addHeadersInFlight blockFetchSize
                                          oldReq addedReq mergedReq
                                          inflight
      writeTVar fetchClientInFlightVar inflight'

      -- Set the peer status to busy if it went over the high watermark.
      currentStatus' <- updateCurrentStatus
                          (busyIfOverHighWatermark inflightlimits)
                          fetchClientStatusVar
                          inflight'

      --TODO: think about status aberrant

      return (inflight', currentStatus')

    traceWith tracer $
      AddedFetchRequest
        addedReq
        inflight' inflightlimits
        currentStatus'
    return currentStatus'
  where
    inflightlimits = calculatePeerFetchInFlightLimits gsvs
    --TODO: if recalculating the limits here is expensive we can pass them
    -- along with the fetch request and the gsvs


-- | This is used by the fetch client threads.
--
acknowledgeFetchRequest :: MonadSTM m
                        => Tracer m (TraceFetchClientState header)
                        -> ControlMessageSTM m
                        -> FetchClientStateVars m header
                        -> m (Maybe
                               ( FetchRequest header
                               , PeerGSV
                               , PeerFetchInFlightLimits ))
acknowledgeFetchRequest tracer controlMessageSTM FetchClientStateVars {fetchClientRequestVar} = do
    result <-
      timeoutWithControlMessage controlMessageSTM (takeTFetchRequestVar fetchClientRequestVar)
    case result of
      Nothing -> return result
      Just (request, _, _) -> do
        traceWith tracer (AcknowledgedFetchRequest request)
        return result

startedFetchBatch :: MonadSTM m
                  => Tracer m (TraceFetchClientState header)
                  -> PeerFetchInFlightLimits
                  -> ChainRange (Point header)
                  -> FetchClientStateVars m header
                  -> m ()
startedFetchBatch tracer inflightlimits range
                  FetchClientStateVars {
                    fetchClientInFlightVar,
                    fetchClientStatusVar
                  } = do
    (inflight, currentStatus) <-
      atomically $ (,) <$> readTVar fetchClientInFlightVar
                       <*> readTVar fetchClientStatusVar
    traceWith tracer $
      StartedFetchBatch
        range
        inflight inflightlimits
        currentStatus

completeBlockDownload :: (MonadSTM m, HasHeader header)
                      => Tracer m (TraceFetchClientState header)
                      -> (header -> SizeInBytes)
                      -> PeerFetchInFlightLimits
                      -> header
                      -> NominalDiffTime
                      -> FetchClientStateVars m header
                      -> m ()

completeBlockDownload tracer blockFetchSize inflightlimits header blockDelay
                      FetchClientStateVars {
                        fetchClientInFlightVar,
                        fetchClientStatusVar
                      } = do
    (inflight', currentStatus') <- atomically $ do
      inflight <- readTVar fetchClientInFlightVar
      let !inflight' = deleteHeaderInFlight blockFetchSize header inflight
      writeTVar fetchClientInFlightVar inflight'

      -- Set our status to ready if we're under the low watermark.
      currentStatus' <- updateCurrentStatus
                          (readyIfUnderLowWatermark inflightlimits)
                          fetchClientStatusVar
                          inflight'

    -- TODO: when do we reset the status from PeerFetchStatusAberrant
    -- to PeerFetchStatusReady/Busy?

      return (inflight', currentStatus')

    traceWith tracer $
      CompletedBlockFetch
        (blockPoint header)
        inflight' inflightlimits
        currentStatus'
        blockDelay


completeFetchBatch :: MonadSTM m
                   => Tracer m (TraceFetchClientState header)
                   -> PeerFetchInFlightLimits
                   -> ChainRange (Point header)
                   -> FetchClientStateVars m header
                   -> m ()
completeFetchBatch tracer inflightlimits range
                   FetchClientStateVars {
                     fetchClientInFlightVar,
                     fetchClientStatusVar
                   } = do
    (inflight, currentStatus) <- atomically $ do
      inflight <- readTVar fetchClientInFlightVar
      let !inflight' =
            assert (if peerFetchReqsInFlight inflight == 1
                       then peerFetchBytesInFlight inflight == 0
                         && Set.null (peerFetchBlocksInFlight inflight)
                       else True)
            inflight {
              peerFetchReqsInFlight = peerFetchReqsInFlight inflight - 1
            }
      writeTVar fetchClientInFlightVar inflight'
      currentStatus' <- readTVar fetchClientStatusVar >>= \case
        PeerFetchStatusReady bs IsNotIdle
          | Set.null bs
            && 0 == peerFetchReqsInFlight inflight'
          -> let status = PeerFetchStatusReady Set.empty IsIdle
             in status <$ writeTVar fetchClientStatusVar status
        currentStatus -> pure currentStatus

      return (inflight', currentStatus')

    traceWith tracer $
      CompletedFetchBatch
        range
        inflight inflightlimits
        currentStatus


rejectedFetchBatch :: (MonadSTM m, HasHeader header)
                   => Tracer m (TraceFetchClientState header)
                   -> (header -> SizeInBytes)
                   -> PeerFetchInFlightLimits
                   -> ChainRange (Point header)
                   -> [header]
                   -> FetchClientStateVars m header
                   -> m ()
rejectedFetchBatch tracer blockFetchSize inflightlimits range headers
                   FetchClientStateVars {
                     fetchClientInFlightVar,
                     fetchClientStatusVar
                   } = do
    (inflight', currentStatus') <- atomically $ do
      inflight <- readTVar fetchClientInFlightVar
      let !inflight' =
            (deleteHeadersInFlight blockFetchSize headers inflight) {
              peerFetchReqsInFlight = peerFetchReqsInFlight inflight - 1
            }
      writeTVar fetchClientInFlightVar inflight'

      -- Set our status to ready if we're under the low watermark.
      currentStatus' <- updateCurrentStatus
                          (readyIfUnderLowWatermark inflightlimits)
                          fetchClientStatusVar
                          inflight'

    -- TODO: when do we reset the status from PeerFetchStatusAberrant
    -- to PeerFetchStatusReady/Busy?

      return (inflight', currentStatus')

    traceWith tracer $
      RejectedFetchBatch
        range
        inflight' inflightlimits
        currentStatus'


-- | Given a 'PeerFetchInFlight' update the 'PeerFetchStatus' accordingly.
-- This can be used with one of two policies:
--
-- * 'busyIfOverHighWatermark'
-- * 'readyIfUnderLowWatermark'
--
updateCurrentStatus :: (MonadSTM m, HasHeader header)
                    => (PeerFetchInFlight header -> PeerFetchStatus header)
                    -> StrictTVar m (PeerFetchStatus header)
                    -> PeerFetchInFlight header
                    -> STM m (PeerFetchStatus header)
updateCurrentStatus decideCurrentStatus fetchClientStatusVar inflight = do

    let currentStatus' = decideCurrentStatus inflight

    -- Only update the variable if it changed, to avoid spurious wakeups.
    currentStatus <- readTVar fetchClientStatusVar
    when (currentStatus' /= currentStatus) $
      writeTVar fetchClientStatusVar currentStatus'
    return currentStatus'

-- | Return 'PeerFetchStatusBusy' if we're now over the high watermark.
--
busyIfOverHighWatermark :: PeerFetchInFlightLimits
                        -> PeerFetchInFlight header
                        -> PeerFetchStatus header
busyIfOverHighWatermark inflightlimits inflight
  | peerFetchBytesInFlight inflight >= inFlightBytesHighWatermark inflightlimits
  = PeerFetchStatusBusy
  | otherwise
  = PeerFetchStatusReady
      (peerFetchBlocksInFlight inflight)
      (idleIf (0 == peerFetchReqsInFlight inflight))

-- | Return 'PeerFetchStatusReady' if we're now under the low watermark.
--
readyIfUnderLowWatermark :: PeerFetchInFlightLimits
                         -> PeerFetchInFlight header
                         -> PeerFetchStatus header
readyIfUnderLowWatermark inflightlimits inflight
  | peerFetchBytesInFlight inflight <= inFlightBytesLowWatermark inflightlimits
  = PeerFetchStatusReady
      (peerFetchBlocksInFlight inflight)
      (idleIf (0 == peerFetchReqsInFlight inflight))
  | otherwise
  = PeerFetchStatusBusy


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
       TMergeVar m (FetchRequest header,
                    Last PeerGSV,
                    Last PeerFetchInFlightLimits)

newTFetchRequestVar :: MonadSTM m => STM m (TFetchRequestVar m header)
newTFetchRequestVar = newTMergeVar


-- | Write to the underlying 'TMergeVar' and return the updated 'FetchRequest'
--
writeTFetchRequestVar :: (MonadSTM m, HasHeader header)
                      => TFetchRequestVar m header
                      -> FetchRequest header
                      -> PeerGSV
                      -> PeerFetchInFlightLimits
                      -> STM m (FetchRequest header)
writeTFetchRequestVar v r g l = do
    (r', _, _) <- writeTMergeVar v (r, Last g, Last l)
    return r'

peekTFetchRequestVar :: MonadSTM m
                     => TFetchRequestVar m header
                     -> STM m (Maybe (FetchRequest header))
peekTFetchRequestVar v = fmap (\(x, _, _) -> x) <$> tryReadTMergeVar v

takeTFetchRequestVar :: MonadSTM m
                     => TFetchRequestVar m header
                     -> STM m (FetchRequest header,
                               PeerGSV,
                               PeerFetchInFlightLimits)
takeTFetchRequestVar v = (\(r,g,l) -> (r, getLast g, getLast l))
                     <$> takeTMergeVar v

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
newtype TMergeVar m a = TMergeVar (StrictTMVar m a)

newTMergeVar :: MonadSTM m => STM m (TMergeVar m a)
newTMergeVar = TMergeVar <$> newEmptyTMVar

-- | Merge the current value with the given one and store it, return the updated
-- value.
--
writeTMergeVar :: (MonadSTM m, Semigroup a) => TMergeVar m a -> a -> STM m a
writeTMergeVar (TMergeVar v) x = do
    mx0 <- tryTakeTMVar v
    case mx0 of
      Nothing -> x  <$ putTMVar v x
      Just x0 -> x' <$ putTMVar v x' where !x' = x0 <> x

takeTMergeVar :: MonadSTM m => TMergeVar m a -> STM m a
takeTMergeVar (TMergeVar v) = takeTMVar v

tryReadTMergeVar :: MonadSTM m
                 => TMergeVar m a
                 -> STM m (Maybe a)
tryReadTMergeVar (TMergeVar v) = tryReadTMVar v

{-------------------------------------------------------------------------------
  Syntactic indicator of key precondition about Consensus time conversions
-------------------------------------------------------------------------------}

-- | A new type used to emphasize the precondition of
-- 'Ouroboros.Network.BlockFetch.headerForgeUTCTime' and
-- 'Ouroboros.Network.BlockFetch.blockForgeUTCTime' at each call site.
--
-- At time of writing, the @a@ is either a header or a block. The headers are
-- literally from Consensus (ie provided by ChainSync). Blocks, on the other
-- hand, are indirectly from Consensus: they were fetched only because we
-- favored the corresponding header that Consensus provided.
--
-- NOTE: We define it here so that it can be used consistently throughout the
-- implementation; definiting it only in
-- 'Ouroboros.Network.BlockFetch.BlockFetchConsensusInterface' would be too
-- late.
newtype FromConsensus a = FromConsensus {unFromConsensus :: a}
  deriving (Functor)

instance Applicative FromConsensus where
  pure = FromConsensus
  FromConsensus f <*> FromConsensus a = FromConsensus (f a)
