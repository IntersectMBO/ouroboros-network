{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}

module Ouroboros.Network.BlockFetch.Client (
    -- * Block fetch protocol client implementation
    blockFetchClient,
    FetchClientPolicy(..),
    TraceFetchClientEvent(..),
    FetchClientStateVars,

    -- * Registry of block fetch clients
    FetchClientRegistry(..),
    newFetchClientRegistry,
    bracketFetchClient,
    readFetchClientsStatus,
    readFetchClientsStates,
    readFetchClientsReqVars,
  ) where

import qualified Data.Set as Set
import qualified Data.Map as Map
import           Data.Map (Map)

import           Control.Monad (unless)
import           Control.Monad.Class.MonadSTM
import           Control.Monad.Class.MonadThrow
import           Control.Monad.Class.MonadTime
import           Control.Exception (assert)
import           Control.Tracer (Tracer, traceWith)

import           Ouroboros.Network.Block

import           Ouroboros.Network.Protocol.BlockFetch.Type
import           Network.TypedProtocol.Core
import           Network.TypedProtocol.Pipelined

import           Ouroboros.Network.BlockFetch.ClientState
                   ( FetchClientStateVars(..), PeerFetchStatus(..)
                   , PeerFetchInFlight(..), initialPeerFetchInFlight
                   , FetchRequest(..)
                   , TFetchRequestVar
                   , newTFetchRequestVar
                   , acceptFetchRequest
                   , completeBlockDownload
                   , completeFetchBatch )
import           Ouroboros.Network.BlockFetch.DeltaQ
                   ( PeerGSV(..), SizeInBytes
                   , PeerFetchInFlightLimits(..)
                   , calculatePeerFetchInFlightLimits )


-- TODO #468 extract this from BlockFetchConsensusInterface
data FetchClientPolicy header block m =
     FetchClientPolicy {
       blockFetchSize     :: header -> SizeInBytes,
       blockMatchesHeader :: header -> block -> Bool,
       addFetchedBlock    :: Point block -> block -> m ()
     }


data BlockFetchProtocolFailure =
       BlockFetchProtocolFailureTooFewBlocks
     | BlockFetchProtocolFailureTooManyBlocks
     | BlockFetchProtocolFailureWrongBlock
     | BlockFetchProtocolFailureInvalidBody
  deriving (Eq, Show)

instance Exception BlockFetchProtocolFailure

data TraceFetchClientEvent header =
       AcceptedFetchRequest
         (FetchRequest (Point header))
         (PeerFetchInFlight header)
          PeerFetchInFlightLimits
         (PeerFetchStatus header)
     | CompletedBlockFetch
         (Point header)
         (PeerFetchInFlight header)
          PeerFetchInFlightLimits
         (PeerFetchStatus header)
  deriving Show

-- | The implementation of the client side of block fetch protocol designed to
-- work in conjunction with our fetch logic.
--
blockFetchClient :: forall header block m.
                    (MonadSTM m, MonadTime m, MonadThrow m,
                     HasHeader header, HasHeader block,
                     HeaderHash header ~ HeaderHash block)
                 => Tracer m (TraceFetchClientEvent header)
                 -> FetchClientPolicy header block m
                 -> FetchClientStateVars header m
                 -> STM m PeerGSV
                 -> PeerPipelined (BlockFetch header block) AsClient BFIdle m ()
blockFetchClient tracer
                 FetchClientPolicy {
                   blockFetchSize,
                   blockMatchesHeader,
                   addFetchedBlock
                 }
                 stateVars
                 readPeerGSVs =
    PeerPipelined (senderIdle Zero [])
  where
    senderIdle :: forall n.
                  Nat n
               -> [[header]]
               -> PeerSender (BlockFetch header block) AsClient
                             BFIdle n () m ()

    -- We have no requests to send. Check if we have any pending pipelined
    -- results to collect. If so, go round and collect any more. If not, block
    -- and wait for some new requests.
    senderIdle (Succ outstanding) [] =
      SenderCollect (Just (senderAwait (Succ outstanding)))
                    (\_ -> senderIdle outstanding [])

    -- And similarly if there are no pending pipelined results at all.
    senderIdle Zero [] = senderAwait Zero
    --TODO: assert nothing in flight here

    -- We now do have some requests that we have accepted but have yet to
    -- actually send out. Lets send out the first one.
    senderIdle outstanding (fragment:fragments) =
      SenderEffect $ do
{-
        now <- getMonotonicTime
        (outboundGSV, inboundGSV) <- atomically readPeerGSVs
        --TODO: should we pair this up with the senderAwait earlier?
        inFlight  <- readTVar fetchClientInFlightVar

        let blockTrailingEdges =
              blockArrivalShedule
                outboundGSV inboundGSV
                inFlight
                (map snd fragment)

        timeout <- newTimeout (head blockTrailingEdges)
        fork $ do
          fired <- awaitTimeout timeout
          when fired $
            atomically (writeTVar _ PeerFetchStatusAberrant)
-}
        let range = assert (not (null fragment)) $
                    ChainRange (blockPoint (head fragment))
                               (blockPoint (last fragment))
        return $
          SenderPipeline
            (ClientAgency TokIdle)
            (MsgRequestRange range)
            (receiverBusy fragment)
            (senderIdle (Succ outstanding) fragments)

    senderAwait :: forall n.
                   Nat n
                -> PeerSender (BlockFetch header block) AsClient
                              BFIdle n () m ()
    senderAwait outstanding =
      SenderEffect $ do
      -- Atomically grab our next request and update our tracking state.
      -- We have now accepted this request.
      --
      -- It is important to note that we only update our tracking state when
      -- we /accept/ the request, not when the fetch logic /sets/ the request.
      -- The fetching logic can update the request up until the point where
      -- we accept it here. From here on the request is considered to be
      -- in-flight, and the tracking state that the fetch logic uses now
      -- reflects that.
      --
      (fragments, inflight, inflightlimits, currentStatus) <-
        acceptFetchRequest blockFetchSize readPeerGSVs stateVars

      traceWith tracer $ AcceptedFetchRequest
                           (fmap blockPoint (FetchRequest fragments))
                           inflight inflightlimits
                           currentStatus

      return (senderIdle outstanding fragments)


    receiverBusy :: [header]
                 -> PeerReceiver (BlockFetch header block) AsClient
                                 BFBusy BFIdle m ()
    receiverBusy headers =
      ReceiverAwait
        (ServerAgency TokBusy) $ \msg ->
        case msg of
          -- The server is reporting that the range we asked for does not exist.
          -- This can happen (even if we didn't make any mistakes) if their
          -- chain forked in the time between when they told us and when we
          -- asked for this range of blocks. If this happens, it should
          -- certainly be the case that this peer doesn't continue to tell us
          -- that this range of blocks is in their chain.
          --
          -- FIXME: For now we will not do the detailed error checking to check
          -- that the peer is not cheating us. Nor will we track these failure
          -- points to make sure we do not ask for extensions of this again.
          MsgNoBlocks   -> ReceiverDone ()
          --TODO: also adjust the in-flight stats

          MsgStartBatch -> ReceiverEffect $ do
            inFlightLimits <- calculatePeerFetchInFlightLimits <$>
                                atomically readPeerGSVs
            return $ receiverStreaming inFlightLimits headers

    receiverStreaming :: PeerFetchInFlightLimits
                      -> [header]
                      -> PeerReceiver (BlockFetch header block) AsClient
                                      BFStreaming BFIdle m ()
    receiverStreaming inFlightLimits headers =
      ReceiverAwait
        (ServerAgency TokStreaming) $ \msg ->
        case (msg, headers) of
          (MsgBatchDone, []) -> ReceiverEffect $ do
            completeFetchBatch stateVars
            return (ReceiverDone ())


          (MsgBlock block, header:headers') -> ReceiverEffect $ do
            --TODO: consider how to enforce expected block size limit.
            -- They've lied and are sending us a massive amount of data.
            -- Resource consumption attack.

{-
            -- Now it's totally possible that the timeout already fired
            -- if not, we can update it, making sure the delay is > 0
            now <- getMonotonicTime
            updateTimeout timeout (diffTime now )
-}

            unless (blockPoint header == castPoint (blockPoint block)) $
              throwM BlockFetchProtocolFailureWrongBlock

            -- This is moderately expensive.
            unless (blockMatchesHeader header block) $
              throwM BlockFetchProtocolFailureInvalidBody

            -- write it to the volatile block store
            --FIXME: this is not atomic wrt the in-flight and status updates
            -- above. This would allow a read where the block is no longer
            -- in-flight but is still not in the fetched block store.
            -- either 1. make it atomic, or 2. do this first, or 3. some safe
            -- interleaving

            -- Add the block to the chain DB, notifying of any new chains.
            addFetchedBlock (castPoint (blockPoint header)) block

            -- Note that we add the block to the chain DB /before/ updating our
            -- current status and in-flight stats. Otherwise blocks will
            -- disappear from our in-flight set without yet appearing in the
            -- fetched block set. The fetch logic would conclude it has to
            -- download the missing block(s) again.

            -- Update our in-flight stats and our current status
            (inflight, currentStatus) <-
              completeBlockDownload blockFetchSize inFlightLimits
                                    header headers' stateVars

            traceWith tracer $ CompletedBlockFetch
                                 (blockPoint header)
                                 inflight inFlightLimits
                                 currentStatus

            return (receiverStreaming inFlightLimits headers')

          (MsgBatchDone, (_:_)) -> ReceiverEffect $
            throwM BlockFetchProtocolFailureTooFewBlocks

          (MsgBlock _, []) -> ReceiverEffect $
            throwM BlockFetchProtocolFailureTooManyBlocks



-- | A registry for the threads that are executing the client side of the
-- 'BlockFetch' protocol to communicate with our peers.
--
-- The registry contains the shared variables we use to communicate with these
-- threads, both to track their status and to provide instructions.
--
-- The threads add\/remove themselves to\/from this registry when they start up
-- and shut down.
--
newtype FetchClientRegistry peer header m =
        FetchClientRegistry (TVar m (Map peer (FetchClientStateVars header m)))

newFetchClientRegistry :: MonadSTM m => m (FetchClientRegistry peer header m)
newFetchClientRegistry = FetchClientRegistry <$> newTVarM Map.empty

bracketFetchClient :: (MonadThrow m, MonadSTM m, Ord peer)
                   => FetchClientRegistry peer header m
                   -> peer
                   -> (FetchClientStateVars header m -> m a)
                   -> m a
bracketFetchClient (FetchClientRegistry registry) peer =
    bracket register unregister
  where
    register = atomically $ do
      fetchClientInFlightVar <- newTVar initialPeerFetchInFlight
      fetchClientStatusVar   <- newTVar (PeerFetchStatusReady Set.empty)
      fetchClientRequestVar  <- newTFetchRequestVar
      let stateVars = FetchClientStateVars {
                        fetchClientStatusVar,
                        fetchClientInFlightVar,
                        fetchClientRequestVar
                      }
      modifyTVar' registry (Map.insert peer stateVars)
      return stateVars

    unregister FetchClientStateVars{fetchClientStatusVar} =
      atomically $ do
        writeTVar fetchClientStatusVar PeerFetchStatusShutdown
        modifyTVar' registry (Map.delete peer)

-- | A read-only 'STM' action to get the current 'PeerFetchStatus' for all
-- fetch clients in the 'FetchClientRegistry'.
--
readFetchClientsStatus :: MonadSTM m
                       => FetchClientRegistry peer header m
                       -> STM m (Map peer (PeerFetchStatus header))
readFetchClientsStatus (FetchClientRegistry registry) =
  readTVar registry >>= traverse (readTVar . fetchClientStatusVar)

-- | A read-only 'STM' action to get the current 'PeerFetchStatus' and
-- 'PeerFetchInFlight' for all fetch clients in the 'FetchClientRegistry'.
--
readFetchClientsStates :: MonadSTM m
                       => FetchClientRegistry peer header m
                       -> STM m (Map peer (PeerFetchStatus   header,
                                           PeerFetchInFlight header))
readFetchClientsStates (FetchClientRegistry registry) =
  readTVar registry >>=
  traverse (\s -> (,) <$> readTVar (fetchClientStatusVar s)
                      <*> readTVar (fetchClientInFlightVar s))

-- | A read-only 'STM' action to get the current 'TFetchRequestVar' for all
-- fetch clients in the 'FetchClientRegistry'.
--
readFetchClientsReqVars :: MonadSTM m
                        => FetchClientRegistry peer header m
                        -> STM m (Map peer (TFetchRequestVar m header))
readFetchClientsReqVars (FetchClientRegistry registry) =
  readTVar registry >>= return . Map.map fetchClientRequestVar

