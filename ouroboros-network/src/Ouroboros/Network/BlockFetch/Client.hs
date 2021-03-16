{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}

-- hic sunt dracones!
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Ouroboros.Network.BlockFetch.Client (
    -- * Block fetch protocol client implementation
    blockFetchClient,
    BlockFetchClient,
    FetchClientContext,
    TraceFetchClientState,
    FetchRequest(..),
    FetchClientStateVars,
    -- * Exception types
    BlockFetchProtocolFailure,
  ) where

import           Control.Monad (unless)
import           Control.Monad.Class.MonadSTM.Strict
import           Control.Monad.Class.MonadThrow
import           Control.Monad.Class.MonadTime
import           Control.Exception (assert)

import qualified Data.Set as Set

import           Control.Tracer (traceWith)

import           Ouroboros.Network.Block

import           Ouroboros.Network.Mux (ControlMessageSTM)
import           Ouroboros.Network.NodeToNode.Version (NodeToNodeVersion)
import           Ouroboros.Network.Protocol.BlockFetch.Type
import           Network.TypedProtocol.Core
import           Network.TypedProtocol.Pipelined

import           Ouroboros.Network.AnchoredFragment (AnchoredFragment)
import qualified Ouroboros.Network.AnchoredFragment as AF
import           Ouroboros.Network.BlockFetch.ClientState
                   ( FetchClientContext(..)
                   , FetchClientPolicy(..)
                   , FetchClientStateVars (fetchClientInFlightVar)
                   , FetchRequest(..)
                   , FromConsensus (..)
                   , PeerFetchInFlight(..)
                   , TraceFetchClientState (..)
                   , fetchClientCtxStateVars
                   , acknowledgeFetchRequest
                   , startedFetchBatch
                   , completeBlockDownload
                   , completeFetchBatch
                   , rejectedFetchBatch )
import           Ouroboros.Network.BlockFetch.DeltaQ
                   ( PeerGSV(..), PeerFetchInFlightLimits(..) )


data BlockFetchProtocolFailure =
       BlockFetchProtocolFailureTooFewBlocks
     | BlockFetchProtocolFailureTooManyBlocks
     | BlockFetchProtocolFailureWrongBlock
     | BlockFetchProtocolFailureInvalidBody
  deriving (Eq, Show)

instance Exception BlockFetchProtocolFailure


-- | TODO: use a fetch client wrapper type rather than the raw
--         PeerPipelined, and eliminate this alias. It is only here
--         to avoid large types leaking into the consensus layer.
type BlockFetchClient header block m a =
  FetchClientContext header block m ->
  PeerPipelined (BlockFetch block (Point block)) AsClient BFIdle m a

-- | The implementation of the client side of block fetch protocol designed to
-- work in conjunction with our fetch logic.
--
blockFetchClient :: forall header block m.
                    (MonadSTM m, MonadThrow m, MonadTime m,
                     HasHeader header, HasHeader block,
                     HeaderHash header ~ HeaderHash block)
                 => NodeToNodeVersion
                 -> ControlMessageSTM m
                 -> FetchClientContext header block m
                 -> PeerPipelined (BlockFetch block (Point block)) AsClient BFIdle m ()
blockFetchClient _version controlMessageSTM
                 FetchClientContext {
                   fetchClientCtxTracer    = tracer,
                   fetchClientCtxPolicy    = FetchClientPolicy {
                                               blockFetchSize,
                                               blockMatchesHeader,
                                               addFetchedBlock,
                                               blockForgeUTCTime
                                             },
                   fetchClientCtxStateVars = stateVars
                 } =
    PeerPipelined (senderAwait Zero)
  where
    senderIdle :: forall n.
                  Nat n
               -> PeerSender (BlockFetch block (Point block)) AsClient
                             BFIdle n () m ()

    -- We have no requests to send. Check if we have any pending pipelined
    -- results to collect. If so, go round and collect any more. If not, block
    -- and wait for some new requests.
    senderIdle (Succ outstanding) =
      SenderCollect (Just (senderAwait (Succ outstanding)))
                    (\_ -> senderIdle outstanding)

    -- And similarly if there are no pending pipelined results at all.
    senderIdle Zero = SenderEffect $ do
      -- assert nothing in flight here
      PeerFetchInFlight {
          peerFetchReqsInFlight,
          peerFetchBytesInFlight,
          peerFetchBlocksInFlight
        } <- atomically $ readTVar (fetchClientInFlightVar stateVars)

      assert
        ( peerFetchReqsInFlight  == 0 &&
          peerFetchBytesInFlight == 0 &&
          Set.null peerFetchBlocksInFlight )
        $ pure (senderAwait Zero)

    senderAwait :: forall n.
                   Nat n
                -> PeerSender (BlockFetch block (Point block)) AsClient
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
      result <-
          acknowledgeFetchRequest tracer controlMessageSTM stateVars

      case result of
        Nothing -> do
          traceWith tracer (ClientTerminating $ natToInt outstanding)
          return $ senderTerminate outstanding
        Just (request, gsvs, inflightlimits) ->
          return $ senderActive outstanding gsvs inflightlimits
                                (fetchRequestFragments request)

    senderActive :: forall n.
                    Nat n
                 -> PeerGSV
                 -> PeerFetchInFlightLimits
                 -> [AnchoredFragment header]
                 -> PeerSender (BlockFetch block (Point block)) AsClient
                               BFIdle n () m ()

    -- We now do have some requests that we have accepted but have yet to
    -- actually send out. Lets send out the first one.
    senderActive outstanding gsvs inflightlimits (fragment:fragments) =
      SenderEffect $ do
{-
        now <- getMonotonicTime
        --TODO: should we pair this up with the senderAwait earlier?
        inFlight  <- readTVar fetchClientInFlightVar

        let blockTrailingEdges =
              blockArrivalShedule
                gsvs
                inFlight
                (map snd fragment)

        timeout <- newTimeout (head blockTrailingEdges)
        fork $ do
          fired <- awaitTimeout timeout
          when fired $
            atomically (writeTVar _ PeerFetchStatusAberrant)
-}
        let range :: ChainRange (Point header)
            !range = assert (not (AF.null fragment)) $
                     ChainRange (blockPoint lower)
                                (blockPoint upper)
              where
                Right lower = AF.last fragment
                Right upper = AF.head fragment

        return $
          SenderPipeline
            (ClientAgency TokIdle)
            (MsgRequestRange (castRange range))
            (receiverBusy range fragment inflightlimits)
            (senderActive (Succ outstanding) gsvs inflightlimits fragments)

    -- And when we run out, go back to idle.
    senderActive outstanding _ _ [] = senderIdle outstanding


    -- Terminate the sender; 'controlMessageSTM' returned 'Terminate'.
    senderTerminate :: forall n.
                       Nat n
                    -> PeerSender (BlockFetch block (Point block)) AsClient
                                  BFIdle n () m ()
    senderTerminate Zero =
      SenderYield (ClientAgency TokIdle)
                  MsgClientDone
                  (SenderDone TokDone ())
    senderTerminate (Succ n) =
      SenderCollect Nothing
                    (\_ -> senderTerminate n)


    receiverBusy :: ChainRange (Point header)
                 -> AnchoredFragment header
                 -> PeerFetchInFlightLimits
                 -> PeerReceiver (BlockFetch block (Point block)) AsClient
                                 BFBusy BFIdle m ()
    receiverBusy range fragment inflightlimits =
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
          MsgNoBlocks   ->
            ReceiverEffect $ do
              -- Update our in-flight stats and our current status
              rejectedFetchBatch tracer blockFetchSize inflightlimits
                                 range headers stateVars
              return (ReceiverDone ())
            where
              headers = AF.toOldestFirst fragment

          MsgStartBatch ->
            ReceiverEffect $ do
              startedFetchBatch tracer inflightlimits range stateVars
              return (receiverStreaming inflightlimits range headers)
            where
              headers = AF.toOldestFirst fragment

    receiverStreaming :: PeerFetchInFlightLimits
                      -> ChainRange (Point header)
                      -> [header]
                      -> PeerReceiver (BlockFetch block (Point block)) AsClient
                                      BFStreaming BFIdle m ()
    receiverStreaming inflightlimits range headers =
      ReceiverAwait
        (ServerAgency TokStreaming) $ \msg ->
        case (msg, headers) of
          (MsgBatchDone, []) -> ReceiverEffect $ do
            completeFetchBatch tracer inflightlimits range stateVars
            return (ReceiverDone ())


          (MsgBlock block, header:headers') -> ReceiverEffect $ do
            now <- getCurrentTime
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
              throwIO BlockFetchProtocolFailureWrongBlock

            -- This is moderately expensive.
            unless (blockMatchesHeader header block) $
              throwIO BlockFetchProtocolFailureInvalidBody

            -- write it to the volatile block store
            --FIXME: this is not atomic wrt the in-flight and status updates
            -- above. This would allow a read where the block is no longer
            -- in-flight but is still not in the fetched block store.
            -- either 1. make it atomic, or 2. do this first, or 3. some safe
            -- interleaving

            -- Add the block to the chain DB, notifying of any new chains.
            addFetchedBlock (castPoint (blockPoint header)) block

            forgeTime <- atomically $ blockForgeUTCTime $ FromConsensus block
            let blockDelay = diffUTCTime now forgeTime

            -- Note that we add the block to the chain DB /before/ updating our
            -- current status and in-flight stats. Otherwise blocks will
            -- disappear from our in-flight set without yet appearing in the
            -- fetched block set. The fetch logic would conclude it has to
            -- download the missing block(s) again.

            -- Update our in-flight stats and our current status
            completeBlockDownload tracer blockFetchSize inflightlimits
                                  header blockDelay stateVars

            return (receiverStreaming inflightlimits range headers')

          (MsgBatchDone, (_:_)) -> ReceiverEffect $
            throwIO BlockFetchProtocolFailureTooFewBlocks

          (MsgBlock _, []) -> ReceiverEffect $
            throwIO BlockFetchProtocolFailureTooManyBlocks

castRange :: (HeaderHash a ~ HeaderHash b)
          => ChainRange (Point a) -> ChainRange (Point b)
castRange (ChainRange l u) = ChainRange (castPoint l) (castPoint u)
