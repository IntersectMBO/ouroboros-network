{-# LANGUAGE BangPatterns             #-}
{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE GADTs                    #-}
{-# LANGUAGE NamedFieldPuns           #-}
{-# LANGUAGE PolyKinds                #-}
{-# LANGUAGE RecordWildCards          #-}
{-# LANGUAGE ScopedTypeVariables      #-}
{-# LANGUAGE StandaloneDeriving       #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies             #-}
{-# LANGUAGE TypeOperators            #-}

-- hic sunt dracones!
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Ouroboros.Network.BlockFetch.Client
  ( -- * Block fetch protocol client implementation
    blockFetchClient
  , FetchClientContext
  , TraceFetchClientState
  , FetchRequest (..)
  , FetchClientStateVars
    -- * Exception types
  , BlockFetchProtocolFailure
  ) where

import           Control.Concurrent.Class.MonadSTM.Strict
import           Control.Exception (assert)
import           Control.Monad (unless)
import           Control.Monad.Class.MonadThrow
import           Control.Monad.Class.MonadTime.SI

import           Data.Kind (Type)
import qualified Data.Set as Set
import           Data.Type.Queue

import           Control.Tracer (traceWith)

import           Ouroboros.Network.Block

import           Ouroboros.Network.ControlMessage (ControlMessageSTM)
import           Ouroboros.Network.Protocol.BlockFetch.Type

import           Ouroboros.Network.AnchoredFragment (AnchoredFragment)
import qualified Ouroboros.Network.AnchoredFragment as AF
import           Ouroboros.Network.BlockFetch.ClientState
                     (FetchClientContext (..), FetchClientPolicy (..),
                     FetchClientStateVars (fetchClientInFlightVar),
                     FetchRequest (..), FromConsensus (..),
                     PeerFetchInFlight (..), TraceFetchClientState (..),
                     acknowledgeFetchRequest, acknowledgeFetchRequestSTM,
                     completeBlockDownload, completeFetchBatch,
                     fetchClientCtxStateVars, rejectedFetchBatch,
                     startedFetchBatch)
import           Ouroboros.Network.BlockFetch.DeltaQ
                     (PeerFetchInFlightLimits (..), PeerGSV (..))
import           Ouroboros.Network.PeerSelection.PeerMetric.Type
                     (FetchedMetricsTracer)
import           Ouroboros.Network.Protocol.BlockFetch.Client
                     (BlockFetchClientPipelined (..), BlockFetchIdle (..))


data BlockFetchProtocolFailure =
       BlockFetchProtocolFailureTooFewBlocks
     | BlockFetchProtocolFailureTooManyBlocks
     | BlockFetchProtocolFailureWrongBlock
     | BlockFetchProtocolFailureInvalidBody
  deriving (Eq, Show)

instance Exception BlockFetchProtocolFailure



-- | Block fetch 'Queue' kind.
--
type BFQueue block = Queue (BlockFetch block (Point block))


-- | 'F' allows us to pattern match on the type queue state.
--
type F :: Type
       -> BlockFetch block (Point block)
       -> BlockFetch block (Point block)
       -> Type
data F header st st' where
    FBusy      :: ChainRange (Point header)
               -> AnchoredFragment header
               -> PeerFetchInFlightLimits
               -> F header BFBusy      BFIdle
    FStreaming :: ChainRange (Point header)
               -> [header]
               -> PeerFetchInFlightLimits
               -> F header BFStreaming BFIdle

deriving instance (StandardHash header, Show header)
               => Show (F header st st')

pipeliningDepth :: forall block header (q :: Queue (BlockFetch block (Point block))).
                   SingQueueF (F header) q
                -> Int
pipeliningDepth  SingEmptyF                 = 0
pipeliningDepth (SingConsF FBusy {} q)      = 1 + pipeliningDepth q
pipeliningDepth (SingConsF FStreaming {} q) =     pipeliningDepth q

-- | The implementation of the client side of block fetch protocol designed to
-- work in conjunction with our fetch logic.
--
blockFetchClient :: forall header block versionNumber m.
                    (MonadSTM m, MonadThrow m, MonadTime m,
                     MonadMonotonicTime m, HasHeader header,
                     HasHeader block, HeaderHash header ~ HeaderHash block)
                 => versionNumber
                 -> ControlMessageSTM m
                 -> FetchedMetricsTracer m
                 -> FetchClientContext header block m
                 -> BlockFetchClientPipelined block (Point block) m ()
blockFetchClient _version controlMessageSTM reportFetched
                 FetchClientContext {
                   fetchClientCtxTracer    = tracer,
                   fetchClientCtxPolicy    = FetchClientPolicy {
                                               blockFetchSize,
                                               blockMatchesHeader,
                                               addFetchedBlock,
                                               blockForgeUTCTime
                                             },
                   fetchClientCtxStateVars = stateVars
                 } = BlockFetchClientPipelined (blockFetchAwait SingEmptyF)
  where
    -- Await for for a next fetch request.
    --
    blockFetchAwait
      :: forall (q :: BFQueue block).
         SingQueueF (F header) q
      -> m (BlockFetchIdle block (Point block) q m ())

    blockFetchAwait outstanding@(SingConsF FBusy{} _) = return $
      CollectStartBatchSTM
        (do result <- acknowledgeFetchRequestSTM controlMessageSTM stateVars
            return $
              -- m action executed if the
              -- 'acknowledgeFetchRequestSTM' stm action returned
              case result of
                Nothing ->
                  return $ blockFetchTerminate outstanding
                Just (request, gsvs, inflightlimits') -> do
                  traceWith tracer (AcknowledgedFetchRequest request)
                  blockFetchActive outstanding gsvs inflightlimits'
                                   (fetchRequestFragments request)
        )
        -- received 'MsgStartBatch'
        (collectMsgStartBatch outstanding)
        -- received 'MsgNoBlocks'
        (collectMsgNoBlocks outstanding)

    blockFetchAwait outstanding@(SingConsF FStreaming{} _) = return $
      CollectBlockSTM
        (do result <- acknowledgeFetchRequestSTM controlMessageSTM stateVars
            return $
              -- m action executed if the
              -- 'acknowledgeFetchRequestSTM' stm action returned
              case result of
                Nothing ->
                  return $ blockFetchTerminate outstanding
                Just (request, gsvs, inflightlimits') -> do
                  traceWith tracer (AcknowledgedFetchRequest request)
                  blockFetchActive outstanding gsvs inflightlimits'
                                   (fetchRequestFragments request)
        )
        -- received 'MsgBlock'
        (collectMsgBlock outstanding)
        -- received 'MsgBatchDone'
        (collectMsgBatchDone outstanding)

    blockFetchAwait outstanding@SingEmptyF = do
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
          traceWith tracer (ClientTerminating $ pipeliningDepth outstanding)
          return $ blockFetchTerminate outstanding
        Just (request, gsvs, inflightlimits) ->
          blockFetchActive outstanding gsvs inflightlimits
                           (fetchRequestFragments request)


    -- Pipeline all requests.
    --
    blockFetchActive
      :: forall (q :: BFQueue block).
         SingQueueF (F header) q
      -> PeerGSV
      -> PeerFetchInFlightLimits
      -> [AnchoredFragment header]
      -> m (BlockFetchIdle block (Point block) q m ())

    blockFetchActive q gsvs inflightlimits (fragment:fragments) = do
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

      traceWith tracer (SendFetchRequest fragment gsvs)
      return $
        SendMsgRequestRangePipelined
          (castRange range)
          (blockFetchActive (q |> FBusy range fragment inflightlimits)
                            gsvs inflightlimits fragments)

    blockFetchActive q _gsvs _inflightlimits [] = do
      -- assert nothing in flight here
      PeerFetchInFlight {
          peerFetchReqsInFlight,
          peerFetchBytesInFlight,
          peerFetchBlocksInFlight
        } <- atomically $ readTVar (fetchClientInFlightVar stateVars)
      assert
        ( case q of
           SingEmptyF ->
             peerFetchReqsInFlight  == 0 &&
             peerFetchBytesInFlight == 0 &&
             Set.null peerFetchBlocksInFlight
           _ -> True )
        $ blockFetchIdle q


    -- Either collect a result or await for next request.
    --
    -- Note that in all three cases if  if there's no available message to
    -- collect continue with 'blockFetchAwait'.
    blockFetchIdle
      :: forall (q :: BFQueue block).
         SingQueueF (F header) q
      -> m (BlockFetchIdle block (Point block) q m ())

    blockFetchIdle q@(SingConsF FBusy {} _) =
      return $ collectBusy q

    blockFetchIdle q@(SingConsF FStreaming {} _) =
      return $ collectBlock q

    blockFetchIdle q@SingEmptyF =
      blockFetchAwait q


    blockFetchTerminate
      :: forall (q :: Queue (BlockFetch block (Point block))).
         SingQueueF (F header) q
      -> BlockFetchIdle block (Point block) q m ()

    blockFetchTerminate SingEmptyF =
      SendMsgDonePipelined ()

    blockFetchTerminate (SingConsF (FBusy range _fragment inflightlimits) q') =
      CollectStartBatch
        Nothing
        (return $ blockFetchTerminate (FStreaming range [] inflightlimits <| q'))
        (return $ blockFetchTerminate q')

    blockFetchTerminate q@(SingConsF FStreaming {} q') =
      CollectBlock
        Nothing
        (\_block -> return $ blockFetchTerminate q)
        (return $ blockFetchTerminate q')


    -- Collect 'MsgStartBatch' or 'MsgNoBlocks', if neither is available
    -- continue with 'blockFetchAwait.
    --
    -- note: used to be 'receiverBusy'
    collectBusy
      :: forall (q :: BFQueue block).
         SingQueueF (F header) (Tr BFBusy BFIdle <| q)
      -> BlockFetchIdle block (Point block) (Tr BFBusy BFIdle <| q) m ()

    collectBusy q@(SingConsF FBusy{} _) =
      CollectStartBatch
        (Just $ blockFetchAwait q)
        (collectMsgStartBatch q)
        (collectMsgNoBlocks q)

    collectMsgStartBatch
      :: forall (q :: BFQueue block).
         SingQueueF (F header) (Tr BFBusy BFIdle <| q)
      -> m (BlockFetchIdle block (Point block) (Tr BFStreaming BFIdle <| q) m ())
    collectMsgStartBatch (SingConsF (FBusy range fragment inflightlimits) q') = do
      startedFetchBatch tracer inflightlimits range stateVars
      let headers = AF.toOldestFirst fragment
      return (collectBlock (FStreaming range headers inflightlimits <| q'))

    collectMsgNoBlocks
      :: forall (q :: BFQueue block).
         SingQueueF (F header) (Tr BFBusy BFIdle <| q)
      -> m (BlockFetchIdle block (Point block) q m ())
    collectMsgNoBlocks (SingConsF (FBusy range fragment inflightlimits) q') = do
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
      let headers = AF.toOldestFirst fragment
      rejectedFetchBatch tracer blockFetchSize inflightlimits
                         range headers stateVars
      blockFetchIdle q'


    -- note: used to be 'receiverStreaming'
    collectBlock
      :: forall (q :: BFQueue block).
         SingQueueF (F header) (Tr BFStreaming BFIdle <| q)
      -> BlockFetchIdle block (Point block) (Tr BFStreaming BFIdle <| q) m ()

    collectBlock q@(SingConsF FStreaming{} _) =
      CollectBlock
        (Just $ blockFetchAwait q)
        -- received 'MsgBlock'
        (collectMsgBlock q)
        -- received 'MsgBatchDone'
        (collectMsgBatchDone q)

    collectMsgBlock
      :: forall (q :: BFQueue block).
         SingQueueF (F header) (Tr BFStreaming BFIdle <| q)
      -> block
      -> m (BlockFetchIdle block (Point block) (Tr BFStreaming BFIdle <| q) m ())
    collectMsgBlock (SingConsF (FStreaming range headers inflightlimits) q') block =
          case headers of
            []                -> throwIO BlockFetchProtocolFailureTooManyBlocks
            header : headers' -> do
              now <- getCurrentTime
              nowMono <- getMonotonicTime
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

              let hf = getHeaderFields header
                  slotNo = headerFieldSlot hf
              atomically $ traceWith reportFetched ( blockFetchSize header
                                                   , slotNo
                                                   , nowMono
                                                   )

              -- Note that we add the block to the chain DB /before/ updating our
              -- current status and in-flight stats. Otherwise blocks will
              -- disappear from our in-flight set without yet appearing in the
              -- fetched block set. The fetch logic would conclude it has to
              -- download the missing block(s) again.

              -- Update our in-flight stats and our current status
              completeBlockDownload tracer blockFetchSize inflightlimits
                                    header blockDelay stateVars

              return $ collectBlock (FStreaming range headers' inflightlimits <| q')

    collectMsgBatchDone
      :: forall (q :: BFQueue block).
         SingQueueF (F header) (Tr BFStreaming BFIdle <| q)
      -> m (BlockFetchIdle block (Point block)           q  m ())
    collectMsgBatchDone (SingConsF (FStreaming range headers inflightlimits) q') =
      case headers of
        _:_ -> throwIO BlockFetchProtocolFailureTooFewBlocks
        [ ] -> completeFetchBatch tracer inflightlimits range stateVars
            >> blockFetchIdle q'

castRange :: (HeaderHash a ~ HeaderHash b)
          => ChainRange (Point a) -> ChainRange (Point b)
castRange (ChainRange l u) = ChainRange (castPoint l) (castPoint u)
