{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Ouroboros.Network.Protocol.ObjectDiffusion.Examples
  ( testObjectDiffusionOutbound
  , TraceObjectDiffusionTestImplem (..)
  , testObjectDiffusionInbound
  , InboundState (..)
  , initialInboundState
  ) where


import Network.TypedProtocol.Core

import Ouroboros.Network.Protocol.ObjectDiffusion.Inbound
import Ouroboros.Network.Protocol.ObjectDiffusion.Outbound
import Ouroboros.Network.Protocol.ObjectDiffusion.Type (NumObjectIdsAck (..),
           NumObjectIdsReq (..), ObjectIdsReplyList (..),
           ObjectIdsRequestKind (..))

import Control.Exception (assert)
import Control.Monad (when)
import Control.Tracer (Tracer, traceWith)
import Data.Foldable qualified as Foldable
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Sequence.Strict (StrictSeq)
import Data.Sequence.Strict qualified as Seq
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Word (Word16)

--
-- Outbound implementation
--

data TraceObjectDiffusionTestImplem objectId object =
    EventRecvMsgRequestObjectIds
      (StrictSeq objectId)
      (Map objectId object)
      [object]
      NumObjectIdsAck
      NumObjectIdsReq
  | EventRecvMsgRequestObjects
      (StrictSeq objectId)
      (Map objectId object)
      [object]
      [objectId]
  deriving Show


testObjectDiffusionOutbound
  :: forall objectId object m.
     (Ord objectId, Show objectId, Monad m)
  => Tracer m (TraceObjectDiffusionTestImplem objectId object)
  -> (object -> objectId)
  -> Word16  -- ^ Maximum number of unacknowledged object IDs allowed
  -> [object]
  -> ObjectDiffusionOutbound objectId object m ()
testObjectDiffusionOutbound tracer objectId maxUnacked =
  ObjectDiffusionOutbound . pure . outboundIdle Seq.empty Map.empty
  where
    outboundIdle :: StrictSeq objectId
           -> Map objectId object
           -> [object]
           -> OutboundStIdle objectId object m ()
    outboundIdle !unackedSeq !unackedMap remainingObjects =
        assert invariant
        OutboundStIdle {
          recvMsgRequestObjectIds,
          recvMsgRequestObjects,
          recvMsgDone
        }
        where
          invariant =
            Map.isSubmapOfBy
              (\_ _ -> True)
              unackedMap
              (Map.fromList [ (x, ()) | x <- Foldable.toList unackedSeq ])

          recvMsgRequestObjectIds :: forall kind.
                                     ObjectIdsRequestKind kind
                                  -> NumObjectIdsAck
                                  -> NumObjectIdsReq
                                  -> m (OutboundStObjectIds kind objectId object m ())
          recvMsgRequestObjectIds requestKind ackNo reqNo = do
            traceWith tracer $
              EventRecvMsgRequestObjectIds
                unackedSeq unackedMap remainingObjects ackNo reqNo

            when (ackNo > fromIntegral (Seq.length unackedSeq)) $
              error $ "testObjectDiffusionOutbound.recvMsgRequestObjectIds: "
                   <> "peer acknowledged more object IDs than possible"

            when (  fromIntegral (Seq.length unackedSeq)
                  - getNumObjectIdsAck ackNo
                  + getNumObjectIdsReq reqNo
                  > maxUnacked) $
              error $ "testObjectDiffusionOutbound.recvMsgRequestObjectIds: "
                   <> "peer requested more object IDs than permitted"

            let unackedSeq' = Seq.drop (fromIntegral ackNo) unackedSeq
                unackedMap' = Foldable.foldl' (flip Map.delete) unackedMap
                                (Seq.take (fromIntegral ackNo) unackedSeq)

            case requestKind of
              RequestObjectIdsBlocking | not (Seq.null unackedSeq')
                -> error $ "testObjectDiffusionOutbound.recvMsgRequestObjectIds: "
                        <> "peer made a blocking request for more object IDs when "
                        <> "there are still unacknowledged object IDs."
              _ -> return ()

            -- This example is eager, it always provides as many as asked for,
            -- up to the number remaining available.
            let unackedExtra      = take (fromIntegral reqNo) remainingObjects
                unackedSeq''      = unackedSeq'
                                 <> Seq.fromList (fmap objectId unackedExtra)
                unackedMap''      = unackedMap'
                                 <> Map.fromList [ (objectId obj, obj)
                                                 | obj <- unackedExtra ]
                remainingObjects' = drop (fromIntegral reqNo) remainingObjects

            return $! case (requestKind, unackedExtra) of
              (RequestObjectIdsBlocking, []) ->
                SendMsgAwaitReply $ pure $
                  SendMsgServerIdle
                    (outboundIdle unackedSeq'' unackedMap'' remainingObjects')

              (RequestObjectIdsBlocking, obj : objs) ->
                SendMsgReplyObjectIds
                  (BlockingReply (fmap objectId (obj :| objs)))
                  (outboundIdle unackedSeq'' unackedMap'' remainingObjects')

              (RequestObjectIdsNonBlocking, objs) ->
                SendMsgReplyObjectIds
                  (NonBlockingReply (fmap objectId objs))
                  (outboundIdle unackedSeq'' unackedMap'' remainingObjects')

          recvMsgRequestObjects :: [objectId]
                                -> m (OutboundStObjects objectId object m ())
          recvMsgRequestObjects objectIds = do
            traceWith tracer $
              EventRecvMsgRequestObjects
                unackedSeq unackedMap remainingObjects objectIds
            case [ objId | objId <- objectIds, objId `Map.notMember` unackedMap ] of
              [] -> pure (SendMsgReplyObjects objects outbound')
                where
                  objects     = fmap (unackedMap Map.!) objectIds
                  outbound'   = outboundIdle unackedSeq unackedMap' remainingObjects
                  unackedMap' = foldr Map.delete unackedMap objectIds
                  -- Here we remove from the map, while the seq stays unchanged.
                  -- This enforces that each object can be requested at most once.

              missing -> error $ "testObjectDiffusionOutbound.recvMsgRequestObjects: "
                              <> "requested missing ObjectIds: " <> show missing

          recvMsgDone :: m ()
          recvMsgDone = pure ()


--
-- Inbound implementation
--

data InboundState objectId object = InboundState {
    requestedObjectIdsInFlight :: NumObjectIdsReq,
    unacknowledgedObjectIds    :: StrictSeq objectId,
    availableObjectIds         :: Set objectId,
    bufferedObjects            :: Map objectId (Maybe object),
    numObjectsToAcknowledge    :: NumObjectIdsAck
  }
  deriving Show


initialInboundState :: InboundState objectId object
initialInboundState = InboundState 0 Seq.empty Set.empty Map.empty 0


testObjectDiffusionInbound
  :: forall objectId object m.
     (Ord objectId, Applicative m)
  => Tracer m (TraceObjectDiffusionTestImplem objectId object)
  -> (object -> objectId)
  -> Word16  -- ^ Maximum number of unacknowledged object IDs allowed
  -> Word16  -- ^ Maximum number of object IDs to request in any one go
  -> Word16  -- ^ Maximum number of objects to request in any one go
  -> ObjectDiffusionInboundPipelined objectId object m [object]
testObjectDiffusionInbound
  _tracer
  objectId
  maxUnacked
  maxObjectIdsToRequest
  maxObjectsToRequest =
    ObjectDiffusionInboundPipelined (inboundIdle [] Zero initialInboundState)
  where
    inboundIdle :: forall (n :: N).
                  [object]
               -> Nat n
               -> InboundState objectId object
               -> InboundStIdle n objectId object m [object]
    inboundIdle accum Zero st
        -- There are no replies in flight, but we do know some more objects we
        -- can ask for, so lets ask for them and more object IDs.
      | canRequestMoreObjects st
      = inboundReqObjects accum Zero st

        -- There's no replies in flight, and we have no more objects we can ask
        -- for so the only remaining thing to do is to ask for more object IDs.
        -- Since this is the only thing to do now, we make this a blocking call.
      | otherwise
      , let numObjectIdsToRequest =
              NumObjectIdsReq $ maxObjectIdsToRequest `min` maxUnacked
      = assert (requestedObjectIdsInFlight st == 0
             && Seq.null (unacknowledgedObjectIds st)
             && Set.null (availableObjectIds st)
             && Map.null (bufferedObjects st)) $
        SendMsgRequestObjectIdsBlocking
          (numObjectsToAcknowledge st)
          numObjectIdsToRequest
          (pure ())
          (handleReply accum Zero st {
                    numObjectsToAcknowledge    = 0,
                    requestedObjectIdsInFlight = numObjectIdsToRequest
                  }
                  . CollectObjectIds numObjectIdsToRequest
                  . NonEmpty.toList)
          (SendMsgDone accum)

    inboundIdle accum (Succ n) st
        -- We have replies in flight and we should eagerly collect them if
        -- available, but there are objects to request too so we should not
        -- block waiting for replies.
        --
        -- Having requested more objects, we opportunistically ask for more
        -- object IDs in a non-blocking way. This is how we pipeline asking for
        -- both objects and object IDs.
        --
        -- It's important not to pipeline more requests for object IDs when we
        -- have no objects to ask for, since (with no other guard) this will
        -- put us into a busy-polling loop.
        --
      | canRequestMoreObjects st
      = CollectPipelined
          (Just (inboundReqObjects accum (Succ n) st))
          (handleReply accum n st)

        -- In this case there is nothing else to do so we block until we
        -- collect a reply.
      | otherwise
      = CollectPipelined
          Nothing
          (handleReply accum n st)

    canRequestMoreObjects :: InboundState k object -> Bool
    canRequestMoreObjects st =
        not (Set.null (availableObjectIds st))

    handleReply :: forall (n :: N).
                   [object]
                -> Nat n
                -> InboundState objectId object
                -> Collect objectId object
                -> InboundStIdle n objectId object m [object]
    handleReply accum n st (CollectObjectIds reqNo objectIds) =
      -- Upon receiving a batch of new object IDs we extend our available set,
      -- and extended the unacknowledged sequence.
      inboundIdle accum n st {
        requestedObjectIdsInFlight = requestedObjectIdsInFlight st - reqNo,
        unacknowledgedObjectIds    = unacknowledgedObjectIds st
                                  <> Seq.fromList objectIds,
        availableObjectIds         = availableObjectIds st
                                  <> Set.fromList objectIds
      }

    handleReply accum n st (CollectObjects objectIds objects) =
      -- When we receive a batch of objects, in general we get a subset of those
      -- that we asked for, with the remainder now deemed unnecessary.
      -- But we still have to acknowledge the object IDs we were given. This
      -- combined with the fact that we request objects out of order means our
      -- bufferedObjects has to track all the object IDs we asked for, even
      -- though not all have replies.
      --
      -- We have to update the unacknowledgedObjectIds here eagerly and not
      -- delay it to inboundReqObjects, otherwise we could end up blocking in
      -- inboundIdle on more pipelined results rather than being able to move on.
      inboundIdle accum' n st {
        bufferedObjects         = bufferedObjects'',
        unacknowledgedObjectIds = unacknowledgedObjectIds',
        numObjectsToAcknowledge = numObjectsToAcknowledge st
                                + fromIntegral (Seq.length acknowledgedObjectIds)
      }
      where
        objectIdsRequestedWithObjectsReceived :: [(objectId, Maybe object)]
        objectIdsRequestedWithObjectsReceived =
          [ (objId, mbObj)
          | let objsMap :: Map objectId object
                objsMap  = Map.fromList [ (objectId obj, obj) | obj <- objects ]
          , objId <- objectIds
          , let !mbObj = Map.lookup objId objsMap
          ]

        bufferedObjects'  = bufferedObjects st
                         <> Map.fromList objectIdsRequestedWithObjectsReceived

        -- Check if having received more objects we can now confirm any (in
        -- strict order in the unacknowledgedObjectIds sequence).
        (acknowledgedObjectIds, unacknowledgedObjectIds') =
          Seq.spanl (`Map.member` bufferedObjects') (unacknowledgedObjectIds st)

        -- If so we can add the acknowledged objects to our accumulating result
        accum' = accum
              <> foldr
                   (\objId r -> maybe r (:r) (bufferedObjects' Map.! objId))
                   []
                   acknowledgedObjectIds

        -- And remove acknowledged objects from our buffer
        bufferedObjects'' =
          Foldable.foldl' (flip Map.delete) bufferedObjects' acknowledgedObjectIds

    inboundReqObjects :: forall (n :: N).
                         [object]
                      -> Nat n
                      -> InboundState objectId object
                      -> InboundStIdle n objectId object m [object]
    inboundReqObjects accum n st =
        SendMsgRequestObjectsPipelined
          (Set.toList objectsToRequest)
          (inboundReqObjectIds accum (Succ n) st {
             availableObjectIds = availableObjectIds'
          })
      where
        (objectsToRequest, availableObjectIds') =
          Set.splitAt (fromIntegral maxObjectsToRequest) (availableObjectIds st)

    inboundReqObjectIds :: forall (n :: N).
                           [object]
                        -> Nat n
                        -> InboundState objectId object
                        -> InboundStIdle n objectId object m [object]
    inboundReqObjectIds accum n st
      | numObjectIdsToRequest > 0
      = SendMsgRequestObjectIdsPipelined
          (numObjectsToAcknowledge st)
          numObjectIdsToRequest
          (inboundIdle accum (Succ n) st {
             requestedObjectIdsInFlight = requestedObjectIdsInFlight st
                                        + numObjectIdsToRequest,
             numObjectsToAcknowledge    = 0
          })

      | otherwise
      = inboundIdle accum n st
      where
        -- This definition is justified by the fact that the
        -- 'numObjectsToAcknowledge' are not included in the
        -- 'unacknowledgedObjectIds'.
        numObjectIdsToRequest =
          NumObjectIdsReq $
                (maxUnacked
                  - fromIntegral (Seq.length (unacknowledgedObjectIds st))
                  - getNumObjectIdsReq (requestedObjectIdsInFlight st))
          `min` maxObjectIdsToRequest
