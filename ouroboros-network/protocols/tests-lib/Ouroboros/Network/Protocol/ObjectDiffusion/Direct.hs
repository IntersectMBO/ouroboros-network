{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}

module Ouroboros.Network.Protocol.ObjectDiffusion.Direct
  ( directPipelined,
    objectDiffusionOutbound,
    TraceObjectDiffusionDirect (..),
    objectDiffusionInbound,
    InboundState (..),
    initialInboundState
  ) where


import Network.TypedProtocol.Core
import Network.TypedProtocol.Proofs (Queue (..), enqueue)

import Ouroboros.Network.Protocol.ObjectDiffusion.Inbound
import Ouroboros.Network.Protocol.ObjectDiffusion.Outbound
import Ouroboros.Network.Protocol.ObjectDiffusion.Type (
    BlockingReplyList (..),
    SingBlockingStyle (..),
    NumObjectIdsReq (..),
    NumObjectIdsAck (..),
  )

import Control.Exception (assert)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Sequence.Strict (StrictSeq)
import Data.Map.Strict (Map)
import Data.Word (Word16)
import Control.Tracer (Tracer, traceWith)
import qualified Data.Sequence.Strict as Seq
import qualified Data.Map.Strict as Map
import qualified Data.Foldable as Foldable
import Control.Monad (when)
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.List.NonEmpty as NonEmpty


directPipelined
  :: forall objectId object m a b.
     Monad m
  => ObjectDiffusionOutbound         objectId object m a
  -> ObjectDiffusionInboundPipelined objectId object m b
  -> m b
directPipelined (ObjectDiffusionOutbound mOutbound)
                (ObjectDiffusionInboundPipelined inbound) = do
    outbound <- mOutbound
    directSender EmptyQ inbound outbound
  where
    directSender :: forall (n :: N).
                    Queue         n (Collect objectId object)
                 -> InboundStIdle n objectId object m b
                 -> OutboundStIdle  objectId object m a
                 -> m b
    directSender q (SendMsgRequestObjectIdsBlocking ackNo reqNo inboundNext)
                   OutboundStIdle{recvMsgRequestObjectIds} = do
      reply <- recvMsgRequestObjectIds SingBlocking ackNo reqNo
      case reply of
        SendMsgReplyObjectIds (BlockingReply objectIds) outbound' -> do
          let inbound' = inboundNext objectIds
          directSender q inbound' outbound'

    directSender q (SendMsgRequestObjectIdsPipelined ackNo reqNo inbound')
                   OutboundStIdle{recvMsgRequestObjectIds} = do
      reply <- recvMsgRequestObjectIds SingNonBlocking ackNo reqNo
      case reply of
        SendMsgReplyObjectIds (NonBlockingReply objectIds) outbound' -> do
          directSender (enqueue (CollectObjectIds reqNo objectIds) q) inbound' outbound'

    directSender q (SendMsgRequestObjectsPipelined objectIds inbound')
                   OutboundStIdle{recvMsgRequestObjects} = do
      SendMsgReplyObjects objects outbound' <- recvMsgRequestObjects objectIds
      directSender (enqueue (CollectObjects objectIds objects) q) inbound' outbound'

    directSender q (CollectPipelined (Just noWaitInbound') _inboundNext) outbound = do
      directSender q noWaitInbound' outbound

    directSender (ConsQ c q) (CollectPipelined _maybeNoWaitInbound' inboundNext) outbound = do
      let inbound' = inboundNext c
      directSender q inbound' outbound

    directSender q (WithEffect mInbound) outbound = do
      inbound' <- mInbound
      directSender q inbound' outbound

    directSender EmptyQ (SendMsgDone v) _outbound = pure v

--
-- Outbound implementation
--

data TraceObjectDiffusionDirect objectId object =
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


objectDiffusionOutbound
  :: forall objectId object m.
     (Ord objectId, Show objectId, Monad m)
  => Tracer m (TraceObjectDiffusionDirect objectId object)
  -> (object -> objectId)
  -> Word16  -- ^ Maximum number of unacknowledged object IDs allowed
  -> [object]
  -> ObjectDiffusionOutbound objectId object m ()
objectDiffusionOutbound tracer objectId maxUnacked =
  ObjectDiffusionOutbound . pure . client Seq.empty Map.empty
  where
    client :: StrictSeq objectId
           -> Map objectId object
           -> [object]
           -> OutboundStIdle objectId object m ()
    client !unackedSeq !unackedMap remainingObjects =
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

          recvMsgRequestObjectIds :: forall blocking.
                                     SingBlockingStyle blocking
                                  -> NumObjectIdsAck
                                  -> NumObjectIdsReq
                                  -> m (OutboundStObjectIds blocking objectId object m ())
          recvMsgRequestObjectIds blocking ackNo reqNo = do
            traceWith tracer $
              EventRecvMsgRequestObjectIds
                unackedSeq unackedMap remainingObjects ackNo reqNo

            when (ackNo > fromIntegral (Seq.length unackedSeq)) $
              error $ "objectDiffusionOutbound.recvMsgRequestObjectIds: "
                   <> "peer acknowledged more object IDs than possible"

            when (  fromIntegral (Seq.length unackedSeq)
                  - getNumObjectIdsAck ackNo
                  + getNumObjectIdsReq reqNo
                  > maxUnacked) $
              error $ "objectDiffusionOutbound.recvMsgRequestObjectIds: "
                   <> "peer requested more object IDs than permitted"

            let unackedSeq' = Seq.drop (fromIntegral ackNo) unackedSeq
                unackedMap' = Foldable.foldl' (flip Map.delete) unackedMap
                                     (Seq.take (fromIntegral ackNo) unackedSeq)

            case blocking of
              SingBlocking | not (Seq.null unackedSeq')
                -> error $ "objectDiffusionOutbound.recvMsgRequestObjectIds: "
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

            return $! case (blocking, unackedExtra) of
              (SingBlocking, []) ->
                -- NOTE: the current version of this protocol doesn't have a
                -- way to inform the inbound side that it's caught up. This
                -- means that there's no propert inhabitant for this type.
                --
                -- This will be solved by:
                -- https://github.com/tweag/cardano-peras/issues/144
                error "We can't return anything meaningful here, so let's crash"

              (SingBlocking, obj:objs) ->
                SendMsgReplyObjectIds
                  (BlockingReply (fmap objectId (obj :| objs)))
                  (client unackedSeq'' unackedMap'' remainingObjects')

              (SingNonBlocking, objs) ->
                SendMsgReplyObjectIds
                  (NonBlockingReply (fmap objectId objs))
                  (client unackedSeq'' unackedMap'' remainingObjects')

          recvMsgRequestObjects :: [objectId]
                                -> m (OutboundStObjects objectId object m ())
          recvMsgRequestObjects objectIds = do
            traceWith tracer $
              EventRecvMsgRequestObjects
                unackedSeq unackedMap remainingObjects objectIds
            case [ objId | objId <- objectIds, objId `Map.notMember` unackedMap ] of
              [] -> pure (SendMsgReplyObjects objects client')
                where
                  objects     = fmap (unackedMap Map.!) objectIds
                  client'     = client unackedSeq unackedMap' remainingObjects
                  unackedMap' = foldr Map.delete unackedMap objectIds
                  -- Here we remove from the map, while the seq stays unchanged.
                  -- This enforces that each object can be requested at most once.

              missing -> error $ "objectDiffusionOutbound.recvMsgRequestObjects: "
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


objectDiffusionInbound
  :: forall objectId object m.
     (Ord objectId)
  => Tracer m (TraceObjectDiffusionDirect objectId object)
  -> (object -> objectId)
  -> Word16  -- ^ Maximum number of unacknowledged object IDs allowed
  -> Word16  -- ^ Maximum number of object IDs to request in any one go
  -> Word16  -- ^ Maximum number of objects to request in any one go
  -> ObjectDiffusionInboundPipelined objectId object m [object]
objectDiffusionInbound
  _tracer
  objectId
  maxUnacked
  maxObjectIdsToRequest
  maxObjectsToRequest =
    ObjectDiffusionInboundPipelined (serverIdle [] Zero initialInboundState)
  where
    serverIdle :: forall (n :: N).
                  [object]
               -> Nat n
               -> InboundState objectId object
               -> InboundStIdle n objectId object m [object]
    serverIdle accum Zero st
        -- There are no replies in flight, but we do know some more objects we
        -- can ask for, so lets ask for them and more object IDs.
      | canRequestMoreObjects st
      = serverReqObjects accum Zero st

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
          (\objectIds -> do
              handleReply accum Zero st {
                 numObjectsToAcknowledge    = 0,
                 requestedObjectIdsInFlight = numObjectIdsToRequest
               }
               . CollectObjectIds numObjectIdsToRequest
               . NonEmpty.toList $ objectIds
          )

    serverIdle accum (Succ n) st
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
          (Just (serverReqObjects accum (Succ n) st))
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
      serverIdle accum n st {
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
      -- delay it to serverReqObjects, otherwise we could end up blocking in
      -- serverIdle on more pipelined results rather than being able to move on.
      serverIdle accum' n st {
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
                objsMap = Map.fromList [ (objectId obj, obj) | obj <- objects ]
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

    serverReqObjects :: forall (n :: N).
                        [object]
                     -> Nat n
                     -> InboundState objectId object
                     -> InboundStIdle n objectId object m [object]
    serverReqObjects accum n st =
        SendMsgRequestObjectsPipelined
          (Set.toList objectsToRequest)
          (serverReqObjectIds accum (Succ n) st {
             availableObjectIds = availableObjectIds'
          })
      where
        (objectsToRequest, availableObjectIds') =
          Set.splitAt (fromIntegral maxObjectsToRequest) (availableObjectIds st)

    serverReqObjectIds :: forall (n :: N).
                          [object]
                       -> Nat n
                       -> InboundState objectId object
                       -> InboundStIdle n objectId object m [object]
    serverReqObjectIds accum n st
      | numObjectIdsToRequest > 0
      = SendMsgRequestObjectIdsPipelined
          (numObjectsToAcknowledge st)
          numObjectIdsToRequest
          (serverIdle accum (Succ n) st {
             requestedObjectIdsInFlight = requestedObjectIdsInFlight st
                                        + numObjectIdsToRequest,
             numObjectsToAcknowledge    = 0
          })

      | otherwise
      = serverIdle accum n st
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
