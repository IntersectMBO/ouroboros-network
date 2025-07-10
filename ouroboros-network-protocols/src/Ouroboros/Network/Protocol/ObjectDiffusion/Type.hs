{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveFoldable             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE EmptyCase                  #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PolyKinds                  #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE StandaloneKindSignatures   #-}
{-# LANGUAGE TypeFamilies               #-}

-- | The type of the object diffusion protocol.
--
-- This is used to diffuse generic objects between nodes.
module Ouroboros.Network.Protocol.ObjectDiffusion.Type
  ( Polarity (..)
  , GetAgency
  , ClientInboundDiffusion
  , ClientOutboundDiffusion
  , ObjectDiffusion (..)
  , Message (..)
  , SingObjectDiffusion (..)
  , SingBlockingStyle (..)
  , StBlockingStyle (..)
  , BlockingReplyList (..)
  , NumObjectIdsToAck (..)
  , NumObjectIdsToReq (..)
  -- re-exports
  , SizeInBytes (..)
  )
where

import           Control.DeepSeq
import           Data.Kind                        (Type)
import           Data.List.NonEmpty               (NonEmpty)
import           Data.Monoid                      (Sum (..))
import           Data.Singletons
import           Data.Word                        (Word16)
import           GHC.Generics
import           Network.TypedProtocol.Core
import           NoThunks.Class                   (NoThunks (..))
import           Ouroboros.Network.SizeInBytes    (SizeInBytes (..))
import           Ouroboros.Network.Util.ShowProxy
import           Quiet                            (Quiet (..))

data Polarity = ClientInbound | ClientOutbound

-- see https://moduscreate.slack.com/archives/C0937JQQ1F0/p1752155221158189 to see why we need such complication
-- Basically, the StateAgency associated type in the Protocol class only inspects the state, but we need to inspect the polarity variable which is part of the kind of the state to know the agency.
type GetAgency ::
  forall polarity objectId object. ObjectDiffusion polarity objectId object -> Agency
type family GetAgency (st :: ObjectDiffusion polarity objectId object) where
  GetAgency StInit = ClientAgency
  GetAgency @ClientInbound StIdle = ClientAgency
  GetAgency @ClientOutbound StIdle = ServerAgency
  GetAgency @ClientInbound (StObjectIds b) = ServerAgency
  GetAgency @ClientOutbound (StObjectIds b) = ClientAgency
  GetAgency @ClientInbound StObjects = ServerAgency
  GetAgency @ClientOutbound StObjects = ClientAgency
  GetAgency StDone = NobodyAgency

-- | The kind of the object diffusion protocol, and the types of the states in
-- the protocol state machine.
--
-- We describe this protocol using the label \"inbound\" for the peer that is
-- receiving objects, and \"outbound\" for the one sending them.
type ObjectDiffusion :: Polarity -> Type -> Type -> Type
data ObjectDiffusion polarity objectId object where
  -- | Initial protocol message.
  StInit :: ObjectDiffusion polarity objectId object
  -- | The inbound node has agency; it can either terminate, ask for object
  -- identifiers or ask for objects.
  --
  -- There is no timeout in this state.
  StIdle :: ObjectDiffusion polarity objectId object
  -- | The outbound node has agency; it must reply with a list of object
  -- identifiers that it wishes to submit.
  --
  -- There are two sub-states for this, for blocking and non-blocking cases.
  StObjectIds :: StBlockingStyle -> ObjectDiffusion polarity objectId object
  -- | The outbound node has agency; it must reply with the list of
  -- objects.
  StObjects :: ObjectDiffusion polarity objectId object
  -- | Nobody has agency; termination state.
  StDone :: ObjectDiffusion polarity objectId object

type ClientInboundDiffusion = ObjectDiffusion 'ClientInbound
type ClientOutboundDiffusion = ObjectDiffusion 'ClientOutbound

instance
  ( ShowProxy objectId
  , ShowProxy object
  ) =>
  ShowProxy (ObjectDiffusion polarity objectId object)
  where
  showProxy _ =
    concat
      [ "ObjectDiffusion "
      , showProxy (Proxy :: Proxy objectId)
      , " "
      , showProxy (Proxy :: Proxy object)
      ]

instance ShowProxy (StIdle :: ObjectDiffusion polarity objectId object) where
  showProxy _ = "StIdle"

type SingObjectDiffusion ::
  ObjectDiffusion polarity objectId object ->
  Type
data SingObjectDiffusion k where
  SingInit :: SingObjectDiffusion StInit
  SingIdle :: SingObjectDiffusion StIdle
  SingObjectIds ::
    SingBlockingStyle stBlocking ->
    SingObjectDiffusion (StObjectIds stBlocking)
  SingObjects :: SingObjectDiffusion StObjects
  SingDone :: SingObjectDiffusion StDone

deriving instance Show (SingObjectDiffusion k)

instance StateTokenI StInit where stateToken = SingInit

instance StateTokenI StIdle where stateToken = SingIdle

instance
  SingI stBlocking =>
  StateTokenI (StObjectIds stBlocking)
  where
  stateToken = SingObjectIds sing

instance StateTokenI StObjects where stateToken = SingObjects

instance StateTokenI StDone where stateToken = SingDone

data StBlockingStyle where
  -- | In this sub-state the reply need not be prompt. There is no timeout.
  StBlocking :: StBlockingStyle
  -- | In this state the peer must reply. There is a timeout.
  StNonBlocking :: StBlockingStyle

newtype NumObjectIdsToAck = NumObjectIdsToAck {getNumObjectIdsToAck :: Word16}
  deriving (Eq, Ord, NFData, Generic)
  deriving newtype (Num, Enum, Real, Integral, Bounded, NoThunks)
  deriving Semigroup via (Sum Word16)
  deriving Monoid via (Sum Word16)
  deriving Show via (Quiet NumObjectIdsToAck)

newtype NumObjectIdsToReq = NumObjectIdsToReq {getNumObjectIdsToReq :: Word16}
  deriving (Eq, Ord, NFData, Generic)
  deriving newtype (Num, Enum, Real, Integral, Bounded, NoThunks)
  deriving Semigroup via (Sum Word16)
  deriving Monoid via (Sum Word16)
  deriving Show via (Quiet NumObjectIdsToReq)

-- | There are some constraints of the protocol that are not captured in the
-- types of the messages, but are documented with the messages. Violation
-- of these constraints is also a protocol error. The constraints are intended
-- to ensure that implementations are able to work in bounded space.
instance Protocol (ObjectDiffusion polarity objectId object) where
  -- \| The messages in the object diffusion protocol.
  --
  -- In this protocol the consumer (inbound side, server role) always
  -- initiates and the producer (outbound side, client role) replies.
  -- This makes it a pull based protocol where the receiver manages the
  -- control flow.
  --
  -- The protocol involves asking for object identifiers, and then
  -- asking for objects corresponding to the identifiers of interest.
  --
  -- There are two ways to ask for object identifiers, blocking and
  -- non-blocking. They otherwise have the same semantics.
  --
  -- The protocol maintains a notional FIFO of \"outstanding\" object
  -- identifiers that have been provided but not yet acknowledged. Only
  -- objects that are outstanding can be requested: they can be
  -- requested in any order, but at most once. Object identifiers are
  -- acknowledged in the same FIFO order they were provided in. The
  -- acknowledgement is included in the same messages used to ask for more
  -- object identifiers.
  data Message (ObjectDiffusion polarity objectId object) from to where
    MsgInit ::
      Message (ObjectDiffusion polarity objectId object) StInit StIdle
    -- \| Request a list of object identifiers from the client, and confirm a
    -- number of outstanding object identifiers.
    --
    -- With 'TokBlocking' this is a blocking operation: the response will always
    -- have at least one object identifier, and it does not expect a prompt
    -- response: there is no timeout. This covers the case when there is nothing
    -- else to do but wait.
    --
    -- With 'TokNonBlocking' this is a non-blocking operation: the response may
    -- be an empty list and this does expect a prompt response. This covers high
    -- throughput use cases where we wish to pipeline, by interleaving requests
    -- for additional object identifiers with requests for objects, which
    -- requires these requests not block.
    --
    -- The request gives the maximum number of object identifiers that can be
    -- accepted in the response. This must be greater than zero in the
    -- 'TokBlocking' case. In the 'TokNonBlocking' case either the numbers
    -- acknowledged or the number requested __MUST__ be non-zero. In either
    -- case, the number requested __MUST__ not put the total outstanding over
    -- the fixed protocol limit.
    --
    -- The request also gives the number of outstanding object identifiers that
    -- can now be acknowledged. The actual objects to acknowledge are known to
    -- the peer based on the FIFO order in which they were provided.
    --
    -- There is no choice about when to use the blocking case versus the
    -- non-blocking case, it depends on whether there are any remaining
    -- unacknowledged objects (after taking into account the ones acknowledged
    -- in this message):
    --
    -- \* The blocking case __MUST__ be used when there are zero remaining
    --   unacknowledged objects.
    --
    -- \* The non-blocking case __MUST__ be used when there are non-zero
    --   remaining unacknowledged objects.
    MsgRequestObjectIds ::
      forall (blocking :: StBlockingStyle) polarity objectId object.
      SingBlockingStyle blocking ->
      NumObjectIdsToAck ->
      -- \^ Acknowledge this number of outstanding objects
      NumObjectIdsToReq ->
      -- \^ Request up to this number of object ids
      Message (ObjectDiffusion polarity objectId object) StIdle (StObjectIds blocking)
    -- \| Reply with a list of object identifiers for available objects, along
    -- with the size of each object.
    --
    -- The list must not be longer than the maximum number requested.
    --
    -- In the 'StObjectIds' 'Blocking' state the list must be non-empty while in
    -- the 'StObjectIds' 'NonBlocking' state the list may be empty.
    --
    -- These objects are added to the notional FIFO of outstanding object
    -- identifiers for the protocol.
    --
    -- The order in which these object identifiers are returned must be the
    -- order in which they are submitted to the mempool, to preserve dependent
    -- objects.
    MsgReplyObjectIds ::
      BlockingReplyList blocking (objectId, SizeInBytes) ->
      Message (ObjectDiffusion polarity objectId object) (StObjectIds blocking) StIdle
    -- \| Request one or more objects corresponding to the given object
    -- identifiers.
    --
    -- While it is the responsibility of the replying peer to keep within
    -- pipelining in-flight limits, the sender must also cooperate by keeping
    -- the total requested across all in-flight requests within the limits.
    --
    -- It is an error to ask for object identifiers that were not
    -- previously announced (via 'MsgReplyObjectIds').
    --
    -- It is an error to ask for object identifiers that are not
    -- outstanding or that were already asked for.
    MsgRequestObjects ::
      [objectId] ->
      Message (ObjectDiffusion polarity objectId object) StIdle StObjects
    -- \| Reply with the requested objects, or implicitly discard.
    --
    -- Objects can become invalid between the time the object
    -- identifier was sent and the object being requested. Invalid
    -- (including committed) objects do not need to be sent.
    --
    -- Any object identifiers requested but not provided in this reply
    -- should be considered as if this peer had never announced them. (Note
    -- that this is no guarantee that the object is invalid, it may still
    -- be valid and available from another peer).
    MsgReplyObjects ::
      [object] ->
      Message (ObjectDiffusion polarity objectId object) StObjects StIdle
    -- \| Termination message, initiated by the client when the server is making
    -- a blocking call for more object identifiers.
    MsgDone ::
      Message (ObjectDiffusion polarity objectId object) (StObjectIds StBlocking) StDone

  type StateAgency st = GetAgency st -- FIXME: do not change
  type StateToken = SingObjectDiffusion

instance
  (NFData objectId, NFData object) =>
  NFData (Message (ObjectDiffusion polarity objectId object) from to)
  where
  rnf MsgInit                          = ()
  rnf (MsgRequestObjectIds tkbs w1 w2) = rnf tkbs `seq` rnf w1 `seq` rnf w2
  rnf (MsgReplyObjectIds brl)          = rnf brl
  rnf (MsgRequestObjects objIds)       = rnf objIds
  rnf (MsgReplyObjects objects)        = rnf objects
  rnf MsgDone                          = ()

-- | The value level equivalent of 'BlockingStyle'.
--
-- This is also used in 'MsgRequestObjectIds' where it is interpreted (and can be
-- encoded) as a 'Bool' with 'True' for blocking, and 'False' for non-blocking.
data SingBlockingStyle (k :: StBlockingStyle) where
  SingBlocking :: SingBlockingStyle StBlocking
  SingNonBlocking :: SingBlockingStyle StNonBlocking

deriving instance Eq (SingBlockingStyle b)

deriving instance Show (SingBlockingStyle b)

type instance Sing = SingBlockingStyle

instance SingI StBlocking where sing = SingBlocking

instance SingI StNonBlocking where sing = SingNonBlocking

instance NFData (SingBlockingStyle b) where
  rnf SingBlocking    = ()
  rnf SingNonBlocking = ()

-- | We have requests for lists of things. In the blocking case the
-- corresponding reply must be non-empty, whereas in the non-blocking case
-- and empty reply is fine.
data BlockingReplyList (blocking :: StBlockingStyle) a where
  BlockingReply :: NonEmpty a -> BlockingReplyList StBlocking a
  NonBlockingReply :: [a] -> BlockingReplyList StNonBlocking a

deriving instance Eq a => Eq (BlockingReplyList blocking a)

deriving instance Show a => Show (BlockingReplyList blocking a)

deriving instance Foldable (BlockingReplyList blocking)

instance NFData a => NFData (BlockingReplyList blocking a) where
  rnf (BlockingReply as)    = rnf as
  rnf (NonBlockingReply as) = rnf as

deriving instance
  (Eq objectId, Eq object) =>
  Eq (Message (ObjectDiffusion polarity objectId object) from to)

deriving instance
  (Show objectId, Show object) =>
  Show (Message (ObjectDiffusion polarity objectId object) from to)
