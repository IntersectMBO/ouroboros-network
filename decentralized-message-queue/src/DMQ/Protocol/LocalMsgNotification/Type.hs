{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE DeriveFoldable           #-}
{-# LANGUAGE FlexibleInstances        #-}
{-# LANGUAGE PolyKinds                #-}
{-# LANGUAGE StandaloneDeriving       #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeData                 #-}
{-# LANGUAGE TypeFamilies             #-}

-- | Defines types for the local message notification protocol
--
module DMQ.Protocol.LocalMsgNotification.Type where

import Control.DeepSeq
import Data.Kind
import Data.List.NonEmpty
import Network.TypedProtocol.Core
import Ouroboros.Network.Util.ShowProxy

-- | There are some constraints of the protocol that are not captured in the
-- types of the messages, but are documented with the messages. Violation
-- of these constraints is also a protocol error. The constraints are intended
-- to ensure that implementations are able to work in bounded space.
--
instance Protocol (LocalMsgNotification msg) where
  -- | The messages in the protocol.
  --
  -- There are two ways to ask for messages, blocking and
  -- non-blocking
  -- TODO: blocking only should suffice?.
  -- They otherwise have the same semantics.
  --
  data Message (LocalMsgNotification msg) from to where

    -- | Request a list of messages from the server,
    --
    -- * The blocking case is used when the server has announced it has no
    --   further messages to provide, and otherwise the non-blocking request
    --   must be used.
    --
    -- TODO: this request should have the max number of messages that shall
    -- be returned?
    MsgRequest
      :: forall blocking msg.
         SingBlockingStyle blocking
      -> Message (LocalMsgNotification msg) StIdle (StBusy blocking)

    -- | Reply with a list of messages, along with a flag indicating
    -- whether the server has more messages that it can provide.
    --
    -- TODO: maybe this should just be always a blocking request?
    --
    MsgReply
      :: forall blocking msg.
         BlockingReplyList blocking msg
      -> HasMore
      -> Message (LocalMsgNotification msg) (StBusy blocking) StIdle

    -- | Termination message, initiated by the server when the client is
    -- making a blocking call for more messages.
    --
    -- TODO: is this message desireable/necessary?
    --
    MsgServerDone
      :: Message (LocalMsgNotification msg) (StBusy StBlocking) StDone

    -- | The client can terminate the exchange when it has the agency
    --
    MsgClientDone
      :: Message (LocalMsgNotification msg) StIdle StDone

  type StateAgency  (StBusy blocking)  = ServerAgency
  type StateAgency  StIdle  = ClientAgency
  type StateAgency  StDone  = NobodyAgency

  type StateToken = SingMsgNotification

instance ( NFData msg
         ) => NFData (Message (LocalMsgNotification msg) from to) where
  rnf (MsgRequest block)  = rnf block
  rnf (MsgReply a !_more) = rnf a
  rnf MsgServerDone{}     = ()
  rnf MsgClientDone{}     = ()

deriving instance (Eq msg) =>
                  Eq (Message (LocalMsgNotification msg) from to)

deriving instance (Show msg) =>
                  Show (Message (LocalMsgNotification msg) from to)


-- | The kind of the local message notification protocol, and the types of
-- the states in the protocol state machine.
--
-- It is parameterised over the type of messages
--
type LocalMsgNotification :: Type -> Type
type data LocalMsgNotification msg where
  StIdle :: LocalMsgNotification msg
  StBusy :: StBlockingStyle -> LocalMsgNotification msg
  StDone :: LocalMsgNotification msg

instance ( ShowProxy msg
         ) => ShowProxy (LocalMsgNotification msg) where
    showProxy _ =
      "LocalMsgNotification " ++ showProxy (Proxy :: Proxy msg)


-- | The kind of blocking state
--
type data StBlockingStyle where

  -- | In this sub-state the reply need not be prompt, and must
  -- contain non-trivial payload.
  --
  StBlocking    :: StBlockingStyle

  -- | In this state the peer shall respond promptly, possibly
  -- with trivial/empty payload.
  --
  StNonBlocking :: StBlockingStyle


-- | The value level equivalent of 'BlockingStyle'.
--
-- This is also used in 'MsgRequest' where it is interpreted (and can be
-- encoded) as a 'Bool' with 'True' for blocking, and 'False' for non-blocking.
--
data SingBlockingStyle (k :: StBlockingStyle) where
  SingBlocking    :: SingBlockingStyle StBlocking
  SingNonBlocking :: SingBlockingStyle StNonBlocking

deriving instance Eq   (SingBlockingStyle b)
deriving instance Show (SingBlockingStyle b)

instance NFData (SingBlockingStyle b) where
  rnf SingBlocking    = ()
  rnf SingNonBlocking = ()


-- | a singleton witness for protocol state
--
type SingMsgNotification :: LocalMsgNotification msg
                         -> Type
data SingMsgNotification k where
    SingIdle  :: SingMsgNotification  StIdle
    SingBusy  :: SingBlockingStyle blocking
              -> SingMsgNotification (StBusy blocking)
    SingDone  :: SingMsgNotification  StDone

deriving instance Show (SingMsgNotification k)

instance StateTokenI StIdle where stateToken = SingIdle
instance {-# OVERLAPPABLE #-} StateTokenI (StBusy blocking) where stateToken = error "StBusy blocking not implemented"
instance StateTokenI (StBusy StBlocking) where stateToken = SingBusy SingBlocking
instance StateTokenI (StBusy StNonBlocking) where stateToken = SingBusy SingNonBlocking
instance StateTokenI StDone where stateToken = SingDone


-- | A boolean-like to indicate whether the server has more messages
-- that it can provide.
--
data HasMore = HasMore | DoesNotHaveMore
  deriving (Eq, Show)


-- | We have requests for lists of things. In the blocking case the
-- corresponding reply must be non-empty, whereas in the non-blocking case
-- and empty reply is fine.
--
data BlockingReplyList (blocking :: StBlockingStyle) a where
  BlockingReply    :: NonEmpty a  -> BlockingReplyList StBlocking    a
  NonBlockingReply ::         [a] -> BlockingReplyList StNonBlocking a

deriving instance Eq   a => Eq   (BlockingReplyList blocking a)
deriving instance Show a => Show (BlockingReplyList blocking a)
deriving instance Foldable (BlockingReplyList blocking)

instance NFData a => NFData (BlockingReplyList blocking a) where
  rnf (BlockingReply as)    = rnf as
  rnf (NonBlockingReply as) = rnf as
