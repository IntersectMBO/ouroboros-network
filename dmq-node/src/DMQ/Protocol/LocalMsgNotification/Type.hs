{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE FlexibleInstances        #-}
{-# LANGUAGE OverloadedStrings        #-}
{-# LANGUAGE PolyKinds                #-}
{-# LANGUAGE StandaloneDeriving       #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeData                 #-}
{-# LANGUAGE TypeFamilies             #-}


-- | Defines types for the local message notification protocol
--
module DMQ.Protocol.LocalMsgNotification.Type
  ( module DMQ.Protocol.LocalMsgNotification.Type
  , module Network.TypedProtocol.Core
    -- re-exports
  , StBlockingStyle (..)
  , BlockingReplyList (..)
  , SingBlockingStyle (..)
  ) where

import Data.Aeson
import Data.Foldable qualified as Foldable
import Data.Kind
import Data.Singletons

import Network.TypedProtocol.Codec (AnyMessage (..))
import Network.TypedProtocol.Core

import Ouroboros.Network.Protocol.TxSubmission2.Type (BlockingReplyList (..),
           SingBlockingStyle (..), StBlockingStyle (..))
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
         SingI blocking
      => SingBlockingStyle blocking
      -> Message (LocalMsgNotification msg) StIdle (StBusy blocking)

    -- | Reply with a list of messages, along with a flag indicating
    -- whether the server has more messages that it can provide.
    --
    MsgReply
      :: forall blocking msg.
         BlockingReplyList blocking msg
      -> HasMore
      -> Message (LocalMsgNotification msg) (StBusy blocking) StIdle

    -- | The client can terminate the exchange when it has the agency
    --
    MsgClientDone
      :: Message (LocalMsgNotification msg) StIdle StDone

  type StateAgency  (StBusy blocking)  = ServerAgency
  type StateAgency  StIdle  = ClientAgency
  type StateAgency  StDone  = NobodyAgency

  type StateToken = SingMsgNotification

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
instance SingI blocking => StateTokenI (StBusy blocking) where
  stateToken = SingBusy sing
instance StateTokenI StDone where stateToken = SingDone


-- | A boolean-like to indicate whether the server has more messages
-- that it can provide.
--
data HasMore = HasMore | DoesNotHaveMore
  deriving (Eq, Show)

instance ToJSON sig => ToJSON (AnyMessage (LocalMsgNotification sig)) where
  toJSON (AnyMessage msg) = case msg of
    MsgRequest blockingStyle ->
      object [ "type" .= String "MsgRequest"
             , "blockingStyle" .= show blockingStyle
             ]
    MsgReply msgs hasMore ->
      object [ "type" .= String "MsgReply"
             , "msgs" .= Foldable.toList msgs
             , "hasMore" .= case hasMore of
                  HasMore         -> True
                  DoesNotHaveMore -> False
             ]
    MsgClientDone ->
      object [ "type" .= String "MsgClientDone"
             ]
