{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE FlexibleInstances        #-}
{-# LANGUAGE GADTs                    #-}
{-# LANGUAGE PolyKinds                #-}
{-# LANGUAGE ScopedTypeVariables      #-}
{-# LANGUAGE StandaloneDeriving       #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeData                 #-}
{-# LANGUAGE TypeFamilies             #-}

-- | Defines the type of local message submission protocol
--
module DMQ.Protocol.LocalMsgSubmission.Type where

import Control.DeepSeq
import Data.Kind
import Network.TypedProtocol.Core
import Ouroboros.Network.Util.ShowProxy

-- | The kind of the local message submission protocol, and the types of
-- the states in the protocol state machine.
--
-- It is parameterised over the type of messages and the type of reasons
-- used when rejecting a message.
--
type LocalMsgSubmission :: Type -> Type -> Type
type data LocalMsgSubmission msg reject where

  -- | The client has agency; it can submit a message or terminate.
  --
  -- There is no timeout in this state.
  --
  StIdle :: LocalMsgSubmission msg reject

  -- | The server has agency; it must process the submitted message and
  -- either accept or reject it (with a reason).
  --
  -- There is a timeout in this state. If the mempool is full and remains so
  -- for a period then the message should be rejected with a suitable
  -- temporary failure reason.
  --
  StBusy :: LocalMsgSubmission msg reject

  -- | Nobody has agency. The terminal state.
  --
  StDone :: LocalMsgSubmission msg reject

instance ( ShowProxy msg
         , ShowProxy reject
         ) => ShowProxy (LocalMsgSubmission msg reject) where
    showProxy _ = concat
      [ "LocalMsgSubmission ("
      , showProxy (Proxy :: Proxy msg)
      , ") ("
      , showProxy (Proxy :: Proxy reject)
      , ")"
      ]

type SingLocalMsgSubmission :: LocalMsgSubmission msg reject
                            -> Type
data SingLocalMsgSubmission k where
  SingIdle :: SingLocalMsgSubmission StIdle
  SingBusy :: SingLocalMsgSubmission StBusy
  SingDone :: SingLocalMsgSubmission StDone

deriving instance Show (SingLocalMsgSubmission k)

instance StateTokenI StIdle where stateToken = SingIdle
instance StateTokenI StBusy where stateToken = SingBusy
instance StateTokenI StDone where stateToken = SingDone

instance Protocol (LocalMsgSubmission msg reject) where

  -- | The messages in the message submission protocol.
  --
  -- In this protocol the client always initiates and the server replies.
  -- This makes it a push based protocol where the client manages the
  -- control flow. It is acceptable for this protocol to be push based
  -- because this protocol is only for use between a node and local client.
  --
  -- The protocol is a very simple request\/response pattern: a single
  -- message is submitted and it is either accepted or rejected.
  -- The confirmation or rejection (with reason) is returned.
  --
  data Message (LocalMsgSubmission msg reject) from to where

    -- | The client submits a single message and MUST wait for a reply.
    --
    MsgSubmit
      :: msg -> Message (LocalMsgSubmission msg reject) StIdle StBusy

    -- | The server can reply to inform the client that it has accepted the
    -- message.
    --
    MsgAccept
      :: Message (LocalMsgSubmission msg reject) StBusy StIdle

    -- | The server can reply to inform the client that it has rejected the
    -- message. A reason for the rejection is included.
    --
    MsgReject
      :: reject -> Message (LocalMsgSubmission msg reject) StBusy StIdle

    -- | The client can terminate the protocol.
    --
    MsgDone
      :: Message (LocalMsgSubmission msg reject) StIdle StDone

  type StateAgency StIdle = ClientAgency
  type StateAgency StBusy = ServerAgency
  type StateAgency StDone = NobodyAgency

  type StateToken = SingLocalMsgSubmission

instance ( NFData msg
         , NFData reject) => NFData (Message (LocalMsgSubmission msg reject) from to) where
  rnf (MsgSubmit msg)    = rnf msg
  rnf MsgAccept          = ()
  rnf (MsgReject reject) = rnf reject
  rnf MsgDone            = ()

deriving instance (Eq msg, Eq reject) =>
                   Eq (Message (LocalMsgSubmission msg reject) from to)

deriving instance (Show msg, Show reject) =>
                   Show (Message (LocalMsgSubmission msg reject) from to)
