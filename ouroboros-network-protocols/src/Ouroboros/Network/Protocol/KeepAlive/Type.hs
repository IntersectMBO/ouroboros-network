{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE EmptyCase                #-}
{-# LANGUAGE FlexibleInstances        #-}
{-# LANGUAGE GADTs                    #-}
{-# LANGUAGE PolyKinds                #-}
{-# LANGUAGE StandaloneDeriving       #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies             #-}

-- | The type of the keep alive protocol.
--
-- The keep alive protocol is used for
--
-- * sending keep alive messages
-- * making round trip measuremnets
--
-- Each side will run its own version of the keep alive protocol.  It should be
-- configured so that any intermediate state (such as in customer premise
-- equipment or in a carrier grade NAT) is kept alive. This has to be a per-node
-- configuration element as this is about the properties of that nodes network
-- connectivity.
--
-- For making round trip measurements its in the interest of the other side to
-- reply promptly.
--
module Ouroboros.Network.Protocol.KeepAlive.Type where

import           Control.Monad.Class.MonadThrow (Exception)
import           Data.Kind (Type)
import           Data.Word (Word16)
import           Network.TypedProtocol.Core
import           Ouroboros.Network.Util.ShowProxy (ShowProxy (..))

-- | A 16bit value used to match responses to requests.
newtype Cookie = Cookie {unCookie :: Word16 } deriving (Eq, Show)

data KeepAliveProtocolFailure =
       KeepAliveCookieMissmatch Cookie Cookie deriving (Eq, Show)

instance Exception KeepAliveProtocolFailure

-- | A kind to identify our protocol, and the types of the states in the state
-- transition diagram of the protocol.
--
data KeepAlive where

    -- | The client can send a request and the server is waiting for a request.
    --
    StClient :: KeepAlive

    -- | The server is responsible for sending response back.
    --
    StServer :: KeepAlive

    -- | Both the client and server are in the terminal state. They're done.
    --
    StDone   :: KeepAlive

instance ShowProxy KeepAlive where
    showProxy _ = "KeepAlive"

-- | Singletons for 'KeepAlive' state types.
--
type SingKeepAlive :: KeepAlive -> Type
data SingKeepAlive k where
    SingClient :: SingKeepAlive StClient
    SingServer :: SingKeepAlive StServer
    SingDone   :: SingKeepAlive StDone

instance StateTokenI StClient where stateToken = SingClient
instance StateTokenI StServer where stateToken = SingServer
instance StateTokenI StDone   where stateToken = SingDone
deriving instance Show (SingKeepAlive st)

instance Protocol KeepAlive where
    -- | The messages in the keep alive protocol.
    --
    -- In this protocol the consumer initiates and the producer replies.
    --
    data Message KeepAlive from to where

      -- | Send a keep alive message.
      --
      MsgKeepAlive
        :: Cookie
        -> Message KeepAlive StClient StServer

      -- | Keep alive response.
      --
      MsgKeepAliveResponse
        :: Cookie
        -> Message KeepAlive StServer StClient

      -- | The client side terminating message of the protocol.
      --
      MsgDone
        :: Message KeepAlive StClient StDone

    type StateAgency StClient = ClientAgency
    type StateAgency StServer = ServerAgency
    type StateAgency StDone   = NobodyAgency

    type StateToken = SingKeepAlive


instance Show (Message KeepAlive from to) where
    show (MsgKeepAlive cookie)         = "MsgKeepAlive " ++ show cookie
    show (MsgKeepAliveResponse cookie) = "MsgKeepAliveResponse " ++ show cookie
    show MsgDone                       = "MsgDone"
