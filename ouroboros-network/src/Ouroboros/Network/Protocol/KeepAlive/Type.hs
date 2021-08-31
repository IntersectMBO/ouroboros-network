{-# LANGUAGE GADTs             #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE EmptyCase         #-}
{-# LANGUAGE PolyKinds         #-}

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

import Control.Monad.Class.MonadThrow (Exception)
import Data.Word (Word16)
import Network.TypedProtocol.Core
import Ouroboros.Network.Util.ShowProxy (ShowProxy (..))

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

    data ClientHasAgency st where
      TokClient :: ClientHasAgency StClient

    data ServerHasAgency st where
      TokServer :: ServerHasAgency StServer

    data NobodyHasAgency st where
      TokDone   :: NobodyHasAgency StDone

    exclusionLemma_ClientAndServerHaveAgency TokClient tok = case tok of {}
    exclusionLemma_NobodyAndClientHaveAgency TokDone   tok = case tok of {}
    exclusionLemma_NobodyAndServerHaveAgency TokDone   tok = case tok of {}


instance Show (Message KeepAlive from to) where
    show (MsgKeepAlive cookie)         = "MsgKeepAlive " ++ show cookie
    show (MsgKeepAliveResponse cookie) = "MsgKeepAliveResponse " ++ show cookie
    show MsgDone                       = "MsgDone"

instance Show (ClientHasAgency (st :: KeepAlive)) where
    show TokClient = "TokClient"

instance Show (ServerHasAgency (st :: KeepAlive)) where
    show TokServer = "TokServer"
