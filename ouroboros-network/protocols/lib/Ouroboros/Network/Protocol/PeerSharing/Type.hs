{-# LANGUAGE CPP                      #-}
{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE DeriveGeneric            #-}
{-# LANGUAGE DerivingVia              #-}
{-# LANGUAGE EmptyCase                #-}
{-# LANGUAGE FlexibleInstances        #-}
{-# LANGUAGE GADTs                    #-}
{-# LANGUAGE PolyKinds                #-}
{-# LANGUAGE ScopedTypeVariables      #-}
{-# LANGUAGE StandaloneDeriving       #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies             #-}

module Ouroboros.Network.Protocol.PeerSharing.Type where

import Control.DeepSeq

import Codec.Serialise.Class (Serialise)
import Data.Kind (Type)
import Data.Word (Word8)
import GHC.Generics (Generic)

import Network.TypedProtocol.Core

import Ouroboros.Network.Util.ShowProxy (ShowProxy (..))

-- | PeerSharing amount new type.
--
-- We use 'Word8' to be faithful to the CDDL specification.
newtype PeerSharingAmount = PeerSharingAmount { getAmount :: Word8 }
  deriving (Eq, Show, Ord, Generic)
  deriving (Enum, Num, Real, Integral, Serialise) via Word8

-- | PeerSharing Result type.
--
-- We need a constructor for the case when the Governor wins the race versus
-- the Mux (when initialising the peer sharing miniprotocol). This leads the
-- Governor to lookup a peer that hasn't been registered yet.
data PeerSharingResult peerAddress = PeerSharingResult [peerAddress]
                                   | PeerSharingNotRegisteredYet
                                   deriving (Eq, Show)

-- | A kind to identify our protocol, and the types of the states in the state
-- transition diagram of the protocol.
--
type PeerSharing :: Type -> Type
data PeerSharing peerAddress where

    -- | The client can send a request and the server is waiting for a request.
    --
    StIdle :: PeerSharing peerAddress

    -- | The server is responsible for sending response back.
    --
    StBusy :: PeerSharing peerAddress

    -- | Both the client and server are in the terminal state. They're done.
    --
    StDone :: PeerSharing peerAddress

instance ShowProxy (PeerSharing peer) where
    showProxy _ = "PeerSharing"

data SingPeerSharing (k :: PeerSharing peerAddress) where
    SingIdle :: SingPeerSharing StIdle
    SingBusy :: SingPeerSharing StBusy
    SingDone :: SingPeerSharing StDone

deriving instance Show (SingPeerSharing peerAddress)

instance StateTokenI StIdle where stateToken = SingIdle
instance StateTokenI StBusy where stateToken = SingBusy
instance StateTokenI StDone where stateToken = SingDone

instance Protocol (PeerSharing peerAddress) where
  data Message (PeerSharing peerAddress) from to where
    -- | Request `PeerSharingAmount` of peers.
    MsgShareRequest :: PeerSharingAmount
                    -> Message (PeerSharing peerAddress) StIdle StBusy
                    --
    -- | Respond with peer address information.  It is a protocol error to
    -- respond with more peers than requested.
    --
    -- The server should only share peers with which it has (or recently had) an
    -- successful inbound or outbound session.
    --
    MsgSharePeers   :: [peerAddress]
                    -> Message (PeerSharing peerAddress) StBusy StIdle

    MsgDone         :: Message (PeerSharing peerAddress) StIdle StDone

  type StateAgency StIdle = ClientAgency
  type StateAgency StBusy = ServerAgency
  type StateAgency StDone = NobodyAgency

  type StateToken = SingPeerSharing

instance NFData peerAddress => NFData (Message (PeerSharing peerAddress) from to) where
  rnf (MsgShareRequest (PeerSharingAmount m)) = rnf m
  rnf (MsgSharePeers peers)                   = rnf peers
  rnf MsgDone                                 = ()

instance Show peer => Show (Message (PeerSharing peer) from to) where
    show (MsgShareRequest amount) = "MsgShareRequest " ++ show amount
    show (MsgSharePeers resp)     = "MsgSharePeers "   ++ show resp
    show MsgDone                  = "MsgDone"

#if __GLASGOW_HASKELL__ < 914
  -- These constraints are REQUIRED for ghc < 9.14 but REDUNDANT for ghc >= 9.14
  -- See https://gitlab.haskell.org/ghc/ghc/-/issues/26381#note_637863
deriving instance (Show peerAddress) => Show (PeerSharing peerAddress)
deriving instance (Eq peerAddress) => Eq (PeerSharing peerAddress)
#else
deriving instance Show (PeerSharing peerAddress)
deriving instance Eq (PeerSharing peerAddress)
#endif
