{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE DerivingVia         #-}
{-# LANGUAGE EmptyCase           #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE TypeFamilies        #-}

module Ouroboros.Network.Protocol.PeerSharing.Type where

import Codec.Serialise.Class (Serialise)
import Control.DeepSeq
import Data.Word (Word8)
import GHC.Generics (Generic)
import Network.TypedProtocol.Core (PeerHasAgency (..), Protocol (..))
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

instance Protocol (PeerSharing peerAddress) where
  data Message (PeerSharing peerAddress) from to where
    MsgShareRequest :: PeerSharingAmount
                    -> Message (PeerSharing peerAddress) StIdle StBusy
    MsgSharePeers   :: [peerAddress]
                    -> Message (PeerSharing peerAddress) StBusy StIdle
    MsgDone         :: Message (PeerSharing peerAddress) StIdle StDone

  data ClientHasAgency st where
    TokIdle :: ClientHasAgency StIdle

  data ServerHasAgency st where
    TokBusy :: ServerHasAgency StBusy

  data NobodyHasAgency st where
    TokDone :: NobodyHasAgency StDone

  exclusionLemma_ClientAndServerHaveAgency TokIdle tok = case tok of {}
  exclusionLemma_NobodyAndClientHaveAgency TokDone tok = case tok of {}
  exclusionLemma_NobodyAndServerHaveAgency TokDone tok = case tok of {}

instance forall peerAddress (st :: PeerSharing peerAddress). NFData (ClientHasAgency st) where
  rnf TokIdle = ()

instance forall peerAddress (st :: PeerSharing peerAddress). NFData (ServerHasAgency st) where
  rnf TokBusy = ()

instance forall peerAddress (st :: PeerSharing peerAddress). NFData (NobodyHasAgency st) where
  rnf TokDone = ()

instance forall peerAddress (st :: PeerSharing peerAddress) pr. NFData (PeerHasAgency pr st) where
  rnf (ClientAgency x) = rnf x
  rnf (ServerAgency x) = rnf x

instance NFData peerAddress => NFData (Message (PeerSharing peerAddress) from to) where
  rnf (MsgShareRequest (PeerSharingAmount m)) = rnf m
  rnf (MsgSharePeers peers)                   = rnf peers
  rnf MsgDone                                 = ()

instance Show peer => Show (Message (PeerSharing peer) from to) where
    show (MsgShareRequest amount) = "MsgShareRequest " ++ show amount
    show (MsgSharePeers resp)     = "MsgSharePeers "   ++ show resp
    show MsgDone                  = "MsgDone"

deriving instance (Show peerAddress) => Show (PeerSharing peerAddress)

deriving instance (Eq peerAddress) => Eq (PeerSharing peerAddress)

instance Show (ClientHasAgency (st :: PeerSharing peerAddress)) where
  show TokIdle = "TokIdle"

instance Show (ServerHasAgency (st :: PeerSharing peerAddress)) where
  show TokBusy = "TokBusy"

