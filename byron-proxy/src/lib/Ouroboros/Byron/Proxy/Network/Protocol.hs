{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTSyntax #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Ouroboros.Byron.Proxy.Network.Protocol where

import qualified Codec.CBOR.Term as CBOR
import Codec.Serialise (Serialise)
import Codec.SerialiseTerm (CodecCBORTerm (..))
import Control.Monad.Class.MonadST (MonadST)
import Control.Monad.Class.MonadThrow (MonadThrow)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Trans.Resource (ResourceT, runResourceT)
import Control.Monad.Trans.Class (lift)
import Control.Tracer (nullTracer)
import qualified Data.Map as Map
import Data.Text (Text)
import Data.Word (Word16)

import qualified Cardano.Chain.Slotting as Cardano

import Network.TypedProtocol.Channel (hoistChannel)
import Network.TypedProtocol.Codec (hoistCodec)
import Network.TypedProtocol.Driver (runPeer)
import Ouroboros.Network.Mux.Interface (AppType (..), MuxApplication (..))
import Ouroboros.Network.Mux.Types (MiniProtocolLimits (..), ProtocolEnum (..))
import Ouroboros.Network.Protocol.ChainSync.Client (ChainSyncClient, chainSyncClientPeer)
import Ouroboros.Network.Protocol.ChainSync.Server (ChainSyncServer, chainSyncServerPeer)
import Ouroboros.Network.Protocol.Handshake.Version

import Ouroboros.Byron.Proxy.ChainSync.Types (Block, Point)
import qualified Ouroboros.Byron.Proxy.ChainSync.Types as ChainSync (codec)

-- | Version number for handshake. Needs Show and Serialise in order to be
-- useful.
newtype VNumber = VNumber
  { getVNumber :: Word16
  } deriving (Show, Eq, Ord, Enum, Serialise)

unitCodecCBORTerm :: CodecCBORTerm Text ()
unitCodecCBORTerm = CodecCBORTerm
  { encodeTerm = const CBOR.TNull
  , decodeTerm = \term -> case term of
      CBOR.TNull -> Right ()
      _          -> Left "expected TNull"
  }

-- | Protocol enum for muxing. Also needs Show for some reason.
data Ptcl where
  PtclChainSync :: Ptcl
  deriving (Show, Eq, Ord, Enum, Bounded)

-- | Required in order to be useful. Why do I also need Enum?
instance ProtocolEnum Ptcl where
  fromProtocolEnum PtclChainSync = 1024
  toProtocolEnum word = case word of
    1024 -> Just PtclChainSync
    _ -> Nothing

-- | Required in order to be useful.
instance MiniProtocolLimits Ptcl where
  maximumMessageSize  = const 0xffffffff
  maximumIngressQueue = const 0xffffffff

initiatorVersions
  :: ( Monad m, MonadST m, MonadUnliftIO m, MonadThrow m, MonadThrow (ResourceT m) )
  => Cardano.EpochSlots -- ^ Needed for the codec, sadly
  -> ChainSyncClient Block Point (ResourceT m) ()
  -> Versions VNumber (CodecCBORTerm Text) (MuxApplication InitiatorApp Ptcl m)
initiatorVersions epochSlots client = Versions $ Map.fromList
  [ (VNumber 0, Sigma () (Version clientMuxApp unitCodecCBORTerm))
  ]
  where
  clientPeer = chainSyncClientPeer client
  codec = hoistCodec lift (ChainSync.codec epochSlots)
  clientMuxApp = Application $ \_ _ -> MuxInitiatorApplication $ \ptcl channel -> case ptcl of
    PtclChainSync -> runResourceT $ runPeer nullTracer codec (hoistChannel lift channel) clientPeer

responderVersions
  :: ( Monad m, MonadST m, MonadUnliftIO m, MonadThrow m, MonadThrow (ResourceT m) )
  => Cardano.EpochSlots -- ^ Needed for the codec; must match that of the initiator.
  -> ChainSyncServer Block Point (ResourceT m) ()
  -> Versions VNumber (CodecCBORTerm Text) (MuxApplication ResponderApp Ptcl m)
responderVersions epochSlots server = Versions $ Map.fromList
  [ (VNumber 0, Sigma () (Version serverMuxApp unitCodecCBORTerm))
  ]
  where
  serverPeer = chainSyncServerPeer server
  codec = hoistCodec lift (ChainSync.codec epochSlots)
  serverMuxApp = Application $ \_ _ -> MuxResponderApplication $ \ptcl channel -> case ptcl of
    PtclChainSync -> runResourceT $ runPeer nullTracer codec (hoistChannel lift channel) serverPeer
