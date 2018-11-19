{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE QuantifiedConstraints #-}

module Ouroboros.Network.Pipe2 where

import           Codec.Serialise.Class (Serialise)
import qualified Codec.CBOR.Read as CBOR (DeserialiseFailure)
import           Data.ByteString (ByteString)
import           Data.Typeable (Typeable)

import           Protocol.Untyped
import           Ouroboros.Network.ByteChannel (ByteChannel)
import           Ouroboros.Network.MsgChannel
import           Ouroboros.Network.Codec
import           Ouroboros.Network.Framing

import           Ouroboros.Network.MonadClass.MonadST
import           Ouroboros.Network.MonadClass.MonadSTM

import           Ouroboros.Network.Chain (Chain, HasHeader, Point)
import qualified Ouroboros.Network.Chain as Chain
import           Ouroboros.Network.ChainProducerState (ChainProducerState)

import           Ouroboros.Network.Protocol.ChainSync.Type
import           Ouroboros.Network.Protocol.ChainSync.Server
import           Ouroboros.Network.Protocol.ChainSync.Client
import           Ouroboros.Network.Protocol.ChainSync.Encoding
import           Ouroboros.Network.ChainSyncExamples


runProducer :: (MonadST m, Chain.HasHeader header, Serialise header,
                forall (t :: ChainSyncState). Typeable t)
            => ChainSyncServer header (Point header) m ()
            -> ByteChannel ByteString m
            -> m (Either (ProtocolError CBOR.DeserialiseFailure) ())
runProducer server channel =
    runPeerWithChannel codec peer channel
  where
    peer  = asUntypedPeer (chainSyncServerPeer server)
    codec = cborCodec encodeChainSyncMessage decodeChainSyncMessage

runConsumer :: (MonadST m, Chain.HasHeader header, Serialise header,
                forall (t :: ChainSyncState). Typeable t)
            => ChainSyncClient header (Point header) m ()
            -> ByteChannel ByteString m
            -> m (Either (ProtocolError CBOR.DeserialiseFailure) ())
runConsumer client channel =
    runPeerWithChannel codec peer channel
  where
    peer  = asUntypedPeer (chainSyncClientPeer client)
    codec = cborCodec encodeChainSyncMessage decodeChainSyncMessage


------------------------------------------------

runExampleProducer :: (MonadST m, MonadSTM m,
                       HasHeader header, Serialise header,
                       forall (t :: ChainSyncState). Typeable t)
                   => TVar m (ChainProducerState header)
                   -> ByteChannel ByteString m
                   -> m (Either (ProtocolError CBOR.DeserialiseFailure) ())
runExampleProducer cpsVar channel = do
    server <- chainSyncServerExample cpsVar
    runProducer server channel

runExampleConsumer :: (MonadST m, MonadSTM m,
                       HasHeader header, Serialise header,
                       forall (t :: ChainSyncState). Typeable t)
                   => TVar m (Chain header)
                   -> ByteChannel ByteString m
                   -> m (Either (ProtocolError CBOR.DeserialiseFailure) ())
runExampleConsumer chainVar channel = do
    client <- chainSyncClientExample chainVar
    runConsumer client channel

------------------------------------------------


data ProtocolError e = ProtocolDecodeFailure e
                     | ProtocolDisconnected
                     | ProtocolStateError

runPeerWithChannel :: forall bytes failure msg m a.
                      Monad m
                   => Codec bytes failure m (SomeMessage msg)
                   -> Peer msg m a
                   -> ByteChannel bytes m
                   -> m (Either (ProtocolError failure) a)
runPeerWithChannel codec peer0 bytechannel =
    go (applyFraming codec bytechannel) peer0
  where
    go :: MsgChannel failure m (SomeMessage msg)
       -> Peer msg m a
       -> m (Either (ProtocolError failure) a)
    go _ (PeerDone x) = return (Right x)

    go channel (PeerLift effect) =
      effect >>= \peer' -> go channel peer'

    go channel (PeerYield msg peer') =
      send channel msg >>= \channel' -> go channel' peer'

    go channel (PeerAwait k) =
      recv channel >>= \mmsg ->
      case mmsg of
        RecvMsg msg channel' ->
          case k msg of
            Just peer' -> go channel' peer'
            Nothing    -> return (Left ProtocolStateError)
        RecvFailure e  -> return (Left (ProtocolDecodeFailure e))
        ChannelClosed  -> return (Left ProtocolDisconnected)

