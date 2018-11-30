{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE DataKinds #-}

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

import           Ouroboros.Network.Protocol.ChainSync.Server
import           Ouroboros.Network.Protocol.ChainSync.Client
import           Ouroboros.Network.Protocol.ChainSync.Encoding
import           Ouroboros.Network.Protocol.ChainSync.Type
import           Ouroboros.Network.ChainSyncExamples


runProducer :: (MonadST m, Chain.HasHeader header, Serialise header, forall (it :: ChainSyncState) . Typeable it)
            => ChainSyncServer header (Point header) m a
            -> ByteChannel ByteString m
            -> m (Either (ProtocolError CBOR.DeserialiseFailure) a)
runProducer server channel =
    runPeerWithChannel codec peer channel
  where
    peer  = asUntypedPeer (chainSyncServerPeer server)
    codec = cborCodec encodeChainSyncMessage decodeChainSyncMessage

runConsumer :: (MonadST m, Chain.HasHeader header, Serialise header, forall (it :: ChainSyncState) . Typeable it)
            => ChainSyncClient header (Point header) m a
            -> ByteChannel ByteString m
            -> m (Either (ProtocolError CBOR.DeserialiseFailure) a)
runConsumer client channel =
    runPeerWithChannel codec peer channel
  where
    peer  = asUntypedPeer (chainSyncClientPeer client)
    codec = cborCodec encodeChainSyncMessage decodeChainSyncMessage


------------------------------------------------

runExampleProducer :: (MonadST m, MonadSTM m,
                       HasHeader header, Serialise header,
                       forall (it :: ChainSyncState) . Typeable it)
                   => a
                   -> TVar m (ChainProducerState header)
                   -> ByteChannel ByteString m
                   -> m (Either (ProtocolError CBOR.DeserialiseFailure) a)
runExampleProducer recvMsgDone cpsVar channel =
  let server = chainSyncServerExample recvMsgDone cpsVar
  in  runProducer server channel

runExampleConsumer :: (MonadST m, MonadSTM m,
                       HasHeader header, Serialise header,
                       forall (it :: ChainSyncState) . Typeable it)
                   => TVar m (Chain header)
                   -> ByteChannel ByteString m
                   -> m (Either (ProtocolError CBOR.DeserialiseFailure) a)
runExampleConsumer chainVar channel =
  let client = chainSyncClientExample chainVar
  in  runConsumer client channel

------------------------------------------------


data ProtocolError e = ProtocolDecodeFailure e
                     | ProtocolDisconnected
                     | ProtocolStateError

runPeerWithChannel :: forall bytes failure msg m a.
                      ( Monad m, forall (it :: ChainSyncState) . Typeable it)
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

