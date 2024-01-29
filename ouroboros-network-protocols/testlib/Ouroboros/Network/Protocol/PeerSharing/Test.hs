{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE NamedFieldPuns    #-}

{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE PolyKinds         #-}
{-# LANGUAGE TypeApplications  #-}

module Ouroboros.Network.Protocol.PeerSharing.Test where

import Codec.CBOR.Decoding qualified as CBOR
import Codec.CBOR.Encoding qualified as CBOR
import Control.Monad.Class.MonadAsync (MonadAsync)
import Control.Monad.Class.MonadST (MonadST)
import Control.Monad.Class.MonadThrow (MonadCatch)
import Control.Monad.IOSim (runSimOrThrow)
import Control.Monad.ST (runST)
import Control.Tracer (nullTracer)
import Data.ByteString.Lazy qualified as BL
import Data.Foldable (foldl')
import Data.Word (Word8)
import Network.TypedProtocol.Codec (AnyMessage (..), AnyMessageAndAgency (..),
           Codec (..), PeerHasAgency (..), prop_codecM, prop_codec_splitsM)
import Network.TypedProtocol.Proofs (TerminalStates (..), connect)
import Ouroboros.Network.Channel (createConnectedChannels)
import Ouroboros.Network.Driver.Limits (ProtocolSizeLimits (..))
import Ouroboros.Network.Driver.Simple (runConnectedPeers)
import Ouroboros.Network.Protocol.PeerSharing.Client (peerSharingClientPeer)
import Ouroboros.Network.Protocol.PeerSharing.Codec (byteLimitsPeerSharing,
           codecPeerSharing)
import Ouroboros.Network.Protocol.PeerSharing.Direct (direct)
import Ouroboros.Network.Protocol.PeerSharing.Examples
           (peerSharingClientCollect, peerSharingServerReplicate)
import Ouroboros.Network.Protocol.PeerSharing.Server (peerSharingServerPeer)
import Ouroboros.Network.Protocol.PeerSharing.Type (ClientHasAgency (..),
           Message (..), NobodyHasAgency (..), PeerSharing,
           PeerSharingAmount (..), ServerHasAgency (..))
import Test.Ouroboros.Network.Testing.Utils (prop_codec_cborM,
           prop_codec_valid_cbor_encoding, splits2, splits3)
import Test.QuickCheck.Function (Fun, applyFun)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (Arbitrary (..), Property, ioProperty, oneof,
           testProperty, withMaxSuccess, (===))

tests :: TestTree
tests =
  testGroup "Ouroboros.Network.Protocol"
    [ testGroup "PeerSharing"
        [ testProperty "direct"           prop_direct
        , testProperty "connect"          prop_connect
        , testProperty "channel ST"       prop_channel_ST
        , testProperty "channel IO"       prop_channel_IO
        , testProperty "codec"            prop_codec
        , testProperty "codec cbor"       prop_codec_cbor
        , testProperty "codec valid cbor" prop_codec_valid_cbor
        , testProperty "codec 2-splits"   prop_codec_splits2
        , testProperty "codec 3-splits"   (withMaxSuccess 33 prop_codec_splits3)
        , testProperty "byteLimits"       prop_byteLimits
        ]
    ]

instance Arbitrary PeerSharingAmount where
  arbitrary = PeerSharingAmount <$> arbitrary
  shrink (PeerSharingAmount amount) = PeerSharingAmount <$> shrink amount

--
-- Properties going directly, not via Peer.
--

prop_direct :: Fun Word8 Int -> [PeerSharingAmount] -> Property
prop_direct f l =
  runSimOrThrow
    (direct (peerSharingServerReplicate f)
            (peerSharingClientCollect l))
  === (snd $ foldl' (\(n, r) (PeerSharingAmount amount)
                      -> (n + 1, replicate (applyFun f amount) n ++ r))
             (0, [])
             l)

--
-- Properties using connect
--

prop_connect :: Fun Word8 Int -> [PeerSharingAmount] -> Property
prop_connect f l =
   case runSimOrThrow
          (connect
            (peerSharingClientPeer (peerSharingClientCollect l))
            (peerSharingServerPeer (peerSharingServerReplicate f))) of
     (ns, _, TerminalStates TokDone TokDone) ->
       let compute = foldl' (\(x, r) (PeerSharingAmount amount)
                              -> (x + 1, replicate (applyFun f amount) x ++ r))
                            (0, [])
                            l
        in ns === snd compute


--
-- Properties using channels, codecs and drivers.
--

prop_channel :: ( MonadST    m
                , MonadAsync m
                , MonadCatch m
                )
             => Fun Word8 Int
             -> [PeerSharingAmount]
             -> m Property
prop_channel f l = do
    (s, _) <- runConnectedPeers createConnectedChannels
                                nullTracer
                                (codecPeerSharing CBOR.encodeInt CBOR.decodeInt)
                                client server
    let compute = foldl' (\(x, r) (PeerSharingAmount amount)
                           -> (x + 1, replicate (applyFun f amount) x ++ r))
                         (0, [])
                         l
    return (s === snd compute)
  where
    client = peerSharingClientPeer (peerSharingClientCollect l)
    server = peerSharingServerPeer (peerSharingServerReplicate f)

prop_channel_ST :: Fun Word8 Int
                -> [PeerSharingAmount]
                -> Property
prop_channel_ST f l =
  runSimOrThrow (prop_channel f l)

prop_channel_IO :: Fun Word8 Int
                -> [PeerSharingAmount]
                -> Property
prop_channel_IO f l =
  ioProperty (prop_channel f l)

--
-- Codec tests
--

instance Arbitrary peer => Arbitrary (AnyMessageAndAgency (PeerSharing peer)) where
  arbitrary = do
    amount <- PeerSharingAmount <$> arbitrary
    resp <- arbitrary
    oneof
      [ pure $ AnyMessageAndAgency (ClientAgency TokIdle) (MsgShareRequest amount)
      , pure $ AnyMessageAndAgency (ServerAgency TokBusy) (MsgSharePeers resp)
      , pure $ AnyMessageAndAgency (ClientAgency TokIdle) MsgDone
      ]

instance Eq peer => Eq (AnyMessage (PeerSharing peer)) where
    AnyMessage (MsgShareRequest amountA) == AnyMessage (MsgShareRequest amountB) = amountA == amountB
    AnyMessage (MsgSharePeers respA)     == AnyMessage (MsgSharePeers respB)     = respA   == respB
    AnyMessage MsgDone                   == AnyMessage MsgDone                   = True
    _ == _                                                                       = False

prop_codec :: AnyMessageAndAgency (PeerSharing Int)
           -> Bool
prop_codec msg =
  runST (prop_codecM (codecPeerSharing CBOR.encodeInt CBOR.decodeInt) msg)

prop_codec_cbor
  :: AnyMessageAndAgency (PeerSharing Int)
  -> Bool
prop_codec_cbor msg =
  runST (prop_codec_cborM (codecPeerSharing CBOR.encodeInt CBOR.decodeInt) msg)

prop_codec_valid_cbor :: AnyMessageAndAgency (PeerSharing Int) -> Property
prop_codec_valid_cbor = prop_codec_valid_cbor_encoding (codecPeerSharing CBOR.encodeInt CBOR.decodeInt)

-- | Check for data chunk boundary problems in the codec using 2 chunks.
--
prop_codec_splits2 :: AnyMessageAndAgency (PeerSharing Int) -> Bool
prop_codec_splits2 msg =
  runST (prop_codec_splitsM splits2 (codecPeerSharing CBOR.encodeInt CBOR.decodeInt) msg)

-- | Check for data chunk boundary problems in the codec using 3 chunks.
--
prop_codec_splits3 :: AnyMessageAndAgency (PeerSharing Int) -> Bool
prop_codec_splits3 msg =
  runST (prop_codec_splitsM splits3 (codecPeerSharing CBOR.encodeInt CBOR.decodeInt) msg)

prop_byteLimits :: AnyMessageAndAgency (PeerSharing Int)
                -> Bool
prop_byteLimits (AnyMessageAndAgency agency msg) =
        dataSize (encode agency msg)
     <= sizeLimitForState agency
  where
    Codec { encode } = codecPeerSharing @IO CBOR.encodeInt CBOR.decodeInt
    ProtocolSizeLimits { sizeLimitForState, dataSize } =
      byteLimitsPeerSharing (fromIntegral . BL.length)
