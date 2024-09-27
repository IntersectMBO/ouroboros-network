{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE TypeApplications    #-}

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
import Data.Foldable as Foldable (foldl')
import Data.Word (Word8)

import Network.TypedProtocol.Codec
import Network.TypedProtocol.Proofs

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
import Ouroboros.Network.Protocol.PeerSharing.Type
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
        , testProperty "codec"            prop_codec_PeerSharing
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
  === (snd $ Foldable.foldl' (\(n, r) (PeerSharingAmount amount)
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
     (ns, _, TerminalStates SingDone SingDone) ->
       let compute = foldl' (\(x, r) (PeerSharingAmount amount)
                              -> (x + 1, replicate (applyFun f amount) x ++ r))
                            (0, [])
                            l
        in ns === snd compute


--
-- Properties using channels, codecs and drivers.
--

prop_channel :: ( MonadAsync m
                , MonadCatch m
                , MonadST    m
                )
             => Fun Word8 Int
             -> [PeerSharingAmount]
             -> m Property
prop_channel f l = do
    (s, _) <- runConnectedPeers createConnectedChannels
                                nullTracer
                                (codecPeerSharing CBOR.encodeInt CBOR.decodeInt)
                                client server
    let compute = Foldable.foldl' (\(x, r) (PeerSharingAmount amount)
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

instance Arbitrary peer => Arbitrary (AnyMessage (PeerSharing peer)) where
  -- TODO: refactor, add shrinker
  arbitrary = do
    amount <- PeerSharingAmount <$> arbitrary
    resp <- arbitrary
    oneof
      [ pure $ AnyMessage (MsgShareRequest amount)
      , pure $ AnyMessage (MsgSharePeers resp)
      , pure $ AnyMessage MsgDone
      ]

instance Eq peer => Eq (AnyMessage (PeerSharing peer)) where
    AnyMessage (MsgShareRequest amountA) == AnyMessage (MsgShareRequest amountB) = amountA == amountB
    AnyMessage (MsgSharePeers respA)     == AnyMessage (MsgSharePeers respB)     = respA   == respB
    AnyMessage MsgDone                   == AnyMessage MsgDone                   = True
    _ == _                                                                       = False

prop_codec_PeerSharing :: AnyMessage (PeerSharing Int)
               -> Bool
prop_codec_PeerSharing msg =
  runST (prop_codecM (codecPeerSharing CBOR.encodeInt CBOR.decodeInt) msg)

-- TODO: this test is not needed; `prop_codec_valid_cbor` and
-- `prop_codec_PeerSharing` subsume it.
prop_codec_cbor
  :: AnyMessage (PeerSharing Int)
  -> Bool
prop_codec_cbor msg =
  runST (prop_codec_cborM (codecPeerSharing CBOR.encodeInt CBOR.decodeInt) msg)

prop_codec_valid_cbor :: AnyMessage (PeerSharing Int) -> Property
prop_codec_valid_cbor = prop_codec_valid_cbor_encoding (codecPeerSharing CBOR.encodeInt CBOR.decodeInt)

-- | Check for data chunk boundary problems in the codec using 2 chunks.
--
prop_codec_splits2 :: AnyMessage (PeerSharing Int) -> Bool
prop_codec_splits2 msg =
  runST (prop_codec_splitsM splits2 (codecPeerSharing CBOR.encodeInt CBOR.decodeInt) msg)

-- | Check for data chunk boundary problems in the codec using 3 chunks.
--
prop_codec_splits3 :: AnyMessage (PeerSharing Int) -> Bool
prop_codec_splits3 msg =
  runST (prop_codec_splitsM splits3 (codecPeerSharing CBOR.encodeInt CBOR.decodeInt) msg)

prop_byteLimits :: AnyMessage (PeerSharing Int)
                -> Bool
prop_byteLimits (AnyMessage (msg :: Message (PeerSharing Int) st st')) =
        dataSize (encode msg)
     <= sizeLimitForState (stateToken :: StateToken st)
  where
    Codec { encode } = codecPeerSharing @IO CBOR.encodeInt CBOR.decodeInt
    ProtocolSizeLimits { sizeLimitForState, dataSize } =
      byteLimitsPeerSharing (fromIntegral . BL.length)
