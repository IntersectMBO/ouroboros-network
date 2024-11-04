{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Ouroboros.Network.Protocol.KeepAlive.Test where

import Control.Monad.Class.MonadAsync
import Control.Monad.Class.MonadST
import Control.Monad.Class.MonadSTM
import Control.Monad.Class.MonadThrow
import Control.Monad.IOSim (runSimOrThrow)
import Control.Monad.ST (runST)
import Control.Tracer (nullTracer)

import Codec.CBOR.Read qualified as CBOR
import Data.ByteString.Lazy (ByteString)
import Data.ByteString.Lazy qualified as BL

import Network.TypedProtocol.Codec hiding (prop_codec)
import Network.TypedProtocol.Proofs

import Ouroboros.Network.Channel
import Ouroboros.Network.Driver.Limits
import Ouroboros.Network.Driver.Simple (runConnectedPeers)

import Ouroboros.Network.Protocol.KeepAlive.Client
import Ouroboros.Network.Protocol.KeepAlive.Codec
import Ouroboros.Network.Protocol.KeepAlive.Direct
import Ouroboros.Network.Protocol.KeepAlive.Examples
import Ouroboros.Network.Protocol.KeepAlive.Server
import Ouroboros.Network.Protocol.KeepAlive.Type

import Test.Ouroboros.Network.Protocol.Utils (prop_codec_valid_cbor_encoding,
           splits2, splits3)


import Test.QuickCheck
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)
import Text.Show.Functions ()


--
-- The list of all properties
--

tests :: TestTree
tests = testGroup "Ouroboros.Network.Protocol.KeepAlive"
  [ testProperty "direct"              prop_direct
  , testProperty "connect"             prop_connect
  , testProperty "channel ST"          prop_channel_ST
  , testProperty "channel IO"          prop_channel_IO
  , testProperty "codec v2"            prop_codec_v2
  , testProperty "codec v2 2-splits"   prop_codec_v2_splits2
  , testProperty "codec v2 3-splits"   (withMaxSuccess 33 prop_codec_v2_splits3)
  , testProperty "codec v2 valid CBOR" prop_codec_v2_valid_cbor
  , testProperty "byteLimits"          prop_byteLimits
  ]

--
-- Properties going directly, not via Peer.
--

prop_direct :: (Int -> Int) -> NonNegative Int -> Property
prop_direct f (NonNegative n) =
      runSimOrThrow
        (direct
          keepAliveServerCount
          (keepAliveClientApply f 0 n))
   ===
      (n, foldr (.) id (replicate n f) 0)

--
-- Properties using connect
--

prop_connect :: (Int -> Int)
             -> NonNegative Int
             -> Bool
prop_connect f (NonNegative n) =
   case runSimOrThrow
          (connect
            (keepAliveServerPeer   keepAliveServerCount)
            (keepAliveClientPeer $ keepAliveClientApply f 0 n))

     of (s, c, TerminalStates SingDone SingDone) ->
          (s, c) == (n, foldr (.) id (replicate n f) 0)

--
-- Properties using channels, codecs and drivers.
--

prop_channel :: ( MonadST    m
                , MonadSTM   m
                , MonadAsync m
                , MonadCatch m
                )
             => (Int -> Int)
             -> Int
             -> m Property
prop_channel f n = do
    (s, c) <- runConnectedPeers createConnectedChannels
                                nullTracer
                                codecKeepAlive_v2
                                server client
    return ((s, c) === (n, foldr (.) id (replicate n f) 0))
  where
    server = keepAliveServerPeer keepAliveServerCount
    client = keepAliveClientPeer (keepAliveClientApply f 0 n)

prop_channel_ST :: (Int -> Int)
                -> NonNegative Int
                -> Property
prop_channel_ST f (NonNegative n) =
    runSimOrThrow (prop_channel f n)

prop_channel_IO :: (Int -> Int)
                -> NonNegative Int
                -> Property
prop_channel_IO f (NonNegative n) =
    ioProperty (prop_channel f n)


--
-- Codec tests
--

instance Arbitrary (AnyMessage KeepAlive) where
  arbitrary = do
    c <- arbitrary
    oneof
      [ pure $ AnyMessage (MsgKeepAlive $ Cookie c)
      , pure $ AnyMessage (MsgKeepAliveResponse $ Cookie c)
      , pure $ AnyMessage MsgDone
      ]

instance Eq (AnyMessage KeepAlive) where
    AnyMessage (MsgKeepAlive cookieA)         == AnyMessage (MsgKeepAlive cookieB)         = cookieA == cookieB
    AnyMessage (MsgKeepAliveResponse cookieA) == AnyMessage (MsgKeepAliveResponse cookieB) = cookieA == cookieB
    AnyMessage MsgDone                        == AnyMessage MsgDone                        = True
    _ == _ = False

prop_codec_v2 :: AnyMessage KeepAlive -> Bool
prop_codec_v2 msg =
    runST (prop_codecM codecKeepAlive_v2 msg)

prop_codec_v2_splits2 :: AnyMessage KeepAlive -> Bool
prop_codec_v2_splits2 msg =
    runST (prop_codec_splitsM splits2 codecKeepAlive_v2 msg)

prop_codec_v2_splits3 :: AnyMessage KeepAlive -> Bool
prop_codec_v2_splits3 msg =
    runST (prop_codec_splitsM splits3 codecKeepAlive_v2 msg)

prop_codec_v2_valid_cbor :: AnyMessage KeepAlive -> Property
prop_codec_v2_valid_cbor msg =
    prop_codec_valid_cbor_encoding codecKeepAlive_v2 msg

prop_byteLimits :: AnyMessage KeepAlive
                -> Bool
prop_byteLimits (AnyMessage (msg :: Message KeepAlive st st')) =
        dataSize (encode msg)
     <= sizeLimitForState (stateToken :: StateToken st)
  where
    Codec { encode } = codecKeepAlive_v2 :: Codec KeepAlive CBOR.DeserialiseFailure IO ByteString
    ProtocolSizeLimits { sizeLimitForState, dataSize } = byteLimitsKeepAlive (fromIntegral . BL.length)
