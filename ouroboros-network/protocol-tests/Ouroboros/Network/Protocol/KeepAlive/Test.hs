{-# LANGUAGE GADTs             #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns    #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Ouroboros.Network.Protocol.KeepAlive.Test where

import           Control.Monad.ST (runST)
import           Control.Monad.Class.MonadST
import           Control.Monad.Class.MonadSTM
import           Control.Monad.Class.MonadAsync
import           Control.Monad.Class.MonadThrow
import           Control.Monad.IOSim (runSimOrThrow)
import           Control.Tracer (nullTracer)

import qualified Codec.CBOR.Read  as CBOR
import           Data.Functor.Identity (Identity (..))
import           Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BL

import           Network.TypedProtocol.Core
import           Network.TypedProtocol.Proofs

import           Ouroboros.Network.Channel
import           Ouroboros.Network.Codec hiding (prop_codec)
import           Ouroboros.Network.Driver.Limits
import           Ouroboros.Network.Driver.Simple (runConnectedPeers)

import           Ouroboros.Network.Protocol.KeepAlive.Type
import           Ouroboros.Network.Protocol.KeepAlive.Client
import           Ouroboros.Network.Protocol.KeepAlive.Server
import           Ouroboros.Network.Protocol.KeepAlive.Codec
import           Ouroboros.Network.Protocol.KeepAlive.Examples
import           Ouroboros.Network.Protocol.KeepAlive.Direct

import Test.Ouroboros.Network.Testing.Utils
        ( splits2
        , splits3
        )

import Test.QuickCheck
import Text.Show.Functions ()
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)


--
-- The list of all properties
--

tests :: TestTree
tests = testGroup "Ouroboros.Network.Protocol.KeepAlive"
  [ testProperty "direct"         prop_direct
  , testProperty "connect"        prop_connect
  , testProperty "channel ST"     prop_channel_ST
  , testProperty "channel IO"     prop_channel_IO
  , testProperty "codec"          prop_codec
  , testProperty "codec 2-splits" prop_codec_splits2
  , testProperty "codec 3-splits" (withMaxSuccess 33 prop_codec_splits3)
  , testProperty "byteLimits"     prop_byteLimits
  ]

--
-- Properties going directly, not via Peer.
--

prop_direct :: (Int -> Int) -> NonNegative Int -> Property
prop_direct f (NonNegative n) =
      runIdentity
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
   case runIdentity
          (connect
            (keepAliveServerPeer   keepAliveServerCount)
            (keepAliveClientPeer $ keepAliveClientApply f 0 n))

     of (s, c, TerminalStates TokDone TokDone) ->
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
                                codecKeepAlive
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

instance Arbitrary (AnyMessageAndAgency KeepAlive) where
  arbitrary = oneof
    [ pure $ AnyMessageAndAgency (ClientAgency TokClient) MsgKeepAlive
    , pure $ AnyMessageAndAgency (ServerAgency TokServer) MsgKeepAliveResponse
    , pure $ AnyMessageAndAgency (ClientAgency TokClient) MsgDone
    ]

instance Show (AnyMessageAndAgency KeepAlive) where
    show (AnyMessageAndAgency _ msg) = show msg

instance Eq (AnyMessage KeepAlive) where
    AnyMessage MsgKeepAlive           == AnyMessage MsgKeepAlive           = True
    AnyMessage (MsgKeepAliveResponse) == AnyMessage (MsgKeepAliveResponse) = True
    AnyMessage MsgDone                == AnyMessage MsgDone                = True
    _ == _ = False

prop_codec :: AnyMessageAndAgency KeepAlive -> Bool
prop_codec msg =
    runST (prop_codecM codecKeepAlive msg)

prop_codec_splits2 :: AnyMessageAndAgency KeepAlive -> Bool
prop_codec_splits2 msg =
    runST (prop_codec_splitsM splits2 codecKeepAlive msg)

prop_codec_splits3 :: AnyMessageAndAgency KeepAlive -> Bool
prop_codec_splits3 msg =
    runST (prop_codec_splitsM splits3 codecKeepAlive msg)


prop_byteLimits :: AnyMessageAndAgency KeepAlive
                         -> Bool
prop_byteLimits (AnyMessageAndAgency agency msg) =
        dataSize (encode agency msg)
     <= sizeLimitForState agency  
  where
    Codec { encode } = (codecKeepAlive :: Codec KeepAlive CBOR.DeserialiseFailure IO ByteString)
    ProtocolSizeLimits { sizeLimitForState, dataSize } = byteLimitsKeepAlive (fromIntegral . BL.length)
