{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PolyKinds                  #-}
{-# LANGUAGE QuantifiedConstraints      #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE ScopedTypeVariables        #-}

{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ConstrainedClassMethods #-}
module Ouroboros.Network.Protocol.ObjectDiffusion.Test
  ( tests
  , ObjectId (..)
  , Object (..)
  ) where

import Control.Monad (void)
import Data.ByteString.Lazy (ByteString)
import Data.List.NonEmpty qualified as NonEmpty

import Control.Monad.Class.MonadAsync (MonadAsync)
import Control.Monad.ST (runST)
import Control.Monad.Class.MonadST
import Control.Monad.Class.MonadFork

import Codec.Serialise (DeserialiseFailure, Serialise)
import Codec.Serialise qualified as Serialise (decode, encode)

import Network.TypedProtocol.Codec
import Network.TypedProtocol.Codec.Properties (prop_codecM, prop_codec_splitsM)
import Network.TypedProtocol.Core (IsPipelined (..))
import Network.TypedProtocol.Peer (Peer, PeerPipelined)
import Network.TypedProtocol.Proofs (connectPipelined)

import Ouroboros.Network.Channel (Channel, createConnectedBufferedChannels,
           createPipelineTestChannels)
import Ouroboros.Network.Driver (runPeer, runPipelinedPeer)
import Ouroboros.Network.Util.ShowProxy

import Ouroboros.Network.Protocol.ObjectDiffusion.Codec
import Ouroboros.Network.Protocol.ObjectDiffusion.Type

import Test.Data.CDDL (Any (..))
import Test.Ouroboros.Network.Protocol.Utils (prop_codec_cborM,
           prop_codec_valid_cbor_encoding, splits2, splits3)
import Test.Ouroboros.Network.Utils (renderRanges, DistinctList (..))

import Control.DeepSeq
import GHC.Generics
import Test.QuickCheck as QC
import Test.QuickCheck.Instances.ByteString ()
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)
import Ouroboros.Network.Protocol.ObjectDiffusion.Inbound (ObjectDiffusionInboundPipelined,
           objectDiffusionInboundPeerPipelined)
import Ouroboros.Network.Protocol.ObjectDiffusion.Outbound (ObjectDiffusionOutbound,
           objectDiffusionOutboundPeer)
import Data.Word (Word16)
import Ouroboros.Network.Protocol.ObjectDiffusion.Direct (
  objectDiffusionInbound,
  objectDiffusionOutbound,
  TraceObjectDiffusionDirect,
  directPipelined
  )
import Control.Tracer (Tracer, nullTracer)
import Control.Monad.IOSim (runSimOrThrow)
import Control.Monad.Class.MonadThrow (MonadCatch, MonadEvaluate)
import GHC.Natural (Natural)


--
-- Test cases
--


tests :: TestTree
tests =
  testGroup "Ouroboros.Network.Protocol"
    [ testGroup "ObjectDiffusion"
        [ testProperty "codec"                      prop_codec
        , testProperty "codec id"                   prop_codec_id
        , testProperty "codec 2-splits"           $ withMaxSize 50
                                                    prop_codec_splits2
        , testProperty "codec 3-splits"           $ withMaxSize 10
                                                    prop_codec_splits3
        , testProperty "codec cbor"                 prop_codec_cbor
        , testProperty "codec valid cbor"           prop_codec_valid_cbor
        , testProperty "direct ST"                  propDirectST
        , testProperty "direct IO"                  propDirectIO
        , testProperty "connect ST"                 propConnectST
        , testProperty "connect IO"                 propConnectIO
        , testProperty "demo channel ST"            propDemoChannelST
        , testProperty "demo channel IO"            propDemoChannelIO
        , testProperty "demo channel (buffered) ST" propDemoChannelBufferedST
        , testProperty "demo channel (buffered) IO" propDemoChannelBufferedIO
        ]
    ]

--
-- Common types & clients and servers used in the tests in this module.
--

newtype Object = Object { getObjectId :: ObjectId }
  deriving (Eq, Show, Arbitrary, Serialise, Generic, NFData)

instance ShowProxy Object where
    showProxy _ = "Object"

-- | We use any `CBOR.Term`.  This allows us to use `any` in cddl specs.
--
newtype ObjectId = ObjectId Any
  deriving (Eq, Ord, Show, Arbitrary, Serialise, Generic, NFData)

instance ShowProxy ObjectId where
    showProxy _ = "ObjectId"

deriving newtype instance Arbitrary NumObjectIdsAck
deriving newtype instance Arbitrary NumObjectIdsReq

instance Arbitrary (AnyMessage (ObjectDiffusion ObjectId Object)) where
  arbitrary = oneof
    [ pure $ AnyMessage MsgInit
    , AnyMessage
        <$> ( MsgRequestObjectIds SingBlocking
            <$> arbitrary
            <*> arbitrary
            )

    , AnyMessage
        <$> ( MsgRequestObjectIds SingNonBlocking
            <$> arbitrary
            <*> arbitrary
            )

    , AnyMessage
        <$> MsgReplyObjectIds
        <$> ( BlockingReply
            . NonEmpty.fromList
            . QC.getNonEmpty
            )
        <$> arbitrary

    , AnyMessage
        <$> MsgReplyObjectIds
        <$> NonBlockingReply
        <$> arbitrary

    , AnyMessage
        <$> MsgRequestObjects
        <$> arbitrary

    , AnyMessage
        <$> MsgReplyObjects
        <$> arbitrary

    , AnyMessage
        <$> pure MsgDone
    ]

instance (Eq objectId
         , Eq object
         )
      => Eq (AnyMessage (ObjectDiffusion objectId object)) where

  (==) (AnyMessage MsgInit)
       (AnyMessage MsgInit) = True

  (==) (AnyMessage (MsgRequestObjectIds SingBlocking ackNo  reqNo))
       (AnyMessage (MsgRequestObjectIds SingBlocking ackNo' reqNo')) =
    (ackNo, reqNo) == (ackNo', reqNo')

  (==) (AnyMessage (MsgRequestObjectIds SingNonBlocking ackNo  reqNo))
       (AnyMessage (MsgRequestObjectIds SingNonBlocking ackNo' reqNo')) =
    (ackNo, reqNo) == (ackNo', reqNo')

  (==) (AnyMessage (MsgReplyObjectIds (BlockingReply objectIds)))
       (AnyMessage (MsgReplyObjectIds (BlockingReply objectIds'))) =
    objectIds == objectIds'

  (==) (AnyMessage (MsgReplyObjectIds (NonBlockingReply objectIds)))
       (AnyMessage (MsgReplyObjectIds (NonBlockingReply objectIds'))) =
    objectIds == objectIds'

  (==) (AnyMessage (MsgRequestObjects objectIds))
       (AnyMessage (MsgRequestObjects objectIds')) = objectIds == objectIds'

  (==) (AnyMessage (MsgReplyObjects txs))
       (AnyMessage (MsgReplyObjects txs')) = txs == txs'

  (==) (AnyMessage MsgDone)
       (AnyMessage MsgDone) = True

  _ == _ = False


codec :: MonadST m
      => Codec
           (ObjectDiffusion ObjectId Object)
           DeserialiseFailure
           m ByteString
codec = codecObjectDiffusion
          Serialise.encode Serialise.decode
          Serialise.encode Serialise.decode


-- | Check the codec round trip property.
--
prop_codec
  :: AnyMessage (ObjectDiffusion ObjectId Object)
  -> Property
prop_codec msg =
  runST (prop_codecM codec msg)

-- | Check the codec round trip property for the id codec.
--
prop_codec_id
  :: AnyMessage (ObjectDiffusion ObjectId Object)
  -> Property
prop_codec_id msg =
  runST (prop_codecM codecObjectDiffusionId msg)

-- | Check for data chunk boundary problems in the codec using 2 chunks.
--
prop_codec_splits2
  :: AnyMessage (ObjectDiffusion ObjectId Object)
  -> Property
prop_codec_splits2 msg =
  runST (prop_codec_splitsM splits2 codec msg)

-- | Check for data chunk boundary problems in the codec using 3 chunks.
--
prop_codec_splits3
  :: AnyMessage (ObjectDiffusion ObjectId Object)
  -> Property
prop_codec_splits3 msg =
  labelMsg msg $
  runST (prop_codec_splitsM splits3 codec msg)

prop_codec_cbor
  :: AnyMessage (ObjectDiffusion ObjectId Object)
  -> Property
prop_codec_cbor msg =
  runST (prop_codec_cborM codec msg)

-- | Check that the encoder produces a valid CBOR.
--
prop_codec_valid_cbor
  :: AnyMessage (ObjectDiffusion ObjectId Object)
  -> Property
prop_codec_valid_cbor = prop_codec_valid_cbor_encoding codec


labelMsg :: AnyMessage (ObjectDiffusion objectId object) -> Property -> Property
labelMsg (AnyMessage msg) =
  label (case msg of
           MsgInit                -> "MsgInit"
           MsgRequestObjectIds {} -> "MsgRequestObjectIds"
           MsgReplyObjectIds as   -> "MsgReplyObjectIds " ++ renderRanges 3 (length as)
           MsgRequestObjects as   -> "MsgRequestObjects " ++ renderRanges 3 (length as)
           MsgReplyObjects as     -> "MsgReplyObjects "   ++ renderRanges 3 (length as)
           MsgDone                -> "MsgDone"
        )

--
-- Direct client and server tests
--

data ObjectDiffusionTestParams =
     ObjectDiffusionTestParams {
       testMaxUnacked        :: Positive (Small Word16),
       testMaxObjectIdsToRequest :: Positive (Small Word16),
       testMaxObjectsToRequest    :: Positive (Small Word16),
       testObjects           :: DistinctList Object
     }
  deriving Show

instance Arbitrary ObjectDiffusionTestParams where
  arbitrary =
    ObjectDiffusionTestParams <$> arbitrary
                              <*> arbitrary
                              <*> arbitrary
                              <*> arbitrary

  shrink (ObjectDiffusionTestParams a b c d) =
    [ ObjectDiffusionTestParams a' b' c' d'
    | (a', b', c', d') <- shrink (a, b, c, d) ]

type ChannelSize = Positive (Small Word16)

channelSizeAsNat :: ChannelSize -> Natural
channelSizeAsNat (Positive (Small n)) = fromIntegral n

testInboundPipelined
  :: (MonadCatch m, MonadEvaluate m)
  => Tracer m (TraceObjectDiffusionDirect ObjectId Object)
  -> ObjectDiffusionTestParams
  -> ObjectDiffusionInboundPipelined ObjectId Object m [Object]
testInboundPipelined
  tracer
  ObjectDiffusionTestParams {
    testMaxUnacked            = Positive (Small maxUnacked),
    testMaxObjectIdsToRequest = Positive (Small maxObjectIdsToRequest),
    testMaxObjectsToRequest   = Positive (Small maxObjectsToRequest)
  } =
    objectDiffusionInbound
      tracer
      getObjectId
      maxUnacked
      maxObjectIdsToRequest
      maxObjectsToRequest

testOutbound
  :: Monad m
  => Tracer m (TraceObjectDiffusionDirect ObjectId Object)
  -> ObjectDiffusionTestParams
  -> ObjectDiffusionOutbound ObjectId Object m ()
testOutbound
  tracer
  ObjectDiffusionTestParams {
    testMaxUnacked = Positive (Small maxUnacked),
    testObjects    = DistinctList objects
  } =
    objectDiffusionOutbound
      tracer
      getObjectId
      maxUnacked
      objects

-- | Run a simple object diffusion client and server, directly on the wrappers,
-- without going via the 'Peer'.
propDirectST :: ObjectDiffusionTestParams
             -> Property
propDirectST params@ObjectDiffusionTestParams{testObjects} = runSimOrThrow $ do
  objects <- directPipelined
               (testOutbound         nullTracer params)
               (testInboundPipelined nullTracer params)
  pure $ objects === fromDistinctList testObjects

propDirectIO :: ObjectDiffusionTestParams
             -> Property
propDirectIO params@ObjectDiffusionTestParams{testObjects} = ioProperty $ do
  objects <- directPipelined
               (testOutbound         nullTracer params)
               (testInboundPipelined nullTracer params)
  pure $ objects === fromDistinctList testObjects


-- | Wrap the inbound (client) test implem as a pipelined typed-protocol 'PeerPipelined'.
testInboundPeerPipelined
  :: (MonadCatch m, MonadEvaluate m)
  => Tracer m (TraceObjectDiffusionDirect ObjectId Object)
  -> ObjectDiffusionTestParams
  -> PeerPipelined (ObjectDiffusion ObjectId Object) AsClient StInit m [Object]
testInboundPeerPipelined tracer params =
    objectDiffusionInboundPeerPipelined (testInboundPipelined tracer params)

-- | Wrap the outbound (server) implem as a non-pipelined typed-protocol 'Peer'.
testOutboundPeer
  :: Monad m
  => Tracer m (TraceObjectDiffusionDirect ObjectId Object)
  -> ObjectDiffusionTestParams
  -> Peer (ObjectDiffusion ObjectId Object) AsServer NonPipelined StInit m ()
testOutboundPeer tracer params =
    objectDiffusionOutboundPeer (testOutbound tracer params)

-- | Run a simple object diffusion client and server, using 'connectPipelined'
-- via the typed-protocol 'Peer' and 'PeerPipelined'.
propConnectST
  :: ObjectDiffusionTestParams
  -> [Bool]
  -> Property
propConnectST params@ObjectDiffusionTestParams{testObjects} choices = runSimOrThrow $ do
    (objects, _, _) <- connectPipelined
                         choices
                         (testInboundPeerPipelined nullTracer params)
                         (testOutboundPeer         nullTracer params)
    pure $ objects === fromDistinctList testObjects

propConnectIO
  :: ObjectDiffusionTestParams
  -> [Bool]
  -> Property
propConnectIO params@ObjectDiffusionTestParams{testObjects} choices = ioProperty $ do
    (objects, _, _) <- connectPipelined
                         choices
                         (testInboundPeerPipelined nullTracer params)
                         (testOutboundPeer         nullTracer params)
    pure $ objects === fromDistinctList testObjects


-- | Run the object diffusion protocol over a pair of connected channels,
-- using the codec, 'runPeer' for the server and 'runPipelinedPeer' for the
-- pipelined client.
objectDiffusionDemoPipelined
  :: forall m.
     ( MonadST    m
     , MonadFork  m
     , MonadCatch m
     , MonadEvaluate m
     , MonadAsync m
     )
  => Channel m ByteString
  -> Channel m ByteString
  -> ObjectDiffusionTestParams
  -> m [Object]
objectDiffusionDemoPipelined clientChan serverChan params = do
  let server = testOutboundPeer         nullTracer params
      client = testInboundPeerPipelined nullTracer params

  -- We only fork the server, and we run the client to completion to extract the
  -- list of objects received as the result.
  _      <- forkIO (void $ runPeer          nullTracer codec serverChan server)
  (r, _) <-                runPipelinedPeer nullTracer codec clientChan client
  pure r

-- | Run the object diffusion demo over pipeline test channels.
propDemoChannelST
  :: ObjectDiffusionTestParams
  -> ChannelSize
  -> Property
propDemoChannelST params@ObjectDiffusionTestParams{testObjects} chanSize = runSimOrThrow $ do
    (clientChan, serverChan) <- createPipelineTestChannels (channelSizeAsNat chanSize)
    objects <- objectDiffusionDemoPipelined clientChan serverChan params
    pure $ objects === fromDistinctList testObjects

propDemoChannelIO
  :: ObjectDiffusionTestParams
  -> ChannelSize
  -> Property
propDemoChannelIO params@ObjectDiffusionTestParams{testObjects} chanSize = ioProperty $ do
    (clientChan, serverChan) <- createPipelineTestChannels (channelSizeAsNat chanSize)
    objects <- objectDiffusionDemoPipelined clientChan serverChan params
    pure $ objects === fromDistinctList testObjects

-- | Run the object diffusion demo over buffered channels.
propDemoChannelBufferedST
  :: ObjectDiffusionTestParams
  -> ChannelSize
  -> Property
propDemoChannelBufferedST params@ObjectDiffusionTestParams{testObjects} chanSize = runSimOrThrow $ do
    (clientChan, serverChan) <- createConnectedBufferedChannels (channelSizeAsNat chanSize)
    objects <- objectDiffusionDemoPipelined clientChan serverChan params
    pure $ objects === fromDistinctList testObjects

propDemoChannelBufferedIO
  :: ObjectDiffusionTestParams
  -> ChannelSize
  -> Property
propDemoChannelBufferedIO params@ObjectDiffusionTestParams{testObjects} chanSize = ioProperty $ do
    (clientChan, serverChan) <- createConnectedBufferedChannels (channelSizeAsNat chanSize)
    objects <- objectDiffusionDemoPipelined clientChan serverChan params
    pure $ objects === fromDistinctList testObjects
