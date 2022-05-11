{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE UndecidableInstances  #-}

{-# OPTIONS_GHC -Wno-orphans #-}
module Ouroboros.Network.Protocol.LocalStateQuery.Test
  ( tests
  , codec
  , Query (..)
  ) where

import qualified Codec.CBOR.Decoding as CBOR
import qualified Codec.CBOR.Encoding as CBOR
import           Data.ByteString.Lazy (ByteString)
import           Data.Map (Map)
import qualified Data.Map as Map

import           Control.Monad.Class.MonadAsync (MonadAsync)
import           Control.Monad.Class.MonadST (MonadST)
import           Control.Monad.Class.MonadSTM (MonadLabelledSTM, STM)
import           Control.Monad.Class.MonadThrow (MonadCatch, MonadMask,
                   MonadThrow)
import           Control.Monad.IOSim
import           Control.Monad.ST (runST)
import           Control.Tracer (nullTracer)

import           Codec.Serialise (DeserialiseFailure)
import qualified Codec.Serialise as Serialise (decode, encode)

import           Network.TypedProtocol.Core
import           Network.TypedProtocol.Codec (AnyMessage (..))
import           Network.TypedProtocol.Stateful.Codec (Codec (..))
import qualified Network.TypedProtocol.Stateful.Codec as Stateful
import           Network.TypedProtocol.Stateful.Proofs as Stateful

import           Ouroboros.Network.Channel
import           Ouroboros.Network.Driver.Stateful (runConnectedPeers)
import           Ouroboros.Network.Util.ShowProxy

import           Ouroboros.Network.MockChain.Chain (Point)
import           Ouroboros.Network.Testing.ConcreteBlock (Block)

import           Ouroboros.Network.Protocol.LocalStateQuery.Client
import           Ouroboros.Network.Protocol.LocalStateQuery.Codec
import           Ouroboros.Network.Protocol.LocalStateQuery.Direct
import           Ouroboros.Network.Protocol.LocalStateQuery.Examples
import           Ouroboros.Network.Protocol.LocalStateQuery.Server
import           Ouroboros.Network.Protocol.LocalStateQuery.Type

import           Test.ChainGenerators ()
import           Test.Ouroboros.Network.Testing.Utils (prop_codec_st_cborM,
                     prop_codec_st_valid_cbor_encoding, splits2, splits3)

import           Test.QuickCheck as QC hiding (Result)
import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.QuickCheck (testProperty)
import           Text.Show.Functions ()


--
-- Test cases
--

tests :: TestTree
tests =
  testGroup "Ouroboros.Network.Protocol"
    [ testGroup "LocalStateQuery"
        [ testProperty "direct"              prop_direct
        , testProperty "connect"             prop_connect
        , testProperty "codec"               prop_codec
        , testProperty "codec 2-splits"      prop_codec_splits2
        , testProperty "codec 3-splits"    $ withMaxSuccess 30
                                             prop_codec_splits3
        , testProperty "codecs V7/V8 compatible"
                                             prop_codec_V7_compatible
        , testProperty "codec cbor"          prop_codec_cbor
        , testProperty "codec valid cbor"    prop_codec_valid_cbor
        , testProperty "channel ST"          prop_channel_ST
        , testProperty "channel IO"          prop_channel_IO
        , testProperty "pipe IO"             prop_pipe_IO
        ]
    ]


--
-- Common types & clients and servers used in the tests in this module.
--

data Query result where
  QueryPoint :: Query (Maybe (Point Block))

deriving instance Show (Query result)
instance ShowProxy Query where

-- | Information to test an example server and client.
data Setup = Setup
  { clientInput   :: [(Maybe (Point Block), Query (Maybe (Point Block)))]
    -- ^ Input for 'localStateQueryClient'
  , serverAcquire :: Maybe (Point Block) -> Either AcquireFailure (Maybe (Point Block))
    -- ^ First input parameter for 'localStateQueryServer'
  , serverAnswer  :: forall result. Maybe (Point Block) -> Query result -> result
    -- ^ Second input parameter for 'localStateQueryServer'
  , expected      :: [(Maybe (Point Block), Either AcquireFailure (Maybe (Point Block)))]
    -- ^ Expected result for the 'localStateQueryClient'.
  }

mkSetup
  :: Map (Maybe (Point Block)) (Maybe AcquireFailure, Query (Maybe (Point Block)))
     -- ^ For each point, the given state queries will be executed. In case of
     -- the second field is an 'AcquireFailure', the server will fail with
     -- that failure.
     --
     -- This is the randomly generated input for the 'Setup'.
  -> Setup
mkSetup input = Setup {
      clientInput   = [(pt, q) | (pt, (_, q)) <- Map.toList input]
    , serverAcquire = \pt -> case Map.lookup pt input of
        Just (Just failure, _qs) -> Left failure
        Just (Nothing,      _qs) -> Right pt
        Nothing                  -> error $
          "a point not in the input was tried to be acquired: " <> show pt
    , serverAnswer  = answer
    , expected      =
        [ (pt, res)
        | (pt, (mbFailure, q)) <- Map.toList input
        , let res = case mbFailure of
                Nothing      -> Right $ answer pt q
                Just failure -> Left failure
        ]
    }
  where
    answer :: Maybe (Point Block) -> Query result -> result
    answer pt q = case q of
      QueryPoint -> pt


--
-- Properties going directly, not via Peer.
--

-- | Run a simple local state query client and server, directly on the wrappers,
-- without going via the 'Peer'.
--
prop_direct :: Map (Maybe (Point Block)) (Maybe AcquireFailure, Query (Maybe (Point Block)))
            -> Property
prop_direct input =
    runSimOrThrow
      (direct
        (localStateQueryClient clientInput)
        (localStateQueryServer serverAcquire serverAnswer))
  ===
    (expected, ())
  where
    Setup { clientInput, serverAcquire, serverAnswer, expected } = mkSetup input


--
-- Properties going via Peer, but without using a channel
--

-- | Run a simple local state query client and server, going via the 'Peer'
-- representation, but without going via a channel.
--
prop_connect :: Map (Maybe (Point Block)) (Maybe AcquireFailure, Query (Maybe (Point Block)))
             -> Property
prop_connect input =
    case runSimOrThrow
           (connect [] [] StateIdle
             (localStateQueryClientPeer $
              localStateQueryClient clientInput)
             (localStateQueryServerPeer $
              localStateQueryServer serverAcquire serverAnswer)) of

      (result, (), TerminalStates SingDone ReflNobodyAgency
                                  SingDone ReflNobodyAgency) ->
        result === expected
  where
    Setup { clientInput, serverAcquire, serverAnswer, expected } = mkSetup input


--
-- Properties using a channel
--

-- | Run a local state query client and server using connected channels.
--
prop_channel :: ( MonadAsync m
                , MonadCatch m
                , MonadLabelledSTM m
                , MonadST    m
                , MonadMask  m
                , MonadThrow m
                , MonadThrow (STM m)
                )
             => m (Channel m ByteString, Channel m ByteString)
             -> Map (Maybe (Point Block)) (Maybe AcquireFailure, Query (Maybe (Point Block)))
             -> m Property
prop_channel createChannels input = do
    r <-
      runConnectedPeers
        createChannels
        nullTracer
        (codec True)
        StateIdle
        (localStateQueryClientPeer $
         localStateQueryClient clientInput)
        (localStateQueryServerPeer $
         localStateQueryServer serverAcquire serverAnswer)
    return $ case r of
      (result, ()) -> result === expected
  where
    Setup { clientInput, serverAcquire, serverAnswer, expected } = mkSetup input

-- | Run 'prop_channel' in the simulation monad.
--
prop_channel_ST :: Map (Maybe (Point Block)) (Maybe AcquireFailure, Query (Maybe (Point Block)))
                -> Property
prop_channel_ST input =
    runSimOrThrow
      (prop_channel createConnectedChannels input)

-- | Run 'prop_channel' in the IO monad.
--
prop_channel_IO :: Map (Maybe (Point Block)) (Maybe AcquireFailure, Query (Maybe (Point Block)))
                -> Property
prop_channel_IO input =
    ioProperty (prop_channel createConnectedChannels input)

-- | Run 'prop_channel' in the IO monad using local pipes.
--
prop_pipe_IO :: Map (Maybe (Point Block)) (Maybe AcquireFailure, Query (Maybe (Point Block)))
             -> Property
prop_pipe_IO input =
    ioProperty (prop_channel createPipeConnectedChannels input)


--
-- Codec properties
--

instance Arbitrary AcquireFailure where
  arbitrary = elements
    [ AcquireFailurePointTooOld
    , AcquireFailurePointNotOnChain
    ]

instance Arbitrary (Query (Maybe (Point Block))) where
  arbitrary = pure QueryPoint

instance Arbitrary (Stateful.AnyMessage (LocalStateQuery Block (Point Block) Query) State) where
  arbitrary = oneof
    [ getAnyMessageV7 <$> arbitrary

    , pure $ Stateful.AnyMessage StateIdle StateAcquiring (MsgAcquire Nothing)

    , pure $ Stateful.AnyMessage StateAcquired StateAcquiring (MsgReAcquire Nothing)
    ]

-- Newtype wrapper which generates only valid data for 'NodeToClientV7' protocol.
--
newtype AnyMessageV7 = AnyMessageV7 {
    getAnyMessageV7
      :: Stateful.AnyMessage (LocalStateQuery Block (Point Block) Query) State
  }

instance Arbitrary AnyMessageV7 where
  arbitrary = AnyMessageV7 <$> oneof
    [ Stateful.AnyMessage StateIdle StateAcquiring <$> (MsgAcquire . Just <$> arbitrary)

    , pure (Stateful.AnyMessage StateAcquiring StateAcquired MsgAcquired)

    , Stateful.AnyMessage StateAcquiring StateIdle <$> (MsgFailure <$> arbitrary)

    , (\query -> Stateful.AnyMessage StateAcquired (StateQuerying query) (MsgQuery query))
      <$> (arbitrary :: Gen (Query (Maybe (Point Block))))

    , Stateful.AnyMessage (StateQuerying QueryPoint) StateAcquired
      <$> (MsgResult QueryPoint <$> arbitrary)

    , pure (Stateful.AnyMessage StateAcquired StateIdle MsgRelease)

    , Stateful.AnyMessage StateAcquired StateAcquiring
      <$> (MsgReAcquire . Just <$> arbitrary)

    , pure (Stateful.AnyMessage StateIdle StateDone MsgDone)
    ]


instance ShowQuery Query where
  showResult QueryPoint = show

instance  Eq (AnyMessage (LocalStateQuery Block (Point Block) Query)) where

  (==) (AnyMessage (MsgAcquire pt))
       (AnyMessage (MsgAcquire pt')) = pt == pt'

  (==) (AnyMessage MsgAcquired)
       (AnyMessage MsgAcquired) = True

  (==) (AnyMessage (MsgFailure failure))
       (AnyMessage (MsgFailure failure')) = failure == failure'

  (==) (AnyMessage (MsgQuery query))
       (AnyMessage (MsgQuery query')) =
         case (query, query') of
           (QueryPoint, QueryPoint) -> True

  (==) (AnyMessage (MsgResult query  result))
       (AnyMessage (MsgResult query' result')) =
         case (query, query') of
           (QueryPoint, QueryPoint) -> result == result'

  (==) (AnyMessage MsgRelease)
       (AnyMessage MsgRelease) = True

  (==) (AnyMessage (MsgReAcquire pt))
       (AnyMessage (MsgReAcquire pt')) = pt == pt'

  (==) (AnyMessage MsgDone)
       (AnyMessage MsgDone) = True

  _ == _ = False


codec :: MonadST m
      => Bool
      -> Codec (LocalStateQuery Block (Point Block) Query)
                DeserialiseFailure
                State
                m ByteString
codec canAcquireTip =
    codecLocalStateQuery
      canAcquireTip
      Serialise.encode Serialise.decode
      encodeQuery      decodeQuery
      encodeResult     decodeResult
  where
    encodeQuery :: Query result -> CBOR.Encoding
    encodeQuery QueryPoint = Serialise.encode ()

    decodeQuery :: forall s . CBOR.Decoder s (Some Query)
    decodeQuery = do
      () <- Serialise.decode
      return $ Some QueryPoint

    encodeResult :: Query result -> result -> CBOR.Encoding
    encodeResult QueryPoint = Serialise.encode

    decodeResult :: Query result -> forall s. CBOR.Decoder s result
    decodeResult QueryPoint = Serialise.decode

-- | Check the codec round trip property.
--
prop_codec
  :: Blind (Stateful.AnyMessage (LocalStateQuery Block (Point Block) Query) State)
  -> Bool
prop_codec (Blind msg) =
  runST (Stateful.prop_codecM (codec True) msg)

-- | Check for data chunk boundary problems in the codec using 2 chunks.
--
prop_codec_splits2
  :: Blind (Stateful.AnyMessage (LocalStateQuery Block (Point Block) Query) State)
  -> Bool
prop_codec_splits2 (Blind msg) =
  runST (Stateful.prop_codec_splitsM splits2 (codec True) msg)

-- | Check for data chunk boundary problems in the codec using 3 chunks.
--
prop_codec_splits3
  :: Blind (Stateful.AnyMessage (LocalStateQuery Block (Point Block) Query) State)
  -> Bool
prop_codec_splits3 (Blind msg) =
  runST (Stateful.prop_codec_splitsM splits3 (codec True) msg)

prop_codec_cbor
  :: Blind (Stateful.AnyMessage (LocalStateQuery Block (Point Block) Query) State)
  -> Bool
prop_codec_cbor (Blind msg) =
  runST (prop_codec_st_cborM (codec True) msg)

-- | Check that the encoder produces a valid CBOR.
--
prop_codec_valid_cbor
  :: Blind (Stateful.AnyMessage (LocalStateQuery Block (Point Block) Query) State)
  -> Property
prop_codec_valid_cbor (Blind msg) = prop_codec_st_valid_cbor_encoding (codec True) msg

prop_codec_V7_compatible
  :: Blind AnyMessageV7
  -> Bool
prop_codec_V7_compatible (Blind (AnyMessageV7 msg)) =
    runST (Stateful.prop_codecs_compatM (codec False) (codec True) msg)
