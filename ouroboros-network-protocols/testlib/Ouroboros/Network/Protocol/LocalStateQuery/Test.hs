{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}

{-# OPTIONS_GHC -Wno-orphans #-}
module Ouroboros.Network.Protocol.LocalStateQuery.Test
  ( tests
  , codec
  , AnyMessageWithResult (..)
  ) where

import qualified Codec.CBOR.Decoding as CBOR
import qualified Codec.CBOR.Encoding as CBOR
import           Data.ByteString.Lazy (ByteString)
import           Data.Map (Map)
import qualified Data.Map as Map

import           Control.Applicative (Alternative)
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

import           Network.TypedProtocol.Codec (AnyMessage (..))
import           Network.TypedProtocol.Stateful.Codec (Codec (..))
import qualified Network.TypedProtocol.Stateful.Codec as Stateful
import           Network.TypedProtocol.Stateful.Proofs as Stateful

import           Ouroboros.Network.Channel
import           Ouroboros.Network.Driver.Stateful (runConnectedPeers)
import           Ouroboros.Network.Util.ShowProxy

import           Ouroboros.Network.Mock.Chain (Point)
import           Ouroboros.Network.Mock.ConcreteBlock (Block)

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
        , testProperty "codec"               prop_codec_LocalStateQuery
        , testProperty "codec 2-splits"      prop_codec_splits2
        , testProperty "codec 3-splits"    $ withMaxSuccess 30
                                             prop_codec_splits3
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

data QueryWithResult query result where
    QueryWithResult :: query result
                    -> result
                    -> QueryWithResult query result
  deriving Show

instance ( Arbitrary (query result)
         , Arbitrary result
         )
      => Arbitrary (QueryWithResult query result) where
    arbitrary = QueryWithResult <$> arbitrary <*> arbitrary

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

      (result, (), TerminalStates SingDone SingDone) ->
        result === expected
  where
    Setup { clientInput, serverAcquire, serverAnswer, expected } = mkSetup input


--
-- Properties using a channel
--

-- | Run a local state query client and server using connected channels.
--
prop_channel :: ( Alternative (STM m)
                , MonadAsync m
                , MonadCatch m
                , MonadLabelledSTM m
                , MonadST m
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
        codec
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

-- | A newtype wrapper which captures type of response generated for all
-- queries.
--
-- Note that this is not as general as the protocol allows, since the protocol
-- admits different result for different queries.
--
newtype AnyMessageWithResult block point query result = AnyMessageWithResult {
    getAnyMessageWithResult :: Stateful.AnyMessage (LocalStateQuery block point query) State
  }
  deriving Show

instance ( Arbitrary point
         , Arbitrary (query result)
         , Arbitrary result
         )
      => Arbitrary (AnyMessageWithResult block point query result) where
      arbitrary = oneof
        [ AnyMessageWithResult . getAnyMessageV7 <$> (arbitrary :: Gen (AnyMessageV7 block point query result))

        , pure $ AnyMessageWithResult $ Stateful.AnyMessage StateIdle StateAcquiring (MsgAcquire Nothing)

        , pure $ AnyMessageWithResult $ Stateful.AnyMessage StateAcquired StateAcquiring (MsgReAcquire Nothing)
        ]

-- Newtype wrapper which generates only valid data for 'NodeToClientV7' protocol.
--
newtype AnyMessageV7 block point query result = AnyMessageV7 {
    getAnyMessageV7
      :: Stateful.AnyMessage (LocalStateQuery block point query) State
  }
  deriving Show

instance ( Arbitrary point
         , Arbitrary (query result)
         , Arbitrary result
         )
      => Arbitrary (AnyMessageV7 block point query result) where
  arbitrary = AnyMessageV7 <$> oneof
    [ Stateful.AnyMessage StateIdle StateAcquiring
        <$> (MsgAcquire . Just <$> arbitrary)

    , pure (Stateful.AnyMessage StateAcquiring StateAcquired MsgAcquired)

    , Stateful.AnyMessage StateAcquiring StateIdle
        <$> (MsgFailure <$> arbitrary)

    , (\query ->
        Stateful.AnyMessage StateAcquired
                            (StateQuerying query)
                            (MsgQuery query))
        <$> (arbitrary :: Gen (query result))

    , (\(QueryWithResult query result) ->
        Stateful.AnyMessage (StateQuerying query)
                            StateAcquired
                            (MsgResult query result))
        <$> (arbitrary :: Gen (QueryWithResult query result))

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
      => Codec (LocalStateQuery Block (Point Block) Query)
                DeserialiseFailure
                State
                m ByteString
codec =
    codecLocalStateQuery
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
prop_codec_LocalStateQuery
  :: AnyMessageWithResult Block (Point Block) Query (Maybe (Point Block))
  -> Bool
prop_codec_LocalStateQuery (AnyMessageWithResult msg) =
  runST (Stateful.prop_codecM codec msg)

-- | Check for data chunk boundary problems in the codec using 2 chunks.
--
prop_codec_splits2
  :: AnyMessageWithResult Block (Point Block) Query (Maybe (Point Block))
  -> Bool
prop_codec_splits2 (AnyMessageWithResult msg) =
  runST (Stateful.prop_codec_splitsM splits2 codec msg)

-- | Check for data chunk boundary problems in the codec using 3 chunks.
--
prop_codec_splits3
  :: AnyMessageWithResult Block (Point Block) Query (Maybe (Point Block))
  -> Bool
prop_codec_splits3 (AnyMessageWithResult msg) =
  runST (Stateful.prop_codec_splitsM splits3 codec msg)

-- TODO: this test is not needed; `prop_codec_valid_cbor` and
-- `prop_codec_LocalStateQuery` subsume it.
prop_codec_cbor
  :: AnyMessageWithResult Block (Point Block) Query (Maybe (Point Block))
  -> Bool
prop_codec_cbor (AnyMessageWithResult msg) =
  runST (prop_codec_st_cborM codec msg)

-- | Check that the encoder produces a valid CBOR.
--
prop_codec_valid_cbor
  :: AnyMessageWithResult Block (Point Block) Query (Maybe (Point Block))
  -> Property
prop_codec_valid_cbor (AnyMessageWithResult msg) =
    prop_codec_st_valid_cbor_encoding codec msg
