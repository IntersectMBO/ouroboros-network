{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeApplications           #-}

{-# OPTIONS_GHC -Wno-orphans #-}
module Ouroboros.Network.Protocol.LocalStateQuery.Test
  ( tests
  , codec
  , AnyMessageAndAgencyWithResult (..)
  ) where

import qualified Codec.CBOR.Decoding as CBOR
import qualified Codec.CBOR.Encoding as CBOR
import           Data.ByteString.Lazy (ByteString)
import           Data.Map (Map)
import qualified Data.Map as Map

import           Control.Monad.Class.MonadAsync (MonadAsync)
import           Control.Monad.Class.MonadST (MonadST)
import           Control.Monad.Class.MonadThrow (MonadCatch)
import           Control.Monad.IOSim
import           Control.Monad.ST (runST)
import           Control.Tracer (nullTracer)

import           Codec.Serialise (DeserialiseFailure)
import qualified Codec.Serialise as Serialise (decode, encode)
import qualified Codec.Serialise.Class as SerialiseClass

import           Network.TypedProtocol.Codec hiding (prop_codec)
import           Network.TypedProtocol.Proofs

import           Ouroboros.Network.Channel
import           Ouroboros.Network.Driver.Simple (runConnectedPeers)
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
import           Test.Ouroboros.Network.Testing.Utils (prop_codec_cborM,
                     prop_codec_valid_cbor_encoding, splits2, splits3)

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
  -- | An arbitrary query that happens to be trivial to implement in this test
  GetTheLedgerState :: Query MockLedgerState

deriving instance Show (Query result)
instance ShowProxy Query where

newtype MockLedgerState = MockLedgerState (Target (Point Block))
  deriving (Arbitrary, Eq, Show, SerialiseClass.Serialise)

-- | Information to test an example server and client.
data Setup = Setup
  { clientInput   :: [(Target (Point Block), Query MockLedgerState)]
    -- ^ Input for 'localStateQueryClient'
  , serverAcquire :: Target (Point Block) -> Either AcquireFailure MockLedgerState
    -- ^ First input parameter for 'localStateQueryServer'
  , serverAnswer  :: forall result. MockLedgerState -> Query result -> result
    -- ^ Second input parameter for 'localStateQueryServer'
  , expected      :: [(Target (Point Block), Either AcquireFailure MockLedgerState)]
    -- ^ Expected result for the 'localStateQueryClient'.
  }

-- | This map determines the input to the server and client defined in
-- "Ouroboros.Network.Protocol.LocalStateQuery.Examples"
--
-- For each entry, in order, the client will attempt to acquire the key, the
-- server will respond either with the given @'Just' 'AcquireFailure'@ or else
-- in the affirmative, in which case the client will issue the given query.
--
-- This is the randomly generated input for the 'Setup'.
type SetupData = Map (Target (Point Block)) (Maybe AcquireFailure, Query MockLedgerState)

mkSetup :: SetupData -> Setup
mkSetup input = Setup {
      clientInput   = [(pt, q) | (pt, (_, q)) <- Map.toList input]
    , serverAcquire = \tgt -> case Map.lookup tgt input of
        Just (Just failure, _q) -> Left failure
        Just (Nothing,      _q) -> Right (MockLedgerState tgt)
        Nothing                 -> error $
          "a point not in the input was tried to be acquired: " <> show tgt
    , serverAnswer  = answer
    , expected      =
        [ (tgt, res)
        | (tgt, (mbFailure, q)) <- Map.toList input
        , let res = case mbFailure of
                Nothing      -> Right $ answer (MockLedgerState tgt) q
                Just failure -> Left failure
        ]
    }
  where
    answer :: MockLedgerState -> Query result -> result
    answer st q = case q of
      GetTheLedgerState -> st


--
-- Properties going directly, not via Peer.
--

-- | Run a simple local state query client and server, directly on the wrappers,
-- without going via the 'Peer'.
--
prop_direct :: SetupData
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
prop_connect :: SetupData
             -> Property
prop_connect input =
    case runSimOrThrow
           (connect
             (localStateQueryClientPeer $
              localStateQueryClient clientInput)
             (localStateQueryServerPeer $
              localStateQueryServer serverAcquire serverAnswer)) of

      (result, (), TerminalStates TokDone TokDone) -> result === expected
  where
    Setup { clientInput, serverAcquire, serverAnswer, expected } = mkSetup input


--
-- Properties using a channel
--

-- | Run a local state query client and server using connected channels.
--
prop_channel :: ( MonadAsync m
                , MonadCatch m
                , MonadST m
                )
             => m (Channel m ByteString, Channel m ByteString)
             -> SetupData
             -> m Property
prop_channel createChannels input =

    ((expected, ()) ===) <$>

    runConnectedPeers
      createChannels
      nullTracer
      codec
      (localStateQueryClientPeer $
       localStateQueryClient clientInput)
      (localStateQueryServerPeer $
       localStateQueryServer serverAcquire serverAnswer)
  where
    Setup { clientInput, serverAcquire, serverAnswer, expected } = mkSetup input

-- | Run 'prop_channel' in the simulation monad.
--
prop_channel_ST :: SetupData
                -> Property
prop_channel_ST input =
    runSimOrThrow
      (prop_channel createConnectedChannels input)

-- | Run 'prop_channel' in the IO monad.
--
prop_channel_IO :: SetupData
                -> Property
prop_channel_IO input =
    ioProperty (prop_channel createConnectedChannels input)

-- | Run 'prop_channel' in the IO monad using local pipes.
--
prop_pipe_IO :: SetupData
             -> Property
prop_pipe_IO input =
    ioProperty (prop_channel createPipeConnectedChannels input)


--
-- Codec properties
--

instance Arbitrary point => Arbitrary (Target point) where
  arbitrary = oneof
    [ pure ImmutableTip
    , SpecificPoint <$> arbitrary
    , pure VolatileTip
    ]

instance SerialiseClass.Serialise point => SerialiseClass.Serialise (Target point)

instance Arbitrary AcquireFailure where
  arbitrary = elements
    [ AcquireFailurePointTooOld
    , AcquireFailurePointNotOnChain
    ]

instance Arbitrary (Query MockLedgerState) where
  arbitrary = pure GetTheLedgerState

-- | A newtype wrapper which captures type of response generated for all
-- queries.
--
-- Note that this is not as general as the protocol allows, since the protocol
-- admits different result for different queries.
--
newtype AnyMessageAndAgencyWithResult block point query result = AnyMessageAndAgencyWithResult {
    getAnyMessageAndAgencyWithResult :: AnyMessageAndAgency (LocalStateQuery block point query)
  }
  deriving Show

instance ( Arbitrary point
         , Arbitrary (query result)
         , Arbitrary result
         )
      => Arbitrary (AnyMessageAndAgencyWithResult block point query result) where
  arbitrary = AnyMessageAndAgencyWithResult <$> oneof
    [ AnyMessageAndAgency (ClientAgency TokIdle) <$>
        (MsgAcquire <$> arbitrary)

    , AnyMessageAndAgency (ServerAgency TokAcquiring) <$>
        pure MsgAcquired

    , AnyMessageAndAgency (ServerAgency TokAcquiring) <$>
        (MsgFailure <$> arbitrary)

    , AnyMessageAndAgency (ClientAgency TokAcquired) <$>
        (MsgQuery <$> (arbitrary :: Gen (query result)))

    , (\(QueryWithResult query result) ->
        AnyMessageAndAgency (ServerAgency (TokQuerying query))
                            (MsgResult query result))
      <$> (arbitrary :: Gen (QueryWithResult query result))

    , AnyMessageAndAgency (ClientAgency TokAcquired) <$>
        pure MsgRelease

    , AnyMessageAndAgency (ClientAgency TokAcquired) <$>
        (MsgReAcquire <$> arbitrary)

    , AnyMessageAndAgency (ClientAgency TokIdle) <$>
        pure MsgDone
    ]

instance ShowQuery Query where
  showResult GetTheLedgerState = show

instance  Eq (AnyMessage (LocalStateQuery Block (Point Block) Query)) where

  (==) (AnyMessage (MsgAcquire tgt))
       (AnyMessage (MsgAcquire tgt')) = tgt == tgt'

  (==) (AnyMessage MsgAcquired)
       (AnyMessage MsgAcquired) = True

  (==) (AnyMessage (MsgFailure failure))
       (AnyMessage (MsgFailure failure')) = failure == failure'

  (==) (AnyMessage (MsgQuery query))
       (AnyMessage (MsgQuery query')) =
         case (query, query') of
           (GetTheLedgerState, GetTheLedgerState) -> True

  (==) (AnyMessage (MsgResult query  result))
       (AnyMessage (MsgResult query' result')) =
         case (query, query') of
           (GetTheLedgerState, GetTheLedgerState) -> result == result'

  (==) (AnyMessage MsgRelease)
       (AnyMessage MsgRelease) = True

  (==) (AnyMessage (MsgReAcquire tgt))
       (AnyMessage (MsgReAcquire tgt')) = tgt == tgt'

  (==) (AnyMessage MsgDone)
       (AnyMessage MsgDone) = True

  _ == _ = False


codec :: MonadST m
      => Codec (LocalStateQuery Block (Point Block) Query)
                DeserialiseFailure
                m ByteString
codec =
    codecLocalStateQuery
      maxBound
      Serialise.encode Serialise.decode
      encodeQuery      decodeQuery
      encodeResult     decodeResult
  where
    encodeQuery :: Query result -> CBOR.Encoding
    encodeQuery GetTheLedgerState = Serialise.encode ()

    decodeQuery :: forall s . CBOR.Decoder s (Some Query)
    decodeQuery = do
      () <- Serialise.decode
      return $ Some GetTheLedgerState

    encodeResult :: Query result -> result -> CBOR.Encoding
    encodeResult GetTheLedgerState = Serialise.encode

    decodeResult :: Query result -> forall s. CBOR.Decoder s result
    decodeResult GetTheLedgerState = Serialise.decode

-- | Check the codec round trip property.
--
prop_codec
  :: AnyMessageAndAgencyWithResult Block (Point Block) Query MockLedgerState
  -> Bool
prop_codec (AnyMessageAndAgencyWithResult msg) =
  runST (prop_codecM codec msg)

-- | Check for data chunk boundary problems in the codec using 2 chunks.
--
prop_codec_splits2
  :: AnyMessageAndAgencyWithResult Block (Point Block) Query MockLedgerState
  -> Bool
prop_codec_splits2 (AnyMessageAndAgencyWithResult msg) =
  runST (prop_codec_splitsM splits2 codec msg)

-- | Check for data chunk boundary problems in the codec using 3 chunks.
--
prop_codec_splits3
  :: AnyMessageAndAgencyWithResult Block (Point Block) Query MockLedgerState
  -> Bool
prop_codec_splits3 (AnyMessageAndAgencyWithResult msg) =
  runST (prop_codec_splitsM splits3 codec msg)

prop_codec_cbor
  :: AnyMessageAndAgencyWithResult Block (Point Block) Query MockLedgerState
  -> Bool
prop_codec_cbor (AnyMessageAndAgencyWithResult msg) =
  runST (prop_codec_cborM codec msg)

-- | Check that the encoder produces a valid CBOR.
--
prop_codec_valid_cbor
  :: AnyMessageAndAgencyWithResult Block (Point Block) Query MockLedgerState
  -> Property
prop_codec_valid_cbor (AnyMessageAndAgencyWithResult msg) = prop_codec_valid_cbor_encoding codec msg
