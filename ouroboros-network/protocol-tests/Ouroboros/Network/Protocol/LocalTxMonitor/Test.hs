{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables        #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Ouroboros.Network.Protocol.LocalTxMonitor.Test (tests) where

import           Codec.Serialise (Serialise)
import qualified Codec.Serialise as S
import           Data.ByteString.Lazy (ByteString)

import           Control.Monad.Class.MonadAsync
import           Control.Monad.Class.MonadST
import           Control.Monad.Class.MonadThrow
import           Control.Monad.IOSim
import           Control.Tracer (nullTracer)
import qualified Control.Monad.ST as ST

import           Network.TypedProtocol.Core
import           Network.TypedProtocol.Proofs

import           Ouroboros.Network.Block (SlotNo)
import           Ouroboros.Network.Channel
import           Ouroboros.Network.Codec
import           Ouroboros.Network.Driver.Simple (runConnectedPeers)
import           Ouroboros.Network.Util.ShowProxy

import           Ouroboros.Network.Protocol.LocalTxMonitor.Client
import           Ouroboros.Network.Protocol.LocalTxMonitor.Codec
import           Ouroboros.Network.Protocol.LocalTxMonitor.Direct
import           Ouroboros.Network.Protocol.LocalTxMonitor.Examples
import           Ouroboros.Network.Protocol.LocalTxMonitor.Server
import           Ouroboros.Network.Protocol.LocalTxMonitor.Type

import           Test.ChainGenerators ()
import           Test.Ouroboros.Network.Testing.Utils (prop_codec_cborM,
                     prop_codec_valid_cbor_encoding, splits2, splits3)
import           Test.QuickCheck hiding (Result)
import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.QuickCheck (testProperty)

tests :: TestTree
tests = testGroup "Ouroboros.Network.Protocol"
  [ testGroup "LocalTxMonitor"
    [ testProperty "codecM" prop_codecM_LocalTxMonitor
    , testProperty "codec 2-splits" (prop_codec_splitsM_LocalTxMonitor splits2)
    , testProperty "codec 3-splits" (prop_codec_splitsM_LocalTxMonitor splits3)
    , testProperty "codec cborM" prop_codec_cborM_LocalTxMonitor
    , testProperty "codec valid cbor encoding" prop_codec_valid_cbor_encoding_LocalTxMonitor

    , testProperty "direct" prop_direct
    , testProperty "connect" prop_connect

    , testProperty "channel ST" prop_channel_ST
    , testProperty "channel IO" prop_channel_IO
    , testProperty "pipe IO" prop_pipe_IO
    ]
  ]

--
-- Codec
--

codec ::
     ( MonadST m
     )
  => Codec (LocalTxMonitor TxId Tx SlotNo) S.DeserialiseFailure m ByteString
codec = codecLocalTxMonitor
  S.encode S.decode
  S.encode S.decode
  S.encode S.decode

--
-- Properties
--

prop_codecM_LocalTxMonitor ::
     AnyMessageAndAgency (LocalTxMonitor TxId Tx SlotNo)
  -> Bool
prop_codecM_LocalTxMonitor msg =
    ST.runST $ prop_codecM codec msg

prop_codec_cborM_LocalTxMonitor ::
     AnyMessageAndAgency (LocalTxMonitor TxId Tx SlotNo)
  -> Bool
prop_codec_cborM_LocalTxMonitor msg =
  ST.runST $ prop_codec_cborM codec msg

prop_codec_splitsM_LocalTxMonitor ::
     (ByteString -> [[ByteString]])
  -> AnyMessageAndAgency (LocalTxMonitor TxId Tx SlotNo)
  -> Bool
prop_codec_splitsM_LocalTxMonitor splitN msg =
  ST.runST $ prop_codec_splitsM splitN codec msg

prop_codec_valid_cbor_encoding_LocalTxMonitor ::
     AnyMessageAndAgency (LocalTxMonitor TxId Tx SlotNo)
  -> Property
prop_codec_valid_cbor_encoding_LocalTxMonitor =
  prop_codec_valid_cbor_encoding codec

--
-- Protocol Executions
--

-- | Run a simple local tx monitor client and server, directly on the wrappers,
-- without going via the 'Peer'.
--
prop_direct ::
     (SlotNo, [Tx])
  -> Property
prop_direct (slot, txs) =
  let ((txs', sz), ()) = runSimOrThrow (direct
                                         (localTxMonitorClient txId)
                                         (localTxMonitorServer txId (slot, txs))
                                       )
   in
    ( txs'
    , numberOfTxs sz
    )
   ===
    ( [ (tx, True) | tx <- txs ]
    , fromIntegral $ length txs
    )

-- | Run a simple tx-monitor client and server, going via the 'Peer'
-- representation, but without going via a channel.
--
-- This test converts the pipelined server peer to a non-pipelined peer
-- before connecting it with the client.
--
prop_connect :: (SlotNo, [Tx]) -> Bool
prop_connect (slot, txs) =
    case runSimOrThrow
           (connect
             (localTxMonitorClientPeer $
                localTxMonitorClient txId)
             (localTxMonitorServerPeer $
                localTxMonitorServer txId (slot, txs))) of

      ((txs', _), (), TerminalStates TokDone TokDone) ->
        txs' == [ (tx, True) | tx <- txs ]

-- | Run a local tx-monitor client and server using connected channels.
--
prop_channel :: (MonadAsync m, MonadCatch m, MonadST m)
             => m (Channel m ByteString, Channel m ByteString)
             -> (SlotNo, [Tx])
             -> m Bool
prop_channel createChannels (slot, txs) = do
  ((txs', _), ()) <- runConnectedPeers createChannels nullTracer codec
                        (localTxMonitorClientPeer $
                          localTxMonitorClient txId)
                        (localTxMonitorServerPeer $
                          localTxMonitorServer txId (slot, txs))
  pure (txs' == [ (tx, True) | tx <- txs ])

-- | Run 'prop_channel' in the simulation monad.
--
prop_channel_ST :: (SlotNo, [Tx]) -> Bool
prop_channel_ST txs =
    runSimOrThrow
      (prop_channel createConnectedChannels txs)

-- | Run 'prop_channel' in the IO monad.
--
prop_channel_IO :: (SlotNo, [Tx]) -> Property
prop_channel_IO txs =
    ioProperty (prop_channel createConnectedChannels txs)

-- | Run 'prop_channel' in the IO monad using local pipes.
--
prop_pipe_IO :: (SlotNo, [Tx]) -> Property
prop_pipe_IO txs =
    ioProperty (prop_channel createPipeConnectedChannels txs)

--
-- Mock Types
--

newtype Tx = Tx { txId :: TxId }
  deriving (Eq, Show, Arbitrary, Serialise)

instance ShowProxy Tx where
    showProxy _ = "Tx"

newtype TxId = TxId Int
  deriving (Eq, Ord, Show, Arbitrary, Serialise)

instance ShowProxy TxId where
    showProxy _ = "TxId"

--
-- Orphans Plumbing
--

instance ShowProxy SlotNo where

instance (Arbitrary txid, Arbitrary tx, Arbitrary slot)
    => Arbitrary (AnyMessageAndAgency (LocalTxMonitor txid tx slot))
  where
    arbitrary = oneof
      [ pure $ AnyMessageAndAgency (ClientAgency TokIdle) MsgAcquire
      , AnyMessageAndAgency (ServerAgency TokAcquiring) . MsgAcquired <$> arbitrary
      , pure $ AnyMessageAndAgency (ClientAgency TokAcquired) MsgAwaitAcquire
      , pure $ AnyMessageAndAgency (ClientAgency TokAcquired) MsgNextTx
      , AnyMessageAndAgency (ServerAgency (TokBusy TokNextTx)) . MsgReplyNextTx <$> arbitrary
      , AnyMessageAndAgency (ClientAgency TokAcquired) . MsgHasTx <$> arbitrary
      , AnyMessageAndAgency (ServerAgency (TokBusy TokHasTx)) . MsgReplyHasTx <$> arbitrary
      , pure $ AnyMessageAndAgency (ClientAgency TokAcquired) MsgGetSizes
      , AnyMessageAndAgency (ServerAgency (TokBusy TokGetSizes)) . MsgReplyGetSizes <$> arbitrary
      , pure $ AnyMessageAndAgency (ClientAgency TokAcquired) MsgRelease
      , pure $ AnyMessageAndAgency (ClientAgency TokIdle) MsgDone
      ]

instance Arbitrary MempoolSizeAndCapacity where
  arbitrary =
    MempoolSizeAndCapacity
      <$> arbitrary
      <*> arbitrary
      <*> arbitrary

instance (Eq txid, Eq tx, Eq slot)
    => Eq (AnyMessage (LocalTxMonitor txid tx slot))
  where
    AnyMessage MsgAcquire           == AnyMessage MsgAcquire           = True
    AnyMessage (MsgAcquired a)      == AnyMessage (MsgAcquired b)      = a == b
    AnyMessage MsgAwaitAcquire      == AnyMessage MsgAwaitAcquire      = True
    AnyMessage MsgNextTx            == AnyMessage MsgNextTx            = True
    AnyMessage (MsgReplyNextTx a)   == AnyMessage (MsgReplyNextTx b)   = a == b
    AnyMessage (MsgHasTx a)         == AnyMessage (MsgHasTx b)         = a == b
    AnyMessage (MsgReplyHasTx a)    == AnyMessage (MsgReplyHasTx b)    = a == b
    AnyMessage MsgGetSizes          == AnyMessage MsgGetSizes          = True
    AnyMessage (MsgReplyGetSizes a) == AnyMessage (MsgReplyGetSizes b) = a == b
    AnyMessage MsgRelease           == AnyMessage MsgRelease           = True
    AnyMessage MsgDone              == AnyMessage MsgDone              = True
    AnyMessage _                    == AnyMessage _                    = False
