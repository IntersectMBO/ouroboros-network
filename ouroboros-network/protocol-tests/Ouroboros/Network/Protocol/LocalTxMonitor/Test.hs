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
import qualified Control.Monad.ST as ST
import           Data.ByteString.Lazy (ByteString)

import           Control.Monad.Class.MonadST

import           Ouroboros.Network.Codec
import           Ouroboros.Network.Util.ShowProxy
import           Ouroboros.Network.Block (SlotNo)

import           Ouroboros.Network.Protocol.LocalTxMonitor.Codec
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

instance (Arbitrary txid, Arbitrary tx, Arbitrary slot)
    => Arbitrary (AnyMessageAndAgency (LocalTxMonitor txid tx slot))
  where
    arbitrary = oneof
      [ pure $ AnyMessageAndAgency (ClientAgency TokIdle) MsgAcquire
      , AnyMessageAndAgency (ServerAgency TokAcquiring) . MsgAcquired <$> arbitrary
      , pure $ AnyMessageAndAgency (ClientAgency TokAcquired) MsgReAcquire
      , pure $ AnyMessageAndAgency (ClientAgency TokAcquired) MsgNextTx
      , AnyMessageAndAgency (ServerAgency (TokBusy TokBusyNext)) . MsgReplyNextTx <$> arbitrary
      , AnyMessageAndAgency (ClientAgency TokAcquired) . MsgHasTx <$> arbitrary
      , AnyMessageAndAgency (ServerAgency (TokBusy TokBusyHas)) . MsgReplyHasTx <$> arbitrary
      , pure $ AnyMessageAndAgency (ClientAgency TokAcquired) MsgGetSizes
      , AnyMessageAndAgency (ServerAgency (TokBusy TokBusySizes)) . MsgReplyGetSizes <$> arbitrary
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
    AnyMessage MsgReAcquire         == AnyMessage MsgReAcquire         = True
    AnyMessage MsgNextTx            == AnyMessage MsgNextTx            = True
    AnyMessage (MsgReplyNextTx a)   == AnyMessage (MsgReplyNextTx b)   = a == b
    AnyMessage (MsgHasTx a)         == AnyMessage (MsgHasTx b)         = a == b
    AnyMessage (MsgReplyHasTx a)    == AnyMessage (MsgReplyHasTx b)    = a == b
    AnyMessage MsgGetSizes          == AnyMessage MsgGetSizes          = True
    AnyMessage (MsgReplyGetSizes a) == AnyMessage (MsgReplyGetSizes b) = a == b
    AnyMessage MsgRelease           == AnyMessage MsgRelease           = True
    AnyMessage MsgDone              == AnyMessage MsgDone              = True
    AnyMessage _                    == AnyMessage _                    = False
