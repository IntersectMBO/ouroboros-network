{-# LANGUAGE GADTs               #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE FlexibleInstances   #-}

{-# OPTIONS_GHC -Wno-orphans #-}
module Test.Network.TypedProtocol.PingPong.Codec (tests) where

import           Control.Monad.ST (runST)

import           Network.TypedProtocol.PingPong.Type
import           Network.TypedProtocol.PingPong.Codec.CBOR
-- TODO: remove this import
import           Test.Network.TypedProtocol.ReqResp.Codec (splits2, splits3)

import           Ouroboros.Network.Codec

import           Test.QuickCheck hiding (Result)
import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.QuickCheck (testProperty)

tests :: TestTree
tests =
  testGroup "Network.Typed.Protocol.PingPong.Codec.Cbor"
  [ testProperty "codec"               prop_codec_PingPong
  , testProperty "codec 2-splits"      prop_codec_splits2_PingPong
  , testProperty "codec 3-splits"      $ withMaxSuccess 30 prop_codec_splits3_PingPong
  ]


--
-- Codec properties
--

instance Arbitrary (AnyMessageAndAgency PingPong) where
  arbitrary = oneof
    [ return $ AnyMessageAndAgency (ClientAgency TokIdle) MsgPing
    , return $ AnyMessageAndAgency (ServerAgency TokBusy) MsgPong
    , return $ AnyMessageAndAgency (ClientAgency TokIdle) MsgDone
    ]

instance Eq (AnyMessage PingPong) where
  AnyMessage MsgPing == AnyMessage MsgPing = True
  AnyMessage MsgPong == AnyMessage MsgPong = True
  AnyMessage MsgDone == AnyMessage MsgDone = True
  _                  == _                  = False

prop_codec_PingPong
  :: AnyMessageAndAgency PingPong
  -> Bool
prop_codec_PingPong msg =
  runST $ prop_codecM codecPingPong msg

prop_codec_splits2_PingPong
  :: AnyMessageAndAgency PingPong
  -> Bool
prop_codec_splits2_PingPong msg =
  runST $ prop_codec_splitsM
      splits2
      codecPingPong
      msg

prop_codec_splits3_PingPong
  :: AnyMessageAndAgency PingPong
  -> Bool
prop_codec_splits3_PingPong msg =
  runST $ prop_codec_splitsM
      splits3
      codecPingPong
      msg

