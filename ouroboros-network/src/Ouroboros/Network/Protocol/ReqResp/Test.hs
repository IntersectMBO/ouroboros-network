{-# LANGUAGE GADTs               #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE FlexibleInstances   #-}

{-# OPTIONS_GHC -Wno-orphans #-}
module Ouroboros.Network.Protocol.ReqResp.Test (tests) where

import           Control.Monad.ST (runST)

import           Network.TypedProtocol.Codec
import           Network.TypedProtocol.ReqResp.Type
import           Ouroboros.Network.Protocol.ReqResp.Codec

import           Test.Ouroboros.Network.Testing.Utils (splits2, splits3)

import           Test.QuickCheck hiding (Result)
import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.QuickCheck (testProperty)

tests :: TestTree
tests =
  testGroup "Ouroboros.Network.Protocol.ReqResp"
  [ testProperty "codec"               prop_codec_ReqResp
  , testProperty "codec 2-splits"      prop_codec_splits2_ReqResp
  , testProperty "codec 3-splits"      $ withMaxSuccess 30 prop_codec_splits3_ReqResp
  ]


--
-- Codec properties
--

instance (Arbitrary req, Arbitrary resp) => Arbitrary (AnyMessageAndAgency (ReqResp req resp)) where
  arbitrary = oneof
    [ AnyMessageAndAgency (ClientAgency TokIdle) . MsgReq  <$> arbitrary
    , AnyMessageAndAgency (ServerAgency TokBusy) . MsgResp <$> arbitrary
    , pure $ AnyMessageAndAgency (ClientAgency TokIdle) MsgDone
    ]

instance (Show req, Show resp) => Show (AnyMessageAndAgency (ReqResp req resp)) where
  show (AnyMessageAndAgency _ msg) = show msg

instance (Eq req, Eq resp) => Eq (AnyMessage (ReqResp req resp)) where
  AnyMessage (MsgReq r1)  == AnyMessage (MsgReq r2)  = r1 == r2
  AnyMessage (MsgResp r1) == AnyMessage (MsgResp r2) = r1 == r2
  AnyMessage MsgDone      == AnyMessage MsgDone      = True
  _                       == _                       = False

prop_codec_ReqResp
  :: AnyMessageAndAgency (ReqResp String String)
  -> Bool
prop_codec_ReqResp msg =
  runST $ prop_codecM codecReqResp msg

prop_codec_splits2_ReqResp
  :: AnyMessageAndAgency (ReqResp String String)
  -> Bool
prop_codec_splits2_ReqResp msg =
  runST $ prop_codec_splitsM
      splits2
      codecReqResp
      msg

prop_codec_splits3_ReqResp
  :: AnyMessageAndAgency (ReqResp String String)
  -> Bool
prop_codec_splits3_ReqResp msg =
  runST $ prop_codec_splitsM
      splits3
      codecReqResp
      msg
