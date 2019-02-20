{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}

-- TODO: remove this flag, now because `direct` is not used.
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Network.TypedProtocol.ReqResp.Tests (tests) where

import           Data.Functor.Identity (Identity (..))

import           Network.TypedProtocol.Core
import           Network.TypedProtocol.Codec

import           Network.TypedProtocol.PingPong.Tests (splits2, splits3)
import           Network.TypedProtocol.ReqResp.Type
import           Network.TypedProtocol.ReqResp.Codec
import           Network.TypedProtocol.ReqResp.Client as Client
import           Network.TypedProtocol.ReqResp.Server as Server

import           Test.QuickCheck
import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.QuickCheck (testProperty)


--
-- The list of all properties
--

tests :: TestTree
tests = testGroup "Network.TypedProtocol.ReqResp"
  [ testProperty "codec"          prop_codec_ReqResp
  , testProperty "codec 2-splits" prop_codec_splits2_ReqResp
  , testProperty "codec 3-splits" prop_codec_splits3_ReqResp
  ]


-- TODO: this module needs to be fleshed out once the PingPong template is done

direct :: Monad m
       => ReqRespClient req resp m a
       -> ReqRespServer req resp m b
       -> m (a, b)

direct (Client.SendMsgDone clientResult) ReqRespServer{recvMsgDone} =
    pure (clientResult, recvMsgDone)

direct (Client.SendMsgReq req kPong) ReqRespServer{recvMsgReq} = do
    (resp, server') <- recvMsgReq req
    client' <- kPong resp
    direct client' server'

--
-- Codec properties
--

instance (Arbitrary req, Arbitrary resp) => Arbitrary (AnyMessageAndAgency (ReqResp req resp)) where
  arbitrary = oneof
    [ AnyMessageAndAgency (ClientAgency TokIdle) . MsgReq <$> arbitrary
    , AnyMessageAndAgency (ServerAgency TokBusy) . MsgResp <$> arbitrary
    , return (AnyMessageAndAgency (ClientAgency TokIdle) MsgDone)
    ]
  shrink (AnyMessageAndAgency a (MsgReq r))  =
    [ AnyMessageAndAgency a (MsgReq r')
    | r' <- shrink r
    ]
  shrink (AnyMessageAndAgency a (MsgResp r)) =
    [ AnyMessageAndAgency a (MsgResp r')
    | r' <- shrink r
    ]
  shrink (AnyMessageAndAgency _ MsgDone)     = []

instance (Show req, Show resp) => Show (AnyMessageAndAgency (ReqResp req resp)) where
  show (AnyMessageAndAgency _ msg) = show msg

instance (Eq req, Eq resp) => Eq (AnyMessage (ReqResp req resp)) where
  (AnyMessage (MsgReq r1))  == (AnyMessage (MsgReq r2))  = r1 == r2
  (AnyMessage (MsgResp r1)) == (AnyMessage (MsgResp r2)) = r1 == r2
  (AnyMessage MsgDone)      == (AnyMessage MsgDone)      = True
  _                         == _                         = False

prop_codec_ReqResp :: AnyMessageAndAgency (ReqResp String String) -> Bool
prop_codec_ReqResp =
    prop_codec
      runIdentity
      codecReqResp

prop_codec_splits2_ReqResp :: AnyMessageAndAgency (ReqResp String String) -> Bool
prop_codec_splits2_ReqResp =
    prop_codec_splits
      splits2
      runIdentity
      codecReqResp

prop_codec_splits3_ReqResp :: AnyMessageAndAgency (ReqResp String String) -> Property
prop_codec_splits3_ReqResp =
    withMaxSuccess 33 .
      prop_codec_splits
        splits3
        runIdentity
        codecReqResp
