{-# LANGUAGE GADTs               #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE FlexibleInstances   #-}

{-# OPTIONS_GHC -Wno-orphans #-}
module Network.TypedProtocol.ReqResp.Codec.Test
  ( tests
  -- TODO: only temporary
  , splits2
  , splits3
  ) where

import           Control.Monad.ST (runST)
import qualified Data.ByteString.Lazy as LBS

import           Network.TypedProtocol.Codec
import           Network.TypedProtocol.ReqResp.Type
import           Network.TypedProtocol.ReqResp.Codec.Cbor

import           Test.QuickCheck hiding (Result)
import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.QuickCheck (testProperty)

tests :: TestTree
tests =
  testGroup "Network.TypedProtocol.ReqResp.Codec.Cbor"
  [ testProperty "codec"               prop_codec_ReqResp
  , testProperty "codec 2-splits"      prop_codec_splits2_ReqResp
  , testProperty "codec 3-splits"      $ withMaxSuccess 30 prop_codec_splits3_ReqResp
  ]

-- | Generate all 2-splits of a string.
--
-- TODO: find a better palce for 'split2' and 'split3' (they are duplicated);
-- possibly put them in 'ouroboros-network-testing'.
--
splits2 :: LBS.ByteString -> [[LBS.ByteString]]
splits2 bs = zipWith (\a b -> [a,b]) (LBS.inits bs) (LBS.tails bs)

-- | Generate all 3-splits of a string.
splits3 :: LBS.ByteString -> [[LBS.ByteString]]
splits3 bs =
    [ [a,b,c]
    | (a,bs') <- zip (LBS.inits bs)  (LBS.tails bs)
    , (b,c)   <- zip (LBS.inits bs') (LBS.tails bs') ]

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
