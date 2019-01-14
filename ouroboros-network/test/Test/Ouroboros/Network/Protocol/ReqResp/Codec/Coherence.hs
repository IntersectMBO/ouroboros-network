{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}

{-# OPTIONS_GHC "-Wno-unticked-promoted-constructors" #-}
module Test.Ouroboros.Network.Protocol.ReqResp.Codec.Coherence where

import Control.Monad.ST (runST)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Type.Equality
import Data.Text (Text)
import qualified Data.Text as T

import Codec.Serialise.Class (Serialise)

import Test.QuickCheck
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)

import Protocol.Codec
import Protocol.Codec.Coherent
import Protocol.Path

import Ouroboros.Network.Protocol.Codec.Cbor (convertCborCodec')
import Ouroboros.Network.Protocol.ReqResp.Type
import Ouroboros.Network.Protocol.ReqResp.Codec.Cbor

prop_reqresp_cbor_coherent
  :: forall request response.
     ( Serialise request
     , Serialise response
     , Show request
     , Show response
     , Arbitrary request
     , Arbitrary response
     )
  => Property
prop_reqresp_cbor_coherent = forAllShow genPath (showSomeTransitionPath show) doTest
 where
  doTest
    :: SomeTransitionPath (ReqRespMessage request response) StIdle
    -> Property
  doTest (SomeTransitionPath path) =
    prop_coherent (==) show show show (runST (codecTree decodeFull (eqTr) (convertCborCodec' codecReqResp) path))

  reqRespPath
    :: request
    -> response
    -> SomeTransitionPath (ReqRespMessage request response) StIdle
  reqRespPath request response = SomeTransitionPath $ PathCons $ Transition (MsgRequest request) (PathCons $ Transition (MsgResponse response) PathNil)

  genPath :: Gen (SomeTransitionPath (ReqRespMessage request response) StIdle)
  genPath = reqRespPath <$> arbitrary <*> arbitrary

  eqTr :: forall from to to' .
          ReqRespMessage request response from to
       -> ReqRespMessage request response from to'
       -> Maybe (to :~: to')
  eqTr MsgRequest{}  MsgRequest{}  = Just Refl
  eqTr MsgResponse{} MsgResponse{} = Just Refl

  decodeFull :: Monad m => ByteString -> Decoder Text ByteString m x -> m (Either Text x)
  decodeFull str decoder = do
    (it, remaining) <- foldOverInput decoder (singletonInput [str])
    case remaining of
      Nothing -> pure it
      -- If there's more input, we'll drain it. giving a list of all of the
      -- remaining inputs. Those inputs happen to be of type [String], so
      -- we mconcat it twice to get the entire remaining input as a String.
      Just input -> drainInput input >>= \lst ->
        let rest = mconcat (mconcat lst)
        in if BS.null rest
             then pure it
             else pure $ Left $ T.pack (show rest)

tests :: TestTree
tests = testGroup "ReqRespProtocol.Codec.Cbor"
  [ testProperty "ReqRespProtocol: coherent cbor codec" (prop_reqresp_cbor_coherent @Int @Int)
  ]
