{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Test.Ouroboros.Network.Protocol.ReqResp where

import           Data.Functor.Identity (Identity (..))

import           Protocol.Core (Those (..), connect)

import Ouroboros.Network.Protocol.ReqResp.Client
import Ouroboros.Network.Protocol.ReqResp.Server
import Ouroboros.Network.Protocol.ReqResp.Direct

import           Test.QuickCheck
import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.QuickCheck (testProperty)

tests :: TestTree
tests =
  testGroup "Ouroboros.Network.Protocol.ReqRespProtocol"
  [ testProperty "direct" (prop_direct @Int @Int)
  , testProperty "connect" (prop_connect @Int @Int)
  ]

prop_direct
  :: forall request response.
     ( Eq request
     , Eq response
     , Show request
     , Show response
     )
  => request
  -> response
  -> Property
prop_direct request response =
  case runIdentity $ direct (Server $ \req -> return (response, req)) (Request request return) of
    (request', response') -> request' === request .&&. response' === response

prop_connect
  :: forall request response.
     ( Eq request
     , Eq response
     , Show request
     , Show response
     )
  => request
  -> response
  -> Property
prop_connect request response =
  case runIdentity $ connect server client of
    These request' response' -> request' === request .&&. response' === response
    _                        -> property False
 where
  server = reqRespServerPeer (Server $ \req -> return (response, req))
  client = reqRespClientPeer (Request request return)
