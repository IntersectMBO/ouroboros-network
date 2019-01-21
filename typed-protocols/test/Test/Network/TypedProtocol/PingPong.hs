{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Test.Network.TypedProtocol.PingPong where

import           Data.Functor.Identity (Identity (..))

import           Test.Tasty
import           Test.QuickCheck
import           Test.Tasty.QuickCheck (testProperty)

import           Network.TypedProtocol.Core (connect)
import           Network.TypedProtocol.PingPong.Client (pingPongClientFixed, pingPongClientPeer)
import           Network.TypedProtocol.PingPong.Server (pingPongServerCount, pingPongServerPeer)
import           Network.TypedProtocol.PingPong.Direct (direct)

tests :: TestTree
tests = testGroup "Network.TypedProtocol.PingPong"
  [ testProperty "direct" prop_direct
  , testProperty "connect" prop_connect
  ]

prop_direct
  :: Positive Int
  -> Property
prop_direct (Positive x) =
  let c = fromIntegral x
  in case runIdentity $ direct (pingPongClientFixed c) pingPongServerCount of
    (_, c') -> c === c'

prop_connect
  :: Positive Int
  -> Property
prop_connect (Positive x) =
  let c = fromIntegral x
  in case runIdentity $ connect (pingPongClientPeer $ pingPongClientFixed c) (pingPongServerPeer pingPongServerCount) of
    (_, c') -> c === c'
