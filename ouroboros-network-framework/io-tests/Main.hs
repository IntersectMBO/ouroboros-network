module Main (main) where

import Main.Utf8 (withUtf8)
import Test.Tasty

import Test.Ouroboros.Network.Driver qualified as Driver
import Test.Ouroboros.Network.Server2.IO qualified as Server2
import Test.Ouroboros.Network.Socket qualified as Socket
import Test.Ouroboros.Network.Subscription qualified as Subscription

main :: IO ()
main = withUtf8 $ defaultMain tests

tests :: TestTree
tests =
  testGroup "ouroboros-network-framework:io-tests" $
  [ Driver.tests
  , Server2.tests
  , Socket.tests
  , Subscription.tests
  ]


