module Main where

import Test.Ouroboros.Network.PublicState qualified as PublicState

import Main.Utf8 (withUtf8)
import Test.Tasty

main :: IO ()
main = withUtf8 $ defaultMain tests

tests :: TestTree
tests =
  testGroup "ouroboros-network-api:test"
  [ PublicState.tests ]
