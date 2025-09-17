module Main (main) where

import Main.Utf8 (withUtf8)
import Test.Tasty

import Test.Cardano.Network.NodeToClient.Version qualified as NodeToClient.Version
import Test.Cardano.Network.NodeToNode.Version qualified as NodeToNode.Version
import Test.Cardano.Network.Version qualified as Version

main :: IO ()
main = withUtf8 $ defaultMain tests

tests :: TestTree
tests =
  testGroup "cardano-diffusion:api"
  [ Version.tests
  , NodeToClient.Version.tests
  , NodeToNode.Version.tests
  ]


