{-# LANGUAGE NamedFieldPuns #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Ouroboros.Network.NodeToClient.Version (tests) where

import Ouroboros.Network.CodecCBORTerm
import Ouroboros.Network.Magic
import Ouroboros.Network.NodeToClient.Version qualified as NodeToClient

import Test.QuickCheck
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)


tests :: TestTree
tests = testGroup "Ouroboros.Network.NodeToClient.Version"
    [ testProperty "nodeToClientCodecCBORTerm" prop_nodeToClientCodec
    ]

data VersionAndVersionData =
    VersionAndVersionData NodeToClient.Version NodeToClient.VersionData
  deriving Show

instance Arbitrary VersionAndVersionData where
    arbitrary =
      VersionAndVersionData
        <$> elements [ minBound .. maxBound]
        <*> (NodeToClient.VersionData . NetworkMagic <$> arbitrary <*> arbitrary)

prop_nodeToClientCodec :: VersionAndVersionData -> Bool
prop_nodeToClientCodec (VersionAndVersionData vNumber vData) =
      case decodeTerm (encodeTerm vData) of
        Right vData' -> NodeToClient.networkMagic vData' == NodeToClient.networkMagic vData
        Left {}      -> False
    where
      CodecCBORTerm { encodeTerm, decodeTerm } = NodeToClient.codecCBORTerm vNumber
