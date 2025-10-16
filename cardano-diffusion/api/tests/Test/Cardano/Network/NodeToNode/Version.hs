{-# LANGUAGE NamedFieldPuns #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Network.NodeToNode.Version (tests) where

import Cardano.Network.NodeToNode.Version

import Ouroboros.Network.CodecCBORTerm

import Ouroboros.Network.PeerSelection.PeerSharing (PeerSharing (..))
import Test.QuickCheck
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)


tests :: TestTree
tests = testGroup "Cardano.Network.NodeToNode.Version"
    [ testProperty "nodeToNodeVersionCodec"  prop_nodeToNodeVersionCodec
    , testProperty "nodeToNodeCodecCBORTerm" prop_nodeToNodeCodec
    ]

instance Arbitrary NodeToNodeVersion where
    arbitrary = arbitraryBoundedEnum

    shrink v
      | v == minBound = []
      | otherwise     = [pred v]

instance Arbitrary NodeToNodeVersionData where
    arbitrary =
      NodeToNodeVersionData
        <$> (NetworkMagic <$> arbitrary)
        <*> oneof [ pure InitiatorOnlyDiffusionMode
                  , pure InitiatorAndResponderDiffusionMode
                  ]
        <*> elements [ PeerSharingDisabled
                     , PeerSharingEnabled
                     ]
        <*> arbitrary


prop_nodeToNodeVersionCodec :: NodeToNodeVersion
                            -> Bool
prop_nodeToNodeVersionCodec version =
    case decodeTerm (encodeTerm version) of
        Right version' -> version == version'
        Left {}        -> False
  where
      CodecCBORTerm { encodeTerm, decodeTerm } = nodeToNodeVersionCodec


prop_nodeToNodeCodec :: NodeToNodeVersion -> NodeToNodeVersionData -> Bool
prop_nodeToNodeCodec ntnVersion ntnData =
      case decodeTerm (encodeTerm ntnData) of
        Right ntnData' -> networkMagic  ntnData' == networkMagic  ntnData
                       && diffusionMode ntnData' == diffusionMode ntnData
        Left {}        -> False
    where
      CodecCBORTerm { encodeTerm, decodeTerm } = nodeToNodeCodecCBORTerm ntnVersion
