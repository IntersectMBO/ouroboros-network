{-# LANGUAGE NamedFieldPuns #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Ouroboros.Network.NodeToNode.Version (tests) where

import Ouroboros.Network.CodecCBORTerm
import Ouroboros.Network.Magic
import Ouroboros.Network.NodeToNode.Version qualified as NodeToNode

import Ouroboros.Network.PeerSelection.PeerSharing (PeerSharing (..))
import Test.QuickCheck
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)


tests :: TestTree
tests = testGroup "Ouroboros.Network.NodeToNode.Version"
    [ testProperty "nodeToNodeCodecCBORTerm" prop_nodeToNodeCodec
    ]

instance Arbitrary NodeToNode.Version where
    arbitrary = arbitraryBoundedEnum

    shrink v
      | v == minBound = []
      | otherwise     = [pred v]

instance Arbitrary NodeToNode.VersionData where
    arbitrary =
      NodeToNode.VersionData
        <$> (NetworkMagic <$> arbitrary)
        <*> oneof [ pure NodeToNode.InitiatorOnlyDiffusionMode
                  , pure NodeToNode.InitiatorAndResponderDiffusionMode
                  ]
        <*> elements [ PeerSharingDisabled
                     , PeerSharingEnabled
                     ]
        <*> arbitrary

prop_nodeToNodeCodec :: NodeToNode.Version -> NodeToNode.VersionData -> Bool
prop_nodeToNodeCodec ntnVersion ntnData =
      case decodeTerm (encodeTerm ntnData) of
        Right ntnData' -> NodeToNode.networkMagic  ntnData' == NodeToNode.networkMagic  ntnData
                       && NodeToNode.diffusionMode ntnData' == NodeToNode.diffusionMode ntnData
        Left {}        -> False
    where
      CodecCBORTerm { encodeTerm, decodeTerm } = NodeToNode.codecCBORTerm ntnVersion
