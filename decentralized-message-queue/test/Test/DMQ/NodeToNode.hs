{-# LANGUAGE NamedFieldPuns #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.DMQ.NodeToNode (tests) where

import Ouroboros.Network.CodecCBORTerm
import Ouroboros.Network.Magic (NetworkMagic (..))
import Ouroboros.Network.NodeToNode.Version (DiffusionMode (..))
import Ouroboros.Network.PeerSelection.PeerSharing (PeerSharing (..))

import DMQ.NodeToNode

import Test.QuickCheck
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)


tests :: TestTree
tests = testGroup "DMQ.Network.NodeToNode"
    [ testProperty "nodeToNodeCodecCBORTerm" prop_nodeToNodeCodec
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

prop_nodeToNodeCodec :: NodeToNodeVersion -> NodeToNodeVersionData -> Bool
prop_nodeToNodeCodec ntnVersion ntnData =
      case decodeTerm (encodeTerm ntnData) of
        Right ntnData' -> networkMagic  ntnData' == networkMagic  ntnData
                       && diffusionMode ntnData' == diffusionMode ntnData
        Left {}        -> False
    where
      CodecCBORTerm { encodeTerm, decodeTerm } = nodeToNodeCodecCBORTerm ntnVersion

