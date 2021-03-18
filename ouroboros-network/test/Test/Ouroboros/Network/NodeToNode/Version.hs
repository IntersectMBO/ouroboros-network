{-# LANGUAGE NamedFieldPuns  #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Ouroboros.Network.NodeToNode.Version (tests) where

import           Ouroboros.Network.CodecCBORTerm
import           Ouroboros.Network.Magic
import           Ouroboros.Network.NodeToNode.Version

import           Test.QuickCheck
import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.QuickCheck (testProperty)


tests :: TestTree
tests = testGroup "Ouroboros.Network.NodeToNode.Version"
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

prop_nodeToNodeCodec :: NodeToNodeVersion -> NodeToNodeVersionData -> Bool
prop_nodeToNodeCodec ntnVersion ntnData =
      case decodeTerm (encodeTerm ntnData) of
        Right ntnData' -> networkMagic  ntnData' == networkMagic  ntnData
                       && if ntnVersion <= NodeToNodeV_3
                            then diffusionMode ntnData' == InitiatorAndResponderDiffusionMode
                            else diffusionMode ntnData' == diffusionMode ntnData
        Left {}        -> False
    where
      CodecCBORTerm { encodeTerm, decodeTerm } = nodeToNodeCodecCBORTerm ntnVersion
