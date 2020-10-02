{-# LANGUAGE NamedFieldPuns  #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Ouroboros.Network.NodeToNode.Version.Test (tests) where

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
    arbitrary = oneof [ pure NodeToNodeV_1
                      , pure NodeToNodeV_2
                      , pure NodeToNodeV_3
                      , pure NodeToNodeV_4
                      ]

    shrink NodeToNodeV_4 = [NodeToNodeV_3]
    shrink NodeToNodeV_3 = [NodeToNodeV_2]
    shrink NodeToNodeV_2 = [NodeToNodeV_1]
    shrink NodeToNodeV_1 = []

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
