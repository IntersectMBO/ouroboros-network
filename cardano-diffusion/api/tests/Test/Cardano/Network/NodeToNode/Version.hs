{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Network.NodeToNode.Version (tests) where

import Cardano.Network.NodeToNode.Version
import Cardano.Network.NodeToNode.Version.TestUtils

import Ouroboros.Network.CodecCBORTerm

import Control.Exception (SomeException, evaluate, try)
import Test.QuickCheck
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)


tests :: TestTree
tests = testGroup "Cardano.Network.NodeToNode.Version"
    [ testProperty "nodeToNodeVersionCodec"  prop_nodeToNodeVersionCodec
    , testProperty "nodeToNodeCodecCBORTerm" prop_nodeToNodeCodec
    , testProperty "nodeToNodeCodecHandleInvalidData" prop_nodeToNodeCodecHandleInvalidData
    ]

instance Arbitrary NodeToNodeVersion where
    arbitrary = genNodeToNodeVersion
    shrink = shrinkNodeToNodeVersion

instance Arbitrary NodeToNodeVersionData where
    arbitrary = genNodeToNodeVersionData
    shrink = shrinkNodeToNodeVersionData

prop_nodeToNodeVersionCodec :: NodeToNodeVersion
                            -> Bool
prop_nodeToNodeVersionCodec version =
    case decodeTerm (encodeTerm version) of
      Right version' -> version == version'
      Left {}        -> False
  where
    CodecCBORTerm { encodeTerm, decodeTerm } = nodeToNodeVersionCodec

prop_nodeToNodeCodec :: NodeToNodeVersion -> NodeToNodeVersionData -> Property
prop_nodeToNodeCodec ntnVersion rawNtnData =
    case decodeTerm (encodeTerm ntnData) of
      Right ntnData' -> ntnData' === ntnData
      Left err       -> counterexample (show err) False
  where
    ntnData = fixNtnVersionDataForVersion ntnVersion rawNtnData
    CodecCBORTerm { encodeTerm, decodeTerm } = nodeToNodeCodecCBORTerm ntnVersion

prop_nodeToNodeCodecHandleInvalidData :: Property
prop_nodeToNodeCodecHandleInvalidData =
    forAll genInvalidNtnVersionAndDataPair checkEncodeFails
  where
    checkEncodeFails (ntnVersion, ntnData) = ioProperty $ do
        r <- try @SomeException (evaluate (encodeTerm ntnData))
        case r of
          Left _  -> pure $ property True
          Right _ -> pure $ counterexample explanation False
      where
        explanation =
             show ntnData
          ++ " was encoded successfully, but should have failed for version "
          ++ show ntnVersion
        CodecCBORTerm { encodeTerm } = nodeToNodeCodecCBORTerm ntnVersion
