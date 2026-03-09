{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Ouroboros.Network.NodeToNode.Version (tests) where

import Ouroboros.Network.CodecCBORTerm
import Ouroboros.Network.Magic

import Cardano.Network.NodeToNode.Version

import Control.Exception (SomeException, evaluate, try)
import Ouroboros.Network.PeerSelection.PeerSharing (PeerSharing (..))
import Test.QuickCheck
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)


tests :: TestTree
tests = testGroup "Ouroboros.Network.NodeToNode.Version"
    [ testProperty "nodeToNodeCodecCBORTerm" prop_nodeToNodeCodec
    , testProperty "nodeToNodeCodecHandleInvalidData" prop_nodeToNodeCodecHandleInvalidData
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
      <*> elements [ PerasUnsupported
                   , PerasSupported
                   ]

prop_nodeToNodeCodec :: NodeToNodeVersion -> NodeToNodeVersionData -> Property
prop_nodeToNodeCodec ntnVersion rawNtnData =
    case decodeTerm (encodeTerm ntnData) of
      Right ntnData' -> ntnData' === ntnData
      Left err       -> counterexample (show err) False
  where
    ntnData = fixNtnVersionDataForVersion ntnVersion rawNtnData
    CodecCBORTerm { encodeTerm, decodeTerm } = nodeToNodeCodecCBORTerm ntnVersion

prop_nodeToNodeCodecHandleInvalidData :: NodeToNodeVersionData -> Property
prop_nodeToNodeCodecHandleInvalidData rawNtnData =
    forAll (elements (invalidateNtnVersionData rawNtnData)) checkEncodeFails
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
