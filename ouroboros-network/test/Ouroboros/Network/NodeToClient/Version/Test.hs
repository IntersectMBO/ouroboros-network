{-# LANGUAGE NamedFieldPuns  #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Ouroboros.Network.NodeToClient.Version.Test (tests) where

import           Ouroboros.Network.CodecCBORTerm
import           Ouroboros.Network.Magic
import           Ouroboros.Network.NodeToClient.Version

import           Test.QuickCheck
import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.QuickCheck (testProperty)


tests :: TestTree
tests = testGroup "Ouroboros.Network.NodeToClient.Version"
    [ testProperty "nodeToClientCodecCBORTerm" prop_nodeToClientCodec
    ]

instance Arbitrary NodeToClientVersionData where
    arbitrary =
      NodeToClientVersionData
        <$> (NetworkMagic <$> arbitrary)

prop_nodeToClientCodec :: NodeToClientVersionData -> Bool
prop_nodeToClientCodec ntcData =
      case decodeTerm (encodeTerm ntcData) of
        Right ntcData' -> networkMagic ntcData' == networkMagic  ntcData
        Left {}        -> False
    where
      CodecCBORTerm { encodeTerm, decodeTerm } = nodeToClientCodecCBORTerm
