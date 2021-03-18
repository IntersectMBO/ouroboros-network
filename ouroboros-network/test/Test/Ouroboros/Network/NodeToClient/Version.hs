{-# LANGUAGE NamedFieldPuns  #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Ouroboros.Network.NodeToClient.Version (tests) where

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

data VersionAndVersionData =
    VersionAndVersionData NodeToClientVersion NodeToClientVersionData
  deriving Show

instance Arbitrary VersionAndVersionData where
    arbitrary =
      VersionAndVersionData
        <$> elements [ NodeToClientV_1, NodeToClientV_2, NodeToClientV_3 ]
        <*> (NodeToClientVersionData . NetworkMagic <$> arbitrary)

prop_nodeToClientCodec :: VersionAndVersionData -> Bool
prop_nodeToClientCodec (VersionAndVersionData vNumber vData) =
      case decodeTerm (encodeTerm vData) of
        Right vData' -> networkMagic vData' == networkMagic vData
        Left {}      -> False
    where
      CodecCBORTerm { encodeTerm, decodeTerm } = nodeToClientCodecCBORTerm vNumber
