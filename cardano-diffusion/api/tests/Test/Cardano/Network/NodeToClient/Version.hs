{-# LANGUAGE NamedFieldPuns #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Network.NodeToClient.Version (tests) where

import Cardano.Network.NodeToClient.Version

import Ouroboros.Network.CodecCBORTerm

import Test.QuickCheck
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)


tests :: TestTree
tests = testGroup "Cardano.Network.NodeToClient.Version"
    [ testProperty "nodeToClientVersionCodec"  prop_nodeToClientVersionCodec
    , testProperty "nodeToClientCodecCBORTerm" prop_nodeToClientCodec
    ]

data VersionAndVersionData =
    VersionAndVersionData NodeToClientVersion NodeToClientVersionData
  deriving Show

instance Arbitrary NodeToClientVersion where
    arbitrary = elements [minBound..maxBound]

instance Arbitrary VersionAndVersionData where
    arbitrary =
      VersionAndVersionData
        <$> elements [ minBound .. maxBound]
        <*> (NodeToClientVersionData . NetworkMagic <$> arbitrary <*> arbitrary)


prop_nodeToClientVersionCodec :: NodeToClientVersion
                              -> Bool
prop_nodeToClientVersionCodec version =
    case decodeTerm (encodeTerm version) of
        Right version' -> version == version'
        Left {}        -> False
  where
      CodecCBORTerm { encodeTerm, decodeTerm } = nodeToClientVersionCodec


prop_nodeToClientCodec :: VersionAndVersionData -> Bool
prop_nodeToClientCodec (VersionAndVersionData vNumber vData) =
      case decodeTerm (encodeTerm vData) of
        Right vData' -> networkMagic vData' == networkMagic vData
        Left {}      -> False
    where
      CodecCBORTerm { encodeTerm, decodeTerm } = nodeToClientCodecCBORTerm vNumber
