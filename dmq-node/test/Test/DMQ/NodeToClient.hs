{-# LANGUAGE NamedFieldPuns #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Test.DMQ.NodeToClient (tests) where

import Ouroboros.Network.CodecCBORTerm
import Ouroboros.Network.Magic (NetworkMagic (..))

import DMQ.NodeToClient

import Test.QuickCheck
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)

tests :: TestTree
tests = testGroup "DMQ.Network.NodeToClient"
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
