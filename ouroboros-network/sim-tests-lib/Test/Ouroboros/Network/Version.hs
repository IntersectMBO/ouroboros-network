{-# LANGUAGE ScopedTypeVariables #-}

-- | Test `NodeToNodeVersion` and `NodeToClientVersion` codecs.
--
module Test.Ouroboros.Network.Version (tests) where

import Ouroboros.Network.CodecCBORTerm
import Ouroboros.Network.NodeToClient qualified as NodeToClient
import Ouroboros.Network.NodeToNode qualified as NodeToNode

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit


tests :: TestTree
tests =
  testGroup "Ouroboros.Network.Protocol.Handshake.Version"
    [ testGroup "NodeToClientVersion"
      [ testCase "NodeToClientVersion round-trip codec property"
                 (roundTripPropAll NodeToClient.versionCodec)
      , testCase "NodeToClientVersion should not deserialise as NodeToNode"
                 (crossFailurePropAll
                   NodeToClient.versionCodec
                   NodeToNode.versionCodec
                   ([minBound .. maxBound] :: [NodeToClient.Version]))
      ]
    , testGroup "NodeToNodeVersion"
      [ testCase "NodeToNodeVersion round-trip codec property"
                 (roundTripPropAll NodeToNode.versionCodec)
      , testCase "NodeToNodeVersion should not deserialise as NodeToClient"
                 (crossFailurePropAll
                   NodeToNode.versionCodec
                   NodeToClient.versionCodec
                   ([minBound .. maxBound] :: [NodeToNode.Version]))
      ]
    ]


roundTripProp :: ( Eq a
                 , Show a
                 , Eq failure
                 , Show failure
                 )
              => CodecCBORTerm failure a
              -> a -> Assertion
roundTripProp codec a =
    Right a @=? decodeTerm codec (encodeTerm codec a)


-- Using `Monoid` instance of `IO ()`
roundTripPropAll
    :: forall failure a.
       ( Eq a
       , Enum a
       , Bounded a
       , Show a
       , Eq failure
       , Show failure
       )
    => CodecCBORTerm failure a -> Assertion
roundTripPropAll codec =
    foldMap (roundTripProp codec) ([minBound..maxBound] :: [a])


crossFailureProp
    :: forall failure a b.
       ( Show a
       , Show b
       , Eq failure
       , Show failure
       )
    => CodecCBORTerm failure a
    -> CodecCBORTerm failure b
    -> a
    -> Assertion
crossFailureProp codecA codecB a =
    case decodeTerm codecB (encodeTerm codecA a) of
      Right b -> assertFailure (show a ++ "should not deserialise as " ++ show b)
      Left  _ -> pure ()


crossFailurePropAll
    :: forall failure a b.
       ( Show a
       , Show b
       , Eq failure
       , Show failure
       )
    => CodecCBORTerm failure a
    -> CodecCBORTerm failure b
    -> [a]
    -> Assertion
crossFailurePropAll codecA codecB = foldMap (crossFailureProp codecA codecB)

