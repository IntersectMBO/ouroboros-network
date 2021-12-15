{-# LANGUAGE ScopedTypeVariables #-}

-- | Test `NodeToNodeVersion` and `NodeToClientVersion` codecs.
--
module Test.Version (tests) where

import           Ouroboros.Network.CodecCBORTerm
import           Ouroboros.Network.NodeToClient (NodeToClientVersion (..),
                     nodeToClientVersionCodec)
import           Ouroboros.Network.NodeToNode (nodeToNodeVersionCodec)

import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.HUnit


tests :: TestTree
tests =
  testGroup "Ouroboros.Network.Protocol.Handshake.Version"
    [ testGroup "NodeToClientVersion"
      [ testCase "NodeToClientVersion round-trip codec property"
                 (roundTripPropAll nodeToClientVersionCodec)
      , testCase "NodeToClientVersion should not deserialise as NodeToNode"
                 (crossFailurePropAll
                   nodeToClientVersionCodec
                   nodeToNodeVersionCodec
                   ([NodeToClientV_2 .. maxBound] :: [NodeToClientVersion]))
      ]
    , testGroup "NodeToNodeVersion"
      [ testCase "NodeToNodeVersion round-trip codec property"
                 (roundTripPropAll nodeToNodeVersionCodec)
      -- TODO: enable this test when `NodeToClientV_1` is removed:
      {--
        - , testCase "NodeToNodeVersion should not deserialise as NodeToClient"
        -            (crossFailurePropAll
        -              nodeToNodeVersionCodec
        -              nodeToClientVersionCodec
        -              ([minBound .. maxBound] :: [NodeToNodeVersion]))
        --}
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

