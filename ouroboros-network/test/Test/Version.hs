{-# LANGUAGE ScopedTypeVariables #-}

-- | Test `NodeToNodeVersion` and `NodeToClientVersion` codecs.
--
module Test.Version (tests) where

import           Data.Proxy (Proxy (..))
import           Codec.Serialise (Serialise,
                                  DeserialiseFailure,
                                  serialise,
                                  deserialiseOrFail)

import           Ouroboros.Network.NodeToClient (NodeToClientVersion (..))
import           Ouroboros.Network.NodeToNode   (NodeToNodeVersion)

import           Test.Tasty (TestTree,
                             testGroup)
import           Test.Tasty.HUnit


tests :: TestTree
tests =
  testGroup "Ouroboros.Network.Protocol.Handshake.Version"
    [ testGroup "NodeToClientVersion"
      [ testCase "NodeToClientVersion round-trip codec property"
                 (roundTripPropAll (Proxy :: Proxy NodeToClientVersion))
      , testCase "NodeToClientVersion should not deserialise as NodeToNode"
                 (crossFailurePropAll
                   (Proxy :: Proxy NodeToNodeVersion)
                   ([NodeToClientV_2 .. maxBound] :: [NodeToClientVersion]))
      ]
    , testGroup "NodeToNodeVersion"
      [ testCase "NodeToNodeVersion round-trip codec property"
                 (roundTripPropAll (Proxy :: Proxy NodeToNodeVersion))
      -- TODO: enable this test when `NodeToClientV_1` is removed:
      {--
        - , testCase "NodeToNodeVersion should not deserialise as NodeToClient"
        -            (crossFailurePropAll
        -              (Proxy :: Proxy NodeToClientVersion)
        -              ([minBound .. maxBound] :: [NodeToNodeVersion]))
        --}
      ]
    ]


roundTripProp :: (Serialise a, Eq a, Show a)
              => a -> Assertion
roundTripProp a =
    Right a @=? deserialiseOrFail (serialise a)


-- Using `Monoid` instance of `IO ()`
roundTripPropAll
    :: forall a.
       ( Serialise a
       , Enum a
       , Bounded a
       , Eq a
       , Show a
       )
    => Proxy a -> Assertion
roundTripPropAll _ =
    foldMap roundTripProp ([minBound..maxBound] :: [a])


crossFailureProp
    :: forall a b.
       ( Serialise a, Show a, 
         Serialise b, Show b
       )
    => Proxy b -> a -> Assertion
crossFailureProp _ a =
    case deserialiseOrFail (serialise a) :: Either DeserialiseFailure b of
      Right (b :: b) -> assertFailure (show a ++ "should not deserialise as " ++ show b)
      Left  _        -> pure ()


crossFailurePropAll
    :: forall a b.
       ( Serialise a, Show a,
         Serialise b, Show b )
    => Proxy b
    -> [a]
    -> Assertion
crossFailurePropAll = foldMap . crossFailureProp

