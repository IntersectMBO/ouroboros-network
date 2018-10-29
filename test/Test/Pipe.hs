{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Test.Pipe (tests) where

import           Block (Block, HeaderHash (..), Slot (..))
import           Chain (Chain (..), Point (..))
import           Ouroboros
import           Pipe (demo2)
import           Protocol
import           Serialise (prop_serialise)

import           Test.Chain (TestBlockChainAndUpdates (..), genBlockChain)
import           Test.DepFn
import           Test.Ouroboros

import           Test.QuickCheck
import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.QuickCheck (testProperty)

--
-- The list of all tests
--

tests :: TestTree
tests =
  testGroup "Pipe"
  [ testProperty "serialise MsgConsumer" prop_serialise_MsgConsumer
  , testProperty "serialise MsgProducer" prop_serialise_MsgProducer
  , testProperty "pipe sync demo"        prop_pipe_demo
  ]


--
-- Properties
--

prop_pipe_demo :: TestBlockChainAndUpdates :-> Property
prop_pipe_demo = simpleProp $ \_ (TestBlockChainAndUpdates chain updates) ->
    ioProperty $ demo2 chain updates

prop_serialise_MsgConsumer :: MsgConsumer -> Bool
prop_serialise_MsgConsumer = prop_serialise

newtype BlockProducer p = BlockProducer {
    blockProducer :: MsgProducer (Block p)
  }
  deriving (Show)

instance SingShow BlockProducer where
  singShow s = singKnownOuroborosProtocol s $ show

prop_serialise_MsgProducer :: BlockProducer :-> Bool
prop_serialise_MsgProducer = simpleProp $ \_ -> prop_serialise . blockProducer

instance Arbitrary MsgConsumer where
  arbitrary = oneof [ pure MsgRequestNext
                    , MsgSetHead <$> arbitrary
                    ]

instance SingArbitrary BlockProducer where
  singArbitrary s = BlockProducer <$>
      oneof [ MsgRollBackward <$> arbitrary
            , MsgRollForward  <$> singArbitrary s
            , pure MsgAwaitReply
            , MsgIntersectImproved <$> arbitrary <*> arbitrary
            , pure MsgIntersectUnchanged
            ]

instance Arbitrary Point where
  arbitrary = Point <$> (Slot <$> arbitraryBoundedIntegral)
                    <*> (HeaderHash <$> arbitraryBoundedIntegral)

instance SingArbitrary Block where
  singArbitrary _ = do
    c <- genBlockChain 1
    case c of
        _ :> b -> return b
        _      -> error "expected chain with exactly one block"
