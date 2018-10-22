{-# OPTIONS_GHC -Wno-orphans #-}
module Test.Pipe (tests) where

import           Block (Block, HeaderHash (..), Slot (..))
import           Chain (Chain (..), Point (..))
import           Pipe (demo2)
import           Protocol
import           Serialise (prop_serialise)

import           Test.Chain (TestBlockChainAndUpdates (..), genBlockChain)

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

prop_pipe_demo :: TestBlockChainAndUpdates p -> Property
prop_pipe_demo (TestBlockChainAndUpdates chain updates) =
    ioProperty $ demo2 chain updates

prop_serialise_MsgConsumer :: MsgConsumer -> Bool
prop_serialise_MsgConsumer = prop_serialise

prop_serialise_MsgProducer :: MsgProducer (Block p) -> Bool
prop_serialise_MsgProducer = prop_serialise

instance Arbitrary MsgConsumer where
  arbitrary = oneof [ pure MsgRequestNext
                    , MsgSetHead <$> arbitrary
                    ]

instance Arbitrary block => Arbitrary (MsgProducer block) where
  arbitrary = oneof [ MsgRollBackward <$> arbitrary
                    , MsgRollForward  <$> arbitrary
                    , pure MsgAwaitReply
                    , MsgIntersectImproved <$> arbitrary <*> arbitrary
                    , pure MsgIntersectUnchanged
                    ]

instance Arbitrary Point where
  arbitrary = Point <$> (Slot <$> arbitraryBoundedIntegral)
                    <*> (HeaderHash <$> arbitraryBoundedIntegral)

instance Arbitrary (Block p) where
  arbitrary = do _ :> b <- genBlockChain 1
                 return b
