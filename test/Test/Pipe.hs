{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators     #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Test.Pipe (tests) where

import           Ouroboros.Network.Block
import           Ouroboros.Network.Chain (Chain (..), Point (..))
import           Ouroboros.Network.Pipe (demo2)
import           Ouroboros.Network.Protocol
import           Ouroboros.Network.Serialise (prop_serialise)
import           Ouroboros.Network.Testing.ConcreteBlock (Block,
                     ConcreteHeaderHash (..))

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

prop_pipe_demo :: TestBlockChainAndUpdates -> Property
prop_pipe_demo (TestBlockChainAndUpdates chain updates) =
    ioProperty $ demo2 chain updates

prop_serialise_MsgConsumer :: MsgConsumer Block -> Bool
prop_serialise_MsgConsumer = prop_serialise

newtype BlockProducer = BlockProducer {
    blockProducer :: MsgProducer Block
  }
  deriving (Show)

prop_serialise_MsgProducer :: BlockProducer -> Bool
prop_serialise_MsgProducer = prop_serialise . blockProducer

instance Arbitrary (MsgConsumer Block) where
  arbitrary = oneof [ pure MsgRequestNext
                    , MsgSetHead <$> arbitrary
                    ]

instance Arbitrary BlockProducer where
  arbitrary = BlockProducer <$>
      oneof [ MsgRollBackward <$> arbitrary
            , MsgRollForward  <$> arbitrary
            , pure MsgAwaitReply
            , MsgIntersectImproved <$> arbitrary <*> arbitrary
            , pure MsgIntersectUnchanged
            ]

instance Arbitrary (Point Block) where
  arbitrary = Point <$> (Slot <$> arbitraryBoundedIntegral)
                    <*> (BlockHash . HeaderHash <$> arbitraryBoundedIntegral)

instance Arbitrary Block where
  arbitrary = do
    c <- genBlockChain 1
    case c of
        _ :> b -> return b
        _      -> error "expected chain with exactly one block"
