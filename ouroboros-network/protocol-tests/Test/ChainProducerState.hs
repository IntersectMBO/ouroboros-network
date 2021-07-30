{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeFamilies   #-}
{-# LANGUAGE TypeOperators  #-}

module Test.ChainProducerState
  ( ChainProducerStateTest (..)
  , ChainProducerStateForkTest (..)
  , tests
  )
 where

import           Data.List (unfoldr)
import qualified Data.Map as Map

import           Test.QuickCheck
import           Test.Tasty
import           Test.Tasty.QuickCheck

import           Ouroboros.Network.Block (HasHeader, genesisPoint, pointSlot)
import           Ouroboros.Network.MockChain.Chain (Chain, ChainUpdate (..),
                     Point (..), headPoint, pointOnChain)
import qualified Ouroboros.Network.MockChain.Chain as Chain
import           Ouroboros.Network.MockChain.ProducerState
import           Ouroboros.Network.Testing.ConcreteBlock (Block)

import           Test.ChainGenerators (TestBlockChain (..),
                     TestBlockChainAndUpdates (..), TestChainFork (..),
                     mkRollbackPoint)

tests :: TestTree
tests =
  testGroup "ChainProducerState"
  [ testGroup "Test Arbitrary instances"
    [ testProperty "ChainProducerStateForkTest's generator"
                   prop_arbitrary_ChainProducerStateForkTest
    , testProperty "ChainProducerStateForkTest's shrinker"
                   (withMaxSuccess 25 prop_shrink_ChainProducerStateForkTest)
    ]
  , testProperty "check initial follower state" prop_init_lookup
  , testProperty "check second follower state"  prop_init_next_lookup
  , testProperty "check follower state after updateFollower" prop_update_lookup
  , testProperty "check follower state after updateFollower2" prop_update_next_lookup
  , testProperty "producer syncronise (1)" prop_producer_sync1
  , testProperty "producer syncronise (2)" prop_producer_sync2
  , testProperty "switch fork" prop_switchFork
  ]

--
-- Properties
--

-- | Check that followers start in the expected state, at the right point and
-- in the rollback state.
--
prop_init_lookup :: ChainProducerStateTest -> Bool
prop_init_lookup (ChainProducerStateTest c _ p) =
    let (c', rid) = initFollower p c in
    lookupFollower c' rid == FollowerState p FollowerBackTo

-- | As above but check that when we move the follower on by one, from the
-- rollback state, they stay at the same point but are now in the forward state.
--
prop_init_next_lookup :: ChainProducerStateTest -> Bool
prop_init_next_lookup (ChainProducerStateTest c _ p) =
    let (c', rid)     = initFollower p c
        Just (u, c'') = followerInstruction rid c'
    in u == RollBack p
    && lookupFollower c'' rid == FollowerState p FollowerForwardFrom

-- | Check that after moving the follower point that the follower is in the
-- expected state, at the right point and in the rollback state.
--
prop_update_lookup :: ChainProducerStateTest -> Bool
prop_update_lookup (ChainProducerStateTest c rid p) =
    let c' = updateFollower rid p c in
    lookupFollower c' rid == FollowerState p FollowerBackTo

-- | As above but check that when we move the follower on by one, from the
-- rollback state, they stay at the same point but are now in the forward state.
--
prop_update_next_lookup :: ChainProducerStateTest -> Bool
prop_update_next_lookup (ChainProducerStateTest c rid p) =
    let c'            = updateFollower rid p c
        Just (u, c'') = followerInstruction rid c'
    in u == RollBack p
    && lookupFollower c'' rid == FollowerState p FollowerForwardFrom

-- | This says that if we take a chain producer and apply a bunch of updates
-- and initialise a consumer to the producer's initial chain, then by
-- applying update instructions from the producer to the consumer then the
-- consumer ends up in the same final state.
--
-- The limitation of this test is that it applies all the updates to the
-- producer first and then syncronises without changing the producer.
--
prop_producer_sync1 :: TestBlockChainAndUpdates -> Bool
prop_producer_sync1 (TestBlockChainAndUpdates c us) =
    let producer0        = initChainProducerState c
        (producer1, rid) = initFollower (Chain.headPoint c) producer0
        Just producer    = applyChainUpdates us producer1

        consumer0        = c
        consumerUpdates  = iterateFollowerUntilDone rid producer
        Just consumer    = Chain.applyChainUpdates consumerUpdates consumer0
     in
        consumer == producerChain producer
  where
    iterateFollowerUntilDone rid = unfoldr (followerInstruction rid)

-- | A variation on 'prop_producer_sync1' where we take an arbitrary
-- interleaving of applying changes to the producer and doing syncronisation
-- steps between the producer and consumer.
--
prop_producer_sync2 :: TestBlockChainAndUpdates -> [Bool] -> Bool
prop_producer_sync2 (TestBlockChainAndUpdates chain0 us0) choices =
    let producer0        = initChainProducerState chain0
        (producer1, rid) = initFollower (Chain.headPoint chain0) producer0

        consumer0        = chain0
        (producer,
         consumer)       = go rid producer1 consumer0 choices us0
     in consumer == producerChain producer
  where
    -- apply update to producer
    go rid p c (False:bs) (u:us) =
      let Just p' = applyChainUpdate u p
       in go rid p' c bs us

    -- all producer updates are done
    go rid p c (False:_bs) [] = go rid p c [] []

    -- apply update to consumer
    go rid p c (True:bs) us =
      case followerInstruction rid p of
        Nothing      -> go rid p  c  bs us
        Just (u, p') -> go rid p' c' bs us
          where Just c' = Chain.applyChainUpdate u c

    -- producer is not changing, just run consumer
    go rid p c [] _ =
      case followerInstruction rid p of
        Nothing      -> (p, c)
        Just (u, p') -> go rid p' c' [] []
          where Just c' = Chain.applyChainUpdate u c

prop_switchFork :: ChainProducerStateForkTest -> Bool
prop_switchFork (ChainProducerStateForkTest cps f) =
  let cps' = switchFork f cps
  in
      invChainProducerState cps'
      && all
        (uncurry followerInv)
        (zip (followerStates cps) (followerStates cps'))
  where
    followerInv :: HasHeader block
                => FollowerState block -> FollowerState block -> Bool
    followerInv fs fs'
      -- points only move backward
       = pointSlot (followerPoint fs') <= pointSlot (followerPoint fs)
      -- if follower's point moves back, `followerNext` is changed to `FollowerBackTo`
      && ((pointSlot (followerPoint fs') < pointSlot (followerPoint fs)) `implies` (followerNext fs' == FollowerBackTo))
      -- if follower's point is not changed, also next instruction is not changed
      && ((followerPoint fs' == followerPoint fs) `implies` (followerNext fs' == followerNext fs))

    implies :: Bool -> Bool -> Bool
    implies a b = not a || b

    followerStates :: ChainProducerState block -> [FollowerState block]
    followerStates = map snd . Map.toAscList . chainFollowers

--
-- Generators
--

data ChainProducerStateTest
    = ChainProducerStateTest
        (ChainProducerState Block) -- ^ producer state with a single follower
        FollowerId                 -- ^ follower's id
        (Point Block)              -- ^ intersection point of the follower
  deriving Show

genFollowerState :: Int   -- ^ length of the chain
                 -> Chain Block
                 -> Gen (FollowerState Block)
genFollowerState n c = do
    followerPoint <- frequency
      [ (2, return (headPoint c))
      , (2, return (mkRollbackPoint c n))
      , (8, mkRollbackPoint c <$> choose (1, fromIntegral n - 1))
      ]
    followerNext <- oneof
      [ return FollowerForwardFrom
      , return FollowerBackTo
      ]
    return $ FollowerState{followerPoint, followerNext}

instance Arbitrary ChainProducerStateTest where
  arbitrary = do
    TestBlockChain c <- arbitrary
    let n = Chain.length c
    rs <- Map.fromList . zip [0..] <$> listOf1 (genFollowerState n c)
    rid <- choose (0, length rs - 1)
    p <- if n == 0
         then return genesisPoint
         else mkRollbackPoint c <$> choose (0, n)
    return (ChainProducerStateTest (ChainProducerState c rs (length rs)) rid p)

data ChainProducerStateForkTest
    = ChainProducerStateForkTest
        (ChainProducerState Block) -- ^ chain producer state
        (Chain Block)              -- ^ fork of the producer's chain
  deriving Show

instance Arbitrary ChainProducerStateForkTest where
  arbitrary = do
    TestChainFork _ c f <- arbitrary
    let l = Chain.length c
    rs <- Map.fromList . zip [0..] <$> listOf (genFollowerState l c)
    return $ ChainProducerStateForkTest (ChainProducerState c rs (length rs)) f

  shrink (ChainProducerStateForkTest (ChainProducerState c rs nr) f)
    -- shrink followers
     = [ ChainProducerStateForkTest (ChainProducerState c rs' nr) f
       | rs' <- map Map.fromList . shrinkList (const []) . Map.toList $ rs
       ]
    -- shrink the fork chain
    ++ [ ChainProducerStateForkTest (ChainProducerState c rs nr) f'
       | TestBlockChain f' <- shrink (TestBlockChain f)
       ]
    -- shrink chain and fix up followers
    ++ [ ChainProducerStateForkTest (ChainProducerState c' (fixupFollowerPointer c' <$> rs) nr) f
       | TestBlockChain c' <- shrink (TestBlockChain c)
       ]
    where
      fixupFollowerPointer :: Chain Block -> FollowerState Block -> FollowerState Block
      fixupFollowerPointer c' fs@FollowerState{followerPoint} =
        if pointOnChain followerPoint c'
          then fs
          else fs { followerPoint = headPoint c' }

prop_arbitrary_ChainProducerStateForkTest :: ChainProducerStateForkTest -> Bool
prop_arbitrary_ChainProducerStateForkTest (ChainProducerStateForkTest c f) =
    invChainProducerState c && Chain.valid f

prop_shrink_ChainProducerStateForkTest :: ChainProducerStateForkTest -> Bool
prop_shrink_ChainProducerStateForkTest c =
    and [ invChainProducerState c' && Chain.valid f
        | ChainProducerStateForkTest c' f <- shrink c
        ]
