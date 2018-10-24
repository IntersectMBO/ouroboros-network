{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeOperators  #-}

module Test.ChainProducerState (tests) where

import           Data.List (unfoldr)

import           Test.QuickCheck
import           Test.Tasty
import           Test.Tasty.QuickCheck

import           Chain (Block, Chain, ChainUpdate (..), Point (..),
                     genesisPoint, headPoint, pointOnChain)
import qualified Chain
import           ChainProducerState
import           Ouroboros

import           Test.Chain (TestBlockChain (..), TestBlockChainAndUpdates (..),
                     TestChainFork (..), mkRollbackPoint)
import           Test.DepFn
import           Test.Ouroboros

tests :: TestTree
tests =
  testGroup "ChainProducerState"
  [ testGroup "Test Arbitrary instances"
    [ testProperty "ChainProducerStateForkTest's generator"
                   prop_arbitrary_ChainProducerStateForkTest
    , testProperty "ChainProducerStateForkTest's shrinker"
                   (withMaxSuccess 25 prop_shrink_ChainProducerStateForkTest)
    ]
  , testProperty "check initial reader state" prop_init_lookup
  , testProperty "check second reader state"  prop_init_next_lookup
  , testProperty "check reader state after updateReader" prop_update_lookup
  , testProperty "check reader state after updateReader2" prop_update_next_lookup
  , testProperty "producer syncronise (1)" prop_producer_sync1
  , testProperty "producer syncronise (2)" prop_producer_sync2
  , testProperty "switch fork" prop_switchFork
  ]


--
-- Properties
--

-- | Check that readers start in the expected state, at the right point and
-- in the rollback state.
--
prop_init_lookup :: ChainProducerStateTest :-> Bool
prop_init_lookup = simpleProp $ \_ (ChainProducerStateTest c _ p) ->
    let (c', rid) = initReader p c in
    lookupReader c' rid == ReaderState p ReaderBackTo rid

-- | As above but check that when we move the reader on by one, from the
-- rollback state, they stay at the same point but are now in the forward state.
--
prop_init_next_lookup :: ChainProducerStateTest :-> Bool
prop_init_next_lookup = simpleProp $ \_ (ChainProducerStateTest c _ p) ->
    let (c', rid)     = initReader p c
        Just (u, c'') = readerInstruction rid c'
    in u == RollBack p
    && lookupReader c'' rid == ReaderState p ReaderForwardFrom rid

-- | Check that after moving the reader point that the reader is in the
-- expected state, at the right point and in the rollback state.
--
prop_update_lookup :: ChainProducerStateTest :-> Bool
prop_update_lookup = simpleProp $ \_ (ChainProducerStateTest c rid p) ->
    let c' = updateReader rid p c in
    lookupReader c' rid == ReaderState p ReaderBackTo rid

-- | As above but check that when we move the reader on by one, from the
-- rollback state, they stay at the same point but are now in the forward state.
--
prop_update_next_lookup :: ChainProducerStateTest :-> Bool
prop_update_next_lookup = simpleProp $ \_ (ChainProducerStateTest c rid p) ->
    let c'            = updateReader rid p c
        Just (u, c'') = readerInstruction rid c'
    in u == RollBack p
    && lookupReader c'' rid == ReaderState p ReaderForwardFrom rid

-- | This says that if we take a chain producer and apply a bunch of updates
-- and initialise a consumer to the producer's initial chain, then by
-- applying update instructions from the producer to the consumer then the
-- consumer ends up in the same final state.
--
-- The limitation of this test is that it applies all the updates to the
-- producer first and then syncronises without changing the producer.
--
prop_producer_sync1 :: TestBlockChainAndUpdates :-> Bool
prop_producer_sync1 = simpleProp $ \_ (TestBlockChainAndUpdates c us) ->
    let producer0        = initChainProducerState c
        (producer1, rid) = initReader (Chain.headPoint c) producer0
        Just producer    = applyChainUpdates us producer1

        consumer0        = c
        consumerUpdates  = iterateReaderUntilDone rid producer
        Just consumer    = Chain.applyChainUpdates consumerUpdates consumer0
     in
        consumer == producerChain producer
  where
    iterateReaderUntilDone rid = unfoldr (readerInstruction rid)

-- | A variation on 'prop_producer_sync1' where we take an arbitrary
-- interleaving of applying changes to the producer and doing syncronisation
-- steps between the producer and consumer.
--
prop_producer_sync2 :: TestBlockChainAndUpdates :-> ([Bool] -> Bool)
prop_producer_sync2 = simpleProp $ \_ (TestBlockChainAndUpdates chain0 us0) choices ->
    let producer0        = initChainProducerState chain0
        (producer1, rid) = initReader (Chain.headPoint chain0) producer0

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
      case readerInstruction rid p of
        Nothing      -> go rid p  c  bs us
        Just (u, p') -> go rid p' c' bs us
          where Just c' = Chain.applyChainUpdate u c

    -- producer is not changing, just run consumer
    go rid p c [] _ =
      case readerInstruction rid p of
        Nothing      -> (p, c)
        Just (u, p') -> go rid p' c' [] []
          where Just c' = Chain.applyChainUpdate u c

prop_switchFork :: ChainProducerStateForkTest :-> Bool
prop_switchFork = simpleProp $ \_ (ChainProducerStateForkTest cps f) ->
  let cps' = switchFork f cps
  in
      invChainProducerState cps'
      && all
        (uncurry readerInv)
        (zip (chainReaders cps) (chainReaders cps'))
  where
    readerInv :: ReaderState -> ReaderState -> Bool
    readerInv r r'
      -- the order of readers has not changed
       = readerId r == readerId r'
      -- points only move backward
      && pointSlot (readerPoint r') <= pointSlot (readerPoint r)
      -- if reader's point moves back, `readerNext` is changed to `ReaderBackTo`
      && ((pointSlot (readerPoint r') < pointSlot (readerPoint r)) `implies` (readerNext r' == ReaderBackTo))
      -- if reader's point is not changed, also next instruction is not changed
      && ((pointSlot (readerPoint r') == pointSlot (readerPoint r)) `implies` (readerNext r' == readerNext r))

    implies :: Bool -> Bool -> Bool
    implies a b = not a || b


--
-- Generators
--

data ChainProducerStateTest p
    = ChainProducerStateTest (ChainProducerState (Block p)) ReaderId Point
  deriving Show

instance SingShow ChainProducerStateTest where
  singShow s = singKnownOuroborosProtocol s $ show

genReaderState :: Int   -- ^ length of the chain
               -> Chain (Block p)
               -> Gen ReaderState
genReaderState n c = do
    readerPoint <- frequency
      [ (2, return (headPoint c))
      , (2, return (mkRollbackPoint c n))
      , (8, mkRollbackPoint c <$> choose (1, fromIntegral n - 1))
      ]
    readerNext <- oneof
      [ return ReaderForwardFrom
      , return ReaderBackTo
      ]
    readerId <- arbitrary
    return $ ReaderState{readerPoint, readerNext, readerId}

fixupReaderStates :: [ReaderState] -> [ReaderState]
fixupReaderStates = go 0
  where
  go _ []       = []
  go n (r : rs) = r { readerId = n } : go (n + 1) rs

instance SingArbitrary ChainProducerStateTest where
  singArbitrary protocol = do
    TestBlockChain c <- singArbitrary protocol
    let n = Chain.length c
    rs <- fixupReaderStates <$> listOf1 (genReaderState n c)
    rid <- choose (0, length rs - 1)
    p <- if n == 0
         then return genesisPoint
         else mkRollbackPoint c <$> choose (0, n)
    return (ChainProducerStateTest (ChainProducerState c rs) rid p)

data ChainProducerStateForkTest p
    = ChainProducerStateForkTest (ChainProducerState (Block p)) (Chain (Block p))
  deriving Show

instance SingShow ChainProducerStateForkTest where
  singShow s = singKnownOuroborosProtocol s $ show

instance SingArbitrary ChainProducerStateForkTest where
  singArbitrary p = do
    TestChainFork _ c f <- singArbitrary p
    let l = Chain.length c
    rs <- fixupReaderStates <$> listOf (genReaderState l c)
    return $ ChainProducerStateForkTest (ChainProducerState c rs) f

  singShrink p (ChainProducerStateForkTest (ChainProducerState c rs) f)
    -- shrink readers
     = [ ChainProducerStateForkTest (ChainProducerState c rs') f
       | rs' <- shrinkList (const []) rs
       ]
    -- shrink the fork chain
    ++ [ ChainProducerStateForkTest (ChainProducerState c rs) f'
       | TestBlockChain f' <- singShrink p (TestBlockChain f)
       ]
    -- shrink chain and fix up readers
    ++ [ ChainProducerStateForkTest (ChainProducerState c' (fixupReaderPointer c' `map` rs)) f
       | TestBlockChain c' <- singShrink p (TestBlockChain c)
       ]
    where
      fixupReaderPointer :: Chain (Block p) -> ReaderState -> ReaderState
      fixupReaderPointer c' r@ReaderState{readerPoint} =
        if pointOnChain readerPoint c'
          then r
          else r { readerPoint = headPoint c' }

prop_arbitrary_ChainProducerStateForkTest :: ChainProducerStateForkTest :-> Bool
prop_arbitrary_ChainProducerStateForkTest = simpleProp $ \_ (ChainProducerStateForkTest c f) ->
    invChainProducerState c && Chain.valid f

prop_shrink_ChainProducerStateForkTest :: ChainProducerStateForkTest :-> Bool
prop_shrink_ChainProducerStateForkTest = simpleProp $ \p c ->
    and [ invChainProducerState c' && Chain.valid f
        | ChainProducerStateForkTest c' f <- singShrink p c
        ]
