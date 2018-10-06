module Test.ChainProducerState (tests) where

import           Data.List (unfoldr)

import Test.QuickCheck
import Test.Tasty
import Test.Tasty.QuickCheck

import           Block (Block, BlockHeader, HasHeader)
import           Chain ( Chain, Point(..), blockPoint, ChainUpdate(..)
                       , genesisPoint, headPoint , pointOnChain )
import qualified Chain
import ChainProducerState

import           Test.Chain
                       ( TestBlockChainAndUpdates(..), TestBlockChain(..)
                       , TestChainFork(..), mkRollbackPoint )

tests :: TestTree
tests =
  testGroup "ChainProducerState"
  [ testProperty "check initial reader state" prop_init_lookup
  , testProperty "check reader state after updateReader" prop_update_lookup
  , testProperty "apply readerInstructions" prop_producer_sync
  , testProperty "swicht fork" prop_switchFork
  , testGroup "Test Arbitrary instances"

    [ testProperty "ChainProducerStateForkTest's generator" prop_arbitrary_ChainProducerStateForkTest
    , testProperty "ChainProducerStateForkTest's shrinker"
      prop_shrink_ChainProducerStateForkTest
    ]
  ]

--
-- Generators
--

data ChainProducerStateTest
    = ChainProducerStateTest (ChainProducerState Block) ReaderId Point
  deriving Show

genReaderState :: Int   -- ^ length of the chain
               -> Chain Block
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
  go _ [] = []
  go n (r : rs) = r { readerId = n } : go (n + 1) rs

instance Arbitrary ChainProducerStateTest where
  arbitrary = do
    TestBlockChain c <- arbitrary
    let n = Chain.length c
    rs <- fixupReaderStates <$> listOf1 (genReaderState n c)
    rid <- choose (0, length rs - 1)
    p <- if n == 0
         then return genesisPoint
         else mkRollbackPoint c <$> choose (0, n)
    return (ChainProducerStateTest (ChainProducerState c rs) rid p)

data ChainProducerStateForkTest
    = ChainProducerStateForkTest (ChainProducerState Block) (Chain Block)
  deriving Show

instance Arbitrary ChainProducerStateForkTest where
  arbitrary = do
    TestChainFork _ c f <- arbitrary
    let l = Chain.length c
    rs <- fixupReaderStates <$> listOf (genReaderState l c)
    return $ ChainProducerStateForkTest (ChainProducerState c rs) f

  shrink (ChainProducerStateForkTest (ChainProducerState c rs) f)
    -- shrink readers
     = [ ChainProducerStateForkTest (ChainProducerState c rs') f
       | rs' <- shrinkList (const []) rs
       ]
    -- shrink the fork chain
    ++ [ ChainProducerStateForkTest (ChainProducerState c rs) f'
       | TestBlockChain f' <- shrink (TestBlockChain f)
       ]
    -- shrink chain and fix up readers
    ++ [ ChainProducerStateForkTest (ChainProducerState c' (fixupReaderPointer c' `map` rs)) f
       | TestBlockChain c' <- shrink (TestBlockChain c)
       ]
    where
      fixupReaderPointer :: Chain Block -> ReaderState -> ReaderState
      fixupReaderPointer c r@ReaderState{readerPoint} = 
        if pointOnChain readerPoint c
          then r
          else r { readerPoint = headPoint c }

--
-- Properties
--

prop_init_lookup :: ChainProducerStateTest -> Bool
prop_init_lookup (ChainProducerStateTest c _ p) =
    let (c', rid) = initReader p c in
    lookupReader c' rid == ReaderState p ReaderBackTo rid

prop_update_lookup :: ChainProducerStateTest -> Bool
prop_update_lookup (ChainProducerStateTest c rid p) =
    let c' = updateReader rid p c in
    lookupReader c' rid == ReaderState p ReaderBackTo rid

prop_producer_sync :: TestBlockChainAndUpdates -> Bool
prop_producer_sync (TestBlockChainAndUpdates c us) =
    let producer0        = initChainProducerState c
        (producer1, rid) = initReader (Chain.headPoint c) producer0
        producer         = applyChainUpdates us producer1

        consumer0        = c
        consumerUpdates  = iterateReaderUntilDone rid producer
        consumer         = Chain.applyChainUpdates consumerUpdates consumer0
     in
        consumer == producerChain producer
  where
    iterateReaderUntilDone rid = unfoldr (readerInstruction rid)

prop_switchFork :: ChainProducerStateForkTest -> Bool
prop_switchFork (ChainProducerStateForkTest cps f) =
  let cps'  = switchFork f cps
      cps'' = switchFork (chainState cps') cps'
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
      && ((pointSlot (readerPoint r') < pointSlot (readerPoint r)) `imply` (readerNext r' == ReaderBackTo))
      -- if reader's point is not changed, also next instruction is not changed
      && ((pointSlot (readerPoint r') == pointSlot (readerPoint r)) `imply` (readerNext r' == readerNext r))

    imply :: Bool -> Bool -> Bool
    imply a b = not a || b

--
-- Generator properties
--

prop_arbitrary_ChainProducerStateForkTest :: ChainProducerStateForkTest -> Bool
prop_arbitrary_ChainProducerStateForkTest (ChainProducerStateForkTest c f)
  = invChainProducerState c && Chain.valid f

prop_shrink_ChainProducerStateForkTest :: ChainProducerStateForkTest -> Property
prop_shrink_ChainProducerStateForkTest c
  = withMaxSuccess 25 $ all id
    [ invChainProducerState c && Chain.valid f
    | ChainProducerStateForkTest c f <- shrink c
    ]
