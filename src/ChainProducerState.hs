{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE TemplateHaskell #-}

module ChainProducerState where

import           Block (Block, BlockHeader, HasHeader)
import           Chain ( Chain, Point(..), blockPoint, ChainUpdate(..)
                       , genesisPoint, headPoint , pointOnChain
                       , TestBlockChainAndUpdates(..), TestBlockChain(..)
                       , TestChainFork(..), mkRollbackPoint
                       , genBlockChain, genPoint)
import qualified Chain

import           Data.List (sort, group, find, unfoldr)
import           Data.Maybe (fromMaybe)
import           Control.Exception (assert)

import           Test.QuickCheck



-- A 'ChainState' plus an associated set of readers/consumers of the chain.

data ChainProducerState block = ChainProducerState {
       chainState   :: Chain block,
       chainReaders :: ReaderStates
     }
  deriving (Eq, Show)

-- | Readers are represented here as a relation.
--
type ReaderStates = [ReaderState]

type ReaderId     = Int
data ReaderState  = ReaderState {
       -- | Where the chain of the consumer and producer intersect. If the
       -- consumer is on the chain then this is the consumer's chain head,
       -- but if the consumer's chain is off the producer's chain then this is
       -- the point the consumer will need to rollback to.
       readerPoint :: Point,

       -- | Where the will go next, roll back to the reader point, or roll
       -- forward from the reader point.
       readerNext  :: ReaderNext,

       -- | A unique tag per reader, to distinguish different readers.
       readerId    :: ReaderId
     }
  deriving (Eq, Show)

data ReaderNext = ReaderBackTo | ReaderForwardFrom
  deriving (Eq, Show)

--
-- Invariant
--

invChainProducerState :: HasHeader block => ChainProducerState block -> Bool
invChainProducerState (ChainProducerState c rs) =
    Chain.valid c
 && invReaderStates c rs

invReaderStates :: HasHeader block => Chain block -> ReaderStates -> Bool
invReaderStates c rs =
    and [ pointOnChain readerPoint c | ReaderState{readerPoint} <- rs ]
 && noDuplicates [ readerId | ReaderState{readerId} <- rs ]

noDuplicates :: Ord a => [a] -> Bool
noDuplicates = all ((== 1) . length) . group . sort


--
-- Operations
--


initChainProducerState :: Chain block -> ChainProducerState block
initChainProducerState c = ChainProducerState c []


-- | Get the recorded state of a chain consumer. The 'ReaderId' is assumed to
-- exist.
--
lookupReader :: ChainProducerState block -> ReaderId -> ReaderState
lookupReader (ChainProducerState _ rs) rid =
    assert (rid `elem` map readerId rs) $
    st
  where
    Just st = find (\r -> readerId r == rid) rs


--producerReaders

producerChain (ChainProducerState c _) = c


-- | Add a new reader with the given intersection point and return the new
-- 'ReaderId'.
initReader :: HasHeader block
           => Point
           -> ChainProducerState block
           -> (ChainProducerState block, ReaderId)
initReader point (ChainProducerState c rs) =
    assert (pointOnChain point c) $
    (ChainProducerState c (r:rs), readerId r)
  where
    r = ReaderState {
          readerPoint = point,
          readerNext  = ReaderBackTo,
          readerId    = freshReaderId rs
        }


-- | Delete an existing reader. The 'ReaderId' is assumed to exist.
--
deleteReader :: ReaderId -> ChainProducerState block -> ChainProducerState block
deleteReader rid (ChainProducerState c rs) =
    assert (rid `elem` map readerId rs) $
    ChainProducerState c [ r | r <- rs, readerId r /= rid ]


-- | Change the intersection point of a reader. This also puts it into
-- the 'ReaderBackTo' state.
--
updateReader :: HasHeader block
             => ReaderId
             -> Point     -- ^ new reader intersection point
             -> ChainProducerState block
             -> ChainProducerState block
updateReader rid point (ChainProducerState c rs) =
    assert (pointOnChain point c) $
    ChainProducerState c (map update rs)
  where
    update r | readerId r == rid = r { readerPoint = point,
                                       readerNext  = ReaderBackTo }
             | otherwise         = r


-- | Switch chains and update readers; if a reader point falls out of the chain,
-- replace it with the intersection of both chains and put it in the
-- `ReaderBackTo` state, otherwise preserve reader state.
switchFork :: HasHeader block
           => Chain block
           -> ChainProducerState block
           -> ChainProducerState block
switchFork c (ChainProducerState c' rs) =
    ChainProducerState c (map update rs)
  where
    ipoint = fromMaybe genesisPoint $ Chain.intersectChains c c'

    update r@ReaderState{readerPoint} =
      if pointOnChain readerPoint c
        then r
        else r { readerPoint = ipoint, readerNext = ReaderBackTo }
          

-- | What a reader needs to do next. Should they move on to the next block or
-- do they need to roll back to a previous point on their chain. Also update
-- the producer state assuming that the reader follows the instruction.
--
readerInstruction :: HasHeader block
                  => ReaderId
                  -> ChainProducerState block
                  -> Maybe (ChainUpdate block, ChainProducerState block)
readerInstruction rid cps@(ChainProducerState c rs) =
    let ReaderState {readerPoint, readerNext} = lookupReader cps rid in
    case readerNext of
      ReaderForwardFrom ->
          assert (pointOnChain readerPoint c) $
          case Chain.successorBlock readerPoint c of
            -- There is no successor block because the reader is at the head
            Nothing -> Nothing

            Just b -> Just (AddBlock b, cps')
              where
                cps' = ChainProducerState c (map setPoint rs)
                setPoint r
                  | readerId r == rid = r { readerPoint = blockPoint b }
                  | otherwise         = r

      ReaderBackTo -> Just (RollBack readerPoint, cps')
        where
          cps' = ChainProducerState c (map setForwardFrom rs)
          setForwardFrom r
            | readerId r == rid = r { readerNext = ReaderForwardFrom }
            | otherwise         = r


-- | Add a block to the chain.
--
addBlock :: HasHeader block
         => block
         -> ChainProducerState block
         -> ChainProducerState block
addBlock b (ChainProducerState c rs) =
    ChainProducerState (Chain.addBlock b c) rs


rollback :: HasHeader block
         => Point
         -> ChainProducerState block
         -> ChainProducerState block
rollback p cps@(ChainProducerState c rs) =
  case Chain.rollback p c of
    Just c' -> ChainProducerState c' rs'
    Nothing -> cps
  where
    rs' = [ if pointSlot p' > pointSlot p
              then r { readerPoint = p, readerNext = ReaderBackTo }
              else r
          | r@ReaderState { readerPoint = p' } <- rs ]


applyChainUpdate :: HasHeader block
                 => ChainUpdate block
                 -> ChainProducerState block
                 -> ChainProducerState block
applyChainUpdate (AddBlock b) c = addBlock b c
applyChainUpdate (RollBack p) c = rollback p c


applyChainUpdates :: HasHeader block
                  => [ChainUpdate block]
                  -> ChainProducerState block
                  -> ChainProducerState block
applyChainUpdates = flip (foldl (flip applyChainUpdate))


--
-- Helpers
--

freshReaderId :: ReaderStates -> ReaderId
freshReaderId [] = 0
freshReaderId rs = 1 + maximum [ readerId | ReaderState{readerId} <- rs ]

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
    NonNegative n <- arbitrary
    c <- genBlockChain n
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

return []
runTests = $quickCheckAll
