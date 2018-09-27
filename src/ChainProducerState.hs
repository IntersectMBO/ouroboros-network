{-# LANGUAGE NamedFieldPuns #-}

module ChainProducerState where

import           Block (Block)
import           Chain (Chain, Point(..), blockPoint, ChainUpdate(..), pointOnChain, TestBlockChainAndUpdates(..))
import qualified Chain (valid, headPoint, addBlock, rollback, successorBlock, applyChainUpdates)

import           Data.List (sort, group, find, unfoldr)
import           Control.Exception (assert)

import           Test.QuickCheck



-- A 'ChainState' plus an associated set of readers/consumers of the chain.

data ChainProducerState = ChainProducerState {
       chainState   :: Chain Block,
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

invChainProducerState :: ChainProducerState -> Bool
invChainProducerState (ChainProducerState c rs) =
    Chain.valid c
 && invReaderStates c rs

invReaderStates :: Chain Block -> ReaderStates -> Bool
invReaderStates c rs =
    and [ pointOnChain readerPoint c | ReaderState{readerPoint} <- rs ]
 && noDuplicates [ readerId | ReaderState{readerId} <- rs ]

noDuplicates :: Ord a => [a] -> Bool
noDuplicates = all ((== 1) . length) . group . sort


--
-- Operations
--


initChainProducerState :: Chain Block -> ChainProducerState
initChainProducerState c = ChainProducerState c []


-- | Get the recorded state of a chain consumer. The 'ReaderId' is assumed to
-- exist.
--
lookupReader :: ChainProducerState -> ReaderId -> ReaderState
lookupReader (ChainProducerState _ rs) rid =
    assert (rid `elem` map readerId rs) $
    st
  where
    Just st = find (\r -> readerId r == rid) rs


--producerReaders

producerChain (ChainProducerState c _) = c


-- | Add a new reader with the given intersection point and return the new
-- 'ReaderId'.
initReader :: Point
           -> ChainProducerState
           -> (ChainProducerState, ReaderId)
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
deleteReader :: ReaderId -> ChainProducerState -> ChainProducerState
deleteReader rid (ChainProducerState c rs) =
    assert (rid `elem` map readerId rs) $
    ChainProducerState c [ r | r <- rs, readerId r /= rid ]


-- | Change the intersection point of a reader. This also puts it into
-- the 'ReaderBackTo' state.
--
updateReader :: ReaderId
             -> Point     -- ^ new reader intersection point
             -> ChainProducerState
             -> ChainProducerState
updateReader rid point (ChainProducerState c rs) =
    assert (pointOnChain point c) $
    ChainProducerState c (map update rs)
  where
    update r | readerId r == rid = r { readerPoint = point,
                                       readerNext  = ReaderBackTo }
             | otherwise         = r


-- | What a reader needs to do next. Should they move on to the next block or
-- do they need to roll back to a previous point on their chain. Also update
-- the producer state assuming that the reader follows the instruction.
--
readerInstruction :: ReaderId
                  -> ChainProducerState
                  -> Maybe (ChainUpdate Block, ChainProducerState)
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
addBlock :: Block -> ChainProducerState -> ChainProducerState
addBlock b (ChainProducerState c rs) =
    ChainProducerState (Chain.addBlock b c) rs


rollback :: Point -> ChainProducerState -> ChainProducerState
rollback p cps@(ChainProducerState c rs) =
  case Chain.rollback p c of
    Just c' -> ChainProducerState c' rs'
    Nothing -> cps
  where
    rs' = [ if pointSlot p' > pointSlot p
              then r { readerPoint = p, readerNext = ReaderBackTo }
              else r
          | r@ReaderState { readerPoint = p' } <- rs ]


applyChainUpdate :: ChainUpdate Block -> ChainProducerState -> ChainProducerState
applyChainUpdate (AddBlock b) c = addBlock b c
applyChainUpdate (RollBack p) c = rollback p c


applyChainUpdates :: [ChainUpdate Block] -> ChainProducerState -> ChainProducerState
applyChainUpdates = flip (foldl (flip applyChainUpdate))


--
-- Helpers
--

freshReaderId :: ReaderStates -> ReaderId
freshReaderId [] = 0
freshReaderId rs = 1 + maximum [ readerId | ReaderState{readerId} <- rs ]

--
-- Properties
--


prop_init_lookup c p =
    let (c', rid) = initReader p c in
    lookupReader c' rid == ReaderState p ReaderBackTo rid

prop_update_lookup c rid p =
    let c' = updateReader rid p c in
    lookupReader c' rid == ReaderState p ReaderBackTo rid

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

