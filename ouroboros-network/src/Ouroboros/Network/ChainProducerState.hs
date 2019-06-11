{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

module Ouroboros.Network.ChainProducerState where

import           Ouroboros.Network.Chain (Chain, ChainUpdate (..), HasHeader,
                     Point, blockPoint, pointOnChain)
import qualified Ouroboros.Network.Chain as Chain

import           Control.Exception (assert)
import           Data.List (find, group, sort)



-- A @'ChainState'@ plus an associated set of readers/consumers of the chain.

data ChainProducerState block = ChainProducerState {
       chainState   :: Chain block,
       chainReaders :: ReaderStates block
     }

deriving instance (Eq block, Eq (Chain.HeaderHash block)) => Eq (ChainProducerState block)
deriving instance (Show block, Show (Chain.HeaderHash block)) => Show (ChainProducerState block)

-- | Readers are represented here as a relation.
--
type ReaderStates block = [ReaderState block]

type ReaderId     = Int
-- |
-- Producer keeps track of consumer chain.  The only information for a producer
-- to know is
--  * @'readerPoint'@: (some) intersection point of consumer's chain and
--    producer's chain;
--  * @'readerNext'@: information what to do on next instruction: either roll
--    forward from the intersection point or roll back to it.
--
-- The second piece of information is needed to distinguish the following two
-- cases:
--
--   * consumer chain is a subchain of the producer chain
--   * it is a fork.
--
-- Since consumer is following the producer chain, the producer has this
-- information at its end.  If producer updates its chain to use another fork it
-- may happen that the reader pointer is not on the new chain.  In this case the
-- producer will set @'RollBackTo'@ and find intersection of the two chains for
-- @'readerPoint'@.  And upon consumer's request will replay with
-- @'MsgRollBackward' 'readerPoint'@.  After sending this message, the  producer
-- assumes that the the consumer is following the protocol (i.e. will rollback
-- its chain) and will reset the @'readerNext'@ field to @'ReaderForwardFrom'@.
-- The second case: when the @'readerNext'@ is @'ReaderForwardFrom'@, then when
-- sending next instruction the producer will either:
--
--   * take the next block (or header) on its chain imediatelly folowing the
--     @'readerPoint'@, updtate @'readerPoint'@ to the point of the new value
--     and send @'MsgRollForward'@ with the new block (or header).
--   * if there is no block, which means that the consumer side and producer
--     side are synchornized, the producer will send @'MsgAwaitResponse'@ and
--     will wait until its chain is updated: either by a fork or by a new block.
--
-- In this implementation a list of @'ReaderState'@ is shared between all
-- producers running on a single node; hence the unique identifier @'ReaderId'@
-- for each reader: this is an implementation detail.
data ReaderState block = ReaderState {
       -- | Where the chain of the consumer and producer intersect. If the
       -- consumer is on the chain then this is the consumer's chain head,
       -- but if the consumer's chain is off the producer's chain then this is
       -- the point the consumer will need to rollback to.
       readerPoint :: Point block,

       -- | Where the will go next, roll back to the reader point, or roll
       -- forward from the reader point.
       readerNext  :: ReaderNext,

       -- | A unique tag per reader, to distinguish different readers.
       readerId    :: ReaderId
     }

deriving instance (Eq (Chain.HeaderHash block)) => Eq (ReaderState block)
deriving instance (Show (Chain.HeaderHash block)) => Show (ReaderState block)

data ReaderNext = ReaderBackTo | ReaderForwardFrom
  deriving (Eq, Show)

--
-- Invariant
--

invChainProducerState :: HasHeader block => ChainProducerState block -> Bool
invChainProducerState (ChainProducerState c rs) =
    Chain.valid c
 && invReaderStates c rs

invReaderStates :: HasHeader block => Chain block -> ReaderStates block -> Bool
invReaderStates c rs =
    and [ pointOnChain readerPoint c | ReaderState{readerPoint} <- rs ]
 && noDuplicates [ readerId | ReaderState{readerId} <- rs ]

noDuplicates :: Ord a => [a] -> Bool
noDuplicates = all ((== 1) . length) . group . sort


--
-- Operations
--


-- | Initialise @'ChainProducerState'@ with a given @'Chain'@ and empty list of
-- readers.
--
initChainProducerState :: Chain block -> ChainProducerState block
initChainProducerState c = ChainProducerState c []


-- | Get the recorded state of a chain consumer. The 'ReaderId' is assumed to
-- exist.
--
lookupReader :: ChainProducerState block -> ReaderId -> ReaderState block
lookupReader (ChainProducerState _ rs) rid =
    assert (rid `elem` map readerId rs) $
    st
  where
    Just st = find (\r -> readerId r == rid) rs


-- | Extract @'Chain'@ from @'ChainProducerState'@.
--
producerChain :: ChainProducerState block -> Chain block
producerChain (ChainProducerState c _) = c

-- | If none of the poitns are on the chain, you get Origin, the point common
-- to every chain.
findFirstPoint :: HasHeader block
               => [Point block]
               -> ChainProducerState block
               -> Point block
findFirstPoint ps = Chain.findFirstPoint ps . producerChain


-- | Add a new reader with the given intersection point and return the new
-- 'ReaderId'.
--
initReader :: HasHeader block
           => Point block
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
             -> Point block    -- ^ new reader intersection point
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
--
switchFork :: HasHeader block
           => Chain block
           -> ChainProducerState block
           -> ChainProducerState block
switchFork c (ChainProducerState c' rs) =
    ChainProducerState c (map update rs)
  where
    ipoint = Chain.intersectChains c c'

    update r@ReaderState{readerPoint} =
      if pointOnChain readerPoint c
        then r
        else r { readerPoint = ipoint, readerNext = ReaderBackTo }


-- | What a reader needs to do next. Should they move on to the next block or
-- do they need to roll back to a previous point on their chain. It also updates
-- the producer's state assuming that the reader follows its instruction.
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
                  | readerId r == rid = r { readerPoint = Chain.Point (blockPoint b) }
                  | otherwise         = r

      ReaderBackTo -> Just (RollBack readerPoint, cps')
        where
          cps' = ChainProducerState c (map setForwardFrom rs)
          setForwardFrom r
            | readerId r == rid = r { readerNext = ReaderForwardFrom }
            | otherwise         = r


-- | Add a block to the chain. It does not require any reader's state changes.
--
addBlock :: HasHeader block
         => block
         -> ChainProducerState block
         -> ChainProducerState block
addBlock b (ChainProducerState c rs) =
    ChainProducerState (Chain.addBlock b c) rs


-- | Rollback producer chain. It requires to update reader states, since some
-- @'readerPoint'@s may not be on the new chain; in this case find intersection
-- of the two chains and set @'readerNext'@ to @'ReaderBackTo'@.
rollback :: HasHeader block
         => Point block
         -> ChainProducerState block
         -> Maybe (ChainProducerState block)
rollback p (ChainProducerState c rs) =
    ChainProducerState <$> Chain.rollback p c
                       <*> pure rs'
  where
    rs' = [ if betterPoint p' p
              then r { readerPoint = p, readerNext = ReaderBackTo }
              else r
          | r@ReaderState { readerPoint = p' } <- rs ]
    betterPoint Chain.Origin _ = False
    betterPoint _ Chain.Origin = True
    betterPoint (Chain.Point p') (Chain.Point p'') =
      Chain.pointSlot p' > Chain.pointSlot p''

-- | Convenient function which combines both @'addBlock'@ and @'rollback'@.
--
applyChainUpdate :: HasHeader block
                 => ChainUpdate block
                 -> ChainProducerState block
                 -> Maybe (ChainProducerState block)
applyChainUpdate (AddBlock b) c = Just (addBlock b c)
applyChainUpdate (RollBack p) c =       rollback p c


-- | Apply a list of @'ChainUpdate'@s.
--
applyChainUpdates :: HasHeader block
                  => [ChainUpdate block]
                  -> ChainProducerState block
                  -> Maybe (ChainProducerState block)
applyChainUpdates []     c = Just c
applyChainUpdates (u:us) c = applyChainUpdates us =<< applyChainUpdate u c


--
-- Helpers
--

-- | Get a new unique @'ReaderId'@.
--
freshReaderId :: ReaderStates block -> ReaderId
freshReaderId [] = 0
freshReaderId rs = 1 + maximum [ readerId | ReaderState{readerId} <- rs ]
