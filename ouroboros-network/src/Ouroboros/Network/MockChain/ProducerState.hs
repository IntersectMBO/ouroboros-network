{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

module Ouroboros.Network.MockChain.ProducerState where

import           Ouroboros.Network.Block (castPoint, genesisPoint, pointSlot)
import           Ouroboros.Network.MockChain.Chain (Chain, ChainUpdate (..), HasHeader,
                     HeaderHash, Point (..), blockPoint, pointOnChain)
import qualified Ouroboros.Network.MockChain.Chain as Chain

import           Control.Exception (assert)
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Maybe (fromMaybe)



-- A @'ChainState'@ plus an associated set of readers/consumers of the chain.

data ChainProducerState block = ChainProducerState {
       chainState   :: Chain block,
       chainReaders :: ReaderStates block,
       nextReaderId :: ReaderId
     }
  deriving (Eq, Show)

-- | Readers are represented here as a relation.
--
type ReaderStates block = Map ReaderId (ReaderState block)

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
-- In this implementation a map from @'ReaderId'@ to @'ReaderState'@ is shared
-- between all producers running on a single node; hence the unique identifier
-- @'ReaderId'@ for each reader: this is an implementation detail.
data ReaderState block = ReaderState {
       -- | Where the chain of the consumer and producer intersect. If the
       -- consumer is on the chain then this is the consumer's chain head,
       -- but if the consumer's chain is off the producer's chain then this is
       -- the point the consumer will need to rollback to.
       readerPoint :: Point block,

       -- | Where the will go next, roll back to the reader point, or roll
       -- forward from the reader point.
       readerNext  :: ReaderNext
     }
  deriving (Eq, Show)

data ReaderNext = ReaderBackTo | ReaderForwardFrom
  deriving (Eq, Show)

--
-- Invariant
--

invChainProducerState :: HasHeader block => ChainProducerState block -> Bool
invChainProducerState (ChainProducerState c rs nrid) =
    Chain.valid c
 && invReaderStates c rs
 && all (< nrid) (Map.keys rs)

invReaderStates :: HasHeader block => Chain block -> ReaderStates block -> Bool
invReaderStates c rs =
    and [ pointOnChain readerPoint c | ReaderState{readerPoint} <- Map.elems rs ]

--
-- Operations
--


-- | Initialise @'ChainProducerState'@ with a given @'Chain'@ and empty list of
-- readers.
--
initChainProducerState :: Chain block -> ChainProducerState block
initChainProducerState c = ChainProducerState c Map.empty 0

-- | Get the recorded state of a chain consumer. The 'ReaderId' is assumed to
-- exist.
--
lookupReader :: ChainProducerState block -> ReaderId -> ReaderState block
lookupReader (ChainProducerState _ rs _) rid = rs Map.! rid

-- | Return 'True' when a reader with the given 'ReaderId' exists.
readerExists :: ReaderId -> ChainProducerState block -> Bool
readerExists rid (ChainProducerState _ rs _) = rid `Map.member` rs

-- | Extract @'Chain'@ from @'ChainProducerState'@.
--
producerChain :: ChainProducerState block -> Chain block
producerChain (ChainProducerState c _ _) = c

findFirstPoint :: HasHeader block
               => [Point block]
               -> ChainProducerState block
               -> Maybe (Point block)
findFirstPoint ps = Chain.findFirstPoint ps . producerChain


-- | Add a new reader with the given intersection point and return the new
-- 'ReaderId'.
--
initReader :: HasHeader block
           => Point block
           -> ChainProducerState block
           -> (ChainProducerState block, ReaderId)
initReader point (ChainProducerState c rs nrid) =
    assert (pointOnChain point c) $
    (ChainProducerState c (Map.insert nrid r rs) (succ nrid), nrid)
  where
    r = ReaderState {
          readerPoint = point,
          readerNext  = ReaderBackTo
        }


-- | Delete an existing reader. The 'ReaderId' is assumed to exist.
--
deleteReader :: ReaderId -> ChainProducerState block -> ChainProducerState block
deleteReader rid (ChainProducerState c rs nrid) =
    assert (rid `Map.member` rs) $
    ChainProducerState c (Map.delete rid rs) nrid


-- | Change the intersection point of a reader. This also puts it into
-- the 'ReaderBackTo' state.
--
updateReader :: HasHeader block
             => ReaderId
             -> Point block    -- ^ new reader intersection point
             -> ChainProducerState block
             -> ChainProducerState block
updateReader rid point (ChainProducerState c rs nrid) =
    assert (pointOnChain point c) $
    ChainProducerState c (Map.adjust update rid rs) nrid
  where
    update r = r { readerPoint = point, readerNext  = ReaderBackTo }

-- | Switch chains and update readers; if a reader point falls out of the chain,
-- replace it with the intersection of both chains and put it in the
-- `ReaderBackTo` state, otherwise preserve reader state.
--
switchFork :: HasHeader block
           => Chain block
           -> ChainProducerState block
           -> ChainProducerState block
switchFork c (ChainProducerState c' rs nrid) =
    ChainProducerState c (update <$> rs) nrid
  where
    ipoint = fromMaybe genesisPoint $ Chain.intersectChains c c'

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
                  -> Maybe (ChainUpdate block block, ChainProducerState block)
readerInstruction rid cps@(ChainProducerState c rs nrid) =
    let ReaderState {readerPoint, readerNext} = lookupReader cps rid in
    case readerNext of
      ReaderForwardFrom ->
          assert (pointOnChain readerPoint c) $
          case Chain.successorBlock readerPoint c of
            -- There is no successor block because the reader is at the head
            Nothing -> Nothing

            Just b -> Just (AddBlock b, cps')
              where
                cps' = ChainProducerState c (Map.adjust setPoint rid rs) nrid
                setPoint r = r { readerPoint = blockPoint b }

      ReaderBackTo -> Just (RollBack readerPoint, cps')
        where
          cps' = ChainProducerState c (Map.adjust setForwardFrom rid rs) nrid
          setForwardFrom r = r { readerNext = ReaderForwardFrom }


-- | Add a block to the chain. It does not require any reader's state changes.
--
addBlock :: HasHeader block
         => block
         -> ChainProducerState block
         -> ChainProducerState block
addBlock b (ChainProducerState c rs nrid) =
    ChainProducerState (Chain.addBlock b c) rs nrid


-- | Rollback producer chain. It requires to update reader states, since some
-- @'readerPoint'@s may not be on the new chain; in this case find intersection
-- of the two chains and set @'readerNext'@ to @'ReaderBackTo'@.
rollback :: (HasHeader block, HeaderHash block ~ HeaderHash block')
         => Point block'
         -> ChainProducerState block
         -> Maybe (ChainProducerState block)
rollback p (ChainProducerState c rs nrid) = do
    c' <- Chain.rollback (castPoint p) c
    return $ ChainProducerState c' (rollbackReader <$> rs) nrid
  where
    rollbackReader r@ReaderState { readerPoint = p' }
      | pointSlot p' > pointSlot p
      = r { readerPoint = (castPoint p), readerNext = ReaderBackTo }
      | otherwise
      = r

-- | Convenient function which combines both @'addBlock'@ and @'rollback'@.
--
applyChainUpdate :: (HasHeader block, HeaderHash block ~ HeaderHash block')
                 => ChainUpdate block' block
                 -> ChainProducerState block
                 -> Maybe (ChainProducerState block)
applyChainUpdate (AddBlock b) c = Just (addBlock b c)
applyChainUpdate (RollBack p) c =       rollback p c


-- | Apply a list of @'ChainUpdate'@s.
--
applyChainUpdates :: (HasHeader block, HeaderHash block ~ HeaderHash block')
                  => [ChainUpdate block' block]
                  -> ChainProducerState block
                  -> Maybe (ChainProducerState block)
applyChainUpdates []     c = Just c
applyChainUpdates (u:us) c = applyChainUpdates us =<< applyChainUpdate u c
