{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

module Ouroboros.Network.MockChain.ProducerState where

import           Ouroboros.Network.Block (HasFullHeader, castPoint,
                     genesisPoint)
import           Ouroboros.Network.MockChain.Chain (Chain, ChainUpdate (..),
                     HasHeader, HeaderHash, Point (..), blockPoint,
                     pointOnChain)
import qualified Ouroboros.Network.MockChain.Chain as Chain

import           Control.Exception (assert)
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Maybe (fromMaybe)



-- A @'ChainState'@ plus an associated set of followers/consumers of the chain.

data ChainProducerState block = ChainProducerState {
       chainState     :: Chain block,
       chainFollowers :: FollowerStates block,
       nextFollowerId :: FollowerId
     }
  deriving (Eq, Show)

-- | Followers are represented here as a relation.
--
type FollowerStates block = Map FollowerId (FollowerState block)

type FollowerId = Int
-- |
-- Producer keeps track of consumer chain.  The only information for a producer
-- to know is
--  * @'followerPoint'@: (some) intersection point of consumer's chain and
--    producer's chain;
--  * @'followerNext'@: information what to do on next instruction: either roll
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
-- may happen that the follower pointer is not on the new chain.  In this case the
-- producer will set @'RollBackTo'@ and find intersection of the two chains for
-- @'followerPoint'@.  And upon consumer's request will replay with
-- @'MsgRollBackward' 'followerPoint'@.  After sending this message, the  producer
-- assumes that the the consumer is following the protocol (i.e. will rollback
-- its chain) and will reset the @'followerNext'@ field to @'FollowerForwardFrom'@.
-- The second case: when the @'followerNext'@ is @'FollowerForwardFrom'@, then when
-- sending next instruction the producer will either:
--
--   * take the next block (or header) on its chain imediatelly folowing the
--     @'followerPoint'@, updtate @'followerPoint'@ to the point of the new value
--     and send @'MsgRollForward'@ with the new block (or header).
--   * if there is no block, which means that the consumer side and producer
--     side are synchornized, the producer will send @'MsgAwaitResponse'@ and
--     will wait until its chain is updated: either by a fork or by a new block.
--
-- In this implementation a map from @'FollowerId'@ to @'FollowerState'@ is shared
-- between all producers running on a single node; hence the unique identifier
-- @'FollowerId'@ for each follower: this is an implementation detail.
data FollowerState block = FollowerState {
       -- | Where the chain of the consumer and producer intersect. If the
       -- consumer is on the chain then this is the consumer's chain head,
       -- but if the consumer's chain is off the producer's chain then this is
       -- the point the consumer will need to rollback to.
       followerPoint :: Point block,

       -- | Where the will go next, roll back to the follower point, or roll
       -- forward from the follower point.
       followerNext  :: FollowerNext
     }
  deriving (Eq, Show)

data FollowerNext = FollowerBackTo | FollowerForwardFrom
  deriving (Eq, Show)

--
-- Invariant
--

invChainProducerState :: HasFullHeader block => ChainProducerState block -> Bool
invChainProducerState (ChainProducerState c cflrst cfid) =
    Chain.valid c
 && invFollowerStates c cflrst
 && all (< cfid) (Map.keys cflrst)

invFollowerStates :: HasHeader block => Chain block -> FollowerStates block -> Bool
invFollowerStates c flrst =
    and [ pointOnChain followerPoint c | FollowerState{followerPoint} <- Map.elems flrst ]

--
-- Operations
--


-- | Initialise @'ChainProducerState'@ with a given @'Chain'@ and empty list of
-- followers.
--
initChainProducerState :: Chain block -> ChainProducerState block
initChainProducerState c = ChainProducerState c Map.empty 0

-- | Get the recorded state of a chain consumer. The 'FollowerId' is assumed to
-- exist.
--
lookupFollower :: ChainProducerState block -> FollowerId -> FollowerState block
lookupFollower (ChainProducerState _ cflrst _) fid = cflrst Map.! fid

-- | Return 'True' when a follower with the given 'FollowerId' exists.
followerExists :: FollowerId -> ChainProducerState block -> Bool
followerExists fid (ChainProducerState _ cflrst _) = fid `Map.member` cflrst

-- | Extract @'Chain'@ from @'ChainProducerState'@.
--
producerChain :: ChainProducerState block -> Chain block
producerChain (ChainProducerState c _ _) = c

findFirstPoint :: HasHeader block
               => [Point block]
               -> ChainProducerState block
               -> Maybe (Point block)
findFirstPoint ps = Chain.findFirstPoint ps . producerChain


-- | Add a new follower with the given intersection point and return the new
-- 'FollowerId'.
--
initFollower :: HasHeader block
             => Point block
             -> ChainProducerState block
             -> (ChainProducerState block, FollowerId)
initFollower point (ChainProducerState c cflrst cfid) =
    assert (pointOnChain point c) $
    (ChainProducerState c (Map.insert cfid flrst cflrst) (succ cfid), cfid)
  where
    flrst = FollowerState {
          followerPoint = point,
          followerNext  = FollowerBackTo
        }


-- | Delete an existing follower. The 'FollowerId' is assumed to exist.
--
deleteFollower :: FollowerId -> ChainProducerState block -> ChainProducerState block
deleteFollower fid (ChainProducerState c cflrst cfid) =
    assert (fid `Map.member` cflrst) $
    ChainProducerState c (Map.delete fid cflrst) cfid


-- | Change the intersection point of a follower. This also puts it into
-- the 'FollowerBackTo' state.
--
updateFollower :: HasHeader block
               => FollowerId
               -> Point block    -- ^ new follower intersection point
               -> ChainProducerState block
               -> ChainProducerState block
updateFollower fid point (ChainProducerState c cflrst cnfid) =
    assert (pointOnChain point c) $
    ChainProducerState c (Map.adjust update fid cflrst) cnfid
  where
    update flrst = flrst { followerPoint = point, followerNext  = FollowerBackTo }

-- | Switch chains and update followers; if a follower point falls out of the chain,
-- replace it with the intersection of both chains and put it in the
-- `FollowerBackTo` state, otherwise preserve follower state.
--
switchFork :: HasHeader block
           => Chain block
           -> ChainProducerState block
           -> ChainProducerState block
switchFork c (ChainProducerState c' cflrst cfid) =
    ChainProducerState c (update <$> cflrst) cfid
  where
    ipoint = fromMaybe genesisPoint $ Chain.intersectChains c c'

    update flrst@FollowerState{followerPoint} =
      if pointOnChain followerPoint c
        then flrst
        else flrst { followerPoint = ipoint, followerNext = FollowerBackTo }


-- | What a follower needs to do next. Should they move on to the next block or
-- do they need to roll back to a previous point on their chain. It also updates
-- the producer's state assuming that the follower follows its instruction.
--
followerInstruction :: HasHeader block
                    => FollowerId
                    -> ChainProducerState block
                    -> Maybe (ChainUpdate block block, ChainProducerState block)
followerInstruction fid cps@(ChainProducerState c cflrst cfid) =
    let FollowerState {followerPoint, followerNext} = lookupFollower cps fid in
    case followerNext of
      FollowerForwardFrom ->
          assert (pointOnChain followerPoint c) $
          case Chain.successorBlock followerPoint c of
            -- There is no successor block because the follower is at the head
            Nothing -> Nothing

            Just b -> Just (AddBlock b, cps')
              where
                cps' = ChainProducerState c (Map.adjust setPoint fid cflrst) cfid
                setPoint flrst = flrst { followerPoint = blockPoint b }

      FollowerBackTo -> Just (RollBack followerPoint, cps')
        where
          cps' = ChainProducerState c (Map.adjust setForwardFrom fid cflrst) cfid
          setForwardFrom flrst = flrst { followerNext = FollowerForwardFrom }


-- | Add a block to the chain. It does not require any follower's state changes.
--
addBlock :: HasHeader block
         => block
         -> ChainProducerState block
         -> ChainProducerState block
addBlock b (ChainProducerState c cflrst cfid) =
    ChainProducerState (Chain.addBlock b c) cflrst cfid


-- | Rollback producer chain. It requires to update follower states, since some
-- @'followerPoint'@s may not be on the new chain; in this case find intersection
-- of the two chains and set @'followerNext'@ to @'FollowerBackTo'@.
rollback :: (HasHeader block, HeaderHash block ~ HeaderHash block')
         => Point block'
         -> ChainProducerState block
         -> Maybe (ChainProducerState block)
rollback p (ChainProducerState c cflrst cfid) = do
    c' <- Chain.rollback (castPoint p) c
    return $ ChainProducerState c' (rollbackFollower <$> cflrst) cfid
  where
    rollbackFollower flrst@FollowerState { followerPoint = p' }
      | Chain.pointIsAfter p' (castPoint p) c
      = flrst { followerPoint = castPoint p, followerNext = FollowerBackTo }
      | otherwise
      = flrst

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
