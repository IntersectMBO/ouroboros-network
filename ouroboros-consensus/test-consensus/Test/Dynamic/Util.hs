{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeFamilies        #-}

module Test.Dynamic.Util (
    allEqual
  , shortestLength
  , tracesToDot
  , leaderScheduleFromTrace
  , CrowdedRun (..)
  , crowdedRunLength
  , longestCrowdedRun
  ) where

import           Data.Foldable (foldl')
import           Data.Function (on)
import           Data.Graph.Inductive.Graph
import           Data.Graph.Inductive.PatriciaTree
import           Data.GraphViz
import           Data.GraphViz.Attributes.Complete
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Maybe (catMaybes)
import           Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Text.Lazy as Text
import           Data.Word (Word64)
import           Numeric.Natural (Natural)
import           Test.QuickCheck

import           Ouroboros.Network.Block
import           Ouroboros.Network.MockChain.Chain (Chain (..))
import qualified Ouroboros.Network.MockChain.Chain as Chain

import           Ouroboros.Consensus.BlockchainTime
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.NodeId
import           Ouroboros.Consensus.Protocol.Abstract (NodeConfig)
import           Ouroboros.Consensus.Protocol.LeaderSchedule
                     (LeaderSchedule (..))
import           Ouroboros.Consensus.Util.Condense
import           Ouroboros.Consensus.Util.Orphans ()

import           Test.Util.HasCreator
import qualified Test.Util.MockChain as Chain

allEqual :: forall b. (Condense b, Eq b, HasHeader b) => [Chain b] -> Property
allEqual []             = property True
allEqual [_]            = property True
allEqual (x : xs@(_:_)) =
    let c = foldl' Chain.commonPrefix x xs
    in  foldl' (\prop d -> prop .&&. f c d) (property True) xs
  where
    f :: Chain b -> Chain b -> Property
    f c d = counterexample (g c d) $ c == d

    g :: Chain b -> Chain b -> String
    g c d = case (Chain.lastSlot c, Chain.lastSlot d) of
        (Nothing, Nothing) -> error "impossible case"
        (Nothing, Just t)  ->    "empty intersection of non-empty chains (one reaches slot "
                              <> show (unSlotNo t)
                              <> " and contains "
                              <> show (Chain.length d)
                              <> " blocks): "
                              <> condense d
        (Just _, Nothing)  -> error "impossible case"
        (Just s, Just t)   ->    "intersection reaches slot "
                              <> show (unSlotNo s)
                              <> " and has length "
                              <> show (Chain.length c)
                              <> ", but at least one chain reaches slot "
                              <> show (unSlotNo t)
                              <> " and has length "
                              <> show (Chain.length d)
                              <> ": "
                              <> condense c
                              <> " /= "
                              <> condense d

shortestLength :: Map NodeId (Chain b) -> Natural
shortestLength = fromIntegral . minimum . map Chain.length . Map.elems

{-------------------------------------------------------------------------------
  Generation of a dot-file to represent the trace as a graph
-------------------------------------------------------------------------------}

data BlockInfo b = BlockInfo
    { biSlot     :: !SlotNo
    , biCreator  :: !(Maybe CoreNodeId)
    , biHash     :: !(ChainHash b)
    , biPrevious :: !(Maybe (ChainHash b))
    }

genesisBlockInfo :: BlockInfo b
genesisBlockInfo = BlockInfo
    { biSlot     = 0
    , biCreator  = Nothing
    , biHash     = GenesisHash
    , biPrevious = Nothing
    }


blockInfo :: (HasHeader b, HasCreator b)
          => NodeConfig (BlockProtocol b) -> b -> BlockInfo b
blockInfo nc b = BlockInfo
    { biSlot     = blockSlot b
    , biCreator  = Just $ getCreator nc b
    , biHash     = BlockHash $ blockHash b
    , biPrevious = Just $ blockPrevHash b
    }

data NodeLabel = NodeLabel
    { nlSlot      :: SlotNo
    , nlCreator   :: Maybe CoreNodeId
    , nlBelievers :: Set NodeId
    }

instance Labellable NodeLabel where
    toLabelValue NodeLabel{..} = StrLabel $ Text.pack $
           show (unSlotNo nlSlot)
        <> " "
        <> maybe "" (showNodeId . fromCoreNodeId) nlCreator
        <> showNodeIds nlBelievers
      where
        fromNodeId :: NodeId -> Maybe Int
        fromNodeId (CoreId nid) = Just nid
        fromNodeId (RelayId _)  = Nothing

        showNodeId :: NodeId -> String
        showNodeId = maybe "" show . fromNodeId

        showNodeIds :: Set NodeId -> String
        showNodeIds nids = case catMaybes $ map fromNodeId $ Set.toList nids of
            [] -> ""
            xs -> " [" <> unwords (map show xs) <> "]"

data EdgeLabel = EdgeLabel

instance Labellable EdgeLabel where
    toLabelValue = const $ StrLabel Text.empty

tracesToDot :: forall b. (HasHeader b, HasCreator b)
            => Map NodeId (NodeConfig (BlockProtocol b), Chain b)
            -> String
tracesToDot traces = Text.unpack $ printDotGraph $ graphToDot quickParams graph
  where
    chainBlockInfos :: NodeConfig (BlockProtocol b) -> Chain b
                    -> Map (ChainHash b) (BlockInfo b)
    chainBlockInfos nc = Chain.foldChain f (Map.singleton GenesisHash genesisBlockInfo)
      where
        f m b = let info = blockInfo nc b
                in  Map.insert (biHash info) info m

    blockInfos :: Map (ChainHash b) (BlockInfo b)
    blockInfos = Map.unions $ map (uncurry chainBlockInfos) $ Map.elems traces

    lastHash :: Chain b -> ChainHash b
    lastHash Genesis  = GenesisHash
    lastHash (_ :> b) = BlockHash $ blockHash b

    blockInfosAndBelievers :: Map (ChainHash b) (BlockInfo b, Set NodeId)
    blockInfosAndBelievers = Map.foldlWithKey f i traces
      where
        i = (\info -> (info, Set.empty)) <$> blockInfos

        f m nid (_, chain) = Map.adjust
            (\(info, believers) ->
              (info, Set.insert nid believers))
            (lastHash chain)
            m

    hashToId :: Map (ChainHash b) Node
    hashToId = Map.fromList $ zip (Map.keys blockInfosAndBelievers) [0..]

    ns :: [LNode NodeLabel]
    ns = [ ( hashToId Map.! h
           , NodeLabel
                { nlSlot      = biSlot info
                , nlCreator   = biCreator info
                , nlBelievers = believers
                }
           )
         | (h, (info, believers)) <- Map.toList blockInfosAndBelievers
         ]

    es :: [LEdge EdgeLabel]
    es = map g
       $ catMaybes
       $ map f
       [ (biHash info, biPrevious info) | info <- Map.elems blockInfos]
      where f (h, mh) = (h,) <$> mh
            g (h1, h2) = (hashToId Map.! h1, hashToId Map.! h2, EdgeLabel)

    graph :: Gr NodeLabel EdgeLabel
    graph = mkGraph ns es

leaderScheduleFromTrace :: forall b. (HasCreator b, HasHeader b)
                        => NumSlots
                        -> Map NodeId (NodeConfig (BlockProtocol b), Chain b)
                        -> LeaderSchedule
leaderScheduleFromTrace (NumSlots numSlots) = LeaderSchedule .
    Map.foldl' (\m (nc, c) -> Chain.foldChain (step nc) m c) initial
  where
    initial :: Map SlotNo [CoreNodeId]
    initial = Map.fromList [(slot, []) | slot <- [1 .. fromIntegral numSlots]]

    step :: NodeConfig (BlockProtocol b)
         -> Map SlotNo [CoreNodeId]
         -> b
         -> Map SlotNo [CoreNodeId]
    step nc m b = Map.adjust (insert $ getCreator nc b) (blockSlot b) m

    insert :: CoreNodeId -> [CoreNodeId] -> [CoreNodeId]
    insert nid xs
        | nid `elem` xs = xs
        | otherwise     = nid : xs

{-------------------------------------------------------------------------------
  Crowded Run - longest multi-leader section of a leader schedule
-------------------------------------------------------------------------------}

-- | Describes a sequence of slots in a leader schedule with slots with
-- more than one leader, possibly interrupted by slots without leader.
-- There can be no such sequence, but if there is, first slot and number of
-- multi-leader slots are given.
newtype CrowdedRun = CrowdedRun (Maybe (SlotNo, Word64))
    deriving (Show, Eq)

crowdedRunLength :: CrowdedRun -> Word64
crowdedRunLength (CrowdedRun m) = maybe 0 snd m

instance Ord CrowdedRun where
    compare = compare `on` crowdedRunLength

noRun :: CrowdedRun
noRun = CrowdedRun Nothing

incCrowdedRun :: SlotNo -> CrowdedRun -> CrowdedRun
incCrowdedRun slot (CrowdedRun Nothing)          = CrowdedRun (Just (slot, 1))
incCrowdedRun _    (CrowdedRun (Just (slot, n))) = CrowdedRun (Just (slot, n + 1))

-- | The longest 'CrowdedRun' of a 'LeaderSchedule'. The tiebreaker favors
-- earlier runs. The \"length\" of a run is the number of slots in it that have
-- more than one leader. The run may also have slots with no leader in it, but
-- these don't make it longer. A run has no slots with exactly one leader.
--
-- Such a run can drive a network away from consensus. If its long enough, it
-- might even prevent consensus thereafter. The length of the run is defined as
-- a number of slots, but it is also interpreted as an upper bound on the
-- number of blocks that can differ among the competing chains' suffixes at the
-- end of the run. Thus, if the length of a run exceeds the security parameter
-- @k@, then that part of the leader schedule itself may be enough to prevent
-- the network from ever re-establishing consensus. The method of failure is as
-- follows.
--
-- * Assume the network is in consensus as of slot @s - 1@: all nodes have the
-- same current chain. Call that chain C.
--
-- * Let the slot @s@ have @N > 1@ leaders. Each leader forges a block that
-- fits onto C. It's most likely that each leader will add the block that it
-- forged to its own chain; a race condition between ChainSync/BlockFetch and
-- forging makes it possible, though less likely, that a leader would have
-- received another leader's new block before it forges its own. The @N@
-- leaders have thereby created (from 1 to) @N@ competing current chains D1 ..
-- DN amongst the network; each non-leader will adopt one of these new chains
-- because they're all longer than C. Thus the network is likely no longer in
-- consensus.
--
-- * If the slot @s + 1@ has exactly one leader, then the leader will forge a
-- block onto whichever of the @N@ chains it happens to have adopted. All
-- non-leaders will switch to this new chain E, because it is longer than each
-- of D1 .. DN. The network has reestablished consensus.
--
-- * If the slot @s + 1@ instead has @M > 1@ leaders, then the pattern from
-- slot @s@ will repeat, and the competing chains will grow by one block. Now
-- instead of the competing chains disagreeing on only the last block, they
-- could disagree on the last two blocks. (If all the @M@ leaders happened to
-- have adopted the same one of the @N@ chains from slot @s@, then all new
-- chains in slot @s + 1@ would again disagree on only the last block. But that
-- is the least common possibility.)
--
-- * If the slot @s + 1@ instead has no leader, then all nodes will retain
-- their chains. The network is still not in consensus.
--
-- Consider a run beginning with a slot @s@ that has multiple leaders, and the
-- subsequent slots include @q@ many slots with multiple leaders before the
-- next slot @s + i@ that has exactly one leader. If @q > k@, then the security
-- parameter @k@ might prevent the network from being able to reach consensus
-- at slot @s + i@ and beyond. Note that @i >= q@, because some of the
-- intervening slots may have no leader; leaderless slots increase the
-- \"duration\" (number of slots) of the run but not its \"length\" (number of
-- potentially-disagreed-upon blocks).
--
-- To be careful, this function assumes that any multi-leader slot will indeed
-- create multiple competing chains in the network as described above.
longestCrowdedRun :: LeaderSchedule -> CrowdedRun
longestCrowdedRun (LeaderSchedule m) = fst
                                     $ foldl' go (noRun, noRun)
                                     $ Map.toList
                                     $ fmap length m
  where
    go :: (CrowdedRun, CrowdedRun) -> (SlotNo, Int) -> (CrowdedRun, CrowdedRun)
    go (x, y) (slot, n)
        | n == 0    = (x, y)
        | n == 1    = (x, noRun)
        | otherwise = let y' = incCrowdedRun slot y in (max x y', y')
