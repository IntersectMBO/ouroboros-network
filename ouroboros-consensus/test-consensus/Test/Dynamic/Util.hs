{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeFamilies        #-}

module Test.Dynamic.Util (
  -- * Chain properties
    prop_all_common_prefix
  , shortestLength
  -- * LeaderSchedule
  , leaderScheduleFromTrace
  , roundRobinLeaderSchedule
  , tooCrowded
  -- * GraphViz Dot
  , tracesToDot
  -- * Re-exports
  , module Test.Dynamic.Util.Expectations
  ) where

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
import           Ouroboros.Consensus.Node.ProtocolInfo.Abstract
                     (NumCoreNodes (..))
import           Ouroboros.Consensus.NodeId
import           Ouroboros.Consensus.Protocol.Abstract (NodeConfig,
                     SecurityParam (..))
import           Ouroboros.Consensus.Protocol.LeaderSchedule
                     (LeaderSchedule (..))
import           Ouroboros.Consensus.Util.Condense
import           Ouroboros.Consensus.Util.Orphans ()

import           Test.Util.HasCreator
import qualified Test.Util.MockChain as Chain

import           Test.Dynamic.Util.Expectations (NumBlocks (..),
                     determineForkLength)

{-------------------------------------------------------------------------------
  Chain properties
-------------------------------------------------------------------------------}

shortestLength :: Map NodeId (Chain b) -> Natural
shortestLength = fromIntegral . minimum . map Chain.length . Map.elems

prop_all_common_prefix :: (HasHeader b, Condense b, Eq b)
                       => Word64 -> [Chain b] -> Property
prop_all_common_prefix _ []     = property True
prop_all_common_prefix l (c:cs) = conjoin [prop_common_prefix l c d | d <- cs]

prop_common_prefix :: forall b. (HasHeader b, Condense b, Eq b)
                   => Word64 -> Chain b -> Chain b -> Property
prop_common_prefix l x y = go x y .&&. go y x
  where
    go c d =
        let (l', c') = findPrefix c d
            e        = "after dropping "
                 <> show l'
                 <> " blocks from "
                 <> showChain c
                 <> ",\n\nthe resulting "
                 <> showChain c'
                 <> "\n\nis a prefix of "
                 <> showChain d
                 <> ",\n\nbut only "
                 <> show l
                 <> " block(s) should have been necessary"
        in  counterexample e $ l' <= l

    findPrefix c' d
        | c' `Chain.isPrefixOf` d = (0, c')
        | otherwise         = let (l', c'') = findPrefix (Chain.dropLastBlocks 1 c') d
                              in  (l' + 1, c'')

    showChain :: Chain b -> String
    showChain c = condense c
                  <> "\n(length "
                  <> show (Chain.length c)
                  <> case Chain.lastSlot c of
                        Nothing -> ")"
                        Just s  ->    ", last slot "
                                   <> show (unSlotNo s)
                                   <> ")"

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

{-------------------------------------------------------------------------------
  Leader Schedule
-------------------------------------------------------------------------------}

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

tooCrowded :: SecurityParam -> LeaderSchedule -> Bool
tooCrowded k schedule = maxForkLength > maxRollbacks k
  where
    NumBlocks maxForkLength = determineForkLength k schedule

roundRobinLeaderSchedule :: NumCoreNodes -> NumSlots -> LeaderSchedule
roundRobinLeaderSchedule (NumCoreNodes n) (NumSlots t) = LeaderSchedule $
    Map.fromList $
    [ (SlotNo (toEnum i), [CoreNodeId (i `mod` n)])
    | i <- [ 0 .. t - 1 ]
    ]
