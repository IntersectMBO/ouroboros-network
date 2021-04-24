{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeFamilies        #-}

module Test.ThreadNet.Util (
    -- * Chain properties
    chainCommonPrefix
  , prop_all_common_prefix
  , shortestLength
    -- * LeaderSchedule
  , consensusExpected
  , emptyLeaderSchedule
  , roundRobinLeaderSchedule
    -- * GraphViz Dot
  , tracesToDot
    -- * Re-exports
  , module Test.ThreadNet.Util.Expectations
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

import           Ouroboros.Network.MockChain.Chain (Chain (..))
import qualified Ouroboros.Network.MockChain.Chain as Chain

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Config.SecurityParam
import           Ouroboros.Consensus.Node.ProtocolInfo (NumCoreNodes (..))
import           Ouroboros.Consensus.NodeId
import           Ouroboros.Consensus.Protocol.LeaderSchedule
import           Ouroboros.Consensus.Util.Condense
import           Ouroboros.Consensus.Util.Orphans ()

import qualified Test.Util.MockChain as Chain
import           Test.Util.Slots (NumSlots (..))

import           Test.ThreadNet.Network (NodeOutput (..))
import           Test.ThreadNet.Util.Expectations (NumBlocks (..),
                     determineForkLength)
import           Test.ThreadNet.Util.HasCreator
import           Test.ThreadNet.Util.NodeJoinPlan (NodeJoinPlan)

{-------------------------------------------------------------------------------
  Chain properties
-------------------------------------------------------------------------------}

shortestLength :: Map NodeId (Chain b) -> Natural
shortestLength = fromIntegral . minimum . map Chain.length . Map.elems

prop_all_common_prefix :: (HasHeader b, Condense (HeaderHash b), Eq b)
                       => Word64 -> [Chain b] -> Property
prop_all_common_prefix _ []     = property True
prop_all_common_prefix l (c:cs) = conjoin [prop_common_prefix l c d | d <- cs]

prop_common_prefix :: forall b. (HasHeader b, Condense (HeaderHash b), Eq b)
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
    showChain c = condense (Chain.headTip c)
                  <> "\n(length "
                  <> show (Chain.length c)
                  <> ")"

-- | Find the common prefix of two chains
chainCommonPrefix :: HasHeader b => Chain b -> Chain b -> Chain b
chainCommonPrefix Genesis        _              = Genesis
chainCommonPrefix _              Genesis        = Genesis
chainCommonPrefix cl@(cl' :> bl) cr@(cr' :> br) =
    case blockNo bl `compare` blockNo br of
      LT -> chainCommonPrefix cl  cr'
      GT -> chainCommonPrefix cl' cr
      EQ ->
          if blockHash bl /= blockHash br
          then chainCommonPrefix cl' cr'
          else cl

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


blockInfo :: (GetPrevHash b, HasCreator b)
          => b -> BlockInfo b
blockInfo b = BlockInfo
    { biSlot     = blockSlot b
    , biCreator  = Just $ getCreator b
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
        fromNodeId :: NodeId -> Maybe Word64
        fromNodeId (CoreId (CoreNodeId nid)) = Just nid
        fromNodeId (RelayId _)               = Nothing

        showNodeId :: NodeId -> String
        showNodeId = maybe "" show . fromNodeId

        showNodeIds :: Set NodeId -> String
        showNodeIds nids = case catMaybes $ map fromNodeId $ Set.toList nids of
            [] -> ""
            xs -> " [" <> unwords (map show xs) <> "]"

data EdgeLabel = EdgeLabel

instance Labellable EdgeLabel where
    toLabelValue = const $ StrLabel Text.empty

tracesToDot :: forall b. (GetPrevHash b, HasCreator b)
            => Map NodeId (NodeOutput b)
            -> String
tracesToDot traces = Text.unpack $ printDotGraph $ graphToDot quickParams graph
  where
    chainBlockInfos :: Chain b
                    -> Map (ChainHash b) (BlockInfo b)
    chainBlockInfos = Chain.foldChain f (Map.singleton GenesisHash genesisBlockInfo)
      where
        f m b = let info = blockInfo b
                in  Map.insert (biHash info) info m

    blockInfos :: Map (ChainHash b) (BlockInfo b)
    blockInfos = Map.unions
      [ chainBlockInfos (nodeOutputFinalChain no)
      | no <- Map.elems traces
      ]

    lastHash :: Chain b -> ChainHash b
    lastHash Genesis  = GenesisHash
    lastHash (_ :> b) = BlockHash $ blockHash b

    blockInfosAndBelievers :: Map (ChainHash b) (BlockInfo b, Set NodeId)
    blockInfosAndBelievers =
        Map.foldlWithKey f i (nodeOutputFinalChain <$> traces)
      where
        i = (\info -> (info, Set.empty)) <$> blockInfos

        f m nid chain = Map.adjust
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

consensusExpected ::
     SecurityParam
  -> NodeJoinPlan
  -> LeaderSchedule
  -> Bool
consensusExpected k nodeJoinPlan schedule =
    maxForkLength <= maxRollbacks k
  where
    NumBlocks maxForkLength = determineForkLength k nodeJoinPlan schedule

emptyLeaderSchedule :: NumSlots -> LeaderSchedule
emptyLeaderSchedule (NumSlots t) = LeaderSchedule $
    Map.fromList $
    [ (SlotNo i, [])
    | i <- [ 0 .. t - 1 ]
    ]

roundRobinLeaderSchedule :: NumCoreNodes -> NumSlots -> LeaderSchedule
roundRobinLeaderSchedule (NumCoreNodes n) (NumSlots t) = LeaderSchedule $
    Map.fromList $
    [ (SlotNo i, [CoreNodeId (i `mod` n)])
    | i <- [ 0 .. t - 1 ]
    ]
