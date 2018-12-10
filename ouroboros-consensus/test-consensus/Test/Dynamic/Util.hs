{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}

module Test.Dynamic.Util (
    allEqual
  , shortestLength
  , tracesToDot
  ) where

import           Data.Foldable (foldl')
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
import           Numeric.Natural (Natural)
import           Test.QuickCheck

import           Ouroboros.Network.Block
import           Ouroboros.Network.Chain
import qualified Ouroboros.Network.Chain as Chain

import           Ouroboros.Consensus.Node
import qualified Ouroboros.Consensus.Util.Chain as Chain
import           Ouroboros.Consensus.Util.Condense
import           Ouroboros.Consensus.Util.Orphans ()

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
                              <> show (getSlot t)
                              <> " and contains "
                              <> show (Chain.length d)
                              <> "blocks): "
                              <> condense d
        (Just _, Nothing)  -> error "impossible case"
        (Just s, Just t)   ->    "intersection reaches slot "
                              <> show (getSlot s)
                              <> " and has length "
                              <> show (Chain.length c)
                              <> ", but at least one chain reaches slot "
                              <> show (getSlot t)
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
    { biSlot     :: !Slot
    , biCreator  :: !(Maybe NodeId)
    , biHash     :: !(Hash b)
    , biPrevious :: !(Maybe (Hash b))
    }

genesisBlockInfo :: BlockInfo b
genesisBlockInfo = BlockInfo
    { biSlot     = 0
    , biCreator  = Nothing
    , biHash     = GenesisHash
    , biPrevious = Nothing
    }

blockInfo :: HasHeader b => (b -> NodeId) -> b -> BlockInfo b
blockInfo creator b = BlockInfo
    { biSlot     = blockSlot b
    , biCreator  = Just $ creator b
    , biHash     = BlockHash $ blockHash b
    , biPrevious = Just $ blockPrevHash b
    }

data NodeLabel = NodeLabel
    { nlSlot      :: Slot
    , nlCreator   :: Maybe NodeId
    , nlBelievers :: Set NodeId
    }

instance Labellable NodeLabel where
    toLabelValue NodeLabel{..} = StrLabel $ Text.pack $
           show (getSlot nlSlot)
        <> " "
        <> maybe "" showNodeId nlCreator
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

tracesToDot :: forall b. HasHeader b
            => (b -> NodeId)
            -> Map NodeId (Chain b)
            -> String
tracesToDot creator traces = Text.unpack $ printDotGraph $ graphToDot quickParams graph
  where
    chainBlockInfos :: Chain b -> Map (Hash b) (BlockInfo b)
    chainBlockInfos = foldChain f (Map.singleton GenesisHash genesisBlockInfo)
      where
        f m b = let info = blockInfo creator b
                in  Map.insert (biHash info) info m

    blockInfos :: Map (Hash b) (BlockInfo b)
    blockInfos = Map.unions $ map chainBlockInfos $ Map.elems traces

    lastHash :: Chain b -> Hash b
    lastHash Genesis  = GenesisHash
    lastHash (_ :> b) = BlockHash $ blockHash b

    blockInfosAndBelievers :: Map (Hash b) (BlockInfo b, Set NodeId)
    blockInfosAndBelievers = Map.foldlWithKey f i traces
      where
        i = (\info -> (info, Set.empty)) <$> blockInfos

        f m nid chain = Map.adjust
            (\(info, believers) -> (info, Set.insert nid believers))
            (lastHash chain)
            m

    hashToId :: Map (Hash b) Node
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
