{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTSyntax #-}

module Algebra.Graph.Labelled.AdjacencyMap.Viz where

import Algebra.Graph.Labelled.AdjacencyMap
import qualified Data.Map as Map
import Data.GraphViz
import Data.GraphViz.Printing (renderDot)
import Data.GraphViz.Attributes.Complete
import Data.Semigroup (Endo (..))
import Data.Text.Lazy (Text, pack)
import Data.Word (Word8)
import Numeric.Natural (Natural)

-- | Visualize an edge-labelled graph as a graphviz dot program.
-- Uses the Enum instance so that Int is the dot vertex type (has a PrintDot
-- instance).
toGraphvizDot
  :: forall nl el cl l .
     ( Ord cl, Enum nl )
  => GraphvizParams Int nl el cl l
  -> AdjacencyMap el nl
  -> DotGraph Int
toGraphvizDot params gr = graphElemsToDot params vertices' edges'
  where
  vertices' :: [(Int, nl)]
  vertices' = Map.toList (Map.mapWithKey (const . toEnum) map')
  edges' :: [(Int, Int, el)]
  edges' = [ (n1, n2, e) | (n1, adj) <- Map.toList map', (n2, e) <- Map.toList adj ]
  map' = fmap (Map.mapKeys fromEnum) (Map.mapKeys fromEnum (adjacencyMap gr))

data Directedness where
  Directed   :: Directedness
  Undirected :: Directedness

newtype SetAttributes s = SetAttributes { setAttributes :: s -> Endo [Attribute] }

getAttributes :: SetAttributes s -> s -> [Attribute]
getAttributes sa s = appEndo (setAttributes sa s) []

instance Semigroup (SetAttributes s) where
  left <> right = SetAttributes (\s -> setAttributes left s <> setAttributes right s)

instance Monoid (SetAttributes s) where
  mappend = (<>)
  mempty = SetAttributes (const mempty)

type VizNode n nl = SetAttributes (n, nl)

type VizEdge n el = SetAttributes (n, n, el)

attributeList :: (s -> [Attribute]) -> SetAttributes s
attributeList f = SetAttributes $ \s -> Endo ((++) (f s))

constAttributes :: [Attribute] -> SetAttributes s
constAttributes attrs = attributeList (const attrs)

nodeShapePoint :: VizNode n nl
nodeShapePoint = constAttributes [Shape PointShape]

nodeWidth :: Double -> VizNode n nl
nodeWidth d = constAttributes [Width d]

nodeHeight :: Double -> VizNode n nl
nodeHeight d = constAttributes [Height d]

strlabel :: (s -> String) -> SetAttributes s
strlabel tt = attributeList $ \s -> [Label (StrLabel (pack (tt s)))]

tooltip :: (s -> String) -> SetAttributes s
tooltip tt = attributeList $ \s -> [Tooltip (pack (tt s))]

filled :: SetAttributes s
filled = constAttributes [Style [SItem Filled []]]

fillColour :: Word8 -> Word8 -> Word8 -> SetAttributes s
fillColour r g b = constAttributes [FillColor [toWC (RGB r g b)]]

fillColourHSV :: Double -> Double -> Double -> SetAttributes s
fillColourHSV h s v = constAttributes [FillColor [toWC (HSV h s v)]]

setEither :: (s -> Either a b) -> SetAttributes a -> SetAttributes b -> SetAttributes s
setEither k left right = SetAttributes $ \s ->
  either (setAttributes left) (setAttributes right) (k s)

setUnless :: (s -> Bool) -> SetAttributes s -> SetAttributes s
setUnless p attrs = setEither k attrs (constAttributes [])
  where
  k = \s -> if p s then Right s else Left s

setWhen :: (s -> Bool) -> SetAttributes s -> SetAttributes s
setWhen p = setUnless (not . p)

viz
  :: ( PrintDot n )
  => Directedness
  -> LayoutMethod n
  -> VizNode n nl
  -> VizEdge n el
  -> GraphvizParams n nl el () nl
viz dir layout vizNode vizEdge = nonClusteredParams
  { isDirected = case dir of
      Directed   -> True
      Undirected -> False
  , globalAttributes = [globalGraph]
  , fmtNode = nodeFmt
  , fmtEdge = edgeFmt
  }
  where
  nodeFmt (n,nl) = getAttributes vizNode (n, nl)
  edgeFmt (initial,terminal,el) = getAttributes vizEdge (initial, terminal, el)
  globalGraph = GraphAttrs { attrs = renderLayoutMethod layout }

data NeatoMode where
  NeatoKK :: NeatoMode

renderNeatoMode :: NeatoMode -> [Attribute]
renderNeatoMode NeatoKK = [Mode KK]

data NeatoModel where
  NeatoCircuit :: NeatoModel
  NeatoSubset  :: NeatoModel

renderNeatoModel :: NeatoModel -> [Attribute]
renderNeatoModel NeatoCircuit = [Model Circuit]
renderNeatoModel NeatoSubset  = [Model SubSet]

-- | Enumeration of some graphviz layout methods and their corresponding
-- parameters. Sourced from https://graphviz.gitlab.io/_pages/pdf/dot.1.pdf
data LayoutMethod n where
  -- | Hierarchical layout. No parameters offered because it's unlikely you
  -- really want to use this layout in our application.
  LayoutDot :: LayoutMethod n
  -- | "Spring" method (minimization problem).
  -- 
  -- epsilon (cutoff for solver, use 0.1 if unsure)
  -- optional seed for the PRNG
  LayoutNeato :: Maybe NeatoMode -> Maybe NeatoModel -> Double -> Maybe Int -> LayoutMethod n
  -- | Circular layout: root set and minimum distance
  LayoutCirco :: [n] -> Double -> LayoutMethod n
  -- | Circular layout in layers: a root and the radial distance between
  -- concentric rings.
  LayoutTwoPi :: n -> Double -> LayoutMethod n
  -- | Ideal node separation, max iterations, and optional PRNG seed.
  -- Set the Bool to True to mean SFDP.
  LayoutFdp   :: Bool -> Double -> Natural -> Maybe Int -> LayoutMethod n

-- | Make global attributes from a layout method.
renderLayoutMethod :: ( PrintDot n ) => LayoutMethod n -> [Attribute]
renderLayoutMethod method = case method of
  LayoutDot -> [Layout Dot]
  LayoutNeato mode model epsilon mSeed ->
    [ Layout Neato
    , Epsilon epsilon
    ]
    ++ maybe [] (pure . Start . StartSeed) mSeed
    ++ maybe [] renderNeatoMode mode
    ++ maybe [] renderNeatoModel model
  LayoutCirco roots minDist ->
    [ Layout Circo
    , MinDist minDist
    , Root (NodeName (renderDot (toDot roots)))
    ]
  LayoutTwoPi root rankSep ->
    [ Layout TwoPi
    , RankSep [rankSep]
    , Root (NodeName (renderDot (toDot root)))
    ]
  LayoutFdp sfdp k maxIter mSeed ->
    [ Layout (if sfdp then Sfdp else Fdp)
    , K k
    , MaxIter (fromIntegral maxIter)
    ]
    ++ maybe [] (pure . Start . StartSeed) mSeed
