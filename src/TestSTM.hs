{-# LANGUAGE ScopedTypeVariables #-}
module TestSTM where

import Data.Array
import Data.Graph
import Control.Applicative
import Control.Monad

import Test.QuickCheck

import MonadClass
import qualified SimSTM as Sim

prop_stm_graph :: TestThreadGraph -> Bool
prop_stm_graph (TestThreadGraph g) = do
    vs <- sequence [ atomically (newTVar False) | _ <- vertices g ]
    form_ (vertices g) $ \v ->
      fork $ do
        -- read all the inputs and wait for them to become true
        -- then write to all the outputs
        atomically (sequence [  | <-  ])
    return ()
  where
    g' = transposeG g

newtype TestThreadGraph = TestThreadGraph Graph

instance Arbitrary TestThreadGraph where
  arbitrary =
    TestThreadGraph <$> arbitraryAcyclicGraph
                          (choose (2,5))
                          (choose (1,5))
                          0.3

arbitraryAcyclicGraph :: Gen Int -> Gen Int -> Float -> Gen Graph
arbitraryAcyclicGraph genNRanks genNPerRank edgeChance = do
    nranks    <- genNRanks
    rankSizes <- replicateM nranks genNPerRank
    let rankStarts = scanl (+) 0 rankSizes
        rankRanges = drop 1 (zip rankStarts (tail rankStarts))
        totalRange = sum rankSizes
    rankEdges <- mapM (uncurry genRank) rankRanges
    return $ buildG (0, totalRange-1) (concat rankEdges)
  where
    genRank :: Vertex -> Vertex -> Gen [Edge]
    genRank rankStart rankEnd =
      filterM (const (pick edgeChance))
        [ (i,j)
        | i <- [0..rankStart-1]
        , j <- [rankStart..rankEnd-1]
        ]

    pick :: Float -> Gen Bool
    pick chance = (< chance) <$> choose (0,1)

