{-# LANGUAGE ScopedTypeVariables #-}
module Test.Sim (tests) where

import Data.Array
import Data.Graph
import Control.Applicative
import Control.Monad
import Control.Monad.ST.Lazy (runST)

import Test.QuickCheck
import Test.Tasty
import Test.Tasty.QuickCheck

import MonadClass
import qualified SimSTM as Sim

tests :: TestTree
tests =
  testGroup "STM simulator"
  [ testProperty "read/write graph (IO)"   prop_stm_graph_io
  , testProperty "read/write graph (SimM)" (withMaxSuccess 1000 prop_stm_graph_sim)
  ]

prop_stm_graph_io :: TestThreadGraph -> Property
prop_stm_graph_io g =
  ioProperty $
    prop_stm_graph g

prop_stm_graph_sim :: TestThreadGraph -> Bool
prop_stm_graph_sim g =
    not $ null [ () | (_t, Sim.ThreadId 0, Sim.EventThreadStopped) <- trace ]
  where
    trace = Sim.runSimM (prop_stm_graph g)

prop_stm_graph :: MonadSTM m stm => TestThreadGraph -> m ()
prop_stm_graph (TestThreadGraph g) = do
    vars <- listArray (bounds g) <$>
            sequence [ atomically (newTVar False) | _ <- vertices g ]
    forM_ (vertices g) $ \v ->
      fork $ do
        -- read all the inputs and wait for them to become true
        -- then write to all the outputs
        let incomming = [ v' | v'  <- g' ! v ]
            outgoing  = [ v' | v'  <- g  ! v ]
        atomically $ do
          sequence_ [ readTVar  (vars ! var) >>= check | var <- incomming ]
          sequence_ [ writeTVar (vars ! var) True      | var <- outgoing  ]

    let -- the vertices with outgoing but no incomming edges
        inputs  = [ v
                  | v <- vertices g
                  , not (null (g  ! v))
                  ,      null (g' ! v) ]
        -- the vertices with incomming but no outgoing edges
        outputs = [ v
                  | v <- vertices g
                  , not (null (g' ! v))
                  ,      null (g  ! v) ]

    -- write to the inputs and wait for the outputs
    fork $ atomically $ sequence_ [ writeTVar (vars ! var) True | var <- inputs  ]
    atomically $ sequence_ [ readTVar (vars ! var) >>= check | var <- outputs ]
  where
    g' = transposeG g -- for incomming edges

newtype TestThreadGraph = TestThreadGraph Graph
  deriving Show

instance Arbitrary TestThreadGraph where
  arbitrary =
    sized $ \sz ->
    TestThreadGraph <$> arbitraryAcyclicGraph
                          (choose (2, 8 `min` (sz `div` 3)))
                          (choose (1, 8 `min` (sz `div` 3)))
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

