{-# LANGUAGE ScopedTypeVariables #-}
module Test.Sim (tests) where

import Data.Array
import Data.Graph
import Data.List (partition, sort, sortBy)
import Control.Applicative
import Control.Monad
import Control.Monad.ST.Lazy (ST, runST)
import Control.Monad.Free (Free)

import Test.QuickCheck
import Test.Tasty
import Test.Tasty.QuickCheck

import MonadClass
import qualified Sim as Sim

tests :: TestTree
tests =
  testGroup "STM simulator"
  [ testProperty "read/write graph (IO)"   prop_stm_graph_io
  , testProperty "read/write graph (SimM)" (withMaxSuccess 1000 prop_stm_graph_sim)
  , testProperty "timers (SimM)"           (withMaxSuccess 1000 prop_timers)
  , testProperty "fork order (SimM)"       (withMaxSuccess 1000 prop_fork_order)
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
        let incomming = g' ! v
            outgoing  = g  ! v
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

newtype TestRationals = TestRationals [Rational]
  deriving Show

-- |
-- Arbitrary non negative rational numbers with a high propbability of
-- repetitions.
instance Arbitrary TestRationals where
  arbitrary = sized $ \n -> TestRationals <$> genN n []
    where
      genN :: Int -> [Rational] -> Gen [Rational]
      genN 0 rs = return rs
      genN n [] = do
        r <- genPositiveRational
        genN (n - 1) [r]
      genN n rs = do
        r <- frequency
          [ (2, elements rs)
          , (1, genPositiveRational)
          ]
        genN (n - 1) (r : rs)

      genPositiveRational :: Gen Rational
      genPositiveRational = do
        Positive (n :: Int) <- arbitrary
        Positive (d :: Int) <- arbitrary
        return $ toRational n / toRational d
  shrink (TestRationals rs) = [ TestRationals rs' | rs' <- shrinkList (const []) rs ]

prop_timers :: TestRationals -> Property
prop_timers (TestRationals xs) = 
    label (lbl xs)
    $ isValid $ runST runExperiment
  where
    countUnique :: Eq a => [a] -> Int
    countUnique [] = 0
    countUnique (a:as) =
      let as' = filter (== a) as 
      in 1 + countUnique as'

    lbl :: Eq a => [a] -> String
    lbl as =
      let p = (if null as then 0 else (100 * countUnique as) `div` length as) `mod` 10 * 10
      in show p ++ "% unique"

    experiment :: Sim.Probe s (Rational, Int) -> Sim.SimM s ()
    experiment p =
      forM_ (zip xs [0..]) $ \(t, idx) ->
        timer (Sim.VTimeDuration t) $ probeOutput p (t, idx)

    runExperiment :: ST s (ProbeTrace (Free (Sim.SimF s)) (Rational, Int))
    runExperiment = do
      p <- newProbe
      Sim.runSimMST (experiment p)
      readProbe p

    isValid :: ProbeTrace (Free (Sim.SimF s)) (Rational, Int) -> Bool
    isValid tr =
         -- all timers should fire
         length tr == length xs
         -- timers should fire in the right order
      && sortBy (\(_, a) (_, a') -> compare a a') tr == tr

prop_fork_order :: Positive Int -> Property
prop_fork_order (Positive n) = isValid $ runST runExperiment
  where
    experiment :: Sim.Probe s Int -> Int -> Sim.SimM s ()
    experiment _ 0 = return ()
    experiment p n = do
      fork $ probeOutput p n
      experiment p (n - 1)

    runExperiment :: ST s (ProbeTrace (Free (Sim.SimF s)) Int)
    runExperiment = do
      p <- newProbe
      Sim.runSimMST (experiment p n)
      readProbe p

    isValid :: ProbeTrace (Free (Sim.SimF s)) Int -> Property
    isValid tr = (map snd tr) === [n,n-1..1]

