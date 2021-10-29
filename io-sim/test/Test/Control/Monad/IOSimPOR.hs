{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes       #-}
{-# LANGUAGE DeriveGeneric    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}

module Test.Control.Monad.IOSimPOR where

import Control.Monad
import Control.Monad.IOSim
import Control.Monad.Class.MonadSTM
import Control.Monad.Class.MonadFork
import Control.Monad.Class.MonadTimer
import Control.Monad.Class.MonadTest

import GHC.Generics

import System.Exit
import System.IO.Unsafe

import Test.QuickCheck
import Data.List
import Data.Map(Map)
import qualified Data.Map as Map
import Control.Exception(try, evaluate, SomeException)
import Control.Parallel
import Data.IORef

data Step =
    WhenSet Int Int
  | ThrowTo Int
  | Delay Int
  | Timeout TimeoutStep
  deriving (Eq, Ord, Show)

data TimeoutStep =
    NewTimeout    Int
  | UpdateTimeout Int
  | CancelTimeout
  | AwaitTimeout
  deriving (Eq, Ord, Show, Generic)

instance Arbitrary Step where
  arbitrary = frequency [(5,do m <- choose (1,20)
                               n <- choose (0,m)
                               return $ WhenSet m n),
                         (1,do NonNegative i <- arbitrary
                               return $ ThrowTo i),
                         (1,do Positive i <- arbitrary
                               return $ Delay i),
                         (1,Timeout <$> arbitrary)]

  shrink (WhenSet m n) = map (WhenSet m) (shrink n) ++
                         map (`WhenSet` n) (filter (>=n) (shrink m))
  shrink (ThrowTo i) = map ThrowTo (shrink i)
  shrink (Delay i)   = map Delay (shrink i)
  shrink (Timeout t) = map Timeout (shrink t)

instance Arbitrary TimeoutStep where
  arbitrary = do Positive i <- arbitrary
                 frequency $ map (fmap return) $
                   [(3,NewTimeout i),
                    (1,UpdateTimeout i),
                    (1,CancelTimeout),
                    (3,AwaitTimeout)]

  shrink = genericShrink


newtype Task = Task [Step]
  deriving (Eq, Ord, Show)

instance Arbitrary Task where
  arbitrary = do
    steps <- arbitrary
    return . Task $ normalize steps
  shrink (Task steps) =
    (Task <$> compressSteps steps) ++
    (Task . normalize <$> shrink steps)

normalize :: [Step] -> [Step]
normalize steps = plug steps wsSteps 1000000
  where wsSteps = reverse $ sort [s | s@(WhenSet _ _) <- steps]
        plug []              []               _ = []
        plug (WhenSet _ _:s) (WhenSet a b:ws) m = WhenSet (min a m) (min b m):plug s ws (min b m)
        plug (step:s)        ws               m = step:plug s ws m
        plug _               _                _ = error "plug: impossible"

compressSteps :: [Step] -> [[Step]]
compressSteps (WhenSet a b:WhenSet c d:steps) =
  [WhenSet a d:steps] ++ ((WhenSet a b:) <$> compressSteps (WhenSet c d:steps))
compressSteps (s:steps) = (s:) <$> compressSteps steps
compressSteps [] = []

newtype Tasks = Tasks [Task]
  deriving Show

instance Arbitrary Tasks where
  arbitrary = Tasks . fixThrowTos <$> scale (min 20) arbitrary
  shrink (Tasks ts) = Tasks . fixThrowTos <$>
         removeTask ts ++
         shrink ts ++
         shrinkDelays ts ++
         advanceThrowTo ts ++
         sortTasks ts

fixThrowTos :: [Task] -> [Task]
fixThrowTos tasks = mapThrowTos (`mod` length tasks) tasks

shrinkDelays :: [Task] -> [[Task]]
shrinkDelays tasks
  | null times = []
  | otherwise  = [map (Task . removeTime d) [steps | Task steps <- tasks]
                 | d <- times]
  where times = foldr union [] [scanl1 (+) [d | Delay d <- t] | Task t <- tasks]
        removeTime 0 steps = steps
        removeTime _ []    = []
        removeTime d (Delay d':steps)
          | d==d' = steps
          | d< d' = Delay (d'-d):steps
          | d> d' = removeTime (d-d') steps
        removeTime d (s:steps) =
          s:removeTime d steps

removeTask :: [Task] -> [[Task]]
removeTask tasks =
  [ mapThrowTos (fixup i) . map (dontThrowTo i) $ take i tasks++drop (i+1) tasks
  | i <- [0..length tasks-1]]
  where fixup i j | j>i       = j-1
                  | otherwise = j
        dontThrowTo i (Task steps) = Task (filter (/=ThrowTo i) steps)

advanceThrowTo :: [Task] -> [[Task]]
advanceThrowTo [] = []
advanceThrowTo (Task steps:ts) =
  ((:ts) . Task <$> advance steps) ++
  ((Task steps:) <$> advanceThrowTo ts)
  where advance (WhenSet a b:ThrowTo i:steppes) =
          [ThrowTo i:WhenSet a b:steppes] ++ (([WhenSet a b,ThrowTo i]++) <$> advance steppes)
        advance (s:steppes) = (s:) <$> advance steppes
        advance []          = []

mapThrowTos :: (Int -> Int) -> [Task] -> [Task]
mapThrowTos f tasks = map mapTask tasks
  where mapTask (Task steps) = Task (map mapStep steps)
        mapStep (ThrowTo i) = ThrowTo (f i)
        mapStep s           = s

sortTasks :: Ord a => [a] -> [[a]]
sortTasks (x:y:xs) | x>y = [y:x:xs] ++ ((x:) <$> sortTasks (y:xs))
sortTasks (x:xs)         = (x:) <$> sortTasks xs
sortTasks []             = []

interpret :: forall s. TVar (IOSim s) Int -> TVar (IOSim s) [ThreadId (IOSim s)] -> Task -> IOSim s (ThreadId (IOSim s))
interpret r t (Task steps) = forkIO $ do
    context <- atomically $ do
      ts <- readTVar t
      when (null ts) retry
      timer <- newTVar Nothing
      return (ts,timer)
    mapM_ (interpretStep context) steps
  where interpretStep _ (WhenSet m n) = atomically $ do
          a <- readTVar r
          when (a/=m) retry
          writeTVar r n
        interpretStep (ts,_) (ThrowTo i) = throwTo (ts !! i) (ExitFailure 0)
        interpretStep _      (Delay i)   = threadDelay (fromIntegral i)
        interpretStep (_,timer) (Timeout tstep) = do
          timerVal <- atomically $ readTVar timer
          case (timerVal,tstep) of
            (_,NewTimeout n)            -> do tout <- newTimeout (fromIntegral n)
                                              atomically $ writeTVar timer (Just tout)
            (Just tout,UpdateTimeout n) -> updateTimeout tout (fromIntegral n)
            (Just tout,CancelTimeout)   -> cancelTimeout tout
            (Just tout,AwaitTimeout)    -> atomically $ awaitTimeout tout >> return ()
            (Nothing,_)                 -> return ()

runTasks :: [Task] -> IOSim s (Int,Int)
runTasks tasks = do
  let m = maximum [maxTaskValue t | Task t <- tasks]
  r  <- atomically $ newTVar m
  t  <- atomically $ newTVar []
  exploreRaces
  ts <- mapM (interpret r t) tasks
  atomically $ writeTVar t ts
  threadDelay 1000000000  -- allow the SUT threads to run
  a  <- atomically $ readTVar r
  return (m,a)

maxTaskValue :: [Step] -> Int
maxTaskValue (WhenSet m _:_) = m
maxTaskValue (_:t)           = maxTaskValue t
maxTaskValue []              = 0

propSimulates :: Tasks -> Property
propSimulates (Tasks tasks) =
  any (not . null . (\(Task steps)->steps)) tasks ==>
    let Right (m,a) = runSim (runTasks tasks) in
    m>=a

propExploration :: Tasks -> Property
propExploration (Tasks tasks) =
  any (not . null . (\(Task steps)->steps)) tasks ==>
    traceNoDuplicates $ \addTrace ->
    --traceCounter $ \addTrace ->
    exploreSimTrace id (runTasks tasks) $ \_ trace ->
    --Debug.trace (("\nTrace:\n"++) . splitTrace . noExceptions $ show trace) $
    addTrace trace $
    counterexample (splitTrace . noExceptions $ show trace) $
    case traceResult False trace of
      Right (m,a) -> property $ m>=a
      Left e      -> counterexample (show e) False

-- Testing propPermutations n should collect every permutation of [1..n] once only.
-- Test manually, and supply a small value of n.
propPermutations :: Int -> Property
propPermutations n =
  traceNoDuplicates $ \addTrace ->
  exploreSimTrace (withScheduleBound 10000) (doit n) $ \_ trace ->
    addTrace trace $
    let Right result = traceResult False trace in
    tabulate "Result" [noExceptions $ show $ result] $
      True

doit :: Int -> IOSim s [Int]
doit n = do
          r <- atomically $ newTVar []
          exploreRaces
          mapM_ (\i -> forkIO $ atomically $ modifyTVar r (++[i])) [1..n]
          threadDelay 1
          atomically $ readTVar r

ordered :: Ord a => [a] -> Bool
ordered xs = and (zipWith (<) xs (drop 1 xs))

noExceptions :: [Char] -> [Char]
noExceptions xs = unsafePerformIO $ try (evaluate xs) >>= \case
  Right []     -> return []
  Right (x:ys) -> return (x:noExceptions ys)
  Left e       -> return ("\n"++show (e :: SomeException))

splitTrace :: [Char] -> [Char]
splitTrace [] = []
splitTrace (x:xs) | begins "(Trace" = "\n(" ++ splitTrace xs
                  | otherwise       = x:splitTrace xs
  where begins s = take (length s) (x:xs) == s

traceCounter :: (Testable prop1, Show a1) => ((a1 -> a2 -> a2) -> prop1) -> Property
traceCounter k = r `pseq` (k addTrace .&&.
                           tabulate "Trace repetitions" (map show $ traceCounts ()) True)
  where
    r = unsafePerformIO $ newIORef (Map.empty :: Map String Int)
    addTrace t x = unsafePerformIO $ do
      atomicModifyIORef r (\m->(Map.insertWith (+) (show t) 1 m,()))
      return x
    traceCounts () = unsafePerformIO $ Map.elems <$> readIORef r

traceNoDuplicates :: (Testable prop1, Show a1) => ((a1 -> a2 -> a2) -> prop1) -> Property
traceNoDuplicates k = r `pseq` (k addTrace .&&. maximum (traceCounts ()) == 1)
  where
    r = unsafePerformIO $ newIORef (Map.empty :: Map String Int)
    addTrace t x = unsafePerformIO $ do
      atomicModifyIORef r (\m->(Map.insertWith (+) (show t) 1 m,()))
      return x
    traceCounts () = unsafePerformIO $ Map.elems <$> readIORef r
