{-# LANGUAGE BangPatterns #-}

module Process.Graph
    ( Node
    , Graph
    , toGraph
    , graphProcess
    , histogram
    ) where

import           Control.Monad
import           Control.Monad.Random (MonadRandom)
import           Data.List (find, foldl')
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import           Process.Core
import           Process.Simulation

type Node = Int

data Graph = Graph
    { nodes :: ![Node]
    , edges :: !(Map Node [Node])
    } deriving (Show, Read, Eq, Ord)

graphProcess :: Graph -> Node -> Process () -> Process ()
graphProcess g start d = do
    ns <- foldM startNode Map.empty $ nodes g
    forM_ ns $ \pid -> send pid $ show ns
    send (ns Map.! start) "ping"
  where

    startNode m n = do
        pid <- fork $ node n
        return $ Map.insert n pid m

    node n = do
        ns <- read <$> receive
        _  <- receive
        logMsg $ show n
        forM_ (edges g Map.! n) $ \n' -> fork $ d >> send (ns Map.! n') "ping"

toGraph :: [Node] -> [(Node, Node)] -> Graph
toGraph ns es = Graph
    { nodes = ns
    , edges = foldl' f (Map.fromList [(n, []) | n <- ns]) es
    }
  where
    f m (n1, n2) = Map.insertWith (++) n1 [n2] m

simGraph :: MonadRandom m => Graph -> Node -> Node -> Process () -> m (Maybe Time)
simGraph g start stop p = do
    logs <- simulate $ graphProcess g start p
    return $ case find (\(_, msg) -> msg == show stop) logs of
        Nothing     -> Nothing
        Just (t, _) -> Just t

histogram :: MonadRandom m 
          => Int 
          -> Graph 
          -> Node 
          -> Node 
          -> Process () 
          -> m (Map (Maybe Time) Double)
histogram n g start stop p = go Map.empty n
  where
    go !m !i
        | i == 0    = return $ (\c -> fromIntegral c/ fromIntegral n) <$> m
        | otherwise = do
            mt <- simGraph g start stop p
            go (Map.insertWith (+) mt (1 :: Int) m) (i - 1)
