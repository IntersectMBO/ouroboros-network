module Example
    ( example
    ) where

import           Control.Monad
import           Data.List (foldl')
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Ratio
import           Text.Printf (printf)

import           Process.Core
import           Process.Graph

d :: Process ()
d = coinM 0.9
        (coinM 0.5
            (delay 1)
            (delay 2))
        failProcess

diamond :: Graph
diamond = toGraph [1 .. 4] [(1, 2), (1, 3), (2, 3), (2, 4), (3, 4)]

exact :: [Rational]
exact = [0 % 1
        ,0 % 1
        ,58239 % 160000
        ,386289 % 800000
        ,12069 % 100000
        ,2187 % 800000
        , 729 % 800000
        ]

exactMap :: Map (Maybe Double) Double
exactMap =
    let p = fromRational $ 1 - sum exact
    in  foldl' f (Map.singleton Nothing p) $ zip [(0 :: Int) ..] exact
  where
    f m (i, e) = Map.insert (Just $ fromIntegral i) (fromRational e) m

example :: Int -> IO ()
example n = do
    putStrLn "exact result:"
    putStrLn ""
    f exactMap

    putStrLn ""
    putStrLn ""
    putStrLn $ "simulating " ++ show n ++ " times:"
    putStrLn ""
    m <- histogram n diamond 1 4 d
    f m
  where
    f :: Map (Maybe Double) Double -> IO ()
    f m = forM_ (Map.toList m) $ \(mt, p) -> do
        let s = maybe "---" (printf "%1f") mt
        printf "%s: %7.5f\n" s p
