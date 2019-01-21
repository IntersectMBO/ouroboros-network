{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Miscellaneous utilities
module Ouroboros.Consensus.Util (
    Dict(..)
  , Some(..)
  , foldlM'
  , repeatedly
  , repeatedlyM
  , chunks
  , byteStringChunks
  , lazyByteStringChunks
  , whenJust
  , checkThat
  , pickOne
  , markLast
  , lastMaybe
  ) where

import qualified Data.ByteString as Strict
import qualified Data.ByteString.Lazy as Lazy
import           Data.Kind (Constraint)
import           Data.List (foldl')
import           GHC.Stack

data Dict (a :: Constraint) where
    Dict :: a => Dict a

data Some (f :: k -> *) where
    Some :: f a -> Some f

foldlM' :: forall m a b. Monad m => (b -> a -> m b) -> b -> [a] -> m b
foldlM' f = go
  where
    go :: b -> [a] -> m b
    go !acc []     = return acc
    go !acc (x:xs) = f acc x >>= \acc' -> go acc' xs

repeatedly :: (a -> b -> b) -> ([a] -> b -> b)
repeatedly = flip . foldl' . flip

repeatedlyM :: Monad m => (a -> b -> m b) -> ([a] -> b -> m b)
repeatedlyM = flip . foldlM' . flip

chunks :: Int -> [a] -> [[a]]
chunks _ [] = []
chunks n xs = let (chunk, xs') = splitAt n xs
              in chunk : chunks n xs'

byteStringChunks :: Int -> Strict.ByteString -> [Strict.ByteString]
byteStringChunks n = map Strict.pack . chunks n . Strict.unpack

lazyByteStringChunks :: Int -> Lazy.ByteString -> [Lazy.ByteString]
lazyByteStringChunks n bs
  | Lazy.null bs = []
  | otherwise    = let (chunk, bs') = Lazy.splitAt (fromIntegral n) bs
                   in chunk : lazyByteStringChunks n bs'

whenJust :: Applicative f => Maybe a -> (a -> f ()) -> f ()
whenJust (Just x) f = f x
whenJust Nothing _  = pure ()

-- | Assertion
--
-- Variation on 'assert' for use in testing code.
checkThat :: (Show a, Monad m)
          => String
          -> (a -> Bool)
          -> a
          -> m ()
checkThat label prd a
  | prd a     = return ()
  | otherwise = error $ label ++ " failed on " ++ show a ++ "\n"
                     ++ prettyCallStack callStack

-- | All possible ways to pick on element from a list, preserving order
--
-- > pickOne [1,2,3] = [ ([], 1, [2, 3])
-- >                   , ([1], 2, [3])
-- >                   , ([1,2], 3, [])
-- >                   ]
pickOne :: [a] -> [([a], a, [a])]
pickOne []     = []
pickOne (x:xs) = ([], x, xs)
               : map (\(as, b, cs) -> (x:as, b, cs)) (pickOne xs)

-- | Mark the last element of the list as 'Right'
markLast :: [a] -> [Either a a]
markLast [] = []
markLast xs = let (y:ys) = reverse xs
              in reverse $ Right y : map Left ys

lastMaybe :: [a] -> Maybe a
lastMaybe []     = Nothing
lastMaybe [x]    = Just x
lastMaybe (_:xs) = lastMaybe xs
