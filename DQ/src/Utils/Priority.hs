{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor  #-}

module Utils.Priority
    ( PriorityQueue
    , empty
    , enqueue
    , dequeue
    ) where

import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Sequence (Seq (..))
import qualified Data.Sequence as Seq

newtype PriorityQueue a b = PQ (Map a (Seq b))
    deriving (Show, Functor, Foldable)

empty :: PriorityQueue a b
empty = PQ Map.empty

enqueue :: Ord a => a -> b -> PriorityQueue a b -> PriorityQueue a b
enqueue a b (PQ m) = PQ $ Map.insertWith (flip (Seq.><)) a (Seq.singleton b) m

dequeue :: Ord a => PriorityQueue a b -> Maybe (a, b, PriorityQueue a b)
dequeue (PQ m) = case Map.minViewWithKey m of
    Nothing            -> Nothing
    Just ((a, bs), m') -> case bs of
        Empty       -> error "impossible case"
        b :<| Empty -> Just (a, b, PQ m')
        b :<| bs'   -> Just (a, b, PQ $ Map.insert a bs' m')
