{-# LANGUAGE ScopedTypeVariables #-}

-- | Utility functions on chains
--
-- Intended for qualified import
-- > import qualified Ouroboros.Consensus.Util.Chain as Chain
module Ouroboros.Consensus.Util.Chain (
    lastSlot
  , commonPrefix
  , upToSlot
  , dropLastBlocks
  , forksAtMostKBlocks
  , intersectionSlot
  ) where

import           Data.Foldable (foldl')
import           Data.Sequence (Seq (..))
import           Data.Set (Set)
import qualified Data.Set as Set

import           Ouroboros.Network.Block
import           Ouroboros.Network.Chain

{-------------------------------------------------------------------------------
  Utility functions on chains
-------------------------------------------------------------------------------}

lastSlot :: HasHeader b => Chain b -> Maybe Slot
lastSlot Genesis  = Nothing
lastSlot (_ :> b) = Just $ blockSlot b

commonPrefix :: Eq b => Chain b -> Chain b -> Chain b
commonPrefix c d = chainFromSeq $ go (chainToSeq c) (chainToSeq d)
  where
    go :: Eq b => Seq b -> Seq b -> Seq b
    go Empty      _          = Empty
    go _          Empty      = Empty
    go (x :<| xs) (y :<| ys)
        | x == y             = x :<| go xs ys
        | otherwise          = Empty

upToSlot :: HasHeader b => Slot -> Chain b -> Chain b
upToSlot slot = go
  where
    go Genesis = Genesis
    go bs@(cs :> b)
        | blockSlot b <= slot = bs
        | otherwise           = go cs

dropLastBlocks :: Int -> Chain b -> Chain b
dropLastBlocks _ Genesis = Genesis
dropLastBlocks i bs@(cs :> _)
    | i <= 0 = bs
    | otherwise = dropLastBlocks (i - 1) cs

forksAtMostKBlocks :: forall b. HasHeader b
                   => Word     -- ^ How many blocks can it fork?
                   -> Chain b  -- ^ Our chain.
                   -> Chain b  -- ^ Their chain.
                   -> Bool     -- ^ Indicates whether their chain forks at most the specified number of blocks.
forksAtMostKBlocks k ours = go
  where
    go Genesis   = GenesisHash `Set.member` forkingPoints
    go (bs :> b) = if BlockHash (blockHash b) `Set.member` forkingPoints
        then True
        else go bs

    -- we can roll back at most k blocks
    forkingPoints :: Set (Hash b)
    forkingPoints = takeR (chainToSeq' ours) $ fromIntegral (k + 1)

    chainToSeq' :: Chain b -> Seq (Hash b)
    chainToSeq' c = GenesisHash :<| (BlockHash . blockHash <$> chainToSeq c)

    takeR :: Ord a => Seq a -> Int -> Set a
    takeR Empty      _ = Set.empty
    takeR (xs :|> x) l
        | l <= 0       = Set.empty
        | otherwise    = Set.insert x $ takeR xs $ l - 1

intersectionSlot :: (Eq b, HasHeader b) => Chain b -> Chain b -> Maybe Slot
intersectionSlot c d = lastSlot $ commonPrefix c d

{-------------------------------------------------------------------------------
  Internal auxiliary
-------------------------------------------------------------------------------}

chainFromSeq :: Seq b -> Chain b
chainFromSeq = foldl' (:>) Genesis

chainToSeq :: Chain b -> Seq b
chainToSeq = foldChain (:|>) Empty
