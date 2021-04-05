{-# LANGUAGE ScopedTypeVariables #-}

-- | Utility functions on chains
--
-- Intended for qualified import
-- > import qualified Test.Util.MockChain as Chain
module Test.Util.MockChain (
    commonPrefix
  , dropLastBlocks
  , lastSlot
  ) where

import           Data.Foldable (foldl')
import           Data.Sequence.Strict (StrictSeq (..))

import           Ouroboros.Network.MockChain.Chain

import           Ouroboros.Consensus.Block

{-------------------------------------------------------------------------------
  Utility functions on chains
-------------------------------------------------------------------------------}

lastSlot :: HasHeader b => Chain b -> Maybe SlotNo
lastSlot Genesis  = Nothing
lastSlot (_ :> b) = Just $ blockSlot b

commonPrefix :: Eq b => Chain b -> Chain b -> Chain b
commonPrefix c d = chainFromSeq $ go (chainToSeq c) (chainToSeq d)
  where
    go :: Eq b => StrictSeq b -> StrictSeq b -> StrictSeq b
    go Empty      _          = Empty
    go _          Empty      = Empty
    go (x :<| xs) (y :<| ys)
        | x == y             = x :<| go xs ys
        | otherwise          = Empty

dropLastBlocks :: Int -> Chain b -> Chain b
dropLastBlocks _ Genesis = Genesis
dropLastBlocks i bs@(cs :> _)
    | i <= 0 = bs
    | otherwise = dropLastBlocks (i - 1) cs

{-------------------------------------------------------------------------------
  Internal auxiliary
-------------------------------------------------------------------------------}

chainFromSeq :: StrictSeq b -> Chain b
chainFromSeq = foldl' (:>) Genesis

chainToSeq :: Chain b -> StrictSeq b
chainToSeq = foldChain (:|>) Empty
