{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase   #-}

module Main where

import BenchBlock
import Data.Word (Word64)
import Ouroboros.Network.AnchoredFragment (AnchoredFragment)
import Ouroboros.Network.AnchoredFragment qualified as AF
import Ouroboros.Network.Block
import Test.Tasty.Bench

main :: IO ()
main = defaultMain
  [ bgroup "Intersect length k fragment with..."
    [ bench "same fragment" $
        whnf (intersectionPoint lengthKFragment) lengthKFragment,
      bench "slightly longer extension fragment" $
        whnf (intersectionPoint lengthKFragment) $
          extendFragment 0 10 lengthKFragment,
      bench "much longer extension fragment" $
        whnf (intersectionPoint lengthKFragment) length4KFragment,
      bench "fragment with same length and anchor, but otherwise disjoint" $
        whnf (intersectionPoint lengthKFragment) $
          extendFragment 1 secParam $ AF.Empty AF.AnchorGenesis,
      bench "fragment with anchor, but otherwise disjoint and much longer" $
        whnf (intersectionPoint lengthKFragment) $
          extendFragment 1 (4 * secParam) $ AF.Empty AF.AnchorGenesis
    ]
  , bgroup "Intersect length 4*k fragment with..."
    [ bench "same fragment" $
        whnf (intersectionPoint length4KFragment) length4KFragment,
      bench "fragment with same length and anchor, but otherwise disjoint" $
        whnf (intersectionPoint length4KFragment) $
          extendFragment 1 (4 * secParam) $ AF.Empty AF.AnchorGenesis
    ]
  ]

intersectionPoint ::
     HasHeader blk
  => AnchoredFragment blk
  -> AnchoredFragment blk
  -> Maybe (Point blk)
intersectionPoint f0 f1 = case AF.intersectionPoint f0 f1 of
  Just !pt -> Just pt
  Nothing  -> Nothing

{-------------------------------------------------------------------------------
  Benchmark fragments
-------------------------------------------------------------------------------}

-- | Cardano mainnet security parameter (@k@).
secParam :: Int
secParam = 2160

-- | A fragment of length 'secParam'. This is the usual length of the currently
-- selected chain in the node.
lengthKFragment :: AnchoredFragment BenchBlock
lengthKFragment = extendFragment 0 secParam $ AF.Empty AF.AnchorGenesis

-- | A fragment of length @4 * 'secParam'@. This is the usual length of a
-- candidate fragment in the ChainSync client while syncing, as it includes both
-- the current selection as well as an additional forecast window worth of
-- headers, which is @3*k@ on average (assuming high chain density).
length4KFragment :: AnchoredFragment BenchBlock
length4KFragment = extendFragment 0 (3 * secParam) lengthKFragment

extendFragment ::
     Word64 -- ^ Fork number to use (see 'mkBenchBlock').
  -> Int -- ^ By how many blocks to extend the fragment.
  -> AnchoredFragment BenchBlock
  -> AnchoredFragment BenchBlock
extendFragment forkNo = go
  where
    go n frag
      | n == 0    = frag
      | otherwise = go (n - 1) $ frag AF.:> succBlock forkNo (AF.headAnchor frag)

succBlock :: Word64 -> AF.Anchor BenchBlock -> BenchBlock
succBlock forkNo = \case
  AF.AnchorGenesis        -> mkBenchBlock (SlotNo 0) (BlockNo 1) forkNo
  AF.Anchor sno _hash bno -> mkBenchBlock (succ sno) (succ bno)  forkNo
