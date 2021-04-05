{-# LANGUAGE RecordWildCards #-}

module Test.Util.ChunkInfo (SmallChunkInfo (..)) where

import           Test.QuickCheck

import           Ouroboros.Consensus.Storage.ImmutableDB.Chunks

{-------------------------------------------------------------------------------
  ChunkInfo
-------------------------------------------------------------------------------}

data SmallChunkInfo = SmallChunkInfo ChunkInfo
  deriving (Show)

instance Arbitrary SmallChunkInfo where
  arbitrary = do
      numRegularBlocks   <- choose (5, 15)
      chunkCanContainEBB <- arbitrary
      return $ SmallChunkInfo $ singleChunkInfo $ ChunkSize{..}

  -- Intentionally no shrinker, as shrinking the epoch size independent from
  -- the rest of the commands may lead to a non-sensical test
