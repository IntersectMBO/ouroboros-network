{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans -Wno-incomplete-uni-patterns #-}

module Test.Ouroboros.Storage.ImmutableDB.Primary (
    tests
  ) where

import           Data.Functor ((<&>))
import           Data.Maybe (fromJust)
import           Data.Proxy (Proxy (..))
import           Data.Vector.Unboxed (Vector)
import qualified Data.Vector.Unboxed as V

import           Test.QuickCheck
import           Test.QuickCheck.Monadic (monadicIO, run)
import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.QuickCheck (testProperty)

import           Ouroboros.Consensus.Block (ConvertRawHash (..))
import           Ouroboros.Consensus.Util.IOLike (try)

import           Ouroboros.Consensus.Storage.FS.API
import           Ouroboros.Consensus.Storage.FS.API.Types
import           Ouroboros.Consensus.Storage.ImmutableDB.Chunks
import           Ouroboros.Consensus.Storage.ImmutableDB.Chunks.Internal
                     (RelativeSlot (..), maxRelativeIndex)
import           Ouroboros.Consensus.Storage.ImmutableDB.Impl.Index.Primary
                     (PrimaryIndex, SecondaryOffset)
import qualified Ouroboros.Consensus.Storage.ImmutableDB.Impl.Index.Primary as Primary
import qualified Ouroboros.Consensus.Storage.ImmutableDB.Impl.Index.Secondary as Secondary
import           Ouroboros.Consensus.Storage.ImmutableDB.Impl.Types (BlockOrEBB)
import           Ouroboros.Consensus.Storage.ImmutableDB.Impl.Validation
                     (ShouldBeFinalised (..), reconstructPrimaryIndex)

import           Test.Util.FS.Sim.MockFS (HandleMock)
import qualified Test.Util.FS.Sim.MockFS as Mock
import qualified Test.Util.FS.Sim.STM as Sim
import           Test.Util.Orphans.Arbitrary ()

import           Test.Ouroboros.Storage.TestBlock (TestBlock)

{------------------------------------------------------------------------------
  The tests
------------------------------------------------------------------------------}

tests :: TestTree
tests = testGroup "PrimaryIndex"
    [ testProperty "isFilledSlot iff in filledSlots"          prop_filledSlots_isFilledSlot
    , testProperty "write/load roundtrip"                     prop_write_load
    , testProperty "open+appendOffsets/load roundtrip"        prop_open_appendOffsets_load
    , testProperty "truncateToSlotFS/truncateToSlot"          prop_truncateToSlotFS_truncateToSlot
    , testProperty "readFirstFilledSlot/load+firstFilledSlot" prop_readFirstFilledSlot_load_firstFilledSlot
    , testProperty "reconstructPrimaryIndex"                  prop_reconstructPrimaryIndex
    ]

{------------------------------------------------------------------------------
  The properties
------------------------------------------------------------------------------}

prop_filledSlots_isFilledSlot :: TestPrimaryIndex -> Property
prop_filledSlots_isFilledSlot (TestPrimaryIndex chunkInfo chunk idx _slot) = conjoin
    [     Primary.isFilledSlot idx slot
      === (slot `elem` Primary.filledSlots chunkInfo idx)
    | slot <- slots
    ]
  where
    slots :: [RelativeSlot]
    slots | totalSlots == 0 = []
          | otherwise       = map (mkRelativeSlot chunkInfo chunk) [0..totalSlots-1]
    totalSlots = Primary.slots idx

prop_write_load :: TestPrimaryIndex -> Property
prop_write_load (TestPrimaryIndex _chunkInfo chunk index _slot) =
    monadicIO $ run $ runFS prop
  where
    prop :: HasFS IO h -> IO Property
    prop hasFS = do
      Primary.write hasFS chunk index
      index' <- Primary.load (Proxy @TestBlock) hasFS chunk
      return $ index === index'

prop_open_appendOffsets_load :: TestPrimaryIndex -> Property
prop_open_appendOffsets_load (TestPrimaryIndex _chunkInfo chunk index _slot) =
    monadicIO $ run $ runFS prop
  where
    prop :: HasFS IO h -> IO Property
    prop hasFS = do
      pHnd <- Primary.open hasFS chunk MustBeNew
      -- Don't write the first offset, which is always 0; it is written by
      -- 'Primary.open'.
      Primary.appendOffsets hasFS pHnd (drop 1 (Primary.toSecondaryOffsets index))
      index' <- Primary.load (Proxy @TestBlock) hasFS chunk
      return $ index === index'

prop_truncateToSlotFS_truncateToSlot :: TestPrimaryIndex -> Property
prop_truncateToSlotFS_truncateToSlot (TestPrimaryIndex chunkInfo chunk index slot) =
    monadicIO $ run $ runFS prop
  where
    prop :: HasFS IO h -> IO Property
    prop hasFS = do
      Primary.write hasFS chunk index
      Primary.truncateToSlotFS hasFS chunk slot
      index' <- Primary.load (Proxy @TestBlock) hasFS chunk
      return $ Primary.truncateToSlot chunkInfo slot index === index'

prop_readFirstFilledSlot_load_firstFilledSlot :: TestPrimaryIndex -> Property
prop_readFirstFilledSlot_load_firstFilledSlot (TestPrimaryIndex chunkInfo chunk index _slot) =
    monadicIO $ run $ runFS prop
  where
    prop :: HasFS IO h -> IO Property
    prop hasFS = do
      Primary.write hasFS chunk index
      mbFirstFilledsLot <-
        Primary.readFirstFilledSlot (Proxy @TestBlock) hasFS chunkInfo chunk
      return $ mbFirstFilledsLot === Primary.firstFilledSlot chunkInfo index

{------------------------------------------------------------------------------
  reconstructPrimaryIndex
------------------------------------------------------------------------------}

-- | DummyBlock to define an instance of 'ConvertRawHash' with a fixed hash
-- size.
data DummyBlock

-- | Only 'hashSize' is used.
instance ConvertRawHash DummyBlock where
  hashSize    _ = 32
  toRawHash   _ = error "not used in the tests"
  fromRawHash _ = error "not used in the tests"

prop_reconstructPrimaryIndex :: TestPrimaryIndex -> Property
prop_reconstructPrimaryIndex (TestPrimaryIndex chunkInfo chunk primaryIndex _slot) =
    counterexample ("filledSlots  : " <> show filledSlots)                      $
    counterexample ("blocksOrEBBs : " <> show blockOrEBBs)                      $
    counterexample ("relativeSlots: " <> show (map toRelativeSlot blockOrEBBs)) $
    counterexample ("primaryIndex': " <> show primaryIndex')                    $
    reconstructedPrimaryIndex === primaryIndex'
  where
    reconstructedPrimaryIndex :: PrimaryIndex
    reconstructedPrimaryIndex =
      reconstructPrimaryIndex
        (Proxy @DummyBlock)
        chunkInfo
        ShouldNotBeFinalised
        chunk
        blockOrEBBs

    -- Remove empty trailing slots because we don't reconstruct them
    primaryIndex' :: PrimaryIndex
    primaryIndex' = case Primary.lastFilledSlot chunkInfo primaryIndex of
      Just slot -> Primary.truncateToSlot chunkInfo slot primaryIndex
      -- Index is empty, use the minimal empty index without any trailing
      -- slots
      Nothing   -> fromJust $ Primary.mk chunk [0]

    filledSlots :: [RelativeSlot]
    filledSlots = Primary.filledSlots chunkInfo primaryIndex

    blockOrEBBs :: [BlockOrEBB]
    blockOrEBBs =
      [ chunkSlotToBlockOrEBB chunkInfo (UnsafeChunkSlot chunk relSlot)
      | relSlot <- filledSlots
      ]

    -- This emulates what 'reconstructPrimaryIndex' does internally
    toRelativeSlot :: BlockOrEBB -> RelativeSlot
    toRelativeSlot = chunkRelative . chunkSlotForBlockOrEBB chunkInfo

{------------------------------------------------------------------------------
  Helpers
------------------------------------------------------------------------------}

runFS :: (HasFS IO HandleMock -> IO Property) -> IO Property
runFS m = try (Sim.runSimFS Mock.empty m) >>= \case
    Left  e           -> fail (prettyFsError e)
    Right (p, mockFS) -> return $ counterexample (Mock.pretty mockFS) p

{------------------------------------------------------------------------------
  Generators
------------------------------------------------------------------------------}

data TestPrimaryIndex = TestPrimaryIndex {
      testChunkInfo    :: ChunkInfo
    , testChunkNo      :: ChunkNo
    , testPrimaryIndex :: PrimaryIndex

      -- | For tests that need it, a random slot for this thunk
      --
      -- We guarantee that the 'relativeChunkNo' matches 'testChunkNo' and is
      -- within the bounds set by the 'ChunkSize' of the chunk (although it
      -- may still be past the actual number of entries in the index).
    , testRandomSlot   :: RelativeSlot
    }
  deriving (Show)

instance Arbitrary TestPrimaryIndex where
  arbitrary = do
      chunkSize <- arbitrary
      let chunkInfo = singleChunkInfo chunkSize
      -- The chunk number is not very relevant here; a single primary index
      -- is anyway for a single chunk and hence a single chunk size.
      chunk     <- arbitrary
      nbOffsets <- fromIntegral <$> choose (1, maxRelativeIndex chunkSize)
      offsets   <- go nbOffsets 0 [] <&> \offsets ->
                     case Primary.mk chunk offsets of
                       Nothing    -> error $ "invalid offsets: " <> show offsets
                       Just index -> index
      slot      <- mkRelativeSlot chunkInfo chunk <$>
                     choose (0, maxRelativeIndex chunkSize)
      return $ TestPrimaryIndex chunkInfo chunk offsets slot
    where
      -- All entries in the secondary index will have the same size
      offsetSize = Secondary.entrySize (Proxy @DummyBlock)

      go :: Int -> SecondaryOffset -> [SecondaryOffset]
         -> Gen [SecondaryOffset]
      go 0 prev acc = return $ reverse (prev:acc)
      go n prev acc = arbitrary >>= \repeatLast ->
        if repeatLast then
          go (n - 1) prev (prev:acc)
        else do
          go (n - 1) (prev + offsetSize) (prev:acc)

  -- Shrinking will
  --
  -- * Remove entries from the end of index
  -- * Shrink the 'ChunkNo'
  shrink test = concat [
        flip map (shrinkIndex $ testPrimaryIndex test) $ \i -> test {
            testPrimaryIndex = i
          }

      , flip map (shrink $ testChunkNo test) $ \chunkNo' -> test {
            testChunkNo      = chunkNo'
          , testPrimaryIndex = (testPrimaryIndex test) {
                                   Primary.primaryIndexChunkNo = chunkNo'
                                 }
          , testRandomSlot   = (testRandomSlot test) {
                                   relativeSlotChunkNo = chunkNo'
                                 }
          }
      ]
    where
      shrinkIndex :: PrimaryIndex -> [PrimaryIndex]
      shrinkIndex i =
          map (\os -> i { Primary.primaryIndexOffsets = os }) $
            shrinkOffsets (Primary.primaryIndexOffsets i)

      -- The generator is careful to insert values of 'offsetSize'
      -- We should maintain this invariant here, so we just drop elements
      -- from the end of the list.
      shrinkOffsets :: Vector SecondaryOffset -> [Vector SecondaryOffset]
      shrinkOffsets = map V.fromList
                    . (\(0:xs) -> map (0:) $ dropOne xs)
                    . V.toList

      dropOne :: [a] -> [[a]]
      dropOne [] = []
      dropOne xs = [reverse . tail . reverse $ xs]
