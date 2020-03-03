{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Test.Ouroboros.Storage.ImmutableDB.Primary (tests) where

import           Data.Functor ((<&>))

import           Test.QuickCheck
import           Test.QuickCheck.Monadic (monadicIO, run)
import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.QuickCheck (testProperty)

import           Ouroboros.Consensus.Storage.Common
import           Ouroboros.Consensus.Storage.FS.API
import           Ouroboros.Consensus.Storage.FS.API.Types
import           Ouroboros.Consensus.Storage.ImmutableDB.Chunks
import           Ouroboros.Consensus.Storage.ImmutableDB.Chunks.Internal
                     (RelativeSlot (..))
import           Ouroboros.Consensus.Storage.ImmutableDB.Impl.Index.Primary
                     (PrimaryIndex, SecondaryOffset)
import qualified Ouroboros.Consensus.Storage.ImmutableDB.Impl.Index.Primary as Primary
import qualified Ouroboros.Consensus.Storage.ImmutableDB.Impl.Index.Secondary as Secondary

import           Test.Ouroboros.Storage.Util (tryFS)

import           Test.Util.FS.Sim.MockFS (HandleMock)
import qualified Test.Util.FS.Sim.MockFS as Mock
import qualified Test.Util.FS.Sim.STM as Sim
import           Test.Util.Orphans.Arbitrary ()

{------------------------------------------------------------------------------
  The tests
------------------------------------------------------------------------------}

tests :: TestTree
tests = testGroup "PrimaryIndex"
    [ testProperty "isFilledSlot iff in filledSlots" prop_filledSlots_isFilledSlot
    , testProperty "write/load roundtrip" prop_write_load
    , testProperty "open+appendOffsets/load roundtrip" prop_open_appendOffsets_load
    , testProperty "truncateToSlotFS/truncateToSlot" prop_truncateToSlotFS_truncateToSlot
    , testProperty "readFirstFilledSlot/load+firstFilledSlot" prop_readFirstFilledSlot_load_firstFilledSlot
    ]

{------------------------------------------------------------------------------
  The properties
------------------------------------------------------------------------------}

prop_filledSlots_isFilledSlot :: PrimaryIndex -> Property
prop_filledSlots_isFilledSlot idx = conjoin
    [ Primary.isFilledSlot idx slot === (slot `elem` Primary.filledSlots idx)
    | slot <- slots ]
  where
    slots :: [RelativeSlot]
    slots | totalSlots == 0 = []
          | otherwise       = map RelativeSlot [0..totalSlots-1]
    totalSlots = unEpochSize $ Primary.slots idx

prop_write_load :: PrimaryIndex -> Property
prop_write_load index = monadicIO $ run $ runFS prop
  where
    chunk = firstChunkNo

    prop :: HasFS IO h -> IO Property
    prop hasFS = do
      Primary.write hasFS chunk index
      index' <- Primary.load hasFS chunk
      return $ index === index'

prop_open_appendOffsets_load :: PrimaryIndex -> Property
prop_open_appendOffsets_load index = monadicIO $ run $ runFS prop
  where
    chunk = firstChunkNo

    prop :: HasFS IO h -> IO Property
    prop hasFS = do
      pHnd <- Primary.open hasFS chunk MustBeNew
      -- Don't write the first offset, which is always 0; it is written by
      -- 'Primary.open'.
      Primary.appendOffsets hasFS pHnd (drop 1 (Primary.toSecondaryOffsets index))
      index' <- Primary.load hasFS chunk
      return $ index === index'

prop_truncateToSlotFS_truncateToSlot :: PrimaryIndex -> RelativeSlot -> Property
prop_truncateToSlotFS_truncateToSlot index slot =
    monadicIO $ run $ runFS prop
  where
    chunk = firstChunkNo

    prop :: HasFS IO h -> IO Property
    prop hasFS = do
      Primary.write hasFS chunk index
      Primary.truncateToSlotFS hasFS chunk slot
      index' <- Primary.load hasFS chunk
      return $ Primary.truncateToSlot slot index === index'

prop_readFirstFilledSlot_load_firstFilledSlot :: PrimaryIndex -> Property
prop_readFirstFilledSlot_load_firstFilledSlot index =
    monadicIO $ run $ runFS prop
  where
    chunk = firstChunkNo

    prop :: HasFS IO h -> IO Property
    prop hasFS = do
      Primary.write hasFS chunk index
      mbFirstFilledsLot <- Primary.readFirstFilledSlot hasFS chunk
      return $ mbFirstFilledsLot === Primary.firstFilledSlot index

{------------------------------------------------------------------------------
  Helpers
------------------------------------------------------------------------------}

runFS :: (HasFS IO HandleMock -> IO Property) -> IO Property
runFS m = tryFS (Sim.runSimFS Mock.empty m) >>= \case
    Left  e           -> fail (prettyFsError e)
    Right (p, mockFS) -> return $ counterexample (Mock.pretty mockFS) p

{------------------------------------------------------------------------------
  Generators
------------------------------------------------------------------------------}

instance Arbitrary PrimaryIndex where
  arbitrary = sized $ \n -> do
      nbOffsets <- choose (1, n)
      go nbOffsets 0 [] <&> \offsets ->
        case Primary.mk offsets of
          Nothing    -> error $ "invalid offsets: " <> show offsets
          Just index -> index
    where
      -- All entries in the secondary index will have the same size
      offsetSize = Secondary.entrySize 32

      go :: Int -> SecondaryOffset -> [SecondaryOffset]
         -> Gen [SecondaryOffset]
      go 0 prev acc = return $ reverse (prev:acc)
      go n prev acc = arbitrary >>= \repeatLast ->
        if repeatLast then
          go (n - 1) prev (prev:acc)
        else do
          go (n - 1) (prev + offsetSize) (prev:acc)
