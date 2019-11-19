{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Test.Ouroboros.Storage.ImmutableDB.Primary (tests) where

import           Data.Coerce (coerce)
import           Data.Functor ((<&>))

import           Test.QuickCheck
import           Test.QuickCheck.Monadic (monadicIO, run)
import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.QuickCheck (testProperty)

import           Ouroboros.Storage.Common
import           Ouroboros.Storage.FS.API
import           Ouroboros.Storage.FS.API.Types
import           Ouroboros.Storage.ImmutableDB.Impl.Index.Primary (PrimaryIndex,
                     SecondaryOffset)
import qualified Ouroboros.Storage.ImmutableDB.Impl.Index.Primary as Primary
import qualified Ouroboros.Storage.ImmutableDB.Impl.Index.Secondary as Secondary
import           Ouroboros.Storage.ImmutableDB.Layout
import qualified Ouroboros.Storage.Util.ErrorHandling as EH

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
          | otherwise       = map coerce [0..totalSlots-1]
    totalSlots = Primary.slots idx

prop_write_load :: PrimaryIndex -> Property
prop_write_load index = monadicIO $ run $ runFS prop
  where
    epoch = 0

    prop :: HasFS IO h -> IO Property
    prop hasFS = do
      Primary.write hasFS epoch index
      index' <- Primary.load hasFS EH.exceptions epoch
      return $ index === index'

prop_open_appendOffsets_load :: PrimaryIndex -> Property
prop_open_appendOffsets_load index = monadicIO $ run $ runFS prop
  where
    epoch = 0

    prop :: HasFS IO h -> IO Property
    prop hasFS = do
      pHnd <- Primary.open hasFS epoch MustBeNew
      -- Don't write the first offset, which is always 0; it is written by
      -- 'Primary.open'.
      Primary.appendOffsets hasFS pHnd (drop 1 (Primary.toSecondaryOffsets index))
      index' <- Primary.load hasFS EH.exceptions epoch
      return $ index === index'

prop_truncateToSlotFS_truncateToSlot :: PrimaryIndex -> RelativeSlot -> Property
prop_truncateToSlotFS_truncateToSlot index slot =
    monadicIO $ run $ runFS prop
  where
    epoch = 0

    prop :: HasFS IO h -> IO Property
    prop hasFS = do
      Primary.write hasFS epoch index
      Primary.truncateToSlotFS hasFS epoch slot
      index' <- Primary.load hasFS EH.exceptions epoch
      return $ Primary.truncateToSlot slot index === index'

prop_readFirstFilledSlot_load_firstFilledSlot :: PrimaryIndex -> Property
prop_readFirstFilledSlot_load_firstFilledSlot index =
    monadicIO $ run $ runFS prop
  where
    epoch = 0

    prop :: HasFS IO h -> IO Property
    prop hasFS = do
      Primary.write hasFS epoch index
      mbFirstFilledsLot <- Primary.readFirstFilledSlot hasFS EH.exceptions epoch
      return $ mbFirstFilledsLot === Primary.firstFilledSlot index

{------------------------------------------------------------------------------
  Helpers
------------------------------------------------------------------------------}

runFS :: (HasFS IO HandleMock -> IO Property) -> IO Property
runFS m = tryFS (Sim.runSimFS EH.exceptions Mock.empty m) >>= \case
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
