{-# LANGUAGE NumericUnderscores  #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import           Control.Concurrent.MVar
import           Control.Concurrent (forkIO, killThread, threadDelay)
import           Control.Exception (AsyncException (..), catch, throwIO, mask)
import           Data.Functor (void)
import           Data.Bits
import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC

import           System.Win32
import           System.Win32.NamedPipes

import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck (testProperty)
import           Test.QuickCheck
import           Test.QuickCheck.Instances.ByteString () 

main :: IO ()
main = defaultMain tests

pipeName :: String
pipeName = "\\\\.\\pipe\\nWin32-named-pipes-test"

tests :: TestTree
tests =
  testGroup "Win32-named-pipes"
  [ testCase "interruptible connectNamedPipe"
      test_interruptible_connectNamedPipe
  , testCase "interruptible readPipe"
      test_interruptible_readPipe
  , testCase "interruptible readPipe twice (synchronous)"
      test_interruptible_readPipe_sync
  , testCase "interruptible readPipe twice (concurrent)"
      test_interruptible_readPipe_conc
  , testProperty "writePipe & readPipe"         prop_WriteRead
  , testProperty "writePipe & readPipe (large)" prop_WriteReadLarge
  , testProperty "interruptible writePipe"      prop_interruptible_Write

  , testGroup "generators"
    [ testProperty "NonEmptyBS" prop_NonEmptyBS
    , testProperty "NonEmptyBS" prop_shrink_NonEmptyBS
    ]
  ]


-- | This test would fail (deadlock) if the blocking call `connectNamedPipe`,
-- would not be interruptible.
--
test_interruptible_connectNamedPipe :: IO ()
test_interruptible_connectNamedPipe = do
    hpipe <- createNamedPipe pipeName
                            pIPE_ACCESS_DUPLEX
                            (pIPE_TYPE_BYTE .|. pIPE_READMODE_BYTE)
                            pIPE_UNLIMITED_INSTANCES
                            512
                            512
                            0
                            Nothing
    tid <- forkIO (connectNamedPipe hpipe Nothing)
    threadDelay 100
    killThread tid
    closePipe hpipe

-- | Check if 'readPipe'`is interruptible
--
test_interruptible_readPipe :: IO ()
test_interruptible_readPipe = do
    hpipe <- createNamedPipe pipeName
                             pIPE_ACCESS_DUPLEX
                             (pIPE_TYPE_BYTE .|. pIPE_READMODE_BYTE)
                             pIPE_UNLIMITED_INSTANCES
                             512
                             512
                             0
                             Nothing
    hpipe' <- createFile pipeName
                         gENERIC_READ
                         fILE_SHARE_NONE
                         Nothing
                         oPEN_EXISTING
                         fILE_ATTRIBUTE_NORMAL
                         Nothing
    tid <- forkIO (void $ readPipe hpipe' 1)
    threadDelay 100
    killThread tid
    closePipe hpipe'
    closePipe hpipe

-- | Interrupt two consecutive reads.
--
test_interruptible_readPipe_sync :: IO ()
test_interruptible_readPipe_sync = do
    hpipe <- createNamedPipe pipeName
                             pIPE_ACCESS_DUPLEX
                             (pIPE_TYPE_BYTE .|. pIPE_READMODE_BYTE)
                             pIPE_UNLIMITED_INSTANCES
                             512
                             512
                             0
                             Nothing
    hpipe' <- createFile pipeName
                        gENERIC_READ
                        fILE_SHARE_NONE
                        Nothing
                        oPEN_EXISTING
                        fILE_ATTRIBUTE_NORMAL
                        Nothing
    tid <- forkIO (void $ readPipe hpipe' 1)
    threadDelay 100
    killThread tid
    tid' <- forkIO (void $ readPipe hpipe' 1)
    threadDelay 100
    killThread tid'
    closePipe hpipe'
    closePipe hpipe


-- | Interrupt two simultanous reads.
--
test_interruptible_readPipe_conc :: IO ()
test_interruptible_readPipe_conc = do
    hpipe <- createNamedPipe pipeName
                             pIPE_ACCESS_DUPLEX
                             (pIPE_TYPE_BYTE .|. pIPE_READMODE_BYTE)
                             pIPE_UNLIMITED_INSTANCES
                             512
                             512
                             0
                             Nothing
    hpipe' <- createFile pipeName
                         gENERIC_READ
                         fILE_SHARE_NONE
                         Nothing
                         oPEN_EXISTING
                         fILE_ATTRIBUTE_NORMAL
                         Nothing
    tid  <- forkIO (void $ readPipe hpipe' 1)
    tid' <- forkIO (void $ readPipe hpipe' 1)
    threadDelay 100
    killThread tid
    killThread tid'
    closePipe hpipe'
    closePipe hpipe


-- | Small non-empty 'ByteString's.
--
data NonEmptyBS = NonEmptyBS { getNonEmptyBS :: ByteString }
  deriving (Eq, Show)

instance Arbitrary NonEmptyBS where
    arbitrary = do
      bs <- arbitrary
      if BS.null bs
        then do
          -- generate a non empty string
          NonEmpty s <- arbitrary
          pure (NonEmptyBS $ BSC.pack s)
        else pure (NonEmptyBS bs)

    shrink (NonEmptyBS bs) =
      [ NonEmptyBS bs'
      | bs' <- shrink bs
      , not (BS.null bs')
      ]

prop_NonEmptyBS :: NonEmptyBS -> Bool
prop_NonEmptyBS (NonEmptyBS bs) = not (BS.null bs)

prop_shrink_NonEmptyBS :: NonEmptyBS -> Bool
prop_shrink_NonEmptyBS = all (\(NonEmptyBS bs) -> not (BS.null bs)) . shrink

-- | Large non-empty 'ByteString's, up to 2.5MB.
--
data LargeNonEmptyBS = LargeNonEmptyBS { getLargeNonEmptyBS :: ByteString }
  deriving (Show, Eq)

instance Arbitrary LargeNonEmptyBS where
    arbitrary = LargeNonEmptyBS . getNonEmptyBS <$> resize 2_500_000 arbitrary
    shrink (LargeNonEmptyBS bs) =
      map (LargeNonEmptyBS . getNonEmptyBS)
        (shrink $ NonEmptyBS bs)

test_WriteRead :: ByteString -> Property
test_WriteRead bs = ioProperty (do
    hRead <- createNamedPipe pipeName
                             pIPE_ACCESS_DUPLEX
                             (pIPE_TYPE_BYTE .|. pIPE_READMODE_BYTE)
                             pIPE_UNLIMITED_INSTANCES
                             maxBound
                             maxBound
                             0
                             Nothing
    hWrite <- createFile pipeName
                         (gENERIC_READ .|. gENERIC_WRITE)
                         fILE_SHARE_NONE
                         Nothing
                         oPEN_EXISTING
                         fILE_ATTRIBUTE_NORMAL
                         Nothing
    _ <- writePipe hWrite bs
    bs' <- readPipe hRead (BS.length bs)
    closePipe hRead
    closePipe hWrite
    pure (bs === bs'))

prop_WriteRead :: NonEmptyBS -> Property
prop_WriteRead = test_WriteRead . getNonEmptyBS

prop_WriteReadLarge :: LargeNonEmptyBS -> Property
prop_WriteReadLarge = test_WriteRead . getLargeNonEmptyBS


prop_interruptible_Write :: Property
prop_interruptible_Write = ioProperty $ do
    let bs = BSC.pack $ replicate 100 'a'
    v <- newEmptyMVar

    -- create a pipe which will block after writing 1 byte
    hRead <- createNamedPipe pipeName
                             pIPE_ACCESS_DUPLEX
                             (pIPE_TYPE_BYTE .|. pIPE_READMODE_BYTE)
                             pIPE_UNLIMITED_INSTANCES
                             1
                             1
                             0
                             Nothing
    hWrite <- createFile pipeName
                         (gENERIC_READ .|. gENERIC_WRITE)
                         fILE_SHARE_NONE
                         Nothing
                         oPEN_EXISTING
                         fILE_ATTRIBUTE_NORMAL
                         Nothing

    tid <- mask $ \unmask -> forkIO $ void $
      unmask (writePipe hWrite bs)
        `catch` \(e :: AsyncException) -> putMVar v e >> throwIO e

    killThread tid
    closePipe hRead
    closePipe hWrite

    (Just ThreadKilled ==) <$> tryReadMVar v
