{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE CPP                        #-}

{-- |

A mock FS implementation, suitable for testing.

--}

module Ouroboros.Storage.FS.Sim (
    -- * Mock FS implementation & monad
      SimFS
    , SimFSE
    , MockHandle(..)
    , MockFS(..)
    , newEmptyMockFS
    , runSimFS
    , prettyShowFS
    -- * Internals for white-box testing
    , FsTree(..)
    , index
    , touchFile
    -- * Testing examples
    , mockDemo
    , mockDemoScript
    , demoMockFS
    ) where

import           Control.Monad.Catch (MonadCatch (..), MonadMask (..),
                     MonadThrow (..), bracket)
import           Control.Monad.Except
import           Control.Monad.Reader

import           GHC.Stack

import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as B16
import           Data.ByteString.Builder (Builder, toLazyByteString)
import qualified Data.ByteString.Builder as BS
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy as BL
import           Data.Functor.Identity
import           Data.List (foldl')
import qualified Data.List as L
import           Data.Map (Map)
import qualified Data.Map.Strict as M
import           Data.Semigroup ((<>))
import           Data.Word (Word64)

import           System.IO (IOMode, SeekMode)
import qualified System.IO as IO

import           Ouroboros.Network.MonadClass
import           Ouroboros.Storage.FS.Class


{------------------------------------------------------------------------------
  The simulation-related types
------------------------------------------------------------------------------}

type SimFSE m = ExceptT FsError (SimFS m)

newtype SimFS m a =
    SimFS { unSimFs :: ReaderT (TVar m MockFS) m a }
    deriving ( Functor
             , Applicative
             , Monad
             , MonadThrow
             , MonadCatch
             , MonadMask
             )

instance MonadTrans SimFS where
    lift = SimFS . lift

newtype TrSimFS m a = TrSimFS (m a)
  deriving (Functor, Applicative, Monad)

instance MonadTrans TrSimFS where
    lift = TrSimFS

instance MonadFork m => MonadFork (SimFS m) where
  fork (SimFS f) = SimFS $ ReaderT $ \e -> fork (runReaderT f e)

instance (MonadFork (SimFS m) , MonadSTM m) => MonadSTM (SimFS m) where
  type Tr (SimFS m)      = TrSimFS (Tr m)
  type TVar (SimFS m)    = TVar m
  type TMVar (SimFS m)   = TMVar m
  type TBQueue (SimFS m) = TBQueue m

  atomically (TrSimFS t) = lift $ atomically t
  newTVar                = lift . newTVar
  readTVar               = lift . readTVar
  writeTVar t a          = lift $ writeTVar t a
  retry                  = lift retry

  newTMVar               = lift . newTMVar
  newTMVarIO             = lift . newTMVarIO
  newEmptyTMVar          = lift newEmptyTMVar
  newEmptyTMVarIO        = lift newEmptyTMVarIO
  takeTMVar              = lift . takeTMVar
  tryTakeTMVar           = lift . tryTakeTMVar
  putTMVar    t a        = lift $ putTMVar t a
  tryPutTMVar t a        = lift $ tryPutTMVar t a
  swapTMVar   t a        = lift $ swapTMVar t a
  readTMVar              = lift . readTMVar
  tryReadTMVar           = lift . tryReadTMVar
  isEmptyTMVar           = lift . isEmptyTMVar

  newTBQueue             = lift . newTBQueue
  readTBQueue            = lift . readTBQueue
  writeTBQueue q a       = lift $ writeTBQueue q a
#if MIN_VERSION_stm(2,5,0)
  lengthTBQueue          = lift . lengthTBQueue
#endif


{-------------------------------------------------------------------------------
  Mock FS types
-------------------------------------------------------------------------------}

data MockFS = MockFS {
    getMockFS :: FsTree ByteString
  } deriving Show


instance Show (MockHandle m) where
   show MockHandle{..} = "MockHandle { mockHandleFilePath = "
                      <> show mockHandleFilePath
                      <> ", mockHandleIOMode = "
                      <> show mockHandleIOMode
                      <> ", mockHandleOffset = <tvar> }"

type DiskOffset = Word64

-- | A simple representation of files and folders on disk, where leaves stores
-- not just the file names but also the actual content of these files.
data FsTree a =
      FileOnDisk a
    | FolderOnDisk (Map String (FsTree a))
    deriving (Show, Eq)

-- | A mock handle to a file on disk.
data MockHandle m = MockHandle {
      mockHandleFilePath :: FsPath
    , mockHandleIOMode   :: IOMode
    , mockHandleOffset   :: TVar m DiskOffset
    }

{------------------------------------------------------------------------------
 Mock FS implementation
------------------------------------------------------------------------------}

instance (MonadMask m, MonadSTM m) => HasFS (SimFSE m) where
    type FsHandle (SimFSE m) = MockHandle m
    data Buffer   (SimFSE m) = BufferMock !BS.ByteString

    dumpState  = mockDumpState
    newBuffer  = newBufferMock
    hOpen      = mockOpen
    hClose     = mockClose
    hSeek      = mockSeek
    hGet       = mockGet
    hPut       = mockPut
    hPutBuffer = mockPutBuffer
    hTruncate  = mockTruncate
    withFile   = mockWithFile

    createDirectory          = mockCreateDirectory
    createDirectoryIfMissing = mockCreateDirectoryIfMissing
    listDirectory            = mockListDirectory
    doesDirectoryExist       = mockDoesDirectoryExist
    doesFileExist            = mockDoesFileExist


modifyMockFS :: MonadSTM m
             => (MockFS -> ExceptT FsError (Tr m) (MockFS, b))
             -> SimFSE m b
modifyMockFS action = do
    fsVar <- getFsVar
    ExceptT $ lift $ atomically $ do
      fs  <- readTVar fsVar
      res <- runExceptT $ action fs
      case res of
        Left  err      -> return (Left err)
        Right (fs', b) -> writeTVar fsVar fs' >> return (Right b)


withMockFS :: MonadSTM m
           => (MockFS -> ExceptT FsError (Tr m) b)
           -> SimFSE m b
withMockFS action = modifyMockFS $ \fs -> (fs, ) <$> action fs


getFsVar :: Monad m => SimFSE m (TVar m MockFS)
getFsVar = lift $ SimFS $ ask


mockDumpState :: MonadSTM m => SimFSE m String
mockDumpState = do
    fsVar <- getFsVar
    fs <- atomically $ readTVar fsVar
    return $ prettyShowFS fs

throwFsError :: (HasCallStack, MonadError FsError m) => FsErrorType -> FsPath -> m a
throwFsError et fp = throwError (FsError et fp callStack)

-- | Mock implementation of 'hOpen'.
mockOpen :: (HasCallStack, MonadSTM m)
         => FsPath
         -> IOMode
         -> SimFSE m (MockHandle m)
mockOpen fp ioMode = modifyMockFS $ \fs -> do
    -- Check if the parent exists
    parent <- noteT (FsError FsResourceDoesNotExist fp callStack) $
                index (init fp) (getMockFS fs)

    -- Check if the file exist
    (fs', fileSize) <- case index [last fp] parent of
        Nothing | ioMode /= IO.ReadMode -> do
            let tree' = touchFile fp (getMockFS fs)
            return (fs { getMockFS = tree' }, 0)
        Just (FileOnDisk f   ) -> return (fs, BS.length f)
        Just (FolderOnDisk _ ) -> throwFsError FsResourceInappropriateType fp
        _ -> throwFsError FsResourceDoesNotExist fp

    let initialOffset
          | ioMode == IO.AppendMode = fileSize
          | otherwise = 0
    hnd <- MockHandle fp ioMode <$> newTVar (toEnum initialOffset)
    return (fs', hnd)

-- | Mock implementation of 'hClose', which is trivial.
mockClose :: Monad m => MockHandle m -> SimFSE m ()
mockClose _ = return ()

-- | Mock implementation of 'hSeek'
mockSeek :: forall m. (HasCallStack, MonadSTM m)
         => MockHandle m
         -> SeekMode
         -> Word64
         -> SimFSE m Word64
mockSeek (MockHandle fp _mode curOffsetVar) mode offset = withMockFS $ \fs ->
    case index fp (getMockFS fs) of
      Nothing -> throwFsError FsResourceDoesNotExist fp
      Just (FolderOnDisk _) -> throwFsError FsResourceInappropriateType fp
      Just (FileOnDisk block) -> do
        currentOffset <- readTVar curOffsetVar
        let offset' = case mode of
             IO.AbsoluteSeek -> offset
             IO.RelativeSeek -> currentOffset + offset
             IO.SeekFromEnd  -> (toEnum $ BS.length block) - offset
        checkOverflow offset' (BS.length block)
        writeTVar curOffsetVar offset'
        return offset'
  where
    checkOverflow :: DiskOffset -> Int -> ExceptT FsError (Tr m) ()
    checkOverflow newOffset blockSize
        | newOffset > toEnum blockSize = throwFsError FsReachedEOF fp
        | otherwise = return ()

mockGet :: (HasCallStack, MonadSTM m)
        => MockHandle m
        -> Int
        -> SimFSE m ByteString
mockGet (MockHandle fp _mode offsetVar) bytesToRead = withMockFS $ \fs -> do
    case index fp (getMockFS fs) of
      Nothing -> throwFsError FsResourceDoesNotExist fp
      Just (FolderOnDisk _) -> throwFsError FsResourceInappropriateType fp
      Just (FileOnDisk block) -> do
        offset <- readTVar offsetVar
        let r = BS.take bytesToRead . BS.drop (fromEnum offset) $ block
        writeTVar offsetVar (offset + toEnum bytesToRead)
        return r

touchFile :: HasCallStack => FsPath -> FsTree ByteString -> FsTree ByteString
touchFile fp =
    adjustIndex (init fp) $
        \case (FolderOnDisk m) -> newFile m
              _                ->
                  error $ "touchFile: found a file, not a folder for fp " <> show fp
  where
    newFile m = FolderOnDisk $ M.insert (last fp) (FileOnDisk mempty) m

replaceFileContent :: HasCallStack
                   => FsPath
                   -> ByteString
                   -> FsTree ByteString
                   -> FsTree ByteString
replaceFileContent fp content =
    adjustIndex fp $
        \case (FileOnDisk _) -> FileOnDisk content
              _              ->
                  error $ "replaceFileContent: found a folder, not a file for fp " <> show fp

mockPut :: MonadSTM m
        => MockHandle m
        -> Builder
        -> SimFSE m Word64
mockPut (MockHandle fp IO.ReadMode _) _ =
    throwFsError FsIllegalOperation fp
mockPut (MockHandle fp _mode handleOffsetVar) builder = modifyMockFS $ \fs -> do
    handleOffset <- readTVar handleOffsetVar
    case index fp (getMockFS fs) of
      Nothing -> throwFsError FsResourceDoesNotExist fp
      Just (FolderOnDisk _) -> throwFsError FsResourceInappropriateType fp
      Just (FileOnDisk block) -> do
        let (unchanged, toModify) = BS.splitAt (fromEnum handleOffset) block
            block' = unchanged <> toWrite <> BS.drop (BS.length toWrite) toModify
        -- Update the offset
        writeTVar handleOffsetVar (handleOffset + bytesWritten)
        return ((fs { getMockFS =
              replaceFileContent fp block' (getMockFS fs)
          }), bytesWritten)
  where
    toWrite      = BL.toStrict . toLazyByteString $ builder
    bytesWritten = toEnum $ BS.length toWrite

mockPutBuffer :: MonadSTM m
              => MockHandle m
              -> Buffer (SimFSE m)
              -> Builder
              -> SimFSE m Word64
mockPutBuffer hnd _buf builder = mockPut hnd builder

mockTruncate :: MonadSTM m
             => MockHandle m
             -> Word64
             -> SimFSE m ()
mockTruncate (MockHandle fp _ _) sz = modifyMockFS $ \fs ->
    case index fp (getMockFS fs) of
      Nothing -> throwFsError FsResourceDoesNotExist fp
      Just (FolderOnDisk _) -> throwFsError FsResourceInappropriateType fp
      Just (FileOnDisk block) -> do
        return ((fs { getMockFS =
            replaceFileContent fp (BS.take (fromEnum sz) block) (getMockFS fs)
          }), ())

mockWithFile :: (MonadMask m, MonadSTM m)
             => FsPath
             -> IOMode
             -> (MockHandle m -> SimFSE m r)
             -> SimFSE m r
mockWithFile fp ioMode = bracket (mockOpen fp ioMode) mockClose

{------------------------------------------------------------------------------
  Operations on directories
------------------------------------------------------------------------------}

mockCreateDirectoryIfMissing :: MonadSTM m
                             => Bool
                             -> FsPath
                             -> SimFSE m ()
mockCreateDirectoryIfMissing createParents path = modifyMockFS $ \fs -> do
    return (fs {
          getMockFS =
              mockCreateDirectoryIfMissingInternal createParents
                                                   path
                                                   (getMockFS fs)
      }, ())

mockCreateDirectoryIfMissingInternal :: Bool
                                     -> FsPath
                                     -> FsTree a
                                     -> FsTree a
mockCreateDirectoryIfMissingInternal createParents path0 files
  | createParents =
      -- returns the parents paths, but not the empty list [].
      -- >    inits ["a", "b", "c"]
      -- > == [[], ["a"], ["a", "b"], ["a", "b", "c"]]
      -- but we do want only the last 3 elements.
      let parents = tail (L.inits path0)
      in foldl (flip mockCreateDirectoryLenient) files parents
  | otherwise     = mockCreateDirectoryLenient path0 files

-- | Creates a directory and does not throw an error if it exists already.
mockCreateDirectoryLenient :: HasCallStack
                           => FsPath
                           -> FsTree a
                           -> FsTree a
mockCreateDirectoryLenient fp =
  adjustIndex (init fp) $
      \case FolderOnDisk m -> newDir m
            _ -> error $ "createDirectory: found file, not folder for fp " <> show fp
  where
    newDir m =
        case M.lookup (last fp) m of
             Nothing -> FolderOnDisk $ M.insert (last fp) (FolderOnDisk mempty) m
             Just _ -> FolderOnDisk m

mockCreateDirectoryStrict :: HasCallStack
                          => FsPath
                          -> FsTree a
                          -> Maybe (FsTree a)
mockCreateDirectoryStrict fp =
  adjustIndexF (init fp) $
      \case FolderOnDisk m -> newDir m
            _ -> error $ "createDirectory: found file, not folder for fp " <> show fp
  where
    newDir :: Map String (FsTree a) -> Maybe (FsTree a)
    newDir m =
        case M.lookup (last fp) m of
             Nothing -> Just $ FolderOnDisk $ M.insert (last fp) (FolderOnDisk mempty) m
             Just _  -> Nothing

mockCreateDirectory :: (HasCallStack, MonadSTM m)
                    => FsPath
                    -> SimFSE m ()
mockCreateDirectory dir = modifyMockFS $ \fs -> do
    _parent <- noteT (FsError FsResourceDoesNotExist dir callStack) $
                   index (init dir) (getMockFS fs)
    fs'     <- noteT (FsError FsResourceAlreadyExist dir callStack) $
                   mockCreateDirectoryStrict dir (getMockFS fs)
    return (fs { getMockFS = fs' }, ())

mockListDirectory :: (HasCallStack, MonadSTM m)
                  => FsPath
                  -> SimFSE m [String]
mockListDirectory fp = withMockFS $ \fs -> do
    case index fp (getMockFS fs) of
      Nothing -> throwFsError FsResourceDoesNotExist fp
      Just (FileOnDisk _)   -> throwFsError FsResourceInappropriateType fp
      Just (FolderOnDisk m) -> return (M.keys m)

mockDoesDirectoryExist :: MonadSTM m
                       => FsPath
                       -> SimFSE m Bool
mockDoesDirectoryExist fp = withMockFS $ \fs ->
    return $ case index fp (getMockFS fs) of
               Just (FolderOnDisk _) -> True
               _                     -> False

mockDoesFileExist :: MonadSTM m
                  => FsPath
                  -> SimFSE m Bool
mockDoesFileExist fp = withMockFS $ \fs ->
    return $ case index fp (getMockFS fs) of
         Just (FileOnDisk _) -> True
         _                   -> False

newBufferMock :: Monad m => Int -> SimFSE m (Buffer (SimFSE m))
newBufferMock len = do
    let buf = BS.pack $ replicate len 0x0
    return $! BufferMock buf


{-------------------------------------------------------------------------------
  Auxiliary and utility functions
-------------------------------------------------------------------------------}

noteT :: MonadError e m => e -> Maybe a -> m a
noteT e = maybe (throwError e) return

-- | Index the FsTree by the given FsPath.
index :: FsPath -> FsTree a -> Maybe (FsTree a)
index [] t                    = Just t
index (_:_)  (FileOnDisk _)   = Nothing
index (p:ps) (FolderOnDisk m) = M.lookup p m >>= index ps

-- | Traverses a 'FsTree' indexing by the given 'FsPath' and, if a match is
-- found, apply the input action on the tree node.
adjustIndex :: FsPath
           -> (FsTree a -> FsTree a)
           -> FsTree a
           -> FsTree a
adjustIndex p f = runIdentity . adjustIndexF p (Identity . f)

-- | General version of 'adjustIndex' parametric over an applicative functor f.
adjustIndexF :: Applicative f
             => FsPath
             -> (FsTree a -> f (FsTree a))
             -> FsTree a
             -> f (FsTree a)
adjustIndexF [] f t = f t
adjustIndexF (_:_) _ (FileOnDisk _)    = error "adjustIndexF: couldn't apply the function."
adjustIndexF (p:ps) f (FolderOnDisk m) =
    let m' = M.alterF (\x -> case x of
                         Nothing -> pure Nothing
                         Just ts -> Just <$> adjustIndexF ps f ts
                     ) p m
    in FolderOnDisk <$> m'

-- | Renders the 'MockFS' in a human-readable fashion.
prettyShowFS :: MockFS -> String
prettyShowFS MockFS{..} =
    let prettyDisk =
            map (\(fp, disk) -> "- " <> fp
                                     <> " -> "
                                     <> show (hexDump $ B16.encode disk)
                                     <> " (" <> show disk <> ")")
                (collectFiles getMockFS)
    in L.intercalate "\n" prettyDisk <> "\n\n" <> show getMockFS
    where
        collectFiles :: FsTree ByteString -> [(String, ByteString)]
        collectFiles (FileOnDisk _)   = []
        collectFiles (FolderOnDisk m) =
            foldl' (\acc x -> case x of
                   (fn, FileOnDisk c) -> (fn, c) : acc
                   (_, dir)           -> acc <> collectFiles dir
            ) [] (M.toList m)

        hexDump :: ByteString -> ByteString
        hexDump = fst
                . BS.foldl' (\(acc, n) w8 ->
                                if n == 2 then (acc <> " " <> BS.singleton w8, 1)
                                          else (acc <> BS.singleton w8, n + 1)
                            ) (mempty, 0 :: Int)


demoMockFS :: MockFS
demoMockFS = MockFS {
    getMockFS = FolderOnDisk $ M.fromList [
        ("usr", FolderOnDisk $ M.fromList [
            ("local", FolderOnDisk $ M.fromList [
                ("bin", FolderOnDisk mempty)
            ])
        ])
      , ("var", FolderOnDisk $ M.fromList [
           ("log", FolderOnDisk mempty)
        ,  ("tmp", FolderOnDisk $ M.fromList [
             ("foo.txt", FileOnDisk mempty)
           ])
        ,  ("mail", FolderOnDisk mempty)
        ,  ("run", FolderOnDisk mempty)
      ])
    ]
    }

newEmptyMockFS :: MockFS
newEmptyMockFS = MockFS {
    getMockFS = FolderOnDisk mempty
  }

{-- This is a good foundation for a AST which can be used by a DSL to generate
    scripts.
data SimFsAction m =
    SimOpen                     FsPath IOMode
  | SimClose                    (MockHandle m)
  | SimSeek                     FsPath SeekMode DiskOffset
  | SimGet                      FsPath Int
  | SimPut                      FsPath ByteString
  | SimTruncate                 FsPath Word64
  | SimListDirectory            FsPath
  | SimCreateDirectory          FsPath
  | SimCreateDirectoryIfMissing FsPath Bool
  deriving Show
--}

-- | Runs a 'SimFs' computation provided an initial 'MockFS', producing a
-- result, the final state of the filesystem and a sequence of actions occurred
-- in the filesystem.
runSimFS :: MonadSTM m
         => SimFS m a
         -> MockFS
         -> m (a, MockFS)
runSimFS m s = do
    fs  <- atomically (newTVar s)
    a   <- runReaderT (unSimFs m) fs
    fs' <- atomically (readTVar fs)
    return (a, fs')

mockDemoScript :: (HasCallStack, HasFS m) => m [ByteString]
mockDemoScript = do
  h1 <- hOpen ["cardano.txt"] IO.ReadWriteMode
  _  <- hPut h1 (BS.byteString $ C8.pack "test")
  _  <- hSeek h1 IO.AbsoluteSeek 0
  r1 <- hGet h1 4
  _  <- hPut h1 (BS.byteString $ C8.pack "ing")
  h2 <- hOpen ["bar.txt"] IO.ReadWriteMode
  _  <- hPut h2 (BS.byteString $ C8.pack "blockchain")
  _  <- hSeek h2 IO.AbsoluteSeek 0
  r2 <- hGet h2 5
  _  <- listDirectory []
  _  <- listDirectory ["var"]
  createDirectory ["var", "tmp", "my-temp-dir"]
  createDirectoryIfMissing True ["home", "adinapoli", "test", "foo", "bar"]
  f1 <- L.sort <$> listDirectory ["var", "tmp"]
  hClose h1
  hClose h2
  checkThat "listDirectory [var, tmp]" ((==) (L.sort ["my-temp-dir", "foo.txt"])) f1
  checkThat "hGet h1 4" ((==) "test") r1
  checkThat "hGet h2 5" ((==) "block") r2
  return [r1, r2]

-- | A homemade version of 'assert' suitable for testing code which won't be
-- run in production, as unlike 'assert' this is always run regardless of the
-- optimise flag used.
checkThat :: (Show a, Monad m)
          => String
          -> (a -> Bool)
          -> a
          -> m ()
checkThat label prd a =
    if prd a then return ()
             else error $ label
                        ++ ": condition didn't hold. Actual value was "
                        ++ show a
                        ++ "\n"
                        ++ prettyCallStack callStack

mockDemo :: IO ()
mockDemo = do
    (res, fs) <- runSimFS (runExceptT demo) demoMockFS
    case res of
      Left  err -> putStrLn (prettyFSError err)
      Right bs  -> putStrLn (prettyShowFS fs) >> print bs
  where
    demo :: ExceptT FsError (SimFS IO) [ByteString]
    demo = mockDemoScript
