{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiWayIf                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TupleSections              #-}

-- | Mock file system implementation
--
-- Intended for qualified import
--
-- > import Test.Util.FS.Sim.MockFS (MockFS)
-- > import qualified Test.Util.FS.Sim.MockFS as Mock
module Test.Util.FS.Sim.MockFS (
    empty
  , example
  , handleIsOpen
  , numOpenHandles
  , pretty
    -- * Debugging
  , dumpState
    -- * Operations on files
  , hClose
  , hGetSize
  , hGetSome
  , hGetSomeAt
  , hIsOpen
  , hOpen
  , hPutSome
  , hSeek
  , hTruncate
    -- * Operations on directories
  , createDirectory
  , createDirectoryIfMissing
  , doesDirectoryExist
  , doesFileExist
  , listDirectory
  , removeFile
  , renameFile
    -- * Exported for the benefit of tests only
  , Files
  , mockFiles
    -- ** opaque
  , ClosedHandleState
  , FilePtr
  , HandleState
  , OpenHandleState
    -- * opaque
  , HandleMock
  , MockFS
  ) where

import           Control.Monad.Except
import           Control.Monad.State
import           Data.Bifunctor
import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as B16
import           Data.Int (Int64)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import           Data.Maybe (mapMaybe)
import           Data.Set (Set)
import qualified Data.Set as S
import qualified Data.Text as Text
import           Data.Word (Word64)
import           GHC.Generics (Generic)
import           NoThunks.Class (NoThunks)

import           Ouroboros.Consensus.Storage.FS.API.Types
import           Ouroboros.Consensus.Util.CallStack

import           Test.Util.FS.Sim.FsTree (FsTree (..), FsTreeError (..))
import qualified Test.Util.FS.Sim.FsTree as FS

{-------------------------------------------------------------------------------
  Mock FS types
-------------------------------------------------------------------------------}

data MockFS = MockFS {
      mockFiles      :: !Files
    , mockHandles    :: !(Map HandleMock HandleState)
    , mockNextHandle :: !HandleMock
    }
  deriving (Generic, Show, NoThunks)

-- | We store the files as an 'FsTree' of the file contents
type Files = FsTree ByteString

-- | A mock handle to a file on disk.
--
-- This is only meaningful when interpreted against a 'MockFS'.
newtype HandleMock = HandleMock Int
  deriving stock   (Show, Eq, Ord, Generic)
  deriving newtype (Enum, NoThunks)

-- | Instantiate 'Handle' with the mock handle
type Handle' = Handle HandleMock

-- | Mock handle internal state
data HandleState =
    HandleOpen !OpenHandleState
  | HandleClosed !ClosedHandleState
  deriving (Show, Generic, NoThunks)

data OpenHandleState = OpenHandle {
      openFilePath :: !FsPath
    , openPtr      :: !FilePtr
    }
  deriving (Show, Generic, NoThunks)

-- | Check whether the file handle is in write/append mode.
isWriteHandle :: OpenHandleState -> Bool
isWriteHandle OpenHandle{..} = case openPtr of
    RW _ True  _ -> True
    Append       -> True
    _            -> False

-- | File pointer
--
-- This is purely an internal abstraction.
data FilePtr =
    -- | Read/write pointer
    --
    -- We record if we can read and/or write, and the current offset
    RW !Bool !Bool !Word64

    -- | Append-only pointer
    --
    -- Offset is always the end of the file in append mode
  | Append
  deriving (Show, Generic, NoThunks)

data ClosedHandleState = ClosedHandle {
      closedFilePath :: FsPath
    }
  deriving (Show, Generic, NoThunks)

-- | Monads in which we can simulate the file system
type CanSimFS m = (HasCallStack, MonadState MockFS m, MonadError FsError m)

empty :: MockFS
empty = MockFS FS.empty M.empty (HandleMock 0)

example :: MockFS
example = empty { mockFiles = FS.example }

{-------------------------------------------------------------------------------
  Auxiliary
-------------------------------------------------------------------------------}

-- | Return 'True' iff the handle is open.
--
-- Throws an exception if the handle is unknown.
handleIsOpen :: MockFS -> HandleMock -> Bool
handleIsOpen MockFS{..} h =
    case M.lookup h mockHandles of
      Nothing ->
        error "handleIOMode: unknown handle"
      Just (HandleOpen OpenHandle{}) -> True
      Just (HandleClosed _)          -> False

openHandles :: MockFS -> [OpenHandleState]
openHandles MockFS{..} = mapMaybe isOpen $ M.elems mockHandles
  where
    isOpen :: HandleState -> Maybe OpenHandleState
    isOpen (HandleOpen   hs) = Just hs
    isOpen (HandleClosed _ ) = Nothing

-- | A set containing each file path that some open handle refers to.
openFilePaths :: MockFS -> Set FsPath
openFilePaths MockFS{..} = foldMap handleOpenFilePath $ M.elems mockHandles
  where
    handleOpenFilePath :: HandleState -> Set FsPath
    handleOpenFilePath (HandleOpen hs)  = S.singleton $ openFilePath hs
    handleOpenFilePath (HandleClosed _) = S.empty

-- | Number of open handles
numOpenHandles :: MockFS -> Int
numOpenHandles = length . openHandles

-- | Updated file pointer
--
-- We lift this out as a separate concept primarily for the benefit of tests.
--
-- See 'hSeek' for limitations.
seekFilePtr :: MonadError FsError m
            => MockFS -> Handle' -> SeekMode -> Int64 -> m FilePtr
seekFilePtr MockFS{..} (Handle h _) seekMode o = do
    case mockHandles M.! h of
      HandleClosed ClosedHandle{..} ->
        throwError FsError {
            fsErrorType   = FsIllegalOperation
          , fsErrorPath   = fsToFsErrorPathUnmounted closedFilePath
          , fsErrorString = "handle closed"
          , fsErrorNo     = Nothing
          , fsErrorStack  = prettyCallStack
          , fsLimitation  = False
          }
      HandleOpen OpenHandle{..} -> do
        file <- checkFsTree $ FS.getFile openFilePath mockFiles
        let fsize = fromIntegral (BS.length file) :: Word64
        case (openPtr, seekMode, sign64 o) of
          (RW r w _cur, AbsoluteSeek, Positive o') -> do
            when (o' > fsize) $ throwError (errPastEnd openFilePath)
            return $ RW r w o'
          (_, AbsoluteSeek, Negative _) ->
            throwError $ errNegative openFilePath
          (RW r w cur, RelativeSeek, Positive o') -> do
            let cur' = cur + o'
            when (cur' > fsize) $ throwError (errPastEnd openFilePath)
            return $ RW r w cur'
          (RW r w cur, RelativeSeek, Negative o') -> do
            when (o' > cur) $ throwError (errNegative openFilePath)
            let cur' = cur - o'
            return $ RW r w cur'
          (RW r w _cur, SeekFromEnd, Positive 0) ->
            return $ RW r w fsize
          (RW _ _ _, SeekFromEnd, Positive _) ->
            throwError (errPastEnd openFilePath)
          (RW r w _, SeekFromEnd, Negative o') -> do
            when (o' > fsize) $ throwError (errNegative openFilePath)
            let cur' = fsize - o'
            return $ RW r w cur'
          (Append, _, _) ->
            throwError (errAppend openFilePath)
  where
    errPastEnd fp  = FsError {
                         fsErrorType   = FsInvalidArgument
                       , fsErrorPath   = fsToFsErrorPathUnmounted fp
                       , fsErrorString = "seek past EOF not supported"
                       , fsErrorNo     = Nothing
                       , fsErrorStack  = prettyCallStack
                       , fsLimitation  = True
                       }
    errAppend  fp  = FsError {
                         fsErrorType   = FsInvalidArgument
                       , fsErrorPath   = fsToFsErrorPathUnmounted fp
                       , fsErrorString = "seek in append mode not supported"
                       , fsErrorNo     = Nothing
                       , fsErrorStack  = prettyCallStack
                       , fsLimitation  = True
                       }
    errNegative fp = FsError {
                         fsErrorType   = FsInvalidArgument
                       , fsErrorPath   = fsToFsErrorPathUnmounted fp
                       , fsErrorString = "seek past beginning of file"
                       , fsErrorNo     = Nothing
                       , fsErrorStack  = prettyCallStack
                       , fsLimitation  = False
                       }

{-------------------------------------------------------------------------------
  Internal utilities for implementing the mock FS
-------------------------------------------------------------------------------}

-- | Modify the mock file system without a file handle
modifyMockFS :: CanSimFS m
             => (MockFS -> m (a, MockFS)) -> m a
modifyMockFS f = do
    st       <- get
    (a, st') <- f st
    put st'
    return a

-- | Access but do not modify the mock file system state without a file handle
readMockFS :: CanSimFS m
           => (Files -> m a) -> m a
readMockFS f = modifyMockFS (\fs -> (, fs) <$> f (mockFiles fs))

-- | Require a file handle and may modify the mock file system
withHandleModify :: CanSimFS m
                 => Handle'
                 -> (    MockFS
                      -> HandleState
                      -> m (a, (Files, HandleState))
                    )
                 -> m a
withHandleModify (Handle h _) f = do
    st <- get
    case M.lookup h (mockHandles st) of
      Just hs -> do
        (a, (fs', hs')) <- f st hs
        put $ st { mockHandles = M.insert h hs' (mockHandles st)
                 , mockFiles   = fs'
                 }
        return a
      Nothing ->
        error "withHandleModify: handle not found"

-- | Require a file handle but do not modify the mock file system
withHandleRead :: CanSimFS m
               => Handle'
               -> (    MockFS
                    -> HandleState
                    -> m (a, HandleState)
                  )
               -> m a
withHandleRead h f =
    withHandleModify h $ \fs hs ->
      second (mockFiles fs, ) <$> f fs hs

-- | Require an open file handle to modify the mock file system
withOpenHandleModify :: CanSimFS m
                     => Handle'
                     -> (    MockFS
                          -> OpenHandleState
                          -> m (a, (Files, OpenHandleState))
                        )
                     -> m a
withOpenHandleModify h f =
    withHandleModify h $ \fs -> \case
      HandleOpen hs ->
        second (second HandleOpen) <$> f fs hs
      HandleClosed ClosedHandle{..} ->
        throwError FsError {
            fsErrorType   = FsIllegalOperation
          , fsErrorPath   = fsToFsErrorPathUnmounted closedFilePath
          , fsErrorString = "handle closed"
          , fsErrorNo     = Nothing
          , fsErrorStack  = prettyCallStack
          , fsLimitation  = False
          }

-- | Require an open file handle but do not modify the mock file system
withOpenHandleRead :: CanSimFS m
                   => Handle'
                   -> (    MockFS
                        -> OpenHandleState
                        -> m (a, OpenHandleState)
                      )
                   -> m a
withOpenHandleRead h f =
    withHandleRead h $ \fs -> \case
      HandleOpen hs ->
        second HandleOpen <$> f fs hs
      HandleClosed ClosedHandle{..} ->
        throwError FsError {
            fsErrorType   = FsIllegalOperation
          , fsErrorPath   = fsToFsErrorPathUnmounted closedFilePath
          , fsErrorString = "handle closed"
          , fsErrorNo     = Nothing
          , fsErrorStack  = prettyCallStack
          , fsLimitation  = False
          }

{-------------------------------------------------------------------------------
  Debugging
-------------------------------------------------------------------------------}

dumpState :: CanSimFS m => m String
dumpState = pretty <$> get

{-------------------------------------------------------------------------------
  Internal auxiliary
-------------------------------------------------------------------------------}

checkFsTree' :: (MonadError FsError m, HasCallStack)
             => Either FsTreeError a -> m (Either FsPath a)
checkFsTree' = go
  where
    go (Left (FsExpectedDir fp _)) =
        throwError FsError {
            fsErrorType   = FsResourceInappropriateType
          , fsErrorPath   = fsToFsErrorPathUnmounted fp
          , fsErrorString = "expected directory"
          , fsErrorNo     = Nothing
          , fsErrorStack  = prettyCallStack
          , fsLimitation  = False
          }
    go (Left (FsExpectedFile fp)) =
        throwError FsError {
            fsErrorType   = FsResourceInappropriateType
          , fsErrorPath   = fsToFsErrorPathUnmounted fp
          , fsErrorString = "expected file"
          , fsErrorNo     = Nothing
          , fsErrorStack  = prettyCallStack
          , fsLimitation  = False
          }
    go (Left (FsMissing fp _)) =
        return (Left fp)
    go (Left (FsExists fp)) =
        throwError FsError {
            fsErrorType   = FsResourceAlreadyExist
          , fsErrorPath   = fsToFsErrorPathUnmounted fp
          , fsErrorString = "file exists"
          , fsErrorNo     = Nothing
          , fsErrorStack  = prettyCallStack
          , fsLimitation  = False
          }
    go (Right a) =
        return (Right a)

checkFsTree :: (MonadError FsError m, HasCallStack)
            => Either FsTreeError a -> m a
checkFsTree ma = do
    ma' <- checkFsTree' ma
    case ma' of
      Left fp -> throwError FsError {
                     fsErrorType   = FsResourceDoesNotExist
                   , fsErrorPath   = fsToFsErrorPathUnmounted fp
                   , fsErrorString = "does not exist"
                   , fsErrorNo     = Nothing
                   , fsErrorStack  = prettyCallStack
                   , fsLimitation  = False
                   }
      Right a -> return a

checkDoesNotExist :: (MonadError FsError m, HasCallStack)
                  => MockFS -> FsPath -> m ()
checkDoesNotExist fs fp = do
    exists <- fmap pathExists $ checkFsTree' $ FS.index fp (mockFiles fs)
    if exists
      then throwError FsError {
               fsErrorType   = FsResourceAlreadyExist
             , fsErrorPath   = fsToFsErrorPathUnmounted fp
             , fsErrorString = "already exists"
             , fsErrorNo     = Nothing
             , fsErrorStack  = prettyCallStack
             , fsLimitation  = False
             }
      else return ()
  where
    pathExists :: Either a b -> Bool
    pathExists (Left _)  = False
    pathExists (Right _) = True

newHandle :: MockFS -> OpenHandleState -> (Handle', MockFS)
newHandle fs hs = (
      Handle (mockNextHandle fs) (openFilePath hs)
    , fs { mockNextHandle = succ (mockNextHandle fs)
         , mockHandles    = M.insert (mockNextHandle fs)
                                     (HandleOpen hs)
                                     (mockHandles fs)
         }
    )

{-------------------------------------------------------------------------------
  Operations on files
-------------------------------------------------------------------------------}

-- | Mock implementation of 'hOpen'.
--
-- NOTE: Differences from Posix:
--
-- * We do not support opening directories.
-- * We do not support more than one concurrent writer
--   (we do however allow a writer and multiple concurrent readers)
-- * We do not support create file on ReadMode.
hOpen :: CanSimFS m => FsPath -> OpenMode -> m Handle'
hOpen fp openMode = do
    dirExists <- doesDirectoryExist fp
    when dirExists $ throwError FsError {
        fsErrorType   = FsResourceInappropriateType
      , fsErrorPath   = fsToFsErrorPathUnmounted fp
      , fsErrorString = "hOpen: directories not supported"
      , fsErrorNo     = Nothing
      , fsErrorStack  = prettyCallStack
      , fsLimitation  = True
      }
    modifyMockFS $ \fs -> do
      let alreadyHasWriter =
            any (\hs -> openFilePath hs == fp && isWriteHandle hs) $
            openHandles fs
      when (openMode /= ReadMode && alreadyHasWriter) $
        throwError FsError {
            fsErrorType   = FsInvalidArgument
          , fsErrorPath   = fsToFsErrorPathUnmounted fp
          , fsErrorString = "more than one concurrent writer not supported"
          , fsErrorNo     = Nothing
          , fsErrorStack  = prettyCallStack
          , fsLimitation  = True
          }
      when (openMode == ReadMode) $ void $
        checkFsTree $ FS.getFile fp (mockFiles fs)
      files' <- checkFsTree $ FS.openFile fp ex (mockFiles fs)
      return $ newHandle (fs { mockFiles = files' })
                         (OpenHandle fp (filePtr openMode))
  where
    ex :: AllowExisting
    ex = allowExisting openMode

    filePtr :: OpenMode -> FilePtr
    filePtr ReadMode          = RW True  False 0
    filePtr (WriteMode     _) = RW False True  0
    filePtr (ReadWriteMode _) = RW True  True  0
    filePtr (AppendMode    _) = Append

-- | Mock implementation of 'hClose'
hClose :: CanSimFS m => Handle' -> m ()
hClose h = withHandleRead h $ \_fs -> \case
    HandleOpen hs ->
      return ((), HandleClosed (ClosedHandle (openFilePath hs)))
    HandleClosed hs ->
      return ((), HandleClosed hs)

-- | Mock implementation of 'hIsOpen'
hIsOpen :: CanSimFS m => Handle' -> m Bool
hIsOpen h = gets (`handleIsOpen` handleRaw h)

-- | Mock implementation of 'hSeek'
--
-- NOTE: This is more restricted than the IO version, because seek has some
-- odd properties:
--
-- * We do not allow seeking at all on files in append mode
-- * We do not allow seeking past the end of the file
--   (this means that when using 'IO.SeekFromEnd', the only valid offset is 0)
-- * We do /not/ return the new file offset
hSeek :: CanSimFS m
      => Handle' -> SeekMode -> Int64 -> m ()
hSeek h seekMode o = withOpenHandleRead h $ \fs hs -> do
    openPtr' <- seekFilePtr fs h seekMode o
    return ((), hs { openPtr = openPtr' })

-- | Get bytes from handle
--
-- NOTE: Unlike real I/O, we disallow 'hGetSome' on a handle in append mode.
hGetSome :: CanSimFS m => Handle' -> Word64 -> m ByteString
hGetSome h n =
    withOpenHandleRead h $ \fs hs@OpenHandle{..} -> do
      file <- checkFsTree $ FS.getFile openFilePath (mockFiles fs)
      case openPtr of
        RW r w o -> do
          unless r $ throwError (errNoReadAccess openFilePath "write")
          let bs = BS.take (fromIntegral n) . BS.drop (fromIntegral o) $ file
          return (bs, hs { openPtr = RW True w (o + fromIntegral (BS.length bs)) })
        Append -> throwError (errNoReadAccess openFilePath "append")
  where
    errNoReadAccess fp mode = FsError {
        fsErrorType   = FsInvalidArgument
      , fsErrorPath   = fsToFsErrorPathUnmounted fp
      , fsErrorString = "cannot hGetSome in " <> mode <> " mode"
      , fsErrorNo     = Nothing
      , fsErrorStack  = prettyCallStack
      , fsLimitation  = True
      }

-- | Thread safe version of 'hGetSome', which doesn't modify or read the file
-- offset.
hGetSomeAt :: CanSimFS m
           => Handle'
           -> Word64
           -> AbsOffset
           -> m ByteString
hGetSomeAt h n o =
  withOpenHandleRead h $ \fs hs@OpenHandle{..} -> do
      file <- checkFsTree $ FS.getFile openFilePath (mockFiles fs)
      let o' = unAbsOffset o
      let fsize = fromIntegral (BS.length file) :: Word64
      case openPtr  of
        RW r _ _ -> do
          unless r $ throwError (errNoReadAccess openFilePath "write")
          let bs = BS.take (fromIntegral n) . BS.drop (fromIntegral o') $ file
          -- This is the same fsLimitation we get when we seek past the end of
          -- EOF, in AbsoluteSeek mode.
          when (o' > fsize) $ throwError (errPastEnd openFilePath)
          return (bs, hs)
        Append -> throwError (errNoReadAccess openFilePath "append")
  where
    errNoReadAccess fp mode = FsError {
        fsErrorType   = FsInvalidArgument
      , fsErrorPath   = fsToFsErrorPathUnmounted fp
      , fsErrorString = "cannot hGetSomeAt in " <> mode <> " mode"
      , fsErrorNo     = Nothing
      , fsErrorStack  = prettyCallStack
      , fsLimitation  = True
      }

    errPastEnd fp = FsError {
        fsErrorType   = FsInvalidArgument
      , fsErrorPath   = fsToFsErrorPathUnmounted fp
      , fsErrorString = "hGetSomeAt offset past EOF not supported"
      , fsErrorNo     = Nothing
      , fsErrorStack  = prettyCallStack
      , fsLimitation  = True
      }

hPutSome :: CanSimFS m => Handle' -> ByteString -> m Word64
hPutSome h toWrite =
    withOpenHandleModify h $ \fs hs@OpenHandle{..} -> do
      case openPtr of
        RW r w o -> do
          unless w $ throwError (errReadOnly openFilePath)
          file <- checkFsTree $ FS.getFile openFilePath (mockFiles fs)
          let file' = replace o toWrite file
          files' <- checkFsTree $ FS.replace openFilePath file' (mockFiles fs)
          return (written, (files', hs { openPtr = RW r w (o + written) }))
        Append -> do
          file <- checkFsTree $ FS.getFile openFilePath (mockFiles fs)
          let file' = file <> toWrite
          files' <- checkFsTree $ FS.replace openFilePath file' (mockFiles fs)
          return (written, (files', hs))
  where
    written = toEnum $ BS.length toWrite

    errReadOnly fp = FsError {
                         fsErrorType   = FsInvalidArgument
                       , fsErrorPath   = fsToFsErrorPathUnmounted fp
                       , fsErrorString = "handle is read-only"
                       , fsErrorNo     = Nothing
                       , fsErrorStack  = prettyCallStack
                       , fsLimitation  = False
                       }

    -- Given
    --
    -- >        A        B         C
    -- > |-----------|-------.-----------|
    -- >             n       .
    -- >                     .
    -- >                 D   .
    -- >             |-------|
    --
    -- return A <> D <> C
    replace :: Word64 -> ByteString -> ByteString -> ByteString
    replace n d abc = a <> d <> c
      where
        (a, c) = snip (fromIntegral n) (BS.length d) abc

    -- Given
    --
    -- >       A         B         C
    -- > |-----------|-------|-----------|
    -- >             n
    -- >             <------->
    -- >                 m
    --
    -- return (A, C)
    snip :: Int -> Int -> ByteString -> (ByteString, ByteString)
    snip n m bs = (a, c)
      where
        (a, bc) = BS.splitAt n bs
        c       = BS.drop m bc

-- | Truncate a file
--
-- NOTE: Differences from Posix:
--
-- * Although this corresponds to Posix @ftruncate@, this can only be used
--   to make files /smaller/, not larger.
-- * We only support this in append mode. The reason is that Posix
--   @ftruncate@ does not modify the file offset, and adds padding with zeroes
--   on subsequent writes. This is however not behaviour we want to emulate.
--   In append mode however the Posix file offset is not used (and we don't
--   even record it at all), appends always happen at the end of the file.
hTruncate :: CanSimFS m => Handle' -> Word64 -> m ()
hTruncate h sz =
    withOpenHandleModify h $ \fs hs@OpenHandle{..} -> do
      file <- checkFsTree $ FS.getFile openFilePath (mockFiles fs)
      ptr' <- case (sz > fromIntegral (BS.length file), openPtr) of
                (True, _) ->
                  throwError FsError {
                      fsErrorType   = FsInvalidArgument
                    , fsErrorPath   = fsToFsErrorPathUnmounted openFilePath
                    , fsErrorString = "truncate cannot make the file larger"
                    , fsErrorNo     = Nothing
                    , fsErrorStack  = prettyCallStack
                    , fsLimitation  = True
                    }
                (False, RW{}) ->
                  throwError FsError {
                      fsErrorType   = FsInvalidArgument
                    , fsErrorPath   = fsToFsErrorPathUnmounted openFilePath
                    , fsErrorString = "truncate only supported in append mode"
                    , fsErrorNo     = Nothing
                    , fsErrorStack  = prettyCallStack
                    , fsLimitation  = True
                    }
                (False, Append) ->
                  return Append
      let file' = BS.take (fromIntegral sz) file
      files' <- checkFsTree $ FS.replace openFilePath file' (mockFiles fs)
      -- TODO: Don't replace the file pointer (not changed)
      return ((), (files', hs { openPtr = ptr' }))

-- | Get file size
--
-- NOTE: In the mock implementation this is thread safe, because there can be
-- only one writer, so concurrent threads cannot change the size of the file.
hGetSize :: CanSimFS m => Handle' -> m Word64
hGetSize h =
    withOpenHandleRead h $ \fs hs@OpenHandle{..} -> do
      file <- checkFsTree $ FS.getFile openFilePath (mockFiles fs)
      return (fromIntegral (BS.length file), hs)

{-------------------------------------------------------------------------------
  Operations on directories
-------------------------------------------------------------------------------}

createDirectory :: CanSimFS m => FsPath -> m ()
createDirectory dir = modifyMockFS $ \fs -> do
    checkDoesNotExist fs dir
    files' <- checkFsTree $ FS.createDirIfMissing dir (mockFiles fs)
    return ((), fs { mockFiles = files' })

createDirectoryIfMissing :: CanSimFS m
                         => Bool -> FsPath -> m ()
createDirectoryIfMissing createParents dir = do
    -- Although @createDirectoryIfMissing /a/b/c@ will fail ("inappropriate
    -- type") if @b@ is a file (not a directory), for some strange reason it
    -- throws "already exists" if @c@ is is a file
    fileExists <- doesFileExist dir
    if fileExists then
      throwError FsError {
          fsErrorType   = FsResourceAlreadyExist
        , fsErrorPath   = fsToFsErrorPathUnmounted dir
        , fsErrorString = "a file with that name already exists"
        , fsErrorNo     = Nothing
        , fsErrorStack  = prettyCallStack
        , fsLimitation  = False
        }
    else modifyMockFS $ \fs -> do
      files' <- checkFsTree $ go createParents (mockFiles fs)
      return ((), fs { mockFiles = files' })
  where
    go :: Bool -> Files -> Either FsTreeError Files
    go True  = FS.createDirWithParents dir
    go False = FS.createDirIfMissing   dir

listDirectory :: CanSimFS m
              => FsPath -> m (Set String)
listDirectory fp = readMockFS $
      fmap (S.fromList . map Text.unpack . M.keys)
    . checkFsTree
    . FS.getDir fp

-- | Check if directory exists
--
-- It seems real I/O maps what would be "inapproriate device" errors to False.
doesDirectoryExist :: CanSimFS m => FsPath -> m Bool
doesDirectoryExist fp = readMockFS $ \fs ->
    return $ case FS.getDir fp fs of
               Left  _ -> False
               Right _ -> True

-- | Check if file exists
--
-- See comments for 'doesDirectoryExist'.
doesFileExist :: CanSimFS m => FsPath -> m Bool
doesFileExist fp = readMockFS $ \fs ->
    return $ case FS.getFile fp fs of
               Left  _ -> False
               Right _ -> True

-- | Remove a file
--
-- The behaviour of @unlink@ is to remove the file after all open file handles
-- that refer to it are closed. The open file handles referring to the file
-- can still be used to write\/read to\/from, while at the same time, the file
-- is invisible for all other operations.
--
-- We do not implement this behaviour and consider this a limitation of the
-- mock file system, and throw an error when removing a file that still has
-- open file handles to it.
--
-- In the state machine tests, removing the root directory may cause the IO
-- implementation to throw an 'FsInsufficientPermissions' error, depending on
-- the permissions of the temporary directory used to run the tests in. In
-- theory it should throw a 'FsResourceInappropriateType' error. To avoid this
-- mismatch during testing, we also consider removing the root folder a
-- limitation of the mock file system.
removeFile :: CanSimFS m => FsPath -> m ()
removeFile fp =
    modifyMockFS $ \fs -> case fsPathToList fp of
      []
        -> throwError FsError {
               fsErrorType   = FsIllegalOperation
             , fsErrorPath   = fsToFsErrorPathUnmounted fp
             , fsErrorString = "cannot remove the root directory"
             , fsErrorNo     = Nothing
             , fsErrorStack  = prettyCallStack
             , fsLimitation  = True
             }
      _ | fp `S.member` openFilePaths fs
        -> throwError FsError {
               fsErrorType   = FsIllegalOperation
             , fsErrorPath   = fsToFsErrorPathUnmounted fp
             , fsErrorString = "cannot remove an open file"
             , fsErrorNo     = Nothing
             , fsErrorStack  = prettyCallStack
             , fsLimitation  = True
             }
      _ -> do
        files' <- checkFsTree $ FS.removeFile fp (mockFiles fs)
        return ((), fs { mockFiles = files' })

renameFile :: CanSimFS m => FsPath -> FsPath -> m ()
renameFile fpOld fpNew =
    modifyMockFS $ \fs -> if
      | not (sameDir fpOld fpNew) ->
        throwError $ errDifferentDir fpOld
      | fpOld `S.member` openFilePaths fs ->
        throwError $ errRenameOpenFile fpOld
      | fpNew `S.member` openFilePaths fs ->
        throwError $ errRenameOpenFile fpNew
      | Right _ <- FS.getDir fpNew (mockFiles fs) ->
        throwError $ errRenameDir fpNew
      | otherwise -> do
        files' <- checkFsTree $ FS.renameFile fpOld fpNew (mockFiles fs)
        return ((), fs { mockFiles = files' })
  where
    sameDir fp1 fp2 =
        (fst <$> fsPathSplit fp1) == (fst <$> fsPathSplit fp2)

    errRenameOpenFile fp = FsError {
        fsErrorType   = FsIllegalOperation
      , fsErrorPath   = fsToFsErrorPathUnmounted fp
      , fsErrorString = "cannot rename opened file"
      , fsErrorNo     = Nothing
      , fsErrorStack  = prettyCallStack
      , fsLimitation  = True
      }

    errRenameDir fp = FsError {
        fsErrorType   = FsResourceInappropriateType
      , fsErrorPath   = fsToFsErrorPathUnmounted fp
      , fsErrorString = "is a directory"
      , fsErrorNo     = Nothing
      , fsErrorStack  = prettyCallStack
      , fsLimitation  = True
      }

    errDifferentDir fp = FsError {
        fsErrorType   = FsIllegalOperation
      , fsErrorPath   = fsToFsErrorPathUnmounted fp
      , fsErrorString = "files must be in the same directory"
      , fsErrorNo     = Nothing
      , fsErrorStack  = prettyCallStack
      , fsLimitation  = True
      }

{-------------------------------------------------------------------------------
  Pretty-printing
-------------------------------------------------------------------------------}

-- | Renders the 'MockFS' in a human-readable fashion.
--
-- TODO: Right now does this not show the state of the handles.
pretty :: MockFS -> String
pretty = FS.pretty renderFile . mockFiles
  where
    renderFile :: ByteString -> String
    renderFile = show . hexDump . B16.encode

    hexDump :: ByteString -> ByteString
    hexDump = fst
            . BS.foldl' (\(acc, n) w8 ->
                            if n == 2 then (acc <> " " <> BS.singleton w8, 1)
                                      else (acc <> BS.singleton w8, n + 1)
                        ) (mempty, 0 :: Int)

{-------------------------------------------------------------------------------
  Auxiliary
-------------------------------------------------------------------------------}

data Sign a = Negative a | Positive a
  deriving (Functor)

sign :: (Num a, Ord a) => a -> Sign a
sign a | a < 0     = Negative (negate a)
       | otherwise = Positive a

sign64 :: Int64 -> Sign Word64
sign64 = fmap fromIntegral . sign
