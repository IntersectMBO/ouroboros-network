{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TupleSections              #-}

-- | Mock file system implementation
--
-- Intended for qualified import
--
-- > import Ouroboros.Storage.FS.Sim.MockFS (MockFS)
-- > import qualified Ouroboros.Storage.FS.Sim.MockFS as Mock
module Ouroboros.Storage.FS.Sim.MockFS (
    MockFS -- opaque
  , Handle -- opaque
  , empty
  , example
  , pretty
  , handleIOMode
  , handleFsPath
  , numOpenHandles
    -- * Debugging
  , dumpState
    -- * Operations on files
  , hOpen
  , hClose
  , hSeek
  , hGet
  , hPut
  , hPutBuffer
  , hTruncate
  , hGetSize
    -- * Operations on directories
  , createDirectory
  , createDirectoryIfMissing
  , listDirectory
  , doesDirectoryExist
  , doesFileExist
  , removeFile
    -- * Exported for the benefit of tests only
  , HandleState       -- opaque
  , OpenHandleState   -- opaque
  , ClosedHandleState -- opaque
  , FilePtr           -- opaque
  , mockFiles
  ) where

import           Control.Monad.State
import           Data.Bifunctor
import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as B16
import           Data.ByteString.Builder (Builder)
import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Lazy as BL
import           Data.Int (Int64)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import           Data.Maybe (mapMaybe)
import           Data.Set (Set)
import qualified Data.Set as S
import           Data.Word (Word64)
import           GHC.Generics (Generic)
import           GHC.Stack
import           System.IO (IOMode, SeekMode)
import qualified System.IO as IO

import           Ouroboros.Storage.FS.API.Types
import           Ouroboros.Storage.FS.Sim.FsTree (FsTree (..), FsTreeError (..))
import qualified Ouroboros.Storage.FS.Sim.FsTree as FS
import           Ouroboros.Storage.Util.ErrorHandling (ErrorHandling (..))

{-------------------------------------------------------------------------------
  Mock FS types
-------------------------------------------------------------------------------}

data MockFS = MockFS {
      mockFiles      :: Files
    , mockHandles    :: Map Handle HandleState
    , mockNextHandle :: Handle
    }
  deriving (Generic, Show)

-- | We store the files as an 'FsTree' of the file contents
type Files = FsTree ByteString

-- | A mock handle to a file on disk.
--
-- This is only meaningful when interpreted against a 'MockFS'.
newtype Handle = Handle Int
  deriving (Show, Eq, Ord, Enum, Generic)

-- | Mock handle internal state
data HandleState =
    HandleOpen OpenHandleState
  | HandleClosed ClosedHandleState
  deriving (Show, Generic)

data OpenHandleState = OpenHandle {
      openFilePath :: FsPath
    , openPtr      :: FilePtr
    }
  deriving (Show, Generic)

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
    RW Bool Bool Word64

    -- | Append-only pointer
    --
    -- Offset is always the end of the file in append mode
  | Append
  deriving (Show, Generic)

data ClosedHandleState = ClosedHandle {
      closedFilePath :: FsPath
    }
  deriving (Show, Generic)

-- | Monads in which we can simulate the file system
type CanSimFS m = (HasCallStack, MonadState MockFS m)

empty :: MockFS
empty = MockFS FS.empty M.empty (Handle 0)

example :: MockFS
example = empty { mockFiles = FS.example }

{-------------------------------------------------------------------------------
  Auxiliary
-------------------------------------------------------------------------------}

-- | The IO mode associated with this handle
--
-- Returns 'Nothing' if the handle is closed.
-- Throws an exception if the handle is unknown.
handleIOMode :: MockFS -> Handle -> Maybe IOMode
handleIOMode MockFS{..} h =
    case M.lookup h mockHandles of
      Nothing ->
        error "handleIOMode: unknown handle"
      Just (HandleOpen OpenHandle{..}) ->
        case openPtr of
          RW True  False _ -> Just IO.ReadMode
          RW False True  _ -> Just IO.WriteMode
          RW True  True  _ -> Just IO.ReadWriteMode
          RW False False _ -> error "handleIOMode: invalid handle"
          Append           -> Just IO.AppendMode
      Just (HandleClosed _) ->
        Nothing

-- | The file path associated with this handle
--
-- Works for closed handles; throws an exception for unknown handles.
handleFsPath :: MockFS -> Handle -> FsPath
handleFsPath MockFS{..} h =
    case M.lookup h mockHandles of
      Nothing ->
        error "handleFsPath: unknown handle"
      Just (HandleOpen OpenHandle{..}) ->
        openFilePath
      Just (HandleClosed ClosedHandle{..}) ->
        closedFilePath

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
seekFilePtr :: Monad m
            => ErrorHandling FsError m
            -> MockFS -> Handle -> SeekMode -> Int64 -> m FilePtr
seekFilePtr err@ErrorHandling{..} MockFS{..} h seekMode o = do
    case mockHandles M.! h of
      HandleClosed ClosedHandle{..} ->
        throwError FsError {
            fsErrorType   = FsIllegalOperation
          , fsErrorPath   = closedFilePath
          , fsErrorString = "handle closed"
          , fsErrorStack  = callStack
          , fsLimitation  = False
          }
      HandleOpen OpenHandle{..} -> do
        file <- checkFsTree err $ FS.getFile openFilePath mockFiles
        let fsize = fromIntegral (BS.length file) :: Word64
        case (openPtr, seekMode, sign64 o) of
          (RW r w _cur, IO.AbsoluteSeek, Positive o') -> do
            when (o' > fsize) $ throwError (errPastEnd openFilePath)
            return $ RW r w o'
          (_, IO.AbsoluteSeek, Negative _) ->
            throwError $ errNegative openFilePath
          (RW r w cur, IO.RelativeSeek, Positive o') -> do
            let cur' = cur + o'
            when (cur' > fsize) $ throwError (errPastEnd openFilePath)
            return $ RW r w cur'
          (RW r w cur, IO.RelativeSeek, Negative o') -> do
            when (o' > cur) $ throwError (errNegative openFilePath)
            let cur' = cur - o'
            return $ RW r w cur'
          (RW r w _cur, IO.SeekFromEnd, Positive 0) ->
            return $ RW r w fsize
          (RW _ _ _, IO.SeekFromEnd, Positive _) ->
            throwError (errPastEnd openFilePath)
          (RW r w _, IO.SeekFromEnd, Negative o') -> do
            when (o' > fsize) $ throwError (errNegative openFilePath)
            let cur' = fsize - o'
            return $ RW r w cur'
          (Append, _, _) ->
            throwError (errAppend openFilePath)
  where
    errPastEnd fp  = FsError {
                         fsErrorType   = FsInvalidArgument
                       , fsErrorPath   = fp
                       , fsErrorString = "seek past EOF not supported"
                       , fsErrorStack  = callStack
                       , fsLimitation  = True
                       }
    errAppend  fp  = FsError {
                         fsErrorType   = FsInvalidArgument
                       , fsErrorPath   = fp
                       , fsErrorString = "seek in append mode not supported"
                       , fsErrorStack  = callStack
                       , fsLimitation  = True
                       }
    errNegative fp = FsError {
                         fsErrorType   = FsInvalidArgument
                       , fsErrorPath   = fp
                       , fsErrorString = "seek past beginning of file"
                       , fsErrorStack  = callStack
                       , fsLimitation  = False
                       }

{-------------------------------------------------------------------------------
  Internal utilities for implementing the mock FS
-------------------------------------------------------------------------------}

-- | Modify the mock file system without a file handle
modifyMockFS :: CanSimFS m
             => ErrorHandling FsError m
             -> (MockFS -> m (a, MockFS)) -> m a
modifyMockFS ErrorHandling{..} f = do
    st       <- get
    (a, st') <- f st
    put st'
    return a

-- | Access but do not modify the mock file system state without a file handle
readMockFS :: CanSimFS m
           => ErrorHandling FsError m
           -> (Files -> m a) -> m a
readMockFS err f = modifyMockFS err (\fs -> (, fs) <$> f (mockFiles fs))

-- | Require a file handle and may modify the mock file system
withHandleModify :: CanSimFS m
                 => ErrorHandling FsError m
                 -> Handle
                 -> (    MockFS
                      -> HandleState
                      -> m (a, (Files, HandleState))
                    )
                 -> m a
withHandleModify ErrorHandling{..} h f = do
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
               => ErrorHandling FsError m
               -> Handle
               -> (    MockFS
                    -> HandleState
                    -> m (a, HandleState)
                  )
               -> m a
withHandleRead err h f =
    withHandleModify err h $ \fs hs ->
      second (mockFiles fs, ) <$> f fs hs

-- | Require an open file handle to modify the mock file system
withOpenHandleModify :: CanSimFS m
                     => ErrorHandling FsError m
                     -> Handle
                     -> (    MockFS
                          -> OpenHandleState
                          -> m (a, (Files, OpenHandleState))
                        )
                     -> m a
withOpenHandleModify err@ErrorHandling{..} h f =
    withHandleModify err h $ \fs -> \case
      HandleOpen hs ->
        second (second HandleOpen) <$> f fs hs
      HandleClosed ClosedHandle{..} ->
        throwError FsError {
            fsErrorType   = FsIllegalOperation
          , fsErrorPath   = closedFilePath
          , fsErrorString = "handle closed"
          , fsErrorStack  = callStack
          , fsLimitation  = False
          }

-- | Require an open file handle but do not modify the mock file system
withOpenHandleRead :: CanSimFS m
                   => ErrorHandling FsError m
                   -> Handle
                   -> (    MockFS
                        -> OpenHandleState
                        -> m (a, OpenHandleState)
                      )
                   -> m a
withOpenHandleRead err@ErrorHandling{..} h f =
    withHandleRead err h $ \fs -> \case
      HandleOpen hs ->
        second HandleOpen <$> f fs hs
      HandleClosed ClosedHandle{..} ->
        throwError FsError {
            fsErrorType   = FsIllegalOperation
          , fsErrorPath   = closedFilePath
          , fsErrorString = "handle closed"
          , fsErrorStack  = callStack
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

checkFsTree' :: (Monad m, HasCallStack)
             => ErrorHandling FsError m
             -> Either FsTreeError a -> m (Either FsPath a)
checkFsTree' ErrorHandling{..} = go
  where
    go (Left (FsExpectedDir fp _)) =
        throwError FsError {
            fsErrorType   = FsResourceInappropriateType
          , fsErrorPath   = fp
          , fsErrorString = "expected directory"
          , fsErrorStack  = callStack
          , fsLimitation  = False
          }
    go (Left (FsExpectedFile fp)) =
        throwError FsError {
            fsErrorType   = FsResourceInappropriateType
          , fsErrorPath   = fp
          , fsErrorString = "expected file"
          , fsErrorStack  = callStack
          , fsLimitation  = False
          }
    go (Left (FsMissing fp _)) =
        return (Left fp)
    go (Right a) =
        return (Right a)

checkFsTree :: (Monad m, HasCallStack)
            => ErrorHandling FsError m
            -> Either FsTreeError a -> m a
checkFsTree err@ErrorHandling{..} ma = do
    ma' <- checkFsTree' err ma
    case ma' of
      Left fp -> throwError FsError {
                     fsErrorType   = FsResourceDoesNotExist
                   , fsErrorPath   = fp
                   , fsErrorString = "does not exist"
                   , fsErrorStack  = callStack
                   , fsLimitation  = False
                   }
      Right a -> return a

checkDoesNotExist :: (Monad m, HasCallStack)
                  => ErrorHandling FsError m -> MockFS -> FsPath -> m ()
checkDoesNotExist err@ErrorHandling{..} fs fp = do
    exists <- fmap pathExists $ checkFsTree' err $ FS.index fp (mockFiles fs)
    if exists
      then throwError FsError {
               fsErrorType   = FsResourceAlreadyExist
             , fsErrorPath   = fp
             , fsErrorString = "already exists"
             , fsErrorStack  = callStack
             , fsLimitation  = False
             }
      else return ()
  where
    pathExists :: Either a b -> Bool
    pathExists (Left _)  = False
    pathExists (Right _) = True

newHandle :: MockFS -> OpenHandleState -> (Handle, MockFS)
newHandle fs hs = (
      mockNextHandle fs
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
hOpen :: CanSimFS m => ErrorHandling FsError m -> FsPath -> IOMode -> m Handle
hOpen err@ErrorHandling{..} fp ioMode = do
    dirExists <- doesDirectoryExist err fp
    when dirExists $ throwError FsError {
        fsErrorType   = FsResourceInappropriateType
      , fsErrorPath   = fp
      , fsErrorString = "hOpen: directories not supported"
      , fsErrorStack  = callStack
      , fsLimitation  = True
      }
    modifyMockFS err $ \fs -> do
      let alreadyHasWriter =
            any (\hs -> openFilePath hs == fp && isWriteHandle hs) $
            openHandles fs
      when (ioMode /= IO.ReadMode && alreadyHasWriter) $
        throwError FsError {
            fsErrorType   = FsInvalidArgument
          , fsErrorPath   = fp
          , fsErrorString = "more than one concurrent writer not supported"
          , fsErrorStack  = callStack
          , fsLimitation  = True
          }
      when (ioMode == IO.ReadMode) $ void $
        checkFsTree err $ FS.getFile fp (mockFiles fs)
      files' <- checkFsTree err $ FS.touch fp (mockFiles fs)
      return $ newHandle (fs { mockFiles = files' })
                         (OpenHandle fp (filePtr ioMode))
  where
    filePtr :: IOMode -> FilePtr
    filePtr IO.ReadMode      = RW True  False 0
    filePtr IO.WriteMode     = RW False True  0
    filePtr IO.ReadWriteMode = RW True  True  0
    filePtr IO.AppendMode    = Append

-- | Mock implementation of 'hClose'
hClose :: CanSimFS m => ErrorHandling FsError m -> Handle -> m ()
hClose err h = withHandleRead err h $ \_fs -> \case
    HandleOpen hs ->
      return ((), HandleClosed (ClosedHandle (openFilePath hs)))
    HandleClosed hs ->
      return ((), HandleClosed hs)

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
      => ErrorHandling FsError m -> Handle -> SeekMode -> Int64 -> m ()
hSeek err h seekMode o = withOpenHandleRead err h $ \fs hs -> do
    openPtr' <- seekFilePtr err fs h seekMode o
    return ((), hs { openPtr = openPtr' })

-- | Get bytes from handle
--
-- NOTE: Unlike real I/O, we disallow 'hGet' on a handle in append mode.
hGet :: CanSimFS m => ErrorHandling FsError m -> Handle -> Int -> m ByteString
hGet err@ErrorHandling{..} h n =
    withOpenHandleRead err h $ \fs hs@OpenHandle{..} -> do
      file <- checkFsTree err $ FS.getFile openFilePath (mockFiles fs)
      case openPtr of
        RW r w o -> do
          let bs = BS.take n . BS.drop (fromIntegral o) $ file
          return (bs, hs { openPtr = RW r w (o + fromIntegral (BS.length bs)) })
        Append -> do
          throwError FsError {
              fsErrorType   = FsInvalidArgument
            , fsErrorPath   = openFilePath
            , fsErrorString = "cannot hGet in append mode"
            , fsErrorStack  = callStack
            , fsLimitation  = True
            }

hPut :: CanSimFS m => ErrorHandling FsError m -> Handle -> Builder -> m Word64
hPut err@ErrorHandling{..} h builder =
    withOpenHandleModify err h $ \fs hs@OpenHandle{..} -> do
      case openPtr of
        RW r w o -> do
          unless w $ throwError (errReadOnly openFilePath)
          file <- checkFsTree err $ FS.getFile openFilePath (mockFiles fs)
          let file' = replace o toWrite file
          files' <- checkFsTree err $ FS.replace openFilePath file' (mockFiles fs)
          return (written, (files', hs { openPtr = RW r w (o + written) }))
        Append -> do
          file <- checkFsTree err $ FS.getFile openFilePath (mockFiles fs)
          let file' = file <> toWrite
          files' <- checkFsTree err $ FS.replace openFilePath file' (mockFiles fs)
          return (written, (files', hs))
  where
    toWrite = BL.toStrict . Builder.toLazyByteString $ builder
    written = toEnum $ BS.length toWrite

    errReadOnly fp = FsError {
                         fsErrorType   = FsInvalidArgument
                       , fsErrorPath   = fp
                       , fsErrorString = "handle is read-only"
                       , fsErrorStack  = callStack
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
        (a, bc) = BS.splitAt (fromIntegral n) bs
        c       = BS.drop m bc

hPutBuffer :: CanSimFS m
           => ErrorHandling FsError m -> Handle -> buf -> Builder -> m Word64
hPutBuffer err hnd _buf builder = hPut err hnd builder

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
hTruncate :: CanSimFS m => ErrorHandling FsError m -> Handle -> Word64 -> m ()
hTruncate err@ErrorHandling{..} h sz =
    withOpenHandleModify err h $ \fs hs@OpenHandle{..} -> do
      file <- checkFsTree err $ FS.getFile openFilePath (mockFiles fs)
      ptr' <- case (sz > fromIntegral (BS.length file), openPtr) of
                (True, _) ->
                  throwError FsError {
                      fsErrorType   = FsInvalidArgument
                    , fsErrorPath   = openFilePath
                    , fsErrorString = "truncate cannot make the file larger"
                    , fsErrorStack  = callStack
                    , fsLimitation  = True
                    }
                (False, RW{}) ->
                  throwError FsError {
                      fsErrorType   = FsInvalidArgument
                    , fsErrorPath   = openFilePath
                    , fsErrorString = "truncate only supported in append mode"
                    , fsErrorStack  = callStack
                    , fsLimitation  = True
                    }
                (False, Append) ->
                  return Append
      let file' = BS.take (fromIntegral sz) file
      files' <- checkFsTree err $ FS.replace openFilePath file' (mockFiles fs)
      -- TODO: Don't replace the file pointer (not changed)
      return ((), (files', hs { openPtr = ptr' }))

-- | Get file size
--
-- NOTE: In the mock implementation this is thread safe, because there can be
-- only one writer, so concurrent threads cannot change the size of the file.
hGetSize :: CanSimFS m => ErrorHandling FsError m -> Handle -> m Word64
hGetSize err h =
    withOpenHandleRead err h $ \fs hs@OpenHandle{..} -> do
      file <- checkFsTree err $ FS.getFile openFilePath (mockFiles fs)
      return (fromIntegral (BS.length file), hs)

{-------------------------------------------------------------------------------
  Operations on directories
-------------------------------------------------------------------------------}

createDirectory :: CanSimFS m => ErrorHandling FsError m -> FsPath -> m ()
createDirectory err dir = modifyMockFS err $ \fs -> do
    checkDoesNotExist err fs dir
    files' <- checkFsTree err $ FS.createDirIfMissing dir (mockFiles fs)
    return ((), fs { mockFiles = files' })

createDirectoryIfMissing :: CanSimFS m
                         => ErrorHandling FsError m -> Bool -> FsPath -> m ()
createDirectoryIfMissing err@ErrorHandling{..} createParents dir = do
    -- Although @createDirectoryIfMissing /a/b/c@ will fail ("inappropriate
    -- type") if @b@ is a file (not a directory), for some strange reason it
    -- throws "already exists" if @c@ is is a file
    fileExists <- doesFileExist err dir
    if fileExists then
      throwError FsError {
          fsErrorType   = FsResourceAlreadyExist
        , fsErrorPath   = dir
        , fsErrorString = "a file with that name already exists"
        , fsErrorStack  = callStack
        , fsLimitation  = False
        }
    else modifyMockFS err $ \fs -> do
      files' <- checkFsTree err $ go createParents (mockFiles fs)
      return ((), fs { mockFiles = files' })
  where
    go :: Bool -> Files -> Either FsTreeError Files
    go True  = FS.createDirWithParents dir
    go False = FS.createDirIfMissing   dir

listDirectory :: CanSimFS m
              => ErrorHandling FsError m -> FsPath -> m (Set String)
listDirectory err fp = readMockFS err $
    fmap M.keysSet . checkFsTree err . FS.getDir fp

-- | Check if directory exists
--
-- It seems real I/O maps what would be "inapproriate device" errors to False.
doesDirectoryExist :: CanSimFS m => ErrorHandling FsError m -> FsPath -> m Bool
doesDirectoryExist err fp = readMockFS err $ \fs ->
    return $ case FS.getDir fp fs of
               Left  _ -> False
               Right _ -> True

-- | Check if file exists
--
-- See comments for 'doesDirectoryExist'.
doesFileExist :: CanSimFS m => ErrorHandling FsError m -> FsPath -> m Bool
doesFileExist err fp = readMockFS err $ \fs ->
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
removeFile :: CanSimFS m => ErrorHandling FsError m -> FsPath -> m ()
removeFile err@ErrorHandling{..} fp = modifyMockFS err $ \fs -> case fp of
    []
      -> throwError FsError {
             fsErrorType   = FsIllegalOperation
           , fsErrorPath   = fp
           , fsErrorString = "cannot remove the root directory"
           , fsErrorStack  = callStack
           , fsLimitation  = True
           }
    _ | fp `S.member` openFilePaths fs
      -> throwError FsError {
             fsErrorType   = FsIllegalOperation
           , fsErrorPath   = fp
           , fsErrorString = "cannot remove an open file"
           , fsErrorStack  = callStack
           , fsLimitation  = True
           }
    _ -> do
      files' <- checkFsTree err $ FS.removeFile fp (mockFiles fs)
      return ((), fs { mockFiles = files' })

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
