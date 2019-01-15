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
    -- * Exported for the benefit of tests only
  , HandleState       -- opaque
  , OpenHandleState   -- opaque
  , ClosedHandleState -- opaque
  , FilePtr           -- opaque
  , mockFiles
  ) where

import           Control.Monad.Except
import           Control.Monad.State
import           Control.Monad.Trans.Except (except)
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
import           Data.Word (Word64)
import           GHC.Generics (Generic)
import           GHC.Stack
import           System.IO (IOMode, SeekMode)
import qualified System.IO as IO

import           Ouroboros.Storage.FS.Class.Types
import           Ouroboros.Storage.FS.Sim.FsTree (FsTree (..), FsTreeError (..))
import qualified Ouroboros.Storage.FS.Sim.FsTree as FS

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
type CanSimFS m = ( HasCallStack
                  , MonadState MockFS m
                  , MonadError FsError m
                  )

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

-- | Number of open handles
numOpenHandles :: MockFS -> Int
numOpenHandles = length . openHandles

-- | Updated file pointer
--
-- We lift this out as a separate concept primarily for the benefit of tests.
--
-- See 'hSeek' for limitations.
seekFilePtr :: MockFS -> Handle -> SeekMode -> Int64 -> Either FsError FilePtr
seekFilePtr MockFS{..} h seekMode o = runExcept $ do
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
        file <- checkFsTree $ FS.getFile openFilePath mockFiles
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
modifyMockFS :: CanSimFS m => (MockFS -> Except FsError (a, MockFS)) -> m a
modifyMockFS f = do
    st <- get
    case runExcept $ f st of
      Left  err      -> throwError err
      Right (a, st') -> put st' >> return a

-- | Access but do not modify the mock file system state without a file handle
readMockFS :: CanSimFS m => (Files -> Except FsError a) -> m a
readMockFS f = modifyMockFS (\fs -> (, fs) <$> f (mockFiles fs))

-- | Require a file andle and may modify the mock file system
withHandleModify :: CanSimFS m
                 => Handle
                 -> (    MockFS
                      -> HandleState
                      -> Except FsError (a, (Files, HandleState))
                    )
                 -> m a
withHandleModify h f = do
    st <- get
    case M.lookup h (mockHandles st) of
      Just hs ->
        case runExcept $ f st hs of
          Left err ->
            throwError err
          Right (a, (fs', hs')) -> do
            put $ st { mockHandles = M.insert h hs' (mockHandles st)
                     , mockFiles   = fs'
                     }
            return a
      Nothing ->
        error "withHandleModify: handle not found"

-- | Require a file handle but do not modify the mock file system
withHandleRead :: CanSimFS m
               => Handle
               -> (    MockFS
                    -> HandleState
                    -> Except FsError (a, HandleState)
                  )
               -> m a
withHandleRead h f = withHandleModify h $ \fs hs ->
                       second (mockFiles fs, ) <$> f fs hs

-- | Require an open file handle to modify the mock file system
withOpenHandleModify :: CanSimFS m
                     => Handle
                     -> (    MockFS
                          -> OpenHandleState
                          -> Except FsError (a, (Files, OpenHandleState))
                        )
                     -> m a
withOpenHandleModify h f = withHandleModify h $ \fs -> \case
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
                   => Handle
                   -> (    MockFS
                        -> OpenHandleState
                        -> Except FsError (a, OpenHandleState)
                      )
                   -> m a
withOpenHandleRead h f = withHandleRead h $ \fs -> \case
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

checkFsTree' :: HasCallStack
             => Either FsTreeError a -> Except FsError (Either FsPath a)
checkFsTree' = go
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

checkFsTree :: HasCallStack => Either FsTreeError a -> Except FsError a
checkFsTree ma = do
    ma' <- checkFsTree' ma
    case ma' of
      Left fp -> throwError FsError {
                     fsErrorType   = FsResourceDoesNotExist
                   , fsErrorPath   = fp
                   , fsErrorString = "does not exist"
                   , fsErrorStack  = callStack
                   , fsLimitation  = False
                   }
      Right a -> return a

checkDoesNotExist :: HasCallStack => MockFS -> FsPath -> Except FsError ()
checkDoesNotExist fs fp = do
    exists <- fmap pathExists $ checkFsTree' $ FS.index fp (mockFiles fs)
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

-- TODO: This should just be a pure function
newHandle :: Monad m => MockFS -> OpenHandleState -> m (Handle, MockFS)
newHandle fs hs = return (
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
hOpen :: CanSimFS m => FsPath -> IOMode -> m Handle
hOpen fp ioMode = do
    dirExists <- doesDirectoryExist fp
    when dirExists $ throwError FsError {
        fsErrorType   = FsResourceInappropriateType
      , fsErrorPath   = fp
      , fsErrorString = "hOpen: directories not supported"
      , fsErrorStack  = callStack
      , fsLimitation  = True
      }
    modifyMockFS $ \fs -> do
      let alreadyHasWriter = any (\hs -> openFilePath hs == fp) $ openHandles fs
      when (ioMode /= IO.ReadMode && alreadyHasWriter) $
        throwError FsError {
            fsErrorType   = FsInvalidArgument
          , fsErrorPath   = fp
          , fsErrorString = "more than one concurrent writer not supported"
          , fsErrorStack  = callStack
          , fsLimitation  = True
          }
      when (ioMode == IO.ReadMode) $ void $
        checkFsTree $ FS.getFile fp (mockFiles fs)
      files' <- checkFsTree $ FS.touch fp (mockFiles fs)
      newHandle (fs { mockFiles = files' }) $ OpenHandle fp (filePtr ioMode)
  where
    filePtr :: IOMode -> FilePtr
    filePtr IO.ReadMode      = RW True  False 0
    filePtr IO.WriteMode     = RW False True  0
    filePtr IO.ReadWriteMode = RW True  True  0
    filePtr IO.AppendMode    = Append

-- | Mock implementation of 'hClose'
hClose :: CanSimFS m => Handle -> m ()
hClose h = withHandleRead h $ \_fs -> \case
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
hSeek :: CanSimFS m => Handle -> SeekMode -> Int64 -> m ()
hSeek h seekMode o = withOpenHandleRead h $ \fs hs -> do
    openPtr' <- except $ seekFilePtr fs h seekMode o
    return ((), hs { openPtr = openPtr' })

-- | Get bytes from handle
--
-- NOTE: Unlike real I/O, we disallow 'hGet' on a handle in append mode.
hGet :: CanSimFS m => Handle -> Int -> m ByteString
hGet h n = withOpenHandleRead h $ \fs hs@OpenHandle{..} -> do
    file <- checkFsTree $ FS.getFile openFilePath (mockFiles fs)
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

hPut :: CanSimFS m => Handle -> Builder -> m Word64
hPut h builder = withOpenHandleModify h $ \fs hs@OpenHandle{..} -> do
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

hPutBuffer :: CanSimFS m => Handle -> buf -> Builder -> m Word64
hPutBuffer hnd _buf builder = hPut hnd builder

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
hTruncate :: CanSimFS m => Handle -> Word64 -> m ()
hTruncate h sz = withOpenHandleModify h $ \fs hs@OpenHandle{..} -> do
    file <- checkFsTree $ FS.getFile openFilePath (mockFiles fs)
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
    files' <- checkFsTree $ FS.replace openFilePath file' (mockFiles fs)
    -- TODO: Don't replace the file pointer (not changed)
    return ((), (files', hs { openPtr = ptr' }))

-- | Get file size
--
-- NOTE: In the mock implementation this is thread safe, because there can be
-- only one writer, so concurrent threads cannot change the size of the file.
hGetSize :: CanSimFS m => Handle -> m Word64
hGetSize h = withOpenHandleRead h $ \fs hs@OpenHandle{..} -> do
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

createDirectoryIfMissing :: CanSimFS m => Bool -> FsPath -> m ()
createDirectoryIfMissing createParents dir = do
    -- Although @createDirectoryIfMissing /a/b/c@ will fail ("inappropriate
    -- type") if @b@ is a file (not a directory), for some strange reason it
    -- throws "already exists" if @c@ is is a file
    fileExists <- doesFileExist dir
    if fileExists then
      throwError FsError {
          fsErrorType   = FsResourceAlreadyExist
        , fsErrorPath   = dir
        , fsErrorString = "a file with that name already exists"
        , fsErrorStack  = callStack
        , fsLimitation  = False
        }
    else modifyMockFS $ \fs -> do
      files' <- checkFsTree $ go createParents (mockFiles fs)
      return ((), fs { mockFiles = files' })
  where
    go :: Bool -> Files -> Either FsTreeError Files
    go True  = FS.createDirWithParents dir
    go False = FS.createDirIfMissing   dir

listDirectory :: CanSimFS m => FsPath -> m (Set String)
listDirectory fp = readMockFS $
    fmap M.keysSet . checkFsTree . FS.getDir fp

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
