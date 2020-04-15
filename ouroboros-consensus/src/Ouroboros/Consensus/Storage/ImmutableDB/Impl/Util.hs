{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE DeriveTraversable   #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
module Ouroboros.Consensus.Storage.ImmutableDB.Impl.Util
  ( -- * Utilities
    Two (..)
  , renderFile
  , fsPathChunkFile
  , fsPathPrimaryIndexFile
  , fsPathSecondaryIndexFile
  , throwUserError
  , throwUnexpectedError
  , wrapFsError
  , tryImmDB
  , parseDBFile
  , validateIteratorRange
  , dbFilesOnDisk
  , removeFilesStartingFrom
  , runGet
  , runGetWithUnconsumed
  , checkChecksum
  ) where

import           Control.Monad (forM_, when)
import           Control.Monad.Except (runExceptT, throwError)
import           Data.Binary.Get (Get)
import qualified Data.Binary.Get as Get
import qualified Data.ByteString.Lazy as Lazy
import           Data.List (foldl')
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Text (Text)
import qualified Data.Text as T
import           GHC.Stack (HasCallStack, callStack)
import           Text.Read (readMaybe)

import           Cardano.Slotting.Slot

import           Ouroboros.Consensus.Util (whenJust)
import           Ouroboros.Consensus.Util.IOLike

import           Ouroboros.Consensus.Storage.FS.API
import           Ouroboros.Consensus.Storage.FS.API.Types
import           Ouroboros.Consensus.Storage.FS.CRC

import           Ouroboros.Consensus.Storage.ImmutableDB.Chunks
import           Ouroboros.Consensus.Storage.ImmutableDB.Chunks.Internal
                     (ChunkNo (..))
import           Ouroboros.Consensus.Storage.ImmutableDB.Types

{------------------------------------------------------------------------------
  Utilities
------------------------------------------------------------------------------}

-- | Useful when you have exactly two values of some type and want to
-- 'traverse' over both of them (which is not possible with a tuple).
data Two a = Two a a
  deriving (Functor, Foldable, Traversable)

fsPathChunkFile :: ChunkNo -> FsPath
fsPathChunkFile = renderFile "chunk"

fsPathPrimaryIndexFile :: ChunkNo -> FsPath
fsPathPrimaryIndexFile = renderFile "primary"

fsPathSecondaryIndexFile :: ChunkNo -> FsPath
fsPathSecondaryIndexFile = renderFile "secondary"

-- | Opposite of 'parseDBFile'.
renderFile :: Text -> ChunkNo -> FsPath
renderFile fileType (ChunkNo chunk) = fsPathFromList [name]
  where
    name = T.justifyRight 5 '0' (T.pack (show chunk)) <> "." <> fileType

-- | Parse the prefix and chunk number from the filename of an index or chunk
-- file.
--
-- > parseDBFile "00001.chunk"
-- Just ("chunk", 1)
-- > parseDBFile "00012.primary"
-- Just ("primary", 12)
parseDBFile :: String -> Maybe (String, ChunkNo)
parseDBFile s = case T.splitOn "." $ T.pack s of
    [n, ext] -> (T.unpack ext,) . ChunkNo <$> readMaybe (T.unpack n)
    _        -> Nothing

-- | Go through all files, making three sets: the set of chunk files, primary
-- index files, and secondary index files, discarding all others.
dbFilesOnDisk :: Set String -> (Set ChunkNo, Set ChunkNo, Set ChunkNo)
dbFilesOnDisk = foldl' categorise mempty
  where
    categorise fs@(!chunk, !primary, !secondary) file =
      case parseDBFile file of
        Just ("chunk",     n) -> (Set.insert n chunk, primary, secondary)
        Just ("primary",   n) -> (chunk, Set.insert n primary, secondary)
        Just ("secondary", n) -> (chunk, primary, Set.insert n secondary)
        _                     -> fs

-- | Remove all chunk and index starting from the given chunk (included).
removeFilesStartingFrom :: (HasCallStack, Monad m)
                        => HasFS m h
                        -> ChunkNo
                        -> m ()
removeFilesStartingFrom HasFS { removeFile, listDirectory } chunk = do
    filesInDBFolder <- listDirectory (mkFsPath [])
    let (chunkFiles, primaryFiles, secondaryFiles) = dbFilesOnDisk filesInDBFolder
    forM_ (takeWhile (>= chunk) (Set.toDescList chunkFiles)) $ \e ->
      removeFile (fsPathChunkFile e)
    forM_ (takeWhile (>= chunk) (Set.toDescList primaryFiles)) $ \i ->
      removeFile (fsPathPrimaryIndexFile i)
    forM_ (takeWhile (>= chunk) (Set.toDescList secondaryFiles)) $ \i ->
      removeFile (fsPathSecondaryIndexFile i)

throwUserError :: (MonadThrow m, HasCallStack) => UserError -> m a
throwUserError e = throwM $ UserError e callStack

throwUnexpectedError :: MonadThrow m => UnexpectedError -> m a
throwUnexpectedError = throwM . UnexpectedError

-- | Rewrap 'FsError' in a 'ImmutableDBError'.
wrapFsError :: MonadCatch m => m a -> m a
wrapFsError = handle $ throwUnexpectedError . FileSystemError

-- | Execute an action and catch the 'ImmutableDBError' and 'FsError' that can
-- be thrown by it, and wrap the 'FsError' in an 'ImmutableDBError' using the
-- 'FileSystemError' constructor.
--
-- This should be used whenever you want to run an action on the ImmutableDB
-- and catch the 'ImmutableDBError' and the 'FsError' (wrapped in the former)
-- it may thrown.
tryImmDB :: MonadCatch m => m a -> m (Either ImmutableDBError a)
tryImmDB = try . wrapFsError

-- | Check whether the given iterator range is valid.
--
-- \"Valid\" means:
--
-- * The start slot <= the end slot
-- * The start slot is <= the tip
-- * The end slot is <= the tip
--
-- The @hash@ is ignored.
--
-- See 'Ouroboros.Consensus.Storage.ImmutableDB.API.streamBinaryBlobs'.
validateIteratorRange
  :: forall m hash. Monad m
  => ChunkInfo
  -> ImmTip
  -> Maybe (SlotNo, hash)  -- ^ range start (inclusive)
  -> Maybe (SlotNo, hash)  -- ^ range end (inclusive)
  -> m (Either ImmutableDBError ())
validateIteratorRange chunkInfo tip mbStart mbEnd = runExceptT $ do
    case (mbStart, mbEnd) of
      (Just (start, _), Just (end, _)) ->
        when (start > end) $
          throwError $ UserError (InvalidIteratorRangeError start end) callStack
      _ -> return ()

    whenJust mbStart $ \(start, _) -> do
      let isNewer = isNewerThanTip start
      when isNewer $
        throwError $ UserError (ReadFutureSlotError start tip) callStack

    whenJust mbEnd $ \(end, _) -> do
      let isNewer = isNewerThanTip end
      when isNewer $
        throwError $ UserError (ReadFutureSlotError end tip) callStack
  where
    isNewerThanTip :: SlotNo -> Bool
    isNewerThanTip slot = case tip of
      Origin -> True
      At b   -> slot > slotNoOfBlockOrEBB chunkInfo b

-- | Wrapper around 'Get.runGetOrFail' that throws an 'InvalidFileError' when
-- it failed or when there was unconsumed input.
runGet
  :: (HasCallStack, MonadThrow m)
  => FsPath
  -> Get a
  -> Lazy.ByteString
  -> m a
runGet file get bl = case Get.runGetOrFail get bl of
    Right (unconsumed, _, primary)
      | Lazy.null unconsumed
      -> return primary
      | otherwise
      -> throwUnexpectedError $ InvalidFileError file "left-over bytes" callStack
    Left (_, _, msg)
      -> throwUnexpectedError $ InvalidFileError file msg callStack

-- | Same as 'runGet', but allows unconsumed input and returns it.
runGetWithUnconsumed
  :: (HasCallStack, MonadThrow m)
  => FsPath
  -> Get a
  -> Lazy.ByteString
  -> m (Lazy.ByteString, a)
runGetWithUnconsumed file get bl = case Get.runGetOrFail get bl of
    Right (unconsumed, _, primary)
      -> return (unconsumed, primary)
    Left (_, _, msg)
      -> throwUnexpectedError $ InvalidFileError file msg callStack

-- | Check whether the given checksums match. If not, throw a
-- 'ChecksumMismatchError'.
checkChecksum
  :: (HasCallStack, MonadThrow m)
  => FsPath
  -> BlockOrEBB
  -> CRC  -- ^ Expected checksum
  -> CRC  -- ^ Actual checksum
  -> m ()
checkChecksum chunkFile blockOrEBB expected actual
    | expected == actual
    = return ()
    | otherwise
    = throwUnexpectedError $
      ChecksumMismatchError blockOrEBB expected actual chunkFile callStack
