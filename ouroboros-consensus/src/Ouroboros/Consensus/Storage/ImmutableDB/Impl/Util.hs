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
  , throwUserError
  , throwUnexpectedError
  , tryImmDB
  , parseDBFile
  , validateIteratorRange
  , epochSlotToTip
  , dbFilesOnDisk
  , removeFilesStartingFrom
  , runGet
  , runGetWithUnconsumed
  , checkChecksum
  ) where

import           Control.Monad (forM_, when)
import           Control.Monad.Except (lift, runExceptT, throwError)
import           Data.Binary.Get (Get)
import qualified Data.Binary.Get as Get
import qualified Data.ByteString.Lazy as Lazy
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
import           Ouroboros.Consensus.Storage.ImmutableDB.Layout
import           Ouroboros.Consensus.Storage.ImmutableDB.Types

{------------------------------------------------------------------------------
  Utilities
------------------------------------------------------------------------------}

-- | Useful when you have exactly two values of some type and want to
-- 'traverse' over both of them (which is not possible with a tuple).
data Two a = Two a a
  deriving (Functor, Foldable, Traversable)

renderFile :: Text -> EpochNo -> FsPath
renderFile fileType (EpochNo epoch) = fsPathFromList [name]
  where
    name = T.justifyRight 5 '0' (T.pack (show epoch)) <> "." <> fileType

throwUserError :: (MonadThrow m, HasCallStack) => UserError -> m a
throwUserError e = throwM $ UserError e callStack

throwUnexpectedError :: MonadThrow m => UnexpectedError -> m a
throwUnexpectedError = throwM . UnexpectedError

-- | Execute an action and catch the 'ImmutableDBError' and 'FsError' that can
-- be thrown by it, and wrap the 'FsError' in an 'ImmutableDBError' using the
-- 'FileSystemError' constructor.
--
-- This should be used whenever you want to run an action on the ImmutableDB
-- and catch the 'ImmutableDBError' and the 'FsError' (wrapped in the former)
-- it may thrown.
tryImmDB :: MonadCatch m => m a -> m (Either ImmutableDBError a)
tryImmDB = fmap squash . try . try
  where
    fromFS = UnexpectedError . FileSystemError

    squash :: Either FsError (Either ImmutableDBError a)
           -> Either ImmutableDBError a
    squash = either (Left . fromFS) id

-- | Parse the prefix and epoch number from the filename of an index or epoch
-- file.
--
-- > parseDBFile "00001.epoch"
-- Just ("epoch", 1)
-- > parseDBFile "00012.primary"
-- Just ("primary", 12)
parseDBFile :: String -> Maybe (String, EpochNo)
parseDBFile s = case T.splitOn "." $ T.pack s of
    [n, ext] -> (T.unpack ext,) . EpochNo <$> readMaybe (T.unpack n)
    _        -> Nothing

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
  => ChunkInfo m
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
      isNewer <- lift $ isNewerThanTip start
      when isNewer $
        throwError $ UserError (ReadFutureSlotError start tip) callStack

    whenJust mbEnd $ \(end, _) -> do
      isNewer <- lift $ isNewerThanTip end
      when isNewer $
        throwError $ UserError (ReadFutureSlotError end tip) callStack
  where
    isNewerThanTip :: SlotNo -> m Bool
    isNewerThanTip slot = case tip of
      Origin               -> return True
      At (EBB   lastEpoch) -> (slot >) <$> epochInfoFirst chunkInfo lastEpoch
      At (Block lastSlot)  -> return $ slot > lastSlot

-- | Convert an 'EpochSlot' to a 'Tip'
epochSlotToTip :: Monad m => ChunkInfo m -> EpochSlot -> m ImmTip
epochSlotToTip _         (EpochSlot epoch 0) = return $ At (EBB epoch)
epochSlotToTip chunkInfo epochSlot           = At . Block <$>
    epochInfoAbsolute chunkInfo epochSlot

-- | Go through all files, making three sets: the set of epoch files, primary
-- index files, and secondary index files,, discarding all others.
dbFilesOnDisk :: Set String -> (Set EpochNo, Set EpochNo, Set EpochNo)
dbFilesOnDisk = foldr categorise mempty
  where
    categorise file fs@(epoch, primary, secondary) =
      case parseDBFile file of
        Just ("epoch",     n) -> (Set.insert n epoch, primary, secondary)
        Just ("primary",   n) -> (epoch, Set.insert n primary, secondary)
        Just ("secondary", n) -> (epoch, primary, Set.insert n secondary)
        _                     -> fs

-- | Remove all epoch and index starting from the given epoch (included).
removeFilesStartingFrom :: (HasCallStack, Monad m)
                        => HasFS m h
                        -> EpochNo
                        -> m ()
removeFilesStartingFrom HasFS { removeFile, listDirectory } epoch = do
    filesInDBFolder <- listDirectory (mkFsPath [])
    let (epochFiles, primaryFiles, secondaryFiles) = dbFilesOnDisk filesInDBFolder
    forM_ (takeWhile (>= epoch) (Set.toDescList epochFiles)) $ \e ->
      removeFile (renderFile "epoch" e)
    forM_ (takeWhile (>= epoch) (Set.toDescList primaryFiles)) $ \i ->
      removeFile (renderFile "primary" i)
    forM_ (takeWhile (>= epoch) (Set.toDescList secondaryFiles)) $ \i ->
      removeFile (renderFile "secondary" i)

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
checkChecksum epochFile blockOrEBB expected actual
    | expected == actual
    = return ()
    | otherwise
    = throwUnexpectedError $
      ChecksumMismatchError blockOrEBB expected actual epochFile callStack
