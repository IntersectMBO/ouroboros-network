{-# LANGUAGE DeriveTraversable   #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
module Ouroboros.Storage.ImmutableDB.Impl.Util
  ( -- * Utilities
    Two (..)
  , renderFile
  , handleUser
  , handleUnexpected
  , throwUserError
  , throwUnexpectedError
  , tryImmDB
  , parseDBFile
  , validateIteratorRange
  , onException
  , epochSlotToTip
  , dbFilesOnDisk
  , removeFilesStartingFrom
  , runGet
  , runGetWithUnconsumed
  , checkChecksum
  ) where

import           Control.Monad (forM_, when)
import           Data.Binary.Get (Get)
import qualified Data.Binary.Get as Get
import qualified Data.ByteString.Lazy as Lazy
import           Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Text as T
import           GHC.Stack (HasCallStack, callStack, popCallStack)
import           Text.Printf (printf)
import           Text.Read (readMaybe)

import           Ouroboros.Consensus.Util (whenJust)

import           Ouroboros.Storage.Common
import           Ouroboros.Storage.EpochInfo.API
import           Ouroboros.Storage.FS.API
import           Ouroboros.Storage.FS.API.Types
import           Ouroboros.Storage.FS.CRC
import           Ouroboros.Storage.Util.ErrorHandling (ErrorHandling (..))
import qualified Ouroboros.Storage.Util.ErrorHandling as EH

import           Ouroboros.Storage.ImmutableDB.Layout
import           Ouroboros.Storage.ImmutableDB.Types

{------------------------------------------------------------------------------
  Utilities
------------------------------------------------------------------------------}

-- | Useful when you have exactly two values of some type and want to
-- 'traverse' over both of them (which is not possible with a tuple).
data Two a = Two a a
  deriving (Functor, Foldable, Traversable)

renderFile :: String -> EpochNo -> FsPath
renderFile fileType (EpochNo epoch) = mkFsPath [printf "%05d.%s" epoch fileType]

handleUser :: HasCallStack
           => ErrorHandling ImmutableDBError m
           -> ErrorHandling UserError        m
handleUser = EH.embed (flip UserError (popCallStack callStack)) $ \case
               UserError e _ -> Just e
               _otherwise    -> Nothing

handleUnexpected :: ErrorHandling ImmutableDBError m
                 -> ErrorHandling UnexpectedError  m
handleUnexpected = EH.embed UnexpectedError $ \case
                     UnexpectedError e -> Just e
                     _otherwise        -> Nothing

throwUserError :: HasCallStack
               => ErrorHandling ImmutableDBError m
               -> UserError -> m a
throwUserError = throwError . handleUser

throwUnexpectedError :: ErrorHandling ImmutableDBError m
                     -> UnexpectedError -> m a
throwUnexpectedError = throwError . handleUnexpected

-- | Execute an action and catch the 'ImmutableDBError' and 'FsError' that can
-- be thrown by it, and wrap the 'FsError' in an 'ImmutableDBError' using the
-- 'FileSystemError' constructor.
--
-- This should be used whenever you want to run an action on the ImmutableDB
-- and catch the 'ImmutableDBError' and the 'FsError' (wrapped in the former)
-- it may thrown.
tryImmDB :: Monad m
         => ErrorHandling FsError          m
         -> ErrorHandling ImmutableDBError m
         -> m a -> m (Either ImmutableDBError a)
tryImmDB fsErr immDBErr = fmap squash . EH.try fsErr . EH.try immDBErr
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
-- See 'Ouroboros.Storage.ImmutableDB.API.streamBinaryBlobs'.
validateIteratorRange
  :: forall m hash. Monad m
  => ErrorHandling ImmutableDBError m
  -> EpochInfo m
  -> ImmTip
  -> Maybe (SlotNo, hash)  -- ^ range start (inclusive)
  -> Maybe (SlotNo, hash)  -- ^ range end (inclusive)
  -> m ()
validateIteratorRange err epochInfo tip mbStart mbEnd = do
    case (mbStart, mbEnd) of
      (Just (start, _), Just (end, _)) ->
        when (start > end) $
          throwUserError err $ InvalidIteratorRangeError start end
      _ -> return ()

    whenJust mbStart $ \(start, _) -> do
      isNewer <- isNewerThanTip start
      when isNewer $
        throwUserError err $ ReadFutureSlotError start tip

    whenJust mbEnd $ \(end, _) -> do
      isNewer <- isNewerThanTip end
      when isNewer $
        throwUserError err $ ReadFutureSlotError end tip
  where
    isNewerThanTip :: SlotNo -> m Bool
    isNewerThanTip slot = case tip of
      TipGen                -> return True
      Tip (EBB   lastEpoch) -> (slot >) <$> epochInfoFirst epochInfo lastEpoch
      Tip (Block lastSlot)  -> return $ slot > lastSlot

-- | Execute some error handler when an 'ImmutableDBError' or an 'FsError' is
-- thrown while executing an action.
onException :: Monad m
            => ErrorHandling FsError m
            -> ErrorHandling ImmutableDBError m
            -> m b  -- ^ What to do when an error is thrown
            -> m a  -- ^ The action to execute
            -> m a
onException fsErr err onErr m =
    EH.onException fsErr (EH.onException err m onErr) onErr

-- | Convert an 'EpochSlot' to a 'Tip'
epochSlotToTip :: Monad m => EpochInfo m -> EpochSlot -> m ImmTip
epochSlotToTip _         (EpochSlot epoch 0) = return $ Tip (EBB epoch)
epochSlotToTip epochInfo epochSlot           = Tip . Block <$>
    epochInfoAbsolute epochInfo epochSlot

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
  :: (HasCallStack, Monad m)
  => ErrorHandling ImmutableDBError m
  -> FsPath
  -> Get a
  -> Lazy.ByteString
  -> m a
runGet err file get bl = case Get.runGetOrFail get bl of
    Right (unconsumed, _, primary)
      | Lazy.null unconsumed
      -> return primary
      | otherwise
      -> throwUnexpectedError err $ InvalidFileError file "left-over bytes" callStack
    Left (_, _, msg)
      -> throwUnexpectedError err $ InvalidFileError file msg callStack

-- | Same as 'runGet', but allows unconsumed input and returns it.
runGetWithUnconsumed
  :: (HasCallStack, Monad m)
  => ErrorHandling ImmutableDBError m
  -> FsPath
  -> Get a
  -> Lazy.ByteString
  -> m (Lazy.ByteString, a)
runGetWithUnconsumed err file get bl = case Get.runGetOrFail get bl of
    Right (unconsumed, _, primary)
      -> return (unconsumed, primary)
    Left (_, _, msg)
      -> throwUnexpectedError err $ InvalidFileError file msg callStack

-- | Check whether the given checksums match. If not, throw a
-- 'ChecksumMismatchError'.
checkChecksum
  :: (HasCallStack, Monad m)
  => ErrorHandling ImmutableDBError m
  -> FsPath
  -> BlockOrEBB
  -> CRC  -- ^ Expected checksum
  -> CRC  -- ^ Actual checksum
  -> m ()
checkChecksum err epochFile blockOrEBB expected actual
    | expected == actual
    = return ()
    | otherwise
    = throwUnexpectedError err $
      ChecksumMismatchError blockOrEBB expected actual epochFile callStack
