{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
module Ouroboros.Storage.ImmutableDB.Util where

import           Control.Monad (when)
import           Control.Monad.Except (MonadError, ExceptT, withExceptT,
                                       throwError)

import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BS
import           Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Text as T

import           GHC.Stack (HasCallStack, callStack, popCallStack)

import           Text.Printf (printf)
import           Text.Read (readMaybe)

import           Ouroboros.Storage.FS.Class
import           Ouroboros.Storage.FS.Class.Types
import           Ouroboros.Storage.ImmutableDB.Types


{------------------------------------------------------------------------------
  Utilities
------------------------------------------------------------------------------}


renderFile :: String -> Epoch -> FsPath
renderFile fileType epoch = [printf "%s-%03d.dat" fileType epoch]

liftFsError :: Functor m => ExceptT FsError m a -> ExceptT ImmutableDBError m a
liftFsError = withExceptT (UnexpectedError . FileSystemError)

throwUserError :: (HasCallStack, Monad m)
               => UserError -> ExceptT ImmutableDBError m a
throwUserError ue = throwError $ UserError ue (popCallStack callStack)
-- 'popCallStack' to remove the call to 'throwError' from the call stack

throwUnexpectedError :: Monad m
                     => UnexpectedError -> ExceptT ImmutableDBError m a
throwUnexpectedError ue = throwError $ UnexpectedError ue


-- | Parse the epoch number from the filename of an index or epoch file.
--
-- > parseEpochNumber "epoch-001.dat"
-- Just 1
parseEpochNumber :: String -> Maybe Epoch
parseEpochNumber = readMaybe
                 . T.unpack
                 . snd
                 . T.breakOnEnd "-"
                 . fst
                 . T.breakOn "."
                 . T.pack

-- | Read all the data from the given file handle 64kB at a time.
--
-- TODO move to Ouroboros.Storage.FS.Class?
readAll :: HasFSE m => FsHandleE m -> ExceptT FsError m BS.Builder
readAll hnd = go mempty
  where
    bufferSize = 64 * 1024
    go acc = do
      bytesRead <- hGet hnd bufferSize
      let acc' = acc <> BS.byteString bytesRead
      case BS.length bytesRead < bufferSize of
        True  -> return acc'
        False -> go acc'

-- | Variant of 'hGet' that throws an 'FsReachedEOF' 'FsError' when the gotten
-- number of bytes didn't match the number of request bytes.
--
-- TODO move to Ouroboros.Storage.FS.Class?
hGetRightSize :: (HasCallStack, HasFSE m, Monad m)
              => FsHandleE m
              -> Int     -- ^ The number of bytes to read.
              -> FsPath  -- ^ The file corresponding with the handle, used for
                         -- error reporting
              -> ExceptT FsError m ByteString
hGetRightSize hnd size file = do
    bytes <- hGet hnd size
    if BS.length bytes /= size
      then throwError $ FsError
             { fsErrorType   = FsReachedEOF
             , fsErrorPath   = file
             , fsErrorString = errMsg
             , fsErrorStack  = callStack
             , fsLimitation  = False
             }
      else return bytes
  where
    errMsg = "different number of bytes read by hGet than expected"

-- | Look up the size of the given 'Epoch'. Variant of 'getEpochSize'.
--
-- This should should not fail if the epoch <= the currently opened epoch and
-- the given mapping is retrieved from the DB, as 'openDB' and 'startNewEpoch'
-- make sure this mapping is complete.
--
-- Throws an 'MissingEpochSizeError' if the epoch is not in the map.
lookupEpochSize :: (HasCallStack, MonadError ImmutableDBError m)
                => Epoch -> Map Epoch EpochSize
                -> m EpochSize
lookupEpochSize epoch epochSizes
    | Just epochSize <- Map.lookup epoch epochSizes
    = return epochSize
    | otherwise
    = throwError $ UserError (MissingEpochSizeError epoch) callStack

-- | Check whether the given iterator range is valid.
--
-- See 'streamBinaryBlobs'.
validateIteratorRange
  :: Monad m
  => EpochSlot            -- ^ Next expected write
  -> Map Epoch EpochSize  -- ^ Epoch sizes
  -> EpochSlot            -- ^ range start (inclusive)
  -> EpochSlot            -- ^ range end (inclusive)
  -> ExceptT ImmutableDBError m ()
validateIteratorRange next epochSizes start end = do
    let EpochSlot startEpoch   startSlot = start
        EpochSlot endEpoch     endSlot   = end

    when (start > end) $
      throwUserError $ InvalidIteratorRangeError start end

    -- Check that the start is not >= the next expected slot
    when (start >= next) $
      throwUserError $ ReadFutureSlotError start next

    -- Check that the end is not >= the next expected slot
    when (end >= next) $
      throwUserError $ ReadFutureSlotError end next

    -- Check that the start slot does not exceed its epoch size
    startEpochSize <- lookupEpochSize startEpoch epochSizes
    when (getRelativeSlot startSlot >= startEpochSize) $
      throwUserError $ SlotGreaterThanEpochSizeError startSlot startEpoch
                       startEpochSize

    -- Check that the end slot does not exceed its epoch size
    endEpochSize <- lookupEpochSize endEpoch epochSizes
    when (getRelativeSlot endSlot >= endEpochSize) $
      throwUserError $ SlotGreaterThanEpochSizeError endSlot endEpoch
                       endEpochSize
