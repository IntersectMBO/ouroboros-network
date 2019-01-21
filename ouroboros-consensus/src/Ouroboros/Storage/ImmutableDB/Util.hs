{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Ouroboros.Storage.ImmutableDB.Util where

import           Control.Monad (when)

import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BS
import           Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Text as T

import           GHC.Stack (HasCallStack, callStack, popCallStack)

import           Text.Printf (printf)
import           Text.Read (readMaybe)

import           Ouroboros.Storage.FS.API
import           Ouroboros.Storage.FS.API.Types
import           Ouroboros.Storage.ImmutableDB.Types
import           Ouroboros.Storage.Util.ErrorHandling (ErrorHandling (..))
import qualified Ouroboros.Storage.Util.ErrorHandling as EH

{------------------------------------------------------------------------------
  Utilities
------------------------------------------------------------------------------}


renderFile :: String -> Epoch -> FsPath
renderFile fileType epoch = [printf "%s-%03d.dat" fileType epoch]

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
readAll :: Monad m => HasFS m -> FsHandle m -> m BS.Builder
readAll HasFS{..} hnd = go mempty
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
hGetRightSize :: (HasCallStack, Monad m)
              => HasFS m
              -> FsHandle m
              -> Int     -- ^ The number of bytes to read.
              -> FsPath  -- ^ The file corresponding with the handle, used for
                         -- error reporting
              -> m ByteString
hGetRightSize HasFS{..} hnd size file = do
    bytes <- hGet hnd size
    if BS.length bytes /= size
      then throwError hasFsErr $ FsError
             { fsErrorType   = FsReachedEOF
             , fsErrorPath   = file
             , fsErrorString = errMsg
             , fsErrorStack  = callStack
             , fsLimitation  = False
             }
      else return bytes
  where
    errMsg = "different number of bytes read by hGet than expected"

-- | Look up the size of the given 'Epoch'.
--
-- This should should not fail if the epoch <= the currently opened epoch and
-- the given mapping is retrieved from the DB, as 'openDB' and 'startNewEpoch'
-- make sure this mapping is complete.
--
-- Throws an 'MissingEpochSizeError' if the epoch is not in the map.
lookupEpochSize :: (Monad m, HasCallStack)
                => ErrorHandling ImmutableDBError m
                -> Epoch
                -> Map Epoch EpochSize
                -> m EpochSize
lookupEpochSize err epoch epochSizes
    | Just epochSize <- Map.lookup epoch epochSizes
    = return epochSize
    | otherwise
    = throwUserError err $ MissingEpochSizeError epoch

-- | Check whether the given iterator range is valid.
--
-- See 'streamBinaryBlobs'.
validateIteratorRange
  :: Monad m
  => ErrorHandling ImmutableDBError m
  -> EpochSlot            -- ^ Next expected write
  -> (Epoch -> m EpochSize)
     -- ^ How to look up the size of an epoch
  -> EpochSlot            -- ^ range start (inclusive)
  -> EpochSlot            -- ^ range end (inclusive)
  -> m ()
validateIteratorRange err next getEpochSize start end = do
    let EpochSlot startEpoch   startSlot = start
        EpochSlot endEpoch     endSlot   = end

    when (start > end) $
      throwUserError err $ InvalidIteratorRangeError start end

    -- Check that the start is not >= the next expected slot
    when (start >= next) $
      throwUserError err $ ReadFutureSlotError start next

    -- Check that the end is not >= the next expected slot
    when (end >= next) $
      throwUserError err $ ReadFutureSlotError end next

    -- Check that the start slot does not exceed its epoch size
    startEpochSize <- getEpochSize startEpoch
    when (getRelativeSlot startSlot >= startEpochSize) $
      throwUserError err $ SlotGreaterThanEpochSizeError
                              startSlot
                              startEpoch
                              startEpochSize

    -- Check that the end slot does not exceed its epoch size
    endEpochSize <- getEpochSize endEpoch
    when (getRelativeSlot endSlot >= endEpochSize) $
      throwUserError err $ SlotGreaterThanEpochSizeError
                             endSlot
                             endEpoch
                             endEpochSize
