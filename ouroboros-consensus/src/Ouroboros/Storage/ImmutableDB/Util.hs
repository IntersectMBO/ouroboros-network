{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TupleSections     #-}
module Ouroboros.Storage.ImmutableDB.Util
  ( renderFile
  , handleUser
  , handleUnexpected
  , throwUserError
  , throwUnexpectedError
  , parseDBFile
  , readAll
  , hGetRightSize
  , validateIteratorRange
  ) where

import           Control.Monad (when)

import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BS
import qualified Data.Text as T

import           GHC.Stack (HasCallStack, callStack, popCallStack)

import           Text.Printf (printf)
import           Text.Read (readMaybe)

import           Ouroboros.Consensus.Util (whenJust)

import           Ouroboros.Storage.FS.API
import           Ouroboros.Storage.FS.API.Types
import           Ouroboros.Storage.ImmutableDB.Types
import           Ouroboros.Storage.Util.ErrorHandling (ErrorHandling (..))
import qualified Ouroboros.Storage.Util.ErrorHandling as EH

{------------------------------------------------------------------------------
  Utilities
------------------------------------------------------------------------------}


renderFile :: String -> Epoch -> FsPath
renderFile fileType (Epoch epoch) = [printf "%s-%03d.dat" fileType epoch]

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

-- | Parse the prefix and epoch number from the filename of an index or epoch
-- file.
--
-- > parseDBFile "epoch-001.dat"
-- Just ("epoch", 1)
-- > parseDBFile "index-012.dat"
-- Just ("index", 12)
parseDBFile :: String -> Maybe (String, Epoch)
parseDBFile s = case T.splitOn "-" . fst . T.breakOn "." . T.pack $ s of
    [prefix, n] -> (T.unpack prefix,) . Epoch <$> readMaybe (T.unpack n)
    _           -> Nothing

-- | Read all the data from the given file handle 64kB at a time.
--
-- TODO move to Ouroboros.Storage.FS.Class?
readAll :: Monad m => HasFS m h -> h -> m BS.Builder
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
              => HasFS m h
              -> h
              -> Int     -- ^ The number of bytes to read.
              -> FsPath  -- ^ The file corresponding with the handle, used for
                         -- error reporting
              -> m ByteString
hGetRightSize HasFS{..} hnd size file = do
    bytes <- hGet hnd size
    -- TODO loop until we get the number of bytes requested, see #277
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

-- | Check whether the given iterator range is valid.
--
-- See 'Ouroboros.Storage.ImmutableDB.API.streamBinaryBlobs'.
validateIteratorRange
  :: Monad m
  => ErrorHandling ImmutableDBError m
  -> Slot        -- ^ Next expected write
  -> Maybe Slot  -- ^ range start (inclusive)
  -> Maybe Slot  -- ^ range end (inclusive)
  -> m ()
validateIteratorRange err next mbStart mbEnd = do
    case (mbStart, mbEnd) of
      (Just start, Just end) ->
        when (start > end) $
          throwUserError err $ InvalidIteratorRangeError start end
      _ -> return ()

    whenJust mbStart $ \start ->
      -- Check that the start is not >= the next expected slot
      when (start >= next) $
        throwUserError err $ ReadFutureSlotError start next

    whenJust mbEnd $ \end ->
      -- Check that the end is not >= the next expected slot
      when (end >= next) $
        throwUserError err $ ReadFutureSlotError end next
