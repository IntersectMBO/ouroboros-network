{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
module Ouroboros.Storage.ImmutableDB.Util
  ( -- * Utilities
    renderFile
  , handleUser
  , handleUnexpected
  , throwUserError
  , throwUnexpectedError
  , tryImmDB
  , parseDBFile
  , validateIteratorRange
  , indexBackfill
    -- * Encoding and decoding the EBB hash
  , deserialiseHash
  , serialiseHash
  ) where

import           Codec.CBOR.Decoding (Decoder)
import           Codec.CBOR.Encoding (Encoding)
import           Codec.CBOR.Read (DeserialiseFailure, deserialiseFromBytes)
import           Codec.CBOR.Write (toLazyByteString)
import           Control.Monad (when)
import           Data.Bifunctor (second)
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import           GHC.Stack (HasCallStack, callStack, popCallStack)
import           Text.Printf (printf)
import           Text.Read (readMaybe)

import           Ouroboros.Consensus.Util (whenJust)

import           Ouroboros.Storage.Common
import           Ouroboros.Storage.EpochInfo.API
import           Ouroboros.Storage.FS.API.Types
import           Ouroboros.Storage.Util.ErrorHandling (ErrorHandling (..))
import qualified Ouroboros.Storage.Util.ErrorHandling as EH

import           Ouroboros.Storage.ImmutableDB.Layout
import           Ouroboros.Storage.ImmutableDB.Types

{------------------------------------------------------------------------------
  Utilities
------------------------------------------------------------------------------}


renderFile :: String -> EpochNo -> FsPath
renderFile fileType (EpochNo epoch) = mkFsPath [printf "%s-%03d.dat" fileType epoch]

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
-- > parseDBFile "epoch-001.dat"
-- Just ("epoch", 1)
-- > parseDBFile "index-012.dat"
-- Just ("index", 12)
parseDBFile :: String -> Maybe (String, EpochNo)
parseDBFile s = case T.splitOn "-" . fst . T.breakOn "." . T.pack $ s of
    [prefix, n] -> (T.unpack prefix,) . EpochNo <$> readMaybe (T.unpack n)
    _           -> Nothing

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
      Tip (Left  lastEpoch) -> (slot >) <$> epochInfoFirst epochInfo lastEpoch
      Tip (Right lastSlot)  -> return $ slot > lastSlot

-- | Return the slots to backfill the index file with.
--
-- A situation may arise in which we \"skip\" some relative slots, and we
-- write into the DB, for example, every other relative slot. In this case, we
-- need to backfill the index file with offsets for the skipped relative
-- slots. Similarly, before we start a new epoch, we must backfill the index
-- file of the current epoch file to indicate that it is finalised.
--
-- For example, say we have written \"a\" to relative slot 0 and \"bravo\" to
-- relative slot 1. We have the following index file:
--
-- > slot:     0   1   2
-- >         ┌───┬───┬───┐
-- > offset: │ 0 │ 1 │ 6 │
-- >         └───┴───┴───┘
--
-- Now we want to store \"haskell\" in relative slot 4, skipping 2 and 3. We
-- first have to backfill the index by repeating the last offset for the two
-- missing slots:
--
-- > slot:     0   1   2   3   4
-- >         ┌───┬───┬───┬───┬───┐
-- > offset: │ 0 │ 1 │ 6 │ 6 │ 6 │
-- >         └───┴───┴───┴───┴───┘
--
-- After backfilling (writing the offset 6 twice), we can write the next
-- offset:
--
-- > slot:     0   1   2   3   4   5
-- >         ┌───┬───┬───┬───┬───┬───┐
-- > offset: │ 0 │ 1 │ 6 │ 6 │ 6 │ 13│
-- >         └───┴───┴───┴───┴───┴───┘
--
-- For the example above, the output of this funciton would thus be: @[6, 6]@.
--
indexBackfill :: RelativeSlot  -- ^ The slot to write to (>= next expected slot)
              -> RelativeSlot  -- ^ The next expected slot to write to
              -> SlotOffset    -- ^ The last 'SlotOffset' written to
              -> [SlotOffset]
indexBackfill (RelativeSlot slot) (RelativeSlot nextExpected) lastOffset =
    replicate gap lastOffset
  where
    gap = fromIntegral $ slot - nextExpected

{-------------------------------------------------------------------------------
  Encoding and decoding the EBB hash

  When no EBB is present we use an empty bytestring.
-------------------------------------------------------------------------------}

deserialiseHash :: (forall s. Decoder s hash)
                -> BL.ByteString
                -> Either DeserialiseFailure (BL.ByteString, CurrentEBB hash)
deserialiseHash hashDecoder bs
  | BL.null bs = Right (BL.empty, NoCurrentEBB)
  | otherwise  = second CurrentEBB <$> (deserialiseFromBytes hashDecoder bs)

serialiseHash :: (hash -> Encoding)
              -> CurrentEBB hash
              -> BL.ByteString
serialiseHash _           NoCurrentEBB      = BL.empty
serialiseHash hashEncoder (CurrentEBB hash) = toLazyByteString (hashEncoder hash)
