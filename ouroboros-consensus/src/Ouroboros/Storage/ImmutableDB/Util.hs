{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeApplications    #-}
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
    -- * EpochFileParser
  , EpochFileError (..)
  , epochFileParser
  ) where

import           Codec.CBOR.Decoding (Decoder)
import           Codec.CBOR.Encoding (Encoding)
import           Codec.CBOR.Read (DeserialiseFailure, deserialiseFromBytes)
import           Codec.CBOR.Write (toLazyByteString)
import           Control.Monad (when)
import           Data.Bifunctor (first, second)
import qualified Data.ByteString.Lazy as BL
import           Data.Proxy (Proxy (..))
import qualified Data.Text as T
import           Data.Word (Word64)
import           GHC.Stack (HasCallStack, callStack, popCallStack)
import           Text.Printf (printf)
import           Text.Read (readMaybe)

import           Ouroboros.Network.Block (HasHeader (..), HeaderHash)

import           Ouroboros.Consensus.Util (whenJust)
import qualified Ouroboros.Consensus.Util.CBOR as Util.CBOR
import           Ouroboros.Consensus.Util.IOLike

import           Ouroboros.Storage.Common
import           Ouroboros.Storage.EpochInfo.API
import           Ouroboros.Storage.FS.API (HasFS)
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
      Tip (EBB   lastEpoch) -> (slot >) <$> epochInfoFirst epochInfo lastEpoch
      Tip (Block lastSlot)  -> return $ slot > lastSlot

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

{-------------------------------------------------------------------------------
  EpochFileParser
-------------------------------------------------------------------------------}

data EpochFileError =
    EpochErrRead Util.CBOR.ReadIncrementalErr
  | EpochErrUnexpectedEBB
  deriving (Eq, Show)

epochFileParser :: forall m blk h. (IOLike m, HasHeader blk)
                => HasFS m h
                -> (forall s. Decoder s (BL.ByteString -> blk))
                -> (blk -> Maybe (HeaderHash blk))
                -> EpochFileParser
                     EpochFileError
                     (HeaderHash blk)
                     m
                     (Word64, SlotNo)
epochFileParser hasFS decodeBlock isEBB =
    EpochFileParser $
        fmap (processEpochs (Proxy @blk))
      . Util.CBOR.readIncrementalOffsets hasFS decoder'
  where
    -- It is important that we don't first parse all blocks, storing them all
    -- in memory, and only /then/ extract the information we need.
    decoder' :: forall s. Decoder s (BL.ByteString -> (SlotNo, CurrentEBB (HeaderHash blk)))
    decoder' = (extractSlotNoAndEbbHash .) <$> decodeBlock

    extractSlotNoAndEbbHash :: blk -> (SlotNo, CurrentEBB (HeaderHash blk))
    extractSlotNoAndEbbHash b =
      -- IMPORTANT: force the slot and the ebbHash, because otherwise we
      -- return thunks that refer to the whole block! See
      -- 'Ouroboros.Consensus.Util.CBOR.readIncrementalOffsets' where we need
      -- to force the decoded value in order to force this computation.
        case isEBB b of
          Nothing         -> (slot, NoCurrentEBB)
          Just (!ebbHash) -> (slot, CurrentEBB ebbHash)
      where
        !slot = blockSlot b

-- | Verify that there is at most one EBB in the epoch file and that it
-- lives at the start of the file
processEpochs :: forall blk.
                 Proxy blk
              -> ( [(Word64, (Word64, (SlotNo, CurrentEBB (HeaderHash blk))))]
                 , Maybe Util.CBOR.ReadIncrementalErr
                 )
              -> ( [(SlotOffset, (Word64, SlotNo))]
                 , CurrentEBB (HeaderHash blk)
                 , Maybe EpochFileError
                 )
processEpochs _ = \(bs, mErr) ->
    case bs of
      []    -> ([], NoCurrentEBB, EpochErrRead <$> mErr)
      b:bs' -> let (bOff, (bSz, (bSlot, bEBB))) = b
                   (slots, mErr') = go bs'
               in ((bOff, (bSz, bSlot)) : slots, bEBB, earlierError mErr mErr')
  where
    -- Check that the rest of the blocks are not EBBs
    go :: [(Word64, (Word64, (SlotNo, CurrentEBB (HeaderHash blk))))]
       -> ( [(SlotOffset, (Word64, SlotNo))]
          , Maybe EpochFileError
          )
    go []     = ( [], Nothing )
    go (b:bs) = let (bOff, (bSz, (bSlot, bEBB))) = b
                in case bEBB of
                     CurrentEBB _ -> ( [], Just EpochErrUnexpectedEBB )
                     NoCurrentEBB -> first ((bOff, (bSz, bSlot)) :) $ go bs

    -- Report the earlier error
    --
    -- The 'ReadIncrementalError' reported by the parser tells us that there
    -- were blocks /after/ the ones that were returned that failed to parse.
    -- If therefore /we/ find an error in those blocks that were returned, that
    -- error happens /earlier/ in the file.
    earlierError :: Maybe Util.CBOR.ReadIncrementalErr
                 -> Maybe EpochFileError
                 -> Maybe EpochFileError
    earlierError _parserErr (Just ourErr) = Just ourErr
    earlierError  parserErr Nothing       = EpochErrRead <$> parserErr
