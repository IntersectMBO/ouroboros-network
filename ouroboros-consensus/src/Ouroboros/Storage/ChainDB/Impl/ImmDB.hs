{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE PatternSynonyms           #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TypeApplications          #-}

{-# OPTIONS_GHC -Wredundant-constraints #-}
-- | Thin wrapper around the ImmutableDB
module Ouroboros.Storage.ChainDB.Impl.ImmDB (
    ImmDB -- Opaque
    -- * Initialization
  , ImmDbArgs(..)
  , defaultArgs
  , openDB
    -- * Getting and parsing blocks
  , getBlockWithPoint
  , getBlockAtTip
  , getPointAtTip
  , getSlotNoAtTip
  , getKnownBlock
  , getBlock
  , getBlob
    -- * Appending a block
  , appendBlock
    -- * Streaming
  , streamBlocksFrom
  , streamBlocksFromUnchecked
  , streamBlocksAfter
    -- * Wrappers
  , closeDB
  , reopen
  , iteratorNext
  , iteratorHasNext
  , iteratorPeek
  , iteratorClose
    -- * Tracing
  , ImmDB.TraceEvent
  , EpochFileError
    -- * Re-exports
  , Iterator
  , IteratorResult(..)
  , ImmDB.ValidationPolicy(..)
  , ImmDB.ImmutableDBError
    -- * Exported for testing purposes
  , mkImmDB
    -- * Exported for utilities
  , epochFileParser
  ) where

import           Codec.CBOR.Decoding (Decoder)
import           Codec.CBOR.Encoding (Encoding)
import qualified Codec.CBOR.Read as CBOR
import qualified Codec.CBOR.Write as CBOR
import           Control.Monad
import           Control.Monad.Except
import           Control.Tracer (Tracer, nullTracer)
import           Data.Bifunctor (first)
import qualified Data.ByteString.Lazy as Lazy
import           Data.Proxy
import           Data.Word
import           GHC.Stack (HasCallStack)
import           System.FilePath ((</>))

import           Control.Monad.Class.MonadST
import           Control.Monad.Class.MonadSTM
import           Control.Monad.Class.MonadThrow

import           Ouroboros.Network.Block (pattern BlockPoint,
                     pattern GenesisPoint, HasHeader (..), HeaderHash, Point,
                     SlotNo, atSlot, blockPoint, genesisPoint, pointSlot,
                     withHash)
import           Ouroboros.Network.Point (WithOrigin (..))

import qualified Ouroboros.Consensus.Util.CBOR as Util.CBOR
import           Ouroboros.Consensus.Util.ResourceRegistry (ResourceRegistry,
                     allocate, release)


import           Ouroboros.Storage.ChainDB.API (ChainDbError (..),
                     ChainDbFailure (..), StreamFrom (..), UnknownRange (..))
import           Ouroboros.Storage.Common
import           Ouroboros.Storage.EpochInfo (EpochInfo (..))
import           Ouroboros.Storage.FS.API (HasFS, createDirectoryIfMissing)
import           Ouroboros.Storage.FS.API.Types (MountPoint (..))
import           Ouroboros.Storage.FS.IO (ioHasFS)
import           Ouroboros.Storage.ImmutableDB (ImmutableDB,
                     Iterator (Iterator), IteratorResult (..))
import qualified Ouroboros.Storage.ImmutableDB as ImmDB
import           Ouroboros.Storage.Util.ErrorHandling (ErrorHandling)
import qualified Ouroboros.Storage.Util.ErrorHandling as EH

-- | Thin wrapper around the ImmutableDB (opaque type)
data ImmDB m blk = ImmDB {
      immDB     :: ImmutableDB (HeaderHash blk) m
    , decBlock  :: forall s. Decoder s blk
    , encBlock  :: blk -> Encoding
    , epochInfo :: EpochInfo m
    , isEBB     :: blk -> Maybe (HeaderHash blk)
    , err       :: ErrorHandling ImmDB.ImmutableDBError m
    }

{-------------------------------------------------------------------------------
  Initialization
-------------------------------------------------------------------------------}

-- | Arguments to initialize the ImmutableDB
--
-- See also 'defaultArgs'.
data ImmDbArgs m blk = forall h. ImmDbArgs {
      immDecodeHash  :: forall s. Decoder s (HeaderHash blk)
    , immDecodeBlock :: forall s. Decoder s blk
    , immEncodeHash  :: HeaderHash blk -> Encoding
    , immEncodeBlock :: blk -> Encoding
    , immErr         :: ErrorHandling ImmDB.ImmutableDBError m
    , immEpochInfo   :: EpochInfo m
    , immValidation  :: ImmDB.ValidationPolicy
    , immIsEBB       :: blk -> Maybe (HeaderHash blk)
    , immHasFS       :: HasFS m h
    , immTracer      :: Tracer m (ImmDB.TraceEvent EpochFileError)
    }

-- | Default arguments when using the 'IO' monad
--
-- The following fields must still be defined:
--
-- * 'immDecodeHash'
-- * 'immDecodeBlock'
-- * 'immEncodeHash'
-- * 'immEncodeBlock'
-- * 'immEpochInfo'
-- * 'immValidation'
-- * 'immIsEBB'
defaultArgs :: FilePath -> ImmDbArgs IO blk
defaultArgs fp = ImmDbArgs{
      immErr   = EH.exceptions
    , immHasFS = ioHasFS $ MountPoint (fp </> "immutable")
      -- Fields without a default
    , immDecodeHash  = error "no default for immDecodeHash"
    , immDecodeBlock = error "no default for immDecodeBlock"
    , immEncodeHash  = error "no default for immEncodeHash"
    , immEncodeBlock = error "no default for immEncodeBlock"
    , immEpochInfo   = error "no default for immEpochInfo"
    , immValidation  = error "no default for immValidation"
    , immIsEBB       = error "no default for immIsEBB"
    , immTracer      = nullTracer
    }

openDB :: (MonadSTM m, MonadST m, MonadCatch m, HasHeader blk)
       => ImmDbArgs m blk -> m (ImmDB m blk)
openDB args@ImmDbArgs{..} = do
    createDirectoryIfMissing immHasFS True []
    immDB <- ImmDB.openDB
               immDecodeHash
               immEncodeHash
               immHasFS
               immErr
               immEpochInfo
               immValidation
               (epochFileParser args)
               immTracer
    return ImmDB
      { immDB        = immDB
      , decBlock     = immDecodeBlock
      , encBlock     = immEncodeBlock
      , epochInfo    = immEpochInfo
      , isEBB        = immIsEBB
      , err          = immErr
      }

-- | For testing purposes
mkImmDB :: ImmutableDB (HeaderHash blk) m
        -> (forall s. Decoder s blk)
        -> (blk -> Encoding)
        -> EpochInfo m
        -> (blk -> Maybe (HeaderHash blk))
        -> ErrorHandling ImmDB.ImmutableDBError m
        -> ImmDB m blk
mkImmDB immDB decBlock encBlock epochInfo isEBB err = ImmDB {..}

{-------------------------------------------------------------------------------
  Getting and parsing blocks
-------------------------------------------------------------------------------}

-- | Return the block corresponding to the given point, if it is part of the
-- ImmutableDB.
--
-- If we have a block at the slot of the point, but its hash differs, we
-- return 'Nothing'.
getBlockWithPoint :: forall m blk. (MonadCatch m, HasHeader blk, HasCallStack)
                  => ImmDB m blk -> Point blk -> m (Maybe blk)
getBlockWithPoint _  GenesisPoint   = throwM $ NoGenesisBlock @blk
getBlockWithPoint db BlockPoint { withHash = hash, atSlot = slot } =
    -- Unfortunately a point does not give us enough information to determine
    -- whether this corresponds to a regular block or an EBB. We will
    -- optimistically assume it refers to a regular block, and only when that
    -- fails try to read the EBB instead. This means that
    --
    -- - If it is indeed a regular block performance is optimal
    -- - If it is an EBB we would needlessly have read a regular block from
    --   disk first, but EBBs are rare (1:20000 before the hard fork, and
    --   non-existent after)
    -- - If the block does not exist in the database at all, we'd have done two
    --   reads before returning 'Nothing', but both of those reads are cheap
    getBlockWithHash (Right slot) >>= \case
      Just block -> return (Just block)
      Nothing    -> do
        epochNo <- epochInfoEpoch slot
        getBlockWithHash (Left epochNo)
  where
    EpochInfo{..} = epochInfo db

    -- Important: we check whether the block's hash matches the point's hash
    getBlockWithHash epochOrSlot = getBlock db epochOrSlot >>= \case
      Just block | blockHash block == hash -> return $ Just block
      _                                    -> return $ Nothing

getBlockAtTip :: (MonadCatch m, HasHeader blk, HasCallStack)
              => ImmDB m blk -> m (Maybe blk)
getBlockAtTip db = do
    immTip <- withDB db $ \imm -> ImmDB.getTip imm
    case immTip of
      TipGen             -> return Nothing
      Tip (Left epochNo) -> Just <$> getKnownBlock db (Left epochNo)
      Tip (Right slotNo) -> Just <$> getKnownBlock db (Right slotNo)

getPointAtTip :: forall m blk.
                 (MonadCatch m, HasHeader blk, HasCallStack)
              => ImmDB m blk -> m (Point blk)
getPointAtTip = fmap mbBlockToPoint . getBlockAtTip
  where
    mbBlockToPoint :: Maybe blk -> Point blk
    mbBlockToPoint Nothing    = genesisPoint
    mbBlockToPoint (Just blk) = blockPoint blk

getSlotNoAtTip :: (MonadCatch m, HasHeader blk)
               => ImmDB m blk -> m (WithOrigin SlotNo)
getSlotNoAtTip db = do
    immTip <- withDB db $ \imm -> ImmDB.getTip imm
    case immTip of
      TipGen             -> return Origin
      Tip (Left epochNo) -> At <$> epochInfoFirst epochNo
      Tip (Right slotNo) -> return (At slotNo)
  where
    EpochInfo{..} = epochInfo db

-- | Get known block from the ImmutableDB that we know should exist
--
-- If it does not exist, we are dealing with data corruption.
getKnownBlock :: (MonadCatch m, HasHeader blk, HasCallStack)
              => ImmDB m blk -> Either EpochNo SlotNo -> m blk
getKnownBlock db epochOrSlot = do
    mBlock <- mustExist epochOrSlot <$> getBlock db epochOrSlot
    case mBlock of
      Right b  -> return b
      Left err -> throwM err

-- | Get block from the ImmutableDB
getBlock :: (MonadCatch m, HasHeader blk, HasCallStack)
         => ImmDB m blk -> Either EpochNo SlotNo -> m (Maybe blk)
getBlock db epochOrSlot = do
    mBlob <- fmap (parse (decBlock db) epochOrSlot) <$> getBlob db epochOrSlot
    case mBlob of
      Nothing         -> return $ Nothing
      Just (Right b)  -> return $ Just b
      Just (Left err) -> throwM $ err

getBlob :: (MonadCatch m, HasHeader blk, HasCallStack)
        => ImmDB m blk -> Either EpochNo SlotNo -> m (Maybe Lazy.ByteString)
getBlob db epochOrSlot = withDB db $ \imm ->
    case epochOrSlot of
      Left epochNo -> fmap snd <$> ImmDB.getEBB imm epochNo
      Right slotNo -> ImmDB.getBinaryBlob imm slotNo

{-------------------------------------------------------------------------------
  Appending a block
-------------------------------------------------------------------------------}

-- | Appends the given block (as a regular block or EBB) to the ImmutableDB.
--
-- Does not check whether the block is in the past or not.
appendBlock :: (MonadCatch m, HasHeader blk, HasCallStack)
            => ImmDB m blk -> blk -> m ()
appendBlock db@ImmDB{..} b = withDB db $ \imm -> case isEBB b of
    Nothing   ->
      ImmDB.appendBinaryBlob imm slotNo (CBOR.toBuilder (encBlock b))
    Just hash -> do
      epochNo <- epochInfoEpoch slotNo
      ImmDB.appendEBB imm epochNo hash (CBOR.toBuilder (encBlock b))
  where
    slotNo = blockSlot b
    EpochInfo{..} = epochInfo

{-------------------------------------------------------------------------------
  Streaming
-------------------------------------------------------------------------------}

-- | Wrapper around 'ImmDB.streamBinaryBlobs' that 'allocate's the iterator in
-- the 'ResourceRegistry' so that 'ImmDB.iteratorClose' is registered as the
-- clean-up action.
--
-- When the returned iterator is closed, it will be 'release'd from the
-- 'ResourceRegistry'.
registeredStream :: (MonadMask m, MonadSTM m, HasHeader blk)
                 => ImmDB m blk
                 -> ResourceRegistry m
                 -> Maybe (SlotNo, HeaderHash blk)
                 -> Maybe (SlotNo, HeaderHash blk)
                 -> m (ImmDB.Iterator (HeaderHash blk) m Lazy.ByteString)
registeredStream db registry start end = do
    (key, it) <- allocate registry
      (withDB db $ \imm -> ImmDB.streamBinaryBlobs imm start end)
      (iteratorClose db)
    return it
      { ImmDB.iteratorClose = release registry key
      }

-- | Stream blocks from the given 'StreamFrom'.
--
-- Checks whether the block at the lower bound has the right hash. If not,
-- 'Nothing' is returned. This check comes at a cost, as to know the hash of a
-- block, it first has be read from disk.
--
-- When the slot of the lower bound is greater than the slot at the tip in the
-- ImmutableDB, we return 'MissingBlock' (instead of throwing a
-- 'ReadFutureSlotError' or 'ReadFutureEBBError').
--
-- When passed @'StreamFromInclusive' pt@ where @pt@ refers to Genesis, a
-- 'NoGenesisBlock' exception will be thrown.
streamBlocksFrom :: forall m blk.
                   ( MonadMask m
                   , MonadSTM  m
                   , HasHeader blk
                   )
                 => ImmDB m blk
                 -> ResourceRegistry m
                 -> StreamFrom blk
                 -> m (Either (UnknownRange blk)
                              (ImmDB.Iterator (HeaderHash blk) m blk))
streamBlocksFrom db registry from = runExceptT $ case from of
    StreamFromExclusive pt@BlockPoint { atSlot = slot, withHash = hash } -> do
      checkFutureSlot pt
      it    <- stream (Just (slot, hash)) Nothing
      itRes <- lift $ iteratorNext db it
      if blockMatchesPoint itRes pt
        then return it
        else do
          lift $ iteratorClose db it
          throwError $ MissingBlock pt
    StreamFromExclusive    GenesisPoint ->
      stream Nothing Nothing
    StreamFromInclusive pt@BlockPoint { atSlot = slot, withHash = hash } -> do
      checkFutureSlot pt
      it    <- stream (Just (slot, hash)) Nothing
      itRes <- lift $ iteratorPeek db it
      if blockMatchesPoint itRes pt
        then return it
        else do
          lift $ iteratorClose db it
          throwError $ MissingBlock pt
    StreamFromInclusive    GenesisPoint ->
      throwM $ NoGenesisBlock @blk
  where
    -- | Check if the slot of the lower bound is <= the slot of the tip. If
    -- not, throw a 'MissingBlock' error.
    --
    -- Note that between this check and the actual opening of the iterator, a
    -- block may be appended to the ImmutableDB such that the requested slot
    -- is no longer in the future, but we have returned 'MissingBlock'
    -- nonetheless. This is fine, since the request to stream from the slot
    -- was made earlier, at a moment where it still was in the future.
    checkFutureSlot :: Point blk -> ExceptT (UnknownRange blk) m ()
    checkFutureSlot pt = do
      slotNoAtTip <- lift $ getSlotNoAtTip db
      when (pointSlot pt > slotNoAtTip) $
        throwError $ MissingBlock pt

    stream start end = lift $ parseIterator db <$>
      registeredStream db registry start end

    -- | Check that the result of the iterator is a block that matches the
    -- given point.
    blockMatchesPoint :: ImmDB.IteratorResult (HeaderHash blk) blk
                      -> Point blk
                      -> Bool
    blockMatchesPoint itRes pt = case itRes of
      ImmDB.IteratorExhausted    -> False
      ImmDB.IteratorResult _ blk -> blockPoint blk == pt
      ImmDB.IteratorEBB  _ _ blk -> blockPoint blk == pt

-- | Same as 'streamBlocksFrom', but without checking the hash of the lower
-- bound.
--
-- There is still a cost when the lower bound is 'StreamFromExclusive': the
-- block will be read from disk, but it won't be parsed.
--
-- When passed @'StreamFromInclusive' pt@ where @pt@ refers to Genesis, a
-- 'NoGenesisBlock' exception will be thrown.
streamBlocksFromUnchecked  :: forall m blk.
                              ( MonadMask m
                              , MonadSTM  m
                              , HasHeader blk
                              )
                           => ImmDB m blk
                           -> ResourceRegistry m
                           -> StreamFrom blk
                           -> m (ImmDB.Iterator (HeaderHash blk) m blk)
streamBlocksFromUnchecked db registry from = case from of
    StreamFromExclusive BlockPoint { atSlot = slot, withHash = hash } -> do
      it <- registeredStream db registry (Just (slot, hash)) Nothing
      void $ iteratorNext db it
      return $ parseIterator db it
    StreamFromExclusive    GenesisPoint ->
      stream Nothing Nothing
    StreamFromInclusive BlockPoint { atSlot = slot, withHash = hash } ->
      stream (Just (slot, hash)) Nothing
    StreamFromInclusive    GenesisPoint ->
      throwM $ NoGenesisBlock @blk
  where
    stream start end = parseIterator db <$>
      registeredStream db registry start end

-- | Stream blocks after the given point
--
-- See also 'streamBlobsAfter'.
--
-- PRECONDITION: the exclusive lower bound is part of the ImmutableDB.
streamBlocksAfter :: forall m blk.
                     ( MonadMask m
                     , MonadSTM  m
                     , HasHeader blk
                     )
                  => ImmDB m blk
                  -> ResourceRegistry m
                  -> Point blk -- ^ Exclusive lower bound
                  -> m (Iterator (HeaderHash blk) m blk)
streamBlocksAfter db registry low =
    parseIterator db <$> streamBlobsAfter db registry low

-- | Stream blobs after the given point
--
-- Our 'Point' based API and the 'SlotNo' + @hash@ based API of the
-- 'ImmutableDB' have a slight impedance mismatch: the bounds offered by the
-- 'ImmutableDB' are inclusive, /if specified/. We can't offer that here,
-- since it does not make sense to stream /from/ (inclusive) the genesis
-- block. Therefore we provide an /exclusive/ lower bound as a 'Point': if the
-- 'Point' refers to the genesis block we don't give the 'ImmutableDB' a lower
-- bound at all; if it doesn't, we pass the hash as the lower bound to the
-- 'ImmutableDB' and then step the iterator one block to skip that first
-- block.
streamBlobsAfter :: forall m blk.
                    ( MonadMask m
                    , MonadSTM  m
                    , HasHeader blk
                    )
                 => ImmDB m blk
                 -> ResourceRegistry m
                 -> Point blk -- ^ Exclusive lower bound
                 -> m (Iterator (HeaderHash blk) m Lazy.ByteString)
streamBlobsAfter db registry low = do
    itr <- registeredStream db registry low' Nothing
    skipAndCheck itr
    return itr
  where
    low' :: Maybe (SlotNo, HeaderHash blk)
    low' = case low of
      GenesisPoint         -> Nothing
      BlockPoint slot hash -> Just (slot, hash)

    -- Skip the first block (if any) to provide an /exclusive/ lower bound
    --
    -- Take advantage of the opportunity to also verify the hash (which the
    -- ImmutableDB can't, since it cannot compute hashes)
    skipAndCheck :: Iterator (HeaderHash blk) m Lazy.ByteString -> m ()
    skipAndCheck itr =
        case low' of
          Nothing           -> return ()
          Just (slot, hash) -> do
            skipped <- parseIteratorResult db =<< iteratorNext db itr
            case skipped of
              IteratorResult slot' blk ->
                  unless (hash == hash' && slot == slot') $
                    throwM $ ImmDbHashMismatch low hash hash'
                where
                  hash' = blockHash blk
              IteratorEBB _epoch hash' _ebb ->
                  -- We can't easily verify the slot, since all we have is the
                  -- epoch number. We could do the conversion but it doesn't
                  -- really matter: the hash uniquely determines the block.
                  unless (hash == hash') $
                    throwM $ ImmDbHashMismatch low hash hash'
              IteratorExhausted ->
                  throwM $ ImmDbUnexpectedIteratorExhausted low

-- | Parse the bytestrings returned by an iterator as blocks.
parseIterator :: (MonadCatch m, HasHeader blk)
              => ImmDB m blk
              -> Iterator (HeaderHash blk) m Lazy.ByteString
              -> Iterator (HeaderHash blk) m blk
parseIterator db itr = Iterator {
      iteratorNext    = parseIteratorResult db =<< iteratorNext db itr
    , iteratorPeek    = parseIteratorResult db =<< iteratorPeek db itr
    , iteratorHasNext = iteratorHasNext db itr
    , iteratorClose   = iteratorClose   db itr
    , iteratorID      = ImmDB.DerivedIteratorID $ ImmDB.iteratorID itr
    }

parseIteratorResult :: (MonadThrow m, HasHeader blk)
                    => ImmDB m blk
                    -> IteratorResult (HeaderHash blk) Lazy.ByteString
                    -> m (IteratorResult (HeaderHash blk) blk)
parseIteratorResult db result =
    case result of
      IteratorExhausted ->
        return IteratorExhausted
      (IteratorResult slotNo bs) ->
        case parse (decBlock db) (Right slotNo) bs of
          Left  err -> throwM err
          Right blk -> return $ IteratorResult slotNo blk
      (IteratorEBB epochNo hash bs) ->
        case parse (decBlock db) (Left epochNo) bs of
          Left  err -> throwM err
          Right blk -> return $ IteratorEBB epochNo hash blk

{-------------------------------------------------------------------------------
  Internal: parsing
-------------------------------------------------------------------------------}

data EpochFileError =
    EpochErrRead Util.CBOR.ReadIncrementalErr
  | EpochErrUnexpectedEBB
  deriving (Eq, Show)

epochFileParser :: forall m blk. (MonadST m, MonadThrow m, HasHeader blk)
                => ImmDbArgs m blk
                -> ImmDB.EpochFileParser
                     EpochFileError
                     (HeaderHash blk)
                     m
                     (Word64, SlotNo)
epochFileParser ImmDbArgs{..} =
    ImmDB.EpochFileParser $
        fmap (processEpochs (Proxy @blk))
      . Util.CBOR.readIncrementalOffsets immHasFS decoder'
  where
    -- It is important that we don't first parse all blocks, storing them all
    -- in memory, and only /then/ extract the information we need.
    decoder' :: forall s. Decoder s (SlotNo, Maybe (HeaderHash blk))
    decoder' = (\b -> (blockSlot b, immIsEBB b)) <$> immDecodeBlock

-- | Verify that there is at most one EBB in the epoch file and that it
-- lives at the start of the file
processEpochs :: forall blk.
                 Proxy blk
              -> ( [(Word64, (Word64, (SlotNo, Maybe (HeaderHash blk))))]
                 , Maybe Util.CBOR.ReadIncrementalErr
                 )
              -> ( [(SlotOffset, (Word64, SlotNo))]
                 , Maybe (HeaderHash blk)
                 , Maybe EpochFileError
                 )
processEpochs _ = \(bs, mErr) ->
    case bs of
      []    -> ([], Nothing, EpochErrRead <$> mErr)
      b:bs' -> let (bOff, (bSz, (bSlot, bEBB))) = b
                   (slots, mErr') = go bs'
               in ((bOff, (bSz, bSlot)) : slots, bEBB, earlierError mErr mErr')
  where
    -- Check that the rest of the blocks are not EBBs
    go :: [(Word64, (Word64, (SlotNo, Maybe (HeaderHash blk))))]
       -> ( [(SlotOffset, (Word64, SlotNo))]
          , Maybe EpochFileError
          )
    go []     = ( [], Nothing )
    go (b:bs) = let (bOff, (bSz, (bSlot, bEBB))) = b
                in case bEBB of
                     Just _  -> ( [], Just EpochErrUnexpectedEBB )
                     Nothing -> first ((bOff, (bSz, bSlot)) :) $ go bs

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

{-------------------------------------------------------------------------------
  Error handling
-------------------------------------------------------------------------------}

-- | Wrap calls to the ImmutableDB and rethrow exceptions that may indicate
-- disk failure and should therefore trigger recovery
withDB :: forall m blk x. (MonadCatch m, HasHeader blk)
       => ImmDB m blk -> (ImmutableDB (HeaderHash blk) m -> m x) -> m x
withDB ImmDB{..} k = EH.catchError err (k immDB) rethrow
  where
    rethrow :: ImmDB.ImmutableDBError -> m x
    rethrow e = case wrap e of
                    Just e' -> throwM e'
                    Nothing -> throwM e

    wrap :: ImmDB.ImmutableDBError -> Maybe (ChainDbFailure blk)
    wrap (ImmDB.UnexpectedError e) = Just (ImmDbFailure e)
    wrap ImmDB.UserError{}         = Nothing

mustExist :: Either EpochNo SlotNo
          -> Maybe blk
          -> Either (ChainDbFailure blk) blk
mustExist epochOrSlot Nothing  = Left  $ ImmDbMissingBlock epochOrSlot
mustExist _           (Just b) = Right $ b

parse :: forall blk.
         (forall s. Decoder s blk)
      -> Either EpochNo SlotNo
      -> Lazy.ByteString
      -> Either (ChainDbFailure blk) blk
parse dec epochOrSlot =
    aux . CBOR.deserialiseFromBytes dec
  where
    aux :: Either CBOR.DeserialiseFailure (Lazy.ByteString, blk)
        -> Either (ChainDbFailure blk) blk
    aux (Right (bs, b))
      | Lazy.null bs = Right b
      | otherwise    = Left $ ImmDbTrailingData epochOrSlot bs
    aux (Left err)   = Left $ ImmDbParseFailure epochOrSlot err

{-------------------------------------------------------------------------------
  Wrappers
-------------------------------------------------------------------------------}

closeDB :: (MonadCatch m, HasHeader blk, HasCallStack)
        => ImmDB m blk -> m ()
closeDB db = withDB db ImmDB.closeDB

reopen :: (MonadCatch m, HasHeader blk, HasCallStack)
       => ImmDB m blk -> m ()
reopen db = withDB db $ \imm ->
    ImmDB.reopen imm ImmDB.ValidateMostRecentEpoch

-- These wrappers ensure that we correctly rethrow exceptions using 'withDB'.

iteratorNext :: (HasCallStack, MonadCatch m, HasHeader blk)
             => ImmDB m blk
             -> ImmDB.Iterator (HeaderHash blk) m a
             -> m (ImmDB.IteratorResult (HeaderHash blk) a)
iteratorNext db it = withDB db $ const $ ImmDB.iteratorNext it

iteratorHasNext :: (HasCallStack, MonadCatch m, HasHeader blk)
                => ImmDB m blk
                -> ImmDB.Iterator (HeaderHash blk) m a
                -> m Bool
iteratorHasNext db it = withDB db $ const $ ImmDB.iteratorHasNext it

iteratorPeek :: (HasCallStack, MonadCatch m, HasHeader blk)
             => ImmDB m blk
             -> ImmDB.Iterator (HeaderHash blk) m a
             -> m (ImmDB.IteratorResult (HeaderHash blk) a)
iteratorPeek db it = withDB db $ const $ ImmDB.iteratorPeek it

iteratorClose :: (HasCallStack, MonadCatch m, HasHeader blk)
              => ImmDB m blk
              -> ImmDB.Iterator (HeaderHash blk) m a
              -> m ()
iteratorClose db it = withDB db $ const $ ImmDB.iteratorClose it
