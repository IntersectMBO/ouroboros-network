{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE NamedFieldPuns            #-}
{-# LANGUAGE PatternSynonyms           #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE ScopedTypeVariables       #-}

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
  , streamBlocksAfter
  , deserialiseIterator
  , deserialisableIterator
  , parseIterator
    -- * Wrappers
  , closeDB
  , reopen
  , iteratorNext
  , iteratorHasNext
  , iteratorPeek
  , iteratorClose
    -- * Tracing
  , TraceEvent
  , ImmDB.EpochFileError
    -- * Re-exports
  , Iterator
  , IteratorResult(..)
  , ImmDB.ValidationPolicy(..)
  , ImmDB.ImmutableDBError
  , ImmDB.BinaryInfo (..)
  , ImmDB.HashInfo (..)
    -- * Exported for testing purposes
  , mkImmDB
    -- * Exported for utilities
  , ImmDB.epochFileParser
  ) where

import           Codec.CBOR.Decoding (Decoder)
import           Codec.CBOR.Encoding (Encoding)
import qualified Codec.CBOR.Read as CBOR
import qualified Codec.CBOR.Write as CBOR
import           Control.Monad
import           Control.Monad.Except
import           Control.Tracer (Tracer, nullTracer)
import qualified Data.ByteString.Lazy as Lazy
import           Data.Functor ((<&>))
import           GHC.Stack (HasCallStack)
import           System.FilePath ((</>))

import           Cardano.Prelude (allNoUnexpectedThunks)

import           Control.Monad.Class.MonadThrow

import           Ouroboros.Network.Block (pattern BlockPoint,
                     pattern GenesisPoint, HasHeader (..), HeaderHash, Point,
                     Serialised (..), SlotNo, atSlot, pointSlot, withHash)
import           Ouroboros.Network.Point (WithOrigin (..))

import           Ouroboros.Consensus.Util.IOLike
import           Ouroboros.Consensus.Util.Orphans ()
import           Ouroboros.Consensus.Util.ResourceRegistry (ResourceRegistry,
                     allocateEither, unsafeRelease)

import           Ouroboros.Storage.ChainDB.API (ChainDbError (..),
                     ChainDbFailure (..), Deserialisable (..), StreamFrom (..),
                     UnknownRange (..))
import           Ouroboros.Storage.Common
import           Ouroboros.Storage.EpochInfo (EpochInfo (..))
import           Ouroboros.Storage.FS.API (HasFS, createDirectoryIfMissing)
import           Ouroboros.Storage.FS.API.Types (MountPoint (..), mkFsPath)
import           Ouroboros.Storage.FS.IO (ioHasFS)
import           Ouroboros.Storage.ImmutableDB (BinaryInfo (..), HashInfo (..),
                     ImmutableDB, Iterator (Iterator), IteratorResult (..),
                     WithHash (..))
import qualified Ouroboros.Storage.ImmutableDB as ImmDB
import qualified Ouroboros.Storage.ImmutableDB.Parser as ImmDB
import           Ouroboros.Storage.Util.ErrorHandling (ErrorHandling)
import qualified Ouroboros.Storage.Util.ErrorHandling as EH

-- | Thin wrapper around the ImmutableDB (opaque type)
data ImmDB m blk = ImmDB {
      immDB     :: !(ImmutableDB (HeaderHash blk) m)
    , decBlock  :: !(forall s. Decoder s (Lazy.ByteString -> blk))
      -- ^ TODO introduce a newtype wrapper around the @s@ so we can use
      -- generics to derive the NoUnexpectedThunks instance.
    , encBlock  :: !(blk -> BinaryInfo Encoding)
    , epochInfo :: !(EpochInfo m)
    , isEBB     :: !(blk -> Maybe EpochNo)
    , err       :: !(ErrorHandling ImmDB.ImmutableDBError m)
    }


-- Universal type; we can't use generics
instance NoUnexpectedThunks (ImmDB m blk) where
  showTypeOf _ = "ImmDB"
  whnfNoUnexpectedThunks ctxt ImmDB {..} = allNoUnexpectedThunks
    [ noUnexpectedThunks ctxt immDB
    , noUnexpectedThunks ctxt decBlock
    , noUnexpectedThunks ctxt encBlock
    , noUnexpectedThunks ctxt epochInfo
    , noUnexpectedThunks ctxt isEBB
    , noUnexpectedThunks ctxt err
    ]

-- | Short-hand for events traced by the ImmDB wrapper.
type TraceEvent blk =
  ImmDB.TraceEvent (ImmDB.EpochFileError (HeaderHash blk)) (HeaderHash blk)

{-------------------------------------------------------------------------------
  Initialization
-------------------------------------------------------------------------------}

-- | Arguments to initialize the ImmutableDB
--
-- See also 'defaultArgs'.
data ImmDbArgs m blk = forall h. ImmDbArgs {
      immDecodeHash     :: forall s. Decoder s (HeaderHash blk)
    , immDecodeBlock    :: forall s. Decoder s (Lazy.ByteString -> blk)
    , immEncodeHash     :: HeaderHash blk -> Encoding
    , immEncodeBlock    :: blk -> BinaryInfo Encoding
    , immErr            :: ErrorHandling ImmDB.ImmutableDBError m
    , immEpochInfo      :: EpochInfo m
    , immHashInfo       :: HashInfo (HeaderHash blk)
    , immValidation     :: ImmDB.ValidationPolicy
    , immIsEBB          :: blk -> Maybe EpochNo
    , immCheckIntegrity :: blk -> Bool
    , immHasFS          :: HasFS m h
    , immTracer         :: Tracer m (TraceEvent blk)
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
-- * 'immHashInfo'
-- * 'immValidation'
-- * 'immIsEBB'
-- * 'immCheckIntegrity'
defaultArgs :: FilePath -> ImmDbArgs IO blk
defaultArgs fp = ImmDbArgs{
      immErr   = EH.exceptions
    , immHasFS = ioHasFS $ MountPoint (fp </> "immutable")
      -- Fields without a default
    , immDecodeHash     = error "no default for immDecodeHash"
    , immDecodeBlock    = error "no default for immDecodeBlock"
    , immEncodeHash     = error "no default for immEncodeHash"
    , immEncodeBlock    = error "no default for immEncodeBlock"
    , immEpochInfo      = error "no default for immEpochInfo"
    , immHashInfo       = error "no default for immHashInfo"
    , immValidation     = error "no default for immValidation"
    , immIsEBB          = error "no default for immIsEBB"
    , immCheckIntegrity = error "no default for immCheckIntegrity"
    , immTracer         = nullTracer
    }

openDB :: (IOLike m, HasHeader blk) => ImmDbArgs m blk -> m (ImmDB m blk)
openDB ImmDbArgs{..} = do
    createDirectoryIfMissing immHasFS True (mkFsPath [])
    immDB <- ImmDB.openDB
               immHasFS
               immErr
               immEpochInfo
               immHashInfo
               immValidation
               parser
               immTracer
    return ImmDB
      { immDB     = immDB
      , decBlock  = immDecodeBlock
      , encBlock  = immEncodeBlock
      , epochInfo = immEpochInfo
      , isEBB     = immIsEBB
      , err       = immErr
      }
  where
    parser = ImmDB.epochFileParser immHasFS immDecodeBlock immIsEBB
      -- TODO a more efficient to accomplish this?
      (void . immEncodeBlock)


-- | For testing purposes
mkImmDB :: ImmutableDB (HeaderHash blk) m
        -> (forall s. Decoder s (Lazy.ByteString -> blk))
        -> (blk -> BinaryInfo Encoding)
        -> EpochInfo m
        -> (blk -> Maybe EpochNo)
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
--
-- If the point corresponds to some slot in the future, a
-- 'ReadFutureSlotError' wrapped in a 'ImmDbFailure' is thrown.
getBlockWithPoint :: forall m blk. (MonadCatch m, HasHeader blk, HasCallStack)
                  => ImmDB m blk -> Point blk -> m (Maybe blk)
getBlockWithPoint _  GenesisPoint = throwM NoGenesisBlock
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
    --
    -- Note that there is one exceptional scenario: the point refers to the
    -- EBB at the ImmutableDB's tip. This means that the regular block with
    -- the same slot number as the EBB hasn't been added to the ImmutableDB
    -- yet. If we first try to read the regular block at that slot, we would
    -- get a 'ReadFutureSlotError' because we're trying to read a block in the
    -- future. If we first read the EBB at that slot (the corresponding epoch,
    -- in fact), then we won't get the 'ReadFutureSlotError'.
    tipIsEBB >>= \case
      Just tipSlot
        | tipSlot == slot
          -- There is an EBB at @slot@
        -> getEBB
      _ -> getBlockThenEBB
  where
    EpochInfo{..} = epochInfo db

    -- | If there's an EBB at the tip of the ImmutableDB, return its 'SlotNo'.
    tipIsEBB :: m (Maybe SlotNo)
    tipIsEBB = withDB db $ \imm -> fmap forgetHash <$> ImmDB.getTip imm >>= \case
      Tip (ImmDB.EBB epochNo) -> Just <$> epochInfoFirst epochNo
      Tip (ImmDB.Block _)     -> return Nothing
      TipGen                  -> return Nothing

    -- TODO do this more efficiently in the ImmutableDB
    -- | First try to read the block at the slot, if the block's hash doesn't
    -- match the expect hash, try reading the EBB at that slot.
    getBlockThenEBB :: m (Maybe blk)
    getBlockThenEBB = getBlockWithHash (Right slot) >>= \case
      Just block -> return (Just block)
      Nothing    -> do
        epochNo <- epochInfoEpoch slot
        ebbSlot <- epochInfoFirst epochNo
        -- The point can only refer to an EBB if its slot refers to the first
        -- slot of the epoch
        if slot == ebbSlot
          then getBlockWithHash (Left epochNo)
          else return Nothing

    -- | Try to read the EBB at the slot, if the EBB's hash doesn't match the
    -- expect hash, return 'Nothing'.
    --
    -- PRECONDITION: there is an EBB at @slot@
    getEBB :: m (Maybe blk)
    getEBB = epochInfoEpoch slot >>= \epochNo ->
      getBlock db (Left epochNo) >>= \case
        Just block
          | blockHash block == hash
          -> return $ Just block
          | otherwise
          -> return $ Nothing
        Nothing -- EBB is missing
          -> throwM $ ImmDbMissingBlock (Left epochNo)

    -- Important: we check whether the block's hash matches the point's hash
    getBlockWithHash :: Either EpochNo SlotNo -> m (Maybe blk)
    getBlockWithHash epochOrSlot = getBlock db epochOrSlot >>= \case
      Just block | blockHash block == hash -> return $ Just block
      _                                    -> return $ Nothing

getBlockAtTip :: (MonadCatch m, HasCallStack)
              => ImmDB m blk -> m (Maybe blk)
getBlockAtTip db = do
    immTip <- withDB db $ \imm -> ImmDB.getTip imm
    case forgetHash <$> immTip of
      TipGen                   -> return Nothing
      Tip (ImmDB.EBB epochNo)  -> Just <$> getKnownBlock db (Left epochNo)
      Tip (ImmDB.Block slotNo) -> Just <$> getKnownBlock db (Right slotNo)

getPointAtTip :: forall m blk.
                 (MonadCatch m, HasCallStack)
              => ImmDB m blk -> m (Point blk)
getPointAtTip db = do
    immTip <- withDB db $ \imm -> ImmDB.getTip imm
    case immTip of
      TipGen ->
        return GenesisPoint
      Tip (WithHash hash (ImmDB.EBB epochNo)) ->
        (`BlockPoint` hash) <$> epochInfoFirst epochNo
      Tip (WithHash hash (ImmDB.Block slotNo)) ->
        return (BlockPoint slotNo hash)
  where
    EpochInfo{..} = epochInfo db

getSlotNoAtTip :: MonadCatch m => ImmDB m blk -> m (WithOrigin SlotNo)
getSlotNoAtTip db = pointSlot <$> getPointAtTip db

-- | Get known block from the ImmutableDB that we know should exist
--
-- If it does not exist, we are dealing with data corruption.
getKnownBlock :: (MonadCatch m, HasCallStack)
              => ImmDB m blk -> Either EpochNo SlotNo -> m blk
getKnownBlock db epochOrSlot = do
    mBlock <- mustExist epochOrSlot <$> getBlock db epochOrSlot
    case mBlock of
      Right b  -> return b
      Left err -> throwM err

-- | Get block from the ImmutableDB
getBlock :: (MonadCatch m, HasCallStack)
         => ImmDB m blk -> Either EpochNo SlotNo -> m (Maybe blk)
getBlock db epochOrSlot = do
    mBlob <- fmap (parse (decBlock db) epochOrSlot) <$> getBlob db epochOrSlot
    case mBlob of
      Nothing         -> return $ Nothing
      Just (Right b)  -> return $ Just b
      Just (Left err) -> throwM $ err

getBlob :: (MonadCatch m, HasCallStack)
        => ImmDB m blk -> Either EpochNo SlotNo -> m (Maybe Lazy.ByteString)
getBlob db epochOrSlot = withDB db $ \imm ->
    -- TODO return hash
    case epochOrSlot of
      Left epochNo -> fmap snd <$> ImmDB.getEBB imm epochNo
      Right slotNo -> fmap snd <$> ImmDB.getBlock imm slotNo

-- TODO 'getHeader'

{-------------------------------------------------------------------------------
  Appending a block
-------------------------------------------------------------------------------}

-- | Appends the given block (as a regular block or EBB) to the ImmutableDB.
--
-- Does not check whether the block is in the past or not.
appendBlock :: (MonadCatch m, HasHeader blk, HasCallStack)
            => ImmDB m blk -> blk -> m ()
appendBlock db@ImmDB{..} b = withDB db $ \imm -> case isEBB b of
    Nothing      ->
      ImmDB.appendBlock imm slotNo  hash (CBOR.toBuilder <$> encBlock b)
    Just epochNo ->
      ImmDB.appendEBB   imm epochNo hash (CBOR.toBuilder <$> encBlock b)
  where
    hash   = blockHash b
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
registeredStream :: forall m blk. IOLike m
                 => ImmDB m blk
                 -> ResourceRegistry m
                 -> Maybe (SlotNo, HeaderHash blk)
                 -> Maybe (SlotNo, HeaderHash blk)
                 -> m (Either (UnknownRange blk)
                              (ImmDB.Iterator (HeaderHash blk) m Lazy.ByteString))
registeredStream db registry start end = do
    errOrKeyAndIt <- allocateEither registry
      (\_key -> withDB db $ \imm -> ImmDB.streamBlocks imm start end)
      (iteratorClose db)
    return $ case errOrKeyAndIt of
      Left e          -> Left (toUnknownRange e)
      -- The iterator will be used by a thread that is unknown to the registry
      -- (which, after all, is entirely internal to the chain DB). This means
      -- that the registry cannot guarantee that the iterator will be live for
      -- the duration of that thread, and indeed, it may not be: the chain DB
      -- might be closed before that thread terminates. We will deal with this
      -- in the chain DB itself (throw ClosedDBError exception).
      Right (key, it) -> Right it { ImmDB.iteratorClose = unsafeRelease key }
  where
    toUnknownRange :: ImmDB.WrongBoundError (HeaderHash blk) -> UnknownRange blk
    toUnknownRange e
      | Just (startSlot, startHash) <- start
      , wrongBoundErrorSlotNo e == startSlot
      = MissingBlock (BlockPoint startSlot startHash)
      | Just (endSlot, endHash) <- end
      , wrongBoundErrorSlotNo e == endSlot
      = MissingBlock (BlockPoint endSlot endHash)
      | otherwise
      = error "WrongBoundError for a different bound than we gave"

    wrongBoundErrorSlotNo :: ImmDB.WrongBoundError (HeaderHash blk) -> SlotNo
    wrongBoundErrorSlotNo = \case
      ImmDB.EmptySlotError slot     -> slot
      ImmDB.WrongHashError slot _ _ -> slot

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
streamBlocksFrom
  :: forall m blk. IOLike m
  => ImmDB m blk
  -> ResourceRegistry m
  -> StreamFrom blk
  -> m (Either (UnknownRange blk)
               (ImmDB.Iterator (HeaderHash blk) m (Deserialisable m blk)))
streamBlocksFrom db registry from = runExceptT $ case from of
    StreamFromExclusive pt@BlockPoint { atSlot = slot, withHash = hash } -> do
      checkFutureSlot pt
      it <- stream (Just (slot, hash)) Nothing
      -- Skip the first block, as the bound is exclusive
      void $ lift $ iteratorNext db it
      return it
    StreamFromExclusive    GenesisPoint ->
      stream Nothing Nothing
    StreamFromInclusive pt@BlockPoint { atSlot = slot, withHash = hash } -> do
      checkFutureSlot pt
      stream (Just (slot, hash)) Nothing
    StreamFromInclusive GenesisPoint ->
      throwM NoGenesisBlock
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

    stream start end = ExceptT $ fmap (deserialisableIterator db) <$>
      registeredStream db registry start end

-- | Stream blocks after the given point
--
-- PRECONDITION: the exclusive lower bound is part of the ImmutableDB, if not,
-- 'ImmDbMissingBlockPoint' is thrown.
streamBlocksAfter :: forall m blk. (IOLike m, HasHeader blk)
                  => ImmDB m blk
                  -> ResourceRegistry m
                  -> Point blk -- ^ Exclusive lower bound
                  -> m (Iterator (HeaderHash blk) m blk)
streamBlocksAfter db registry low =
    registeredStream db registry low' Nothing >>= \case
      Left  _   -> throwM $ ImmDbMissingBlockPoint low
      Right itr -> do
        case low of
          GenesisPoint           -> return ()
          -- Skip the exclusive lower bound
          BlockPoint _slot _hash -> iteratorNext db itr >>= \case
              IteratorExhausted        ->
                throwM $ ImmDbUnexpectedIteratorExhausted low
              -- The hash must match the lower bound's hash. The ImmutableDB
              -- already checks this.
              IteratorResult {} -> return ()
              IteratorEBB    {} -> return ()
        return $ parseIterator db itr
  where
    low' :: Maybe (SlotNo, HeaderHash blk)
    low' = case low of
      GenesisPoint         -> Nothing
      BlockPoint slot hash -> Just (slot, hash)

deserialisableIteratorResult
  :: MonadThrow m
  => ImmDB m blk
  -> IteratorResult (HeaderHash blk) Lazy.ByteString
  -> m (IteratorResult (HeaderHash blk) (Deserialisable m blk))
deserialisableIteratorResult db@ImmDB { epochInfo } = \case
    IteratorExhausted             -> return $ IteratorExhausted
    IteratorResult slotNo hash bs -> return $ IteratorResult slotNo hash $
      Deserialisable
        { serialised         = Serialised bs
        , deserialisableSlot = slotNo
        , deserialisableHash = hash
        , deserialise        = case parse (decBlock db) (Right slotNo) bs of
            Left  err -> throwM err
            Right blk -> return blk
        }
    -- We only need @m@ for this whole function to convert the 'EpochNo' of
    -- the EBB to a 'SlotNo'.
    IteratorEBB epochNo hash bs   -> epochInfoFirst epochNo <&> \ebbSlotNo ->
      IteratorEBB epochNo hash $ Deserialisable
        { serialised         = Serialised bs
        , deserialisableSlot = ebbSlotNo
        , deserialisableHash = hash
        , deserialise        = case parse (decBlock db) (Left epochNo) bs of
            Left  err -> throwM err
            Right blk -> return blk
        }
  where
    EpochInfo { epochInfoFirst } = epochInfo

traverseIterator
  :: MonadCatch m
  => ImmDB m blk
  -> (       IteratorResult (HeaderHash blk) a
       -> m (IteratorResult (HeaderHash blk) b)
     )
  -> Iterator (HeaderHash blk) m a
  -> Iterator (HeaderHash blk) m b
traverseIterator db f itr = Iterator {
      iteratorNext    = iteratorNext db itr >>= f
    , iteratorPeek    = iteratorPeek db itr >>= f
    , iteratorHasNext = iteratorHasNext db itr
    , iteratorClose   = iteratorClose   db itr
    , iteratorID      = ImmDB.DerivedIteratorID $ ImmDB.iteratorID itr
    }

deserialisableIterator
  :: MonadCatch m
  => ImmDB m blk
  -> Iterator (HeaderHash blk) m Lazy.ByteString
  -> Iterator (HeaderHash blk) m (Deserialisable m blk)
deserialisableIterator db = traverseIterator db (deserialisableIteratorResult db)

deserialiseIterator
  :: MonadCatch m
  => ImmDB m blk
  -> Iterator (HeaderHash blk) m (Deserialisable m blk)
  -> Iterator (HeaderHash blk) m blk
deserialiseIterator db = traverseIterator db (traverse deserialise)

parseIterator
  :: MonadCatch m
  => ImmDB m blk
  -> Iterator (HeaderHash blk) m Lazy.ByteString
  -> Iterator (HeaderHash blk) m blk
parseIterator db = traverseIterator db
  (deserialisableIteratorResult db >=> traverse deserialise)

{-------------------------------------------------------------------------------
  Error handling
-------------------------------------------------------------------------------}

-- | Wrap calls to the ImmutableDB and rethrow exceptions that may indicate
-- disk failure and should therefore trigger recovery
withDB :: forall m blk x. MonadCatch m
       => ImmDB m blk -> (ImmutableDB (HeaderHash blk) m -> m x) -> m x
withDB ImmDB{..} k = EH.catchError err (k immDB) rethrow
  where
    rethrow :: ImmDB.ImmutableDBError -> m x
    rethrow e = case wrap e of
                    Just e' -> throwM e'
                    Nothing -> throwM e

    wrap :: ImmDB.ImmutableDBError -> Maybe ChainDbFailure
    wrap (ImmDB.UnexpectedError e) = Just (ImmDbFailure e)
    wrap ImmDB.UserError{}         = Nothing

mustExist :: Either EpochNo SlotNo
          -> Maybe blk
          -> Either ChainDbFailure blk
mustExist epochOrSlot Nothing  = Left  $ ImmDbMissingBlock epochOrSlot
mustExist _           (Just b) = Right $ b

parse :: forall blk.
         (forall s. Decoder s (Lazy.ByteString -> blk))
      -> Either EpochNo SlotNo
      -> Lazy.ByteString
      -> Either ChainDbFailure blk
parse dec epochOrSlot bytes =
    aux (CBOR.deserialiseFromBytes dec bytes)
  where
    aux :: Either CBOR.DeserialiseFailure
                  (Lazy.ByteString, Lazy.ByteString -> blk)
        -> Either ChainDbFailure blk
    aux (Right (bs, blk))
      | Lazy.null bs = Right (blk bytes)
      | otherwise    = Left $ ImmDbTrailingData epochOrSlot bs
    aux (Left err)   = Left $ ImmDbParseFailure epochOrSlot err

{-------------------------------------------------------------------------------
  Wrappers
-------------------------------------------------------------------------------}

closeDB :: (MonadCatch m, HasCallStack) => ImmDB m blk -> m ()
closeDB db = withDB db ImmDB.closeDB

reopen :: (MonadCatch m, HasCallStack) => ImmDB m blk -> m ()
reopen db = withDB db $ \imm ->
    ImmDB.reopen imm ImmDB.ValidateMostRecentEpoch

-- These wrappers ensure that we correctly rethrow exceptions using 'withDB'.

iteratorNext :: (HasCallStack, MonadCatch m)
             => ImmDB m blk
             -> ImmDB.Iterator (HeaderHash blk) m a
             -> m (ImmDB.IteratorResult (HeaderHash blk) a)
iteratorNext db it = withDB db $ const $ ImmDB.iteratorNext it

iteratorHasNext :: (HasCallStack, MonadCatch m)
                => ImmDB m blk
                -> ImmDB.Iterator (HeaderHash blk) m a
                -> m Bool
iteratorHasNext db it = withDB db $ const $ ImmDB.iteratorHasNext it

iteratorPeek :: (HasCallStack, MonadCatch m)
             => ImmDB m blk
             -> ImmDB.Iterator (HeaderHash blk) m a
             -> m (ImmDB.IteratorResult (HeaderHash blk) a)
iteratorPeek db it = withDB db $ const $ ImmDB.iteratorPeek it

iteratorClose :: (HasCallStack, MonadCatch m)
              => ImmDB m blk
              -> ImmDB.Iterator (HeaderHash blk) m a
              -> m ()
iteratorClose db it = withDB db $ const $ ImmDB.iteratorClose it
