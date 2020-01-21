{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE PatternSynonyms           #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TupleSections             #-}

{-# OPTIONS_GHC -Wredundant-constraints #-}
-- | Thin wrapper around the ImmutableDB
module Ouroboros.Storage.ChainDB.Impl.ImmDB (
    ImmDB -- Opaque
    -- * Initialization
  , ImmDbArgs(..)
  , defaultArgs
  , withImmDB
    -- * Getting and parsing blocks
  , hasBlock
  , getTipInfo
  , getPointAtTip
  , getSlotNoAtTip
  , getKnownBlockComponent
  , getBlockComponent
  , getBlockComponentAtTip
  , getBlockComponentWithPoint
    -- * Appending a block
  , appendBlock
    -- * Streaming
  , stream
  , streamAfter
  , streamAfterKnownBlock
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
  , ImmDB.Iterator
  , ImmDB.IteratorResult (..)
  , ImmDB.ValidationPolicy (..)
  , ImmDB.ImmutableDBError
  , ImmDB.BinaryInfo (..)
  , ImmDB.HashInfo (..)
  , Index.CacheConfig (..)
    -- * Exported for testing purposes
  , openDB
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
import           Data.Bifunctor
import qualified Data.ByteString.Lazy as Lazy
import           Data.Functor (($>), (<&>))
import           Data.Word (Word32)
import           GHC.Stack
import           System.FilePath ((</>))

import           Cardano.Prelude (allNoUnexpectedThunks)

import           Control.Monad.Class.MonadThrow

import           Ouroboros.Network.Block (pattern BlockPoint, ChainHash (..),
                     pattern GenesisPoint, HasHeader (..), HeaderHash, Point,
                     SlotNo, atSlot, pointHash, pointSlot, withHash)
import           Ouroboros.Network.Point (WithOrigin (..))

import           Ouroboros.Consensus.Block (GetHeader (..), IsEBB (..))
import           Ouroboros.Consensus.Util.IOLike
import           Ouroboros.Consensus.Util.Orphans ()
import           Ouroboros.Consensus.Util.ResourceRegistry (ResourceRegistry,
                     allocateEither, unsafeRelease)

import           Ouroboros.Storage.ChainDB.API (ChainDB)
import           Ouroboros.Storage.ChainDB.API hiding (ChainDB (..),
                     Iterator (..), closeDB)
import           Ouroboros.Storage.ChainDB.Impl.BlockComponent
import           Ouroboros.Storage.Common (EpochNo, Tip (..))
import           Ouroboros.Storage.EpochInfo (EpochInfo (..))
import           Ouroboros.Storage.FS.API (HasFS, createDirectoryIfMissing)
import           Ouroboros.Storage.FS.API.Types (MountPoint (..), mkFsPath)
import           Ouroboros.Storage.FS.IO (ioHasFS)
import           Ouroboros.Storage.ImmutableDB (BinaryInfo (..), HashInfo (..),
                     ImmutableDB, WithHash (..))
import qualified Ouroboros.Storage.ImmutableDB as ImmDB
import qualified Ouroboros.Storage.ImmutableDB.Impl.Index as Index
                     (CacheConfig (..))
import qualified Ouroboros.Storage.ImmutableDB.Parser as ImmDB
import           Ouroboros.Storage.Util.ErrorHandling (ErrorHandling)
import qualified Ouroboros.Storage.Util.ErrorHandling as EH

-- | Thin wrapper around the ImmutableDB (opaque type)
data ImmDB m blk = ImmDB {
      immDB     :: !(ImmutableDB (HeaderHash blk) m)
    , decHeader :: !(forall s. Decoder s (Lazy.ByteString -> Header blk))
    , decBlock  :: !(forall s. Decoder s (Lazy.ByteString -> blk))
      -- ^ TODO introduce a newtype wrapper around the @s@ so we can use
      -- generics to derive the NoUnexpectedThunks instance.
    , encBlock  :: !(blk -> BinaryInfo Encoding)
    , epochInfo :: !(EpochInfo m)
    , isEBB     :: !(Header blk -> Maybe EpochNo)
    , addHdrEnv :: !(IsEBB -> Word32 -> Lazy.ByteString -> Lazy.ByteString)
    , err       :: !(ErrorHandling ImmDB.ImmutableDBError m)
    }


-- Universal type; we can't use generics
instance NoUnexpectedThunks (ImmDB m blk) where
  showTypeOf _ = "ImmDB"
  whnfNoUnexpectedThunks ctxt ImmDB {..} = allNoUnexpectedThunks
    [ noUnexpectedThunks ctxt immDB
    , noUnexpectedThunks ctxt decHeader
    , noUnexpectedThunks ctxt decBlock
    , noUnexpectedThunks ctxt encBlock
    , noUnexpectedThunks ctxt epochInfo
    , noUnexpectedThunks ctxt isEBB
    , noUnexpectedThunks ctxt addHdrEnv
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
    , immDecodeHeader   :: forall s. Decoder s (Lazy.ByteString -> Header blk)
    , immEncodeHash     :: HeaderHash blk -> Encoding
    , immEncodeBlock    :: blk -> BinaryInfo Encoding
    , immErr            :: ErrorHandling ImmDB.ImmutableDBError m
    , immEpochInfo      :: EpochInfo m
    , immHashInfo       :: HashInfo (HeaderHash blk)
    , immValidation     :: ImmDB.ValidationPolicy
    , immIsEBB          :: Header blk -> Maybe EpochNo
    , immCheckIntegrity :: blk -> Bool
    , immAddHdrEnv      :: IsEBB -> Word32 -> Lazy.ByteString -> Lazy.ByteString
    , immHasFS          :: HasFS m h
    , immTracer         :: Tracer m (TraceEvent blk)
    , immCacheConfig    :: Index.CacheConfig
    , immRegistry       :: ResourceRegistry m
    }

-- | Default arguments when using the 'IO' monad
--
-- The following fields must still be defined:
--
-- * 'immDecodeHash'
-- * 'immDecodeBlock'
-- * 'immDecodeHeader'
-- * 'immEncodeHash'
-- * 'immEncodeBlock'
-- * 'immEpochInfo'
-- * 'immHashInfo'
-- * 'immValidation'
-- * 'immIsEBB'
-- * 'immCheckIntegrity'
-- * 'immAddHdrEnv'
-- * 'immRegistry'
defaultArgs :: FilePath -> ImmDbArgs IO blk
defaultArgs fp = ImmDbArgs{
      immErr          = EH.exceptions
    , immHasFS        = ioHasFS $ MountPoint (fp </> "immutable")
    , immCacheConfig  = cacheConfig
    , immTracer         = nullTracer
      -- Fields without a default
    , immDecodeHash     = error "no default for immDecodeHash"
    , immDecodeBlock    = error "no default for immDecodeBlock"
    , immDecodeHeader   = error "no default for immDecodeHeader"
    , immEncodeHash     = error "no default for immEncodeHash"
    , immEncodeBlock    = error "no default for immEncodeBlock"
    , immEpochInfo      = error "no default for immEpochInfo"
    , immHashInfo       = error "no default for immHashInfo"
    , immValidation     = error "no default for immValidation"
    , immIsEBB          = error "no default for immIsEBB"
    , immCheckIntegrity = error "no default for immCheckIntegrity"
    , immAddHdrEnv      = error "no default for immAddHdrEnv"
    , immRegistry       = error "no default for immRegistry"
    }
  where
    -- Cache 250 past epochs by default. This will take roughly 250 MB of RAM.
    -- At the time of writing (1/2020), there are 166 epochs, so even one year
    -- from now, we will be able to cache all epochs' indices in the chain.
    --
    -- If this number were too low, i.e., less than the number of epochs that
    -- that clients are requesting blocks from, we would constantly evict and
    -- reparse indices, causing a much higher CPU load.
    cacheConfig = Index.CacheConfig
      { pastEpochsToCache = 250
      , expireUnusedAfter = 5 * 60 -- Expire after 1 minute
      }

withImmDB :: (IOLike m, HasHeader blk, GetHeader blk)
          => ImmDbArgs m blk -> (ImmDB m blk -> m a) -> m a
withImmDB args = bracket (openDB args) closeDB

-- | For testing purposes
openDB :: (IOLike m, HasHeader blk, GetHeader blk)
       => ImmDbArgs m blk
       -> m (ImmDB m blk)
openDB ImmDbArgs{..} = do
    createDirectoryIfMissing immHasFS True (mkFsPath [])
    (immDB, _internal) <- ImmDB.openDBInternal
      immRegistry
      immHasFS
      immErr
      immEpochInfo
      immHashInfo
      immValidation
      parser
      immTracer
      immCacheConfig
    return ImmDB
      { immDB     = immDB
      , decHeader = immDecodeHeader
      , decBlock  = immDecodeBlock
      , encBlock  = immEncodeBlock
      , epochInfo = immEpochInfo
      , isEBB     = immIsEBB
      , addHdrEnv = immAddHdrEnv
      , err       = immErr
      }
  where
    parser = ImmDB.epochFileParser immHasFS immDecodeBlock (immIsEBB . getHeader)
      -- TODO a more efficient to accomplish this?
      (void . immEncodeBlock) immCheckIntegrity


-- | For testing purposes
mkImmDB :: ImmutableDB (HeaderHash blk) m
        -> (forall s. Decoder s (Lazy.ByteString -> Header blk))
        -> (forall s. Decoder s (Lazy.ByteString -> blk))
        -> (blk -> BinaryInfo Encoding)
        -> EpochInfo m
        -> (Header blk -> Maybe EpochNo)
        -> (IsEBB -> Word32 -> Lazy.ByteString -> Lazy.ByteString)
        -> ErrorHandling ImmDB.ImmutableDBError m
        -> ImmDB m blk
mkImmDB immDB decHeader decBlock encBlock epochInfo isEBB addHdrEnv err = ImmDB {..}

{-------------------------------------------------------------------------------
  Getting and parsing blocks
-------------------------------------------------------------------------------}

-- | Return 'True' when the given point is in the ImmutableDB.
--
-- This is much more efficient than 'getBlockOrHeaderWithPoint' as no block or
-- header has to be read from disk.
--
-- If the point corresponds to some slot in the future, 'False' is returned,
-- alleviating the user from the non-trivial check that a block is not in the
-- future.
hasBlock
  :: (MonadCatch m, HasHeader blk, HasCallStack)
  => ImmDB m blk
  -> Point blk
  -> m Bool
hasBlock db = \case
    GenesisPoint -> throwM NoGenesisBlock
    BlockPoint { withHash = hash, atSlot = slot } ->
      withDB db $ \imm -> do
        immTip <- ImmDB.getTip imm
        (slotNoAtTip, ebbAtTip) <- case forgetHash <$> immTip of
          TipGen                   -> return (Origin, Nothing)
          Tip (ImmDB.EBB epochNo)  -> (, Just epochNo) . At  <$> epochInfoFirst epochNo
          Tip (ImmDB.Block slotNo) -> return (At slotNo, Nothing)

        case At slot `compare` slotNoAtTip of
          -- The request is greater than the tip, so we cannot have the block
          GT -> return False
          -- Same slot, but our tip is an EBB, so we cannot check if the
          -- regular block in that slot exists, because that's in the future.
          EQ | Just epochNo <- ebbAtTip
             -> (== Just hash) <$> ImmDB.getEBBComponent imm GetHash epochNo
          -- Slot in the past or equal to the tip, but the tip is a regular
          -- block.
          _ -> do
            hasRegularBlock <- (== Just hash) <$>
              ImmDB.getBlockComponent imm GetHash slot
            if hasRegularBlock then
              return True
            else do
              epochNo <- epochInfoEpoch slot
              ebbSlot <- epochInfoFirst epochNo
              -- If it's a slot that can also contain an EBB, check if we have
              -- an EBB
              if slot == ebbSlot then
                (== Just hash) <$>
                  ImmDB.getEBBComponent imm GetHash epochNo
              else
                return False
  where
    EpochInfo{..} = epochInfo db

getTipInfo :: forall m blk.
              (MonadCatch m, HasCallStack)
           => ImmDB m blk -> m (WithOrigin (SlotNo, HeaderHash blk, IsEBB))
getTipInfo db = do
    immTip <- withDB db $ \imm -> ImmDB.getTip imm
    case immTip of
      TipGen -> return Origin
      Tip (WithHash hash (ImmDB.EBB epochNo)) ->
        At . (, hash, IsEBB) <$> epochInfoFirst epochNo
      Tip (WithHash hash (ImmDB.Block slotNo)) ->
        return $ At (slotNo, hash, IsNotEBB)
  where
    EpochInfo{..} = epochInfo db

getPointAtTip :: forall m blk.
                 (MonadCatch m, HasCallStack)
              => ImmDB m blk -> m (Point blk)
getPointAtTip db = getTipInfo db <&> \case
    Origin             -> GenesisPoint
    At (slot, hash, _) -> BlockPoint slot hash

getSlotNoAtTip :: MonadCatch m => ImmDB m blk -> m (WithOrigin SlotNo)
getSlotNoAtTip db = pointSlot <$> getPointAtTip db

getKnownBlockComponent
  :: (MonadCatch m, HasHeader blk, HasCallStack)
  => ImmDB m blk
  -> BlockComponent (ChainDB m blk) b
  -> Either EpochNo SlotNo
  -> m b
getKnownBlockComponent db blockComponent epochOrSlot = do
    mBlock <- mustExist epochOrSlot <$>
      getBlockComponent db blockComponent epochOrSlot
    case mBlock of
      Right b  -> return b
      Left err -> throwM err

getBlockComponent
  :: forall m blk b. (MonadCatch m, HasHeader blk, HasCallStack)
  => ImmDB m blk
  -> BlockComponent (ChainDB m blk) b
  -> Either EpochNo SlotNo
  -> m (Maybe b)
getBlockComponent db blockComponent epochOrSlot = withDB db $ \imm ->
    case epochOrSlot of
      Left epoch -> ImmDB.getEBBComponent   imm blockComponent' epoch
      Right slot -> ImmDB.getBlockComponent imm blockComponent' slot
  where
    blockComponent' = translateToRawDB (parse db) (addHdrEnv db) blockComponent

getBlockComponentAtTip
  :: (MonadCatch m, HasHeader blk, HasCallStack)
  => ImmDB m blk -> BlockComponent (ChainDB m blk) b -> m (Maybe b)
getBlockComponentAtTip db blockComponent = do
    immTip <- withDB db $ \imm -> ImmDB.getTip imm
    case forgetHash <$> immTip of
      TipGen -> return Nothing
      Tip (ImmDB.EBB epoch) ->
        Just <$> getKnownBlockComponent db blockComponent (Left epoch)
      Tip (ImmDB.Block slot) ->
        Just <$> getKnownBlockComponent db blockComponent (Right slot)

-- | Return the block component of the block corresponding to the given point,
-- if it is part of the ImmutableDB.
--
-- If we have a block at the slot of the point, but its hash differs, we
-- return 'Nothing'.
--
-- If the point corresponds to some slot in the future, a
-- 'ReadFutureSlotError' wrapped in a 'ImmDbFailure' is thrown.
getBlockComponentWithPoint
  :: forall m blk b. (MonadCatch m, HasHeader blk, HasCallStack)
  => ImmDB m blk
  -> BlockComponent (ChainDB m blk) b
  -> Point blk
  -> m (Maybe b)
getBlockComponentWithPoint db blockComponent = \case
    GenesisPoint -> throwM NoGenesisBlock
    BlockPoint { withHash = hash, atSlot = slot } -> withDB db $ \imm ->
      ImmDB.getBlockOrEBBComponent imm blockComponent' slot hash
  where
    blockComponent' = translateToRawDB (parse db) (addHdrEnv db) blockComponent

{-------------------------------------------------------------------------------
  Appending a block
-------------------------------------------------------------------------------}

-- | Appends the given block (as a regular block or EBB) to the ImmutableDB.
--
-- Does not check whether the block is in the past or not.
appendBlock :: (MonadCatch m, HasHeader blk, GetHeader blk, HasCallStack)
            => ImmDB m blk -> blk -> m ()
appendBlock db@ImmDB{..} b = withDB db $ \imm -> case isEBB (getHeader b) of
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

-- | Wrapper around 'ImmDB.stream' that 'allocate's the iterator in the
-- 'ResourceRegistry' so that 'ImmDB.iteratorClose' is registered as the
-- clean-up action. Translates the requested 'BlockComponent' into the
-- 'ImmDB.BlockComponent' the ImmutableDB understands.
--
-- When the returned iterator is closed, it will be 'release'd from the
-- 'ResourceRegistry'.
registeredStream
  :: forall m blk b. (IOLike m, HasHeader blk)
  => ImmDB m blk
  -> ResourceRegistry m
  -> BlockComponent (ChainDB m blk) b
  -> Maybe (SlotNo, HeaderHash blk)
  -> Maybe (SlotNo, HeaderHash blk)
  -> m (Either (ImmDB.WrongBoundError (HeaderHash blk))
               (ImmDB.Iterator (HeaderHash blk) m b))
registeredStream db registry blockComponent start end = do
    errOrKeyAndIt <- allocateEither registry
      (\_key -> withDB db $ \imm ->
        ImmDB.stream imm blockComponent' start end)
      (iteratorClose db)
    return $ case errOrKeyAndIt of
      Left e          -> Left e
      -- The iterator will be used by a thread that is unknown to the registry
      -- (which, after all, is entirely internal to the chain DB). This means
      -- that the registry cannot guarantee that the iterator will be live for
      -- the duration of that thread, and indeed, it may not be: the chain DB
      -- might be closed before that thread terminates. We will deal with this
      -- in the chain DB itself (throw ClosedDBError exception).
      Right (key, it) -> Right it { ImmDB.iteratorClose = unsafeRelease key }
  where
    blockComponent' = translateToRawDB (parse db) (addHdrEnv db) blockComponent

-- | Stream headers/blocks from the given 'StreamFrom' to the given
-- 'StreamTo'.
--
-- When passed @'StreamFromInclusive' pt@ where @pt@ refers to Genesis, a
-- 'NoGenesisBlock' exception will be thrown.
stream
  :: forall m blk b. (IOLike m, HasHeader blk)
  => ImmDB m blk
  -> ResourceRegistry m
  -> BlockComponent (ChainDB m blk) b
  -> StreamFrom blk
  -> StreamTo   blk
  -> m (Either (UnknownRange blk) (ImmDB.Iterator (HeaderHash blk) m b))
stream db registry blockComponent from to = runExceptT $ do
    -- Also check if the slot of the bound is <= the slot of the tip. If not,
    -- throw a 'MissingBlock' error.
    --
    -- Note that between this check and the actual opening of the iterator, a
    -- block may be appended to the ImmutableDB such that the requested slot
    -- is no longer in the future, but we have returned 'MissingBlock'
    -- nonetheless. This is fine, since the request to stream from the slot
    -- was made earlier, at a moment where it still was in the future.
    slotNoAtTip <- lift $ getSlotNoAtTip db

    end <- case to of
      StreamToExclusive pt@BlockPoint { atSlot = slot, withHash = hash } -> do
        when (pointSlot pt > slotNoAtTip) $ throwError $ MissingBlock pt
        return $ Just (slot, hash)
      StreamToExclusive GenesisPoint ->
        throwM NoGenesisBlock
      StreamToInclusive pt@BlockPoint { atSlot = slot, withHash = hash } -> do
        when (pointSlot pt > slotNoAtTip) $ throwError $ MissingBlock pt
        return $ Just (slot, hash)
      StreamToInclusive GenesisPoint ->
        throwM NoGenesisBlock

    case from of
      StreamFromExclusive pt@BlockPoint { atSlot = slot, withHash = hash } -> do
        when (pointSlot pt > slotNoAtTip) $ throwError $ MissingBlock pt
        it <- openRegisteredStream (Just (slot, hash)) end
        -- Skip the first block, as the bound is exclusive
        void $ lift $ iteratorNext db it
        return it
      StreamFromExclusive    GenesisPoint ->
        openRegisteredStream Nothing end
      StreamFromInclusive pt@BlockPoint { atSlot = slot, withHash = hash } -> do
        when (pointSlot pt > slotNoAtTip) $ throwError $ MissingBlock pt
        openRegisteredStream (Just (slot, hash)) end
      StreamFromInclusive GenesisPoint ->
        throwM NoGenesisBlock
  where
    openRegisteredStream
      :: Maybe (SlotNo, HeaderHash blk)
      -> Maybe (SlotNo, HeaderHash blk)
      -> ExceptT (UnknownRange blk)
                 m
                 (ImmDB.Iterator (HeaderHash blk) m b)
    openRegisteredStream start end = ExceptT $
        bimap toUnknownRange (fmap snd . stopAt to) <$>
      -- 'stopAt' needs to know the hash of each streamed block, so we \"Get\"
      -- it in addition to @b@, but we drop it afterwards.
        registeredStream db registry ((,) <$> GetHash <*> blockComponent) start end
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

    -- | The ImmutableDB doesn't support an exclusive end bound, so we stop
    -- the iterator when it reaches its exclusive end bound.
    stopAt :: forall a.
              StreamTo blk
           -> ImmDB.Iterator (HeaderHash blk) m (HeaderHash blk, a)
           -> ImmDB.Iterator (HeaderHash blk) m (HeaderHash blk, a)
    stopAt = \case
      StreamToInclusive _  -> id
      StreamToExclusive pt -> \it -> it
          { ImmDB.iteratorNext    = ignoreExclusiveBound <$> ImmDB.iteratorNext it
          , ImmDB.iteratorPeek    = ignoreExclusiveBound <$> ImmDB.iteratorPeek it
          , ImmDB.iteratorHasNext = ImmDB.iteratorHasNext it >>= \case
              Nothing -> return Nothing
              Just next@(_epochOrSlot, hash)
                | isEnd hash
                -> ImmDB.iteratorClose it $> Nothing
                | otherwise
                -> return $ Just next
          }
        where
          isEnd hash = pointHash pt == BlockHash hash
          ignoreExclusiveBound = \case
            ImmDB.IteratorResult (hash, _)
              | isEnd hash
              -> ImmDB.IteratorExhausted
            itRes -> itRes

-- | Stream headers/blocks after the given point
--
-- Returns 'ImmDB.WrongBoundError' if the lower bound is not part of the
-- ImmutableDB.
streamAfter
  :: forall m blk b. (IOLike m, HasHeader blk, HasCallStack)
  => ImmDB m blk
  -> ResourceRegistry m
  -> BlockComponent (ChainDB m blk) b
  -> Point blk -- ^ Exclusive lower bound
  -> m (Either
          (ImmDB.WrongBoundError (HeaderHash blk))
          (ImmDB.Iterator (HeaderHash blk) m b))
streamAfter db registry blockComponent low =
    registeredStream db registry blockComponent low' Nothing >>= \case
      Left  err -> return $ Left err
      Right itr -> do
        case low of
          GenesisPoint           -> return ()
          -- Skip the exclusive lower bound
          BlockPoint _slot _hash -> iteratorNext db itr >>= \case
              ImmDB.IteratorExhausted ->
                throwM $ ImmDbUnexpectedIteratorExhausted low
              -- The hash must match the lower bound's hash. The ImmutableDB
              -- already checks this.
              ImmDB.IteratorResult {} -> return ()
        return $ Right itr
  where
    low' :: Maybe (SlotNo, HeaderHash blk)
    low' = case low of
      GenesisPoint         -> Nothing
      BlockPoint slot hash -> Just (slot, hash)

-- | Variation on 'streamAfter' that has the presence of the lower bound
-- as a precondition
--
-- Throws an exception if the precondition is violated.
streamAfterKnownBlock
  :: forall m blk b. (IOLike m, HasHeader blk, HasCallStack)
  => ImmDB m blk
  -> ResourceRegistry m
  -> BlockComponent (ChainDB m blk) b
  -> Point blk -- ^ Exclusive lower bound
  -> m (ImmDB.Iterator (HeaderHash blk) m b)
streamAfterKnownBlock db registry blockComponent low = do
    mItr <- streamAfter db registry blockComponent low
    case mItr of
      Left  err -> throwM $ ImmDbMissingBlockPoint low err callStack
      Right itr -> return itr

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
          -> Maybe b
          -> Either ChainDbFailure b
mustExist epochOrSlot Nothing  = Left  $ ImmDbMissingBlock epochOrSlot
mustExist _           (Just b) = Right $ b

-- TODO unify with VolDB.parse
parse :: forall m blk b. (HasHeader blk, MonadThrow m)
      => ImmDB m blk
      -> BlockOrHeader blk b
      -> BlockRef blk
      -> Lazy.ByteString
      -> m b  -- ^ Throws 'ChainDbFailure'
parse db blockOrHeader blockRef bytes =
    aux (CBOR.deserialiseFromBytes dec bytes)
  where
    dec :: forall s. Decoder s (Lazy.ByteString -> b)
    dec = case blockOrHeader of
      Block  -> decBlock  db
      Header -> decHeader db

    aux :: Either CBOR.DeserialiseFailure
                  (Lazy.ByteString, Lazy.ByteString -> b)
        -> m b
    aux (Right (bs, b))
      | Lazy.null bs = return $ b bytes
      | otherwise    = throwM $ ImmDbTrailingData blockRef bs
    aux (Left err)   = throwM $ ImmDbParseFailure blockRef err

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
             -> m (ImmDB.IteratorResult a)
iteratorNext db it = withDB db $ const $ ImmDB.iteratorNext it

iteratorHasNext :: (HasCallStack, MonadCatch m)
                => ImmDB m blk
                -> ImmDB.Iterator (HeaderHash blk) m a
                -> m (Maybe (Either EpochNo SlotNo, HeaderHash blk))
iteratorHasNext db it = withDB db $ const $ ImmDB.iteratorHasNext it

iteratorPeek :: (HasCallStack, MonadCatch m)
             => ImmDB m blk
             -> ImmDB.Iterator (HeaderHash blk) m a
             -> m (ImmDB.IteratorResult a)
iteratorPeek db it = withDB db $ const $ ImmDB.iteratorPeek it

iteratorClose :: (HasCallStack, MonadCatch m)
              => ImmDB m blk
              -> ImmDB.Iterator (HeaderHash blk) m a
              -> m ()
iteratorClose db it = withDB db $ const $ ImmDB.iteratorClose it
