{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE NamedFieldPuns            #-}
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
  , getBlockOrHeaderWithPoint
  , getDeserialisableBlockOrHeaderWithPoint
  , getBlockOrHeaderAtTip
  , getPointAtTip
  , getSlotNoAtTip
  , getKnownBlockOrHeader
  , getBlockOrHeader
  , getDeserialisableBlockOrHeader
  , getBlob
    -- * Appending a block
  , appendBlock
    -- * Streaming
  , stream
  , streamAfter
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
import           Data.Bifunctor (second)
import qualified Data.ByteString.Lazy as Lazy
import           Data.Functor (($>), (<&>))
import           GHC.Stack
import           System.FilePath ((</>))

import           Cardano.Prelude (allNoUnexpectedThunks)

import           Control.Monad.Class.MonadThrow

import           Ouroboros.Network.Block (pattern BlockPoint, ChainHash (..),
                     pattern GenesisPoint, HasHeader (..), HeaderHash, Point,
                     Serialised (..), SlotNo, atSlot, pointHash, pointSlot,
                     withHash)
import           Ouroboros.Network.Point (WithOrigin (..))

import           Ouroboros.Consensus.Block (Header, IsEBB (..))
import           Ouroboros.Consensus.Util.IOLike
import           Ouroboros.Consensus.Util.Orphans ()
import           Ouroboros.Consensus.Util.ResourceRegistry (ResourceRegistry,
                     allocateEither, unsafeRelease)

import           Ouroboros.Storage.ChainDB.API (BlockOrHeader (..),
                     ChainDbError (..), ChainDbFailure (..),
                     Deserialisable (..), StreamFrom (..), StreamTo (..),
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
    , isEBB     :: !(blk -> Maybe EpochNo)
    , addHdrEnv :: !(IsEBB -> Lazy.ByteString -> Lazy.ByteString)
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
    , immIsEBB          :: blk -> Maybe EpochNo
    , immCheckIntegrity :: blk -> Bool
    , immAddHdrEnv      :: IsEBB -> Lazy.ByteString -> Lazy.ByteString
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

withImmDB :: (IOLike m, HasHeader blk)
          => ImmDbArgs m blk -> (ImmDB m blk -> m a) -> m a
withImmDB args = bracket (openDB args) closeDB

-- | For testing purposes
openDB :: (IOLike m, HasHeader blk)
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
    parser = ImmDB.epochFileParser immHasFS immDecodeBlock immIsEBB
      -- TODO a more efficient to accomplish this?
      (void . immEncodeBlock) immCheckIntegrity


-- | For testing purposes
mkImmDB :: ImmutableDB (HeaderHash blk) m
        -> (forall s. Decoder s (Lazy.ByteString -> Header blk))
        -> (forall s. Decoder s (Lazy.ByteString -> blk))
        -> (blk -> BinaryInfo Encoding)
        -> EpochInfo m
        -> (blk -> Maybe EpochNo)
        -> (IsEBB -> Lazy.ByteString -> Lazy.ByteString)
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
             -> (== Just hash) <$> ImmDB.getEBBHash imm epochNo
          -- Slot in the past or equal to the tip, but the tip is a regular
          -- block.
          _ -> do
            hasRegularBlock <- (== Just hash) <$> ImmDB.getBlockHash imm slot
            if hasRegularBlock then
              return True
            else do
              epochNo <- epochInfoEpoch slot
              ebbSlot <- epochInfoFirst epochNo
              -- If it's a slot that can also contain an EBB, check if we have
              -- an EBB
              if slot == ebbSlot then
                (== Just hash) <$> ImmDB.getEBBHash imm epochNo
              else
                return False
  where
    EpochInfo{..} = epochInfo db

-- | Return the block or header corresponding to the given point, if it is
-- part of the ImmutableDB.
--
-- If we have a block at the slot of the point, but its hash differs, we
-- return 'Nothing'.
--
-- If the point corresponds to some slot in the future, a
-- 'ReadFutureSlotError' wrapped in a 'ImmDbFailure' is thrown.
getBlockOrHeaderWithPoint
  :: forall m blk b. (MonadCatch m, HasCallStack)
  => ImmDB m blk
  -> BlockOrHeader blk b
  -> Point blk
  -> m (Maybe b)
getBlockOrHeaderWithPoint db blockOrHeader pt =
    traverse deserialise =<<
    getDeserialisableBlockOrHeaderWithPoint db blockOrHeader pt

getDeserialisableBlockOrHeaderWithPoint
  :: (MonadCatch m, HasCallStack)
  => ImmDB m blk
  -> BlockOrHeader blk b
  -> Point blk
  -> m (Maybe (Deserialisable m blk b))
getDeserialisableBlockOrHeaderWithPoint db@ImmDB { addHdrEnv } blockOrHeader = \case
    GenesisPoint -> throwM NoGenesisBlock
    BlockPoint { withHash = hash, atSlot = slot } ->
      withDB db $ \imm -> getBlockOrHeaderBlob imm slot hash >>= \case
        Nothing                -> return $ Nothing
        Just (epochOrSlot, bs) -> return $ Just $ Deserialisable
          { serialised         = Serialised bs
          , deserialisableSlot = slot
          , deserialisableHash = hash
          , deserialise        = case parse db blockOrHeader epochOrSlot bs of
              Left  err -> throwM err
              Right blk -> return blk
          }
  where
    getBlockOrHeaderBlob imm slot hash = case blockOrHeader of
      Block  -> ImmDB.getBlockOrEBB       imm slot hash
      Header -> ImmDB.getBlockOrEBBHeader imm slot hash <&>
        fmap (uncurry addHdrEnvelope)

    addHdrEnvelope
      :: Either EpochNo SlotNo
      -> Lazy.ByteString
      -> (Either EpochNo SlotNo, Lazy.ByteString)
    addHdrEnvelope epochOrSlot bs = (epochOrSlot, addHdrEnv isEBB bs)
      where
        isEBB = case epochOrSlot of
          Left _epochNo -> IsEBB
          Right _slotNo -> IsNotEBB

getBlockOrHeaderAtTip
  :: (MonadCatch m, HasCallStack)
  => ImmDB m blk -> BlockOrHeader blk b -> m (Maybe b)
getBlockOrHeaderAtTip db blockOrHeader = do
    immTip <- withDB db $ \imm -> ImmDB.getTip imm
    case forgetHash <$> immTip of
      TipGen                   -> return Nothing
      Tip (ImmDB.EBB epochNo)  ->
        Just <$> getKnownBlockOrHeader db blockOrHeader (Left epochNo)
      Tip (ImmDB.Block slotNo) ->
        Just <$> getKnownBlockOrHeader db blockOrHeader (Right slotNo)

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

-- | Get a block or header from the ImmutableDB that we know should exist
--
-- If it does not exist, we are dealing with data corruption.
getKnownBlockOrHeader
  :: (MonadCatch m, HasCallStack)
  => ImmDB m blk -> BlockOrHeader blk b -> Either EpochNo SlotNo -> m b
getKnownBlockOrHeader db blockOrHeader epochOrSlot = do
    mBlockOrHeader <- mustExist epochOrSlot <$>
      getBlockOrHeader db blockOrHeader epochOrSlot
    case mBlockOrHeader of
      Right b  -> return b
      Left err -> throwM err

-- | Get a block or header from the ImmutableDB
getBlockOrHeader
  :: (MonadCatch m, HasCallStack)
  => ImmDB m blk
  -> BlockOrHeader blk b
  -> Either EpochNo SlotNo
  -> m (Maybe b)
getBlockOrHeader db blockOrHeader epochOrSlot =
    traverse deserialise =<<
    getDeserialisableBlockOrHeader db blockOrHeader epochOrSlot

getDeserialisableBlockOrHeader
  :: (MonadCatch m, HasCallStack)
  => ImmDB m blk
  -> BlockOrHeader blk b
  -> Either EpochNo SlotNo
  -> m (Maybe (Deserialisable m blk b))
getDeserialisableBlockOrHeader db blockOrHeader epochOrSlot =
   getBlob db blockOrHeader epochOrSlot >>= \case
     Nothing           -> return Nothing
     Just (hash, blob) -> Just <$>
       mkDeserialisable db blockOrHeader epochOrSlot hash blob

mkDeserialisable
  :: MonadThrow m
  => ImmDB m blk
  -> BlockOrHeader blk b
  -> Either EpochNo SlotNo
  -> HeaderHash blk
  -> Lazy.ByteString
  -> m (Deserialisable m blk b)
mkDeserialisable db@ImmDB { epochInfo } blockOrHeader epochOrSlot hash bs = do
    slotNo <- case epochOrSlot of
      Left epochNo -> epochInfoFirst epochNo
      Right slotNo -> return slotNo
    return Deserialisable
      { serialised         = Serialised bs
      , deserialisableSlot = slotNo
      , deserialisableHash = hash
      , deserialise        = case parse db blockOrHeader epochOrSlot bs of
          Left  err -> throwM err
          Right blk -> return blk
      }
  where
    EpochInfo { epochInfoFirst } = epochInfo

getBlob
  :: (MonadCatch m, HasCallStack)
  => ImmDB m blk
  -> BlockOrHeader blk b
  -> Either EpochNo SlotNo
  -> m (Maybe  (HeaderHash blk, Lazy.ByteString))
getBlob db@ImmDB { addHdrEnv } blockOrHeader epochOrSlot = withDB db $ \imm ->
    case blockOrHeader of
      Block  -> case epochOrSlot of
        Left epochNo -> ImmDB.getEBB   imm epochNo
        Right slotNo -> ImmDB.getBlock imm slotNo
      Header -> case epochOrSlot of
        Left epochNo -> fmap (second (addHdrEnv IsEBB))    <$>
          ImmDB.getEBBHeader   imm epochNo
        Right slotNo -> fmap (second (addHdrEnv IsNotEBB)) <$>
          ImmDB.getBlockHeader imm slotNo

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
registeredStream :: forall m blk b. IOLike m
                 => ImmDB m blk
                 -> ResourceRegistry m
                 -> BlockOrHeader blk b
                 -> Maybe (SlotNo, HeaderHash blk)
                 -> Maybe (SlotNo, HeaderHash blk)
                 -> m (Either (UnknownRange blk)
                              (ImmDB.Iterator (HeaderHash blk) m Lazy.ByteString))
registeredStream db@ImmDB { addHdrEnv } registry blockOrHeader start end = do
    errOrKeyAndIt <- allocateEither registry
      (\_key -> withDB db $ \imm -> go imm)
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
    go imm = case blockOrHeader of
      Block  -> ImmDB.streamBlocks  imm start end
      Header -> ImmDB.streamHeaders imm start end <&>
        fmap (traverseIterator db (traverseIteratorResult db addHdrEnvelope))

    addHdrEnvelope
      :: Either EpochNo SlotNo
      -> HeaderHash blk
      -> Lazy.ByteString
      -> m Lazy.ByteString
    addHdrEnvelope epochOrSlot _hash = return . addHdrEnv isEBB
      where
        isEBB = case epochOrSlot of
          Left _epochNo -> IsEBB
          Right _slotNo -> IsNotEBB

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

-- | Stream headers/blocks from the given 'StreamFrom' to the given
-- 'StreamTo'.
--
-- When passed @'StreamFromInclusive' pt@ where @pt@ refers to Genesis, a
-- 'NoGenesisBlock' exception will be thrown.
stream
  :: forall m blk b. (IOLike m, HasHeader blk)
  => ImmDB m blk
  -> ResourceRegistry m
  -> BlockOrHeader blk b
  -> StreamFrom blk
  -> StreamTo   blk
  -> m (Either (UnknownRange blk)
               (ImmDB.Iterator (HeaderHash blk) m (Deserialisable m blk b)))
stream db registry blockOrHeader from to = runExceptT $ do
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
    openRegisteredStream start end = ExceptT $
      fmap (deserialisableIterator db blockOrHeader . stopAt to) <$>
      registeredStream db registry blockOrHeader start end

    -- | The ImmutableDB doesn't support an exclusive end bound, so we stop
    -- the iterator when it reaches its exclusive end bound.
    stopAt :: StreamTo blk
           -> ImmDB.Iterator (HeaderHash blk) m a
           -> ImmDB.Iterator (HeaderHash blk) m a
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
            ImmDB.IteratorResult _slotNo  hash _
              | isEnd hash
              -> ImmDB.IteratorExhausted
            ImmDB.IteratorEBB    _epochNo hash _
              | isEnd hash
              -> ImmDB.IteratorExhausted
            itRes -> itRes

-- | Stream headers/blocks after the given point
--
-- PRECONDITION: the exclusive lower bound is part of the ImmutableDB, if not,
-- 'ImmDbMissingBlockPoint' is thrown.
streamAfter
  :: forall m blk b. (IOLike m, HasHeader blk, HasCallStack)
  => ImmDB m blk
  -> ResourceRegistry m
  -> BlockOrHeader blk b
  -> Point blk -- ^ Exclusive lower bound
  -> m (Iterator (HeaderHash blk) m (Deserialisable m blk b))
streamAfter db registry blockOrHeader low =
    registeredStream db registry blockOrHeader low' Nothing >>= \case
      Left  _   -> throwM $ ImmDbMissingBlockPoint low callStack
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
        return $ deserialisableIterator db blockOrHeader itr
  where
    low' :: Maybe (SlotNo, HeaderHash blk)
    low' = case low of
      GenesisPoint         -> Nothing
      BlockPoint slot hash -> Just (slot, hash)

traverseIteratorResult
  :: Applicative m
  => proxy blk  -- ^ Needed for 'HeaderHash'
  -> (Either EpochNo SlotNo -> HeaderHash blk -> a -> m b)
     -- ^ Left for an EBB, right for a regular block
  -> IteratorResult (HeaderHash blk) a
  -> m (IteratorResult (HeaderHash blk) b)
traverseIteratorResult _proxy f = \case
    IteratorExhausted              ->
      pure IteratorExhausted
    IteratorEBB    epochNo hash bs ->
      IteratorEBB    epochNo hash <$> f (Left epochNo) hash bs
    IteratorResult slotNo  hash bs ->
      IteratorResult slotNo  hash <$> f (Right slotNo) hash bs

deserialisableIteratorResult
  :: MonadThrow m
  => ImmDB m blk
  -> BlockOrHeader blk b
     -- ^ 'BlockOrHeader' must match the iterator contents
  -> IteratorResult (HeaderHash blk) Lazy.ByteString
  -> m (IteratorResult (HeaderHash blk) (Deserialisable m blk b))
deserialisableIteratorResult db blockOrHeader =
    traverseIteratorResult db (mkDeserialisable db blockOrHeader)

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
  -> BlockOrHeader blk b
  -> Iterator (HeaderHash blk) m Lazy.ByteString
  -> Iterator (HeaderHash blk) m (Deserialisable m blk b)
deserialisableIterator db blockOrHeader =
  traverseIterator db (deserialisableIteratorResult db blockOrHeader)

deserialiseIterator
  :: MonadCatch m
  => ImmDB m blk
  -> Iterator (HeaderHash blk) m (Deserialisable m blk b)
  -> Iterator (HeaderHash blk) m b
deserialiseIterator db = traverseIterator db (traverse deserialise)

parseIterator
  :: MonadCatch m
  => ImmDB m blk
  -> BlockOrHeader blk b
  -> Iterator (HeaderHash blk) m Lazy.ByteString
  -> Iterator (HeaderHash blk) m b
parseIterator db blockOrHeader = traverseIterator db
  (deserialisableIteratorResult db blockOrHeader >=> traverse deserialise)

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

parse :: forall m blk b.
         ImmDB m blk
      -> BlockOrHeader blk b
      -> Either EpochNo SlotNo
      -> Lazy.ByteString
      -> Either ChainDbFailure b
parse db blockOrHeader epochOrSlot bytes =
    aux (CBOR.deserialiseFromBytes dec bytes)
  where
    dec :: forall s. Decoder s (Lazy.ByteString -> b)
    dec = case blockOrHeader of
      Block  -> decBlock  db
      Header -> decHeader db

    aux :: Either CBOR.DeserialiseFailure
                  (Lazy.ByteString, Lazy.ByteString -> b)
        -> Either ChainDbFailure b
    aux (Right (bs, b))
      | Lazy.null bs = Right (b bytes)
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
                -> m (Maybe (Either EpochNo SlotNo, HeaderHash blk))
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
