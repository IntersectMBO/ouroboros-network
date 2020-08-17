{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE PatternSynonyms     #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeApplications    #-}

-- | Thin wrapper around the ImmutableDB
module Ouroboros.Consensus.Storage.ChainDB.Impl.ImmDB (
    ImmDB -- Opaque
  , ImmDbSerialiseConstraints
    -- * Initialization
  , ImmDbArgs(..)
  , defaultArgs
  , hashInfo
  , withImmDB
    -- * Getting and parsing blocks
  , hasBlock
  , getTipInfo
  , getPointAtTip
  , getAnchorForTip
  , getSlotNoAtTip
  , getKnownBlockComponent
  , getBlockComponent
  , getBlockComponentWithPoint
    -- * Appending a block
  , appendBlock
    -- * Streaming
  , stream
  , streamAfter
  , streamAfterKnownBlock
    -- * Wrappers
  , closeDB
  , iteratorNext
  , iteratorHasNext
  , iteratorClose
    -- * Tracing
  , TraceEvent
  , ImmDB.ChunkFileError
    -- * Re-exports
  , ImmDB.Iterator
  , ImmDB.IteratorResult (..)
  , ImmDB.ValidationPolicy (..)
  , ImmDB.ImmutableDBError
  , BinaryBlockInfo (..)
  , HashInfo (..)
  , Index.CacheConfig (..)
    -- * Re-exports of aspects of 'ChunkInfo'
  , ChunkInfo
  , chunkIndexOfSlot
  , ChunkNo
  , firstChunkNo
    -- * Exported for testing purposes
  , openDB
  , mkImmDB
    -- * Exported for utilities
  , ImmDB.chunkFileParser
  ) where

import           Codec.CBOR.Decoding (Decoder)
import qualified Codec.CBOR.Read as CBOR
import qualified Codec.CBOR.Write as CBOR
import           Control.Monad
import           Control.Monad.Except
import           Control.Tracer (Tracer, nullTracer)
import           Data.Bifunctor
import           Data.Binary.Get (Get)
import qualified Data.Binary.Get as Get
import           Data.Binary.Put (Put)
import qualified Data.Binary.Put as Put
import qualified Data.ByteString.Lazy as Lazy
import           Data.ByteString.Short (ShortByteString)
import           Data.Functor ((<&>))
import           Data.Word (Word32)
import           GHC.Generics (Generic)
import           GHC.Stack
import           System.FilePath ((</>))

import qualified Ouroboros.Network.AnchoredFragment as AF

import           Ouroboros.Consensus.Block hiding (hashSize)
import qualified Ouroboros.Consensus.Block as Block
import           Ouroboros.Consensus.Util.IOLike
import           Ouroboros.Consensus.Util.Orphans ()
import           Ouroboros.Consensus.Util.ResourceRegistry (ResourceRegistry)

import           Ouroboros.Consensus.Storage.Common
import           Ouroboros.Consensus.Storage.FS.API (SomeHasFS (..),
                     createDirectoryIfMissing)
import           Ouroboros.Consensus.Storage.FS.API.Types (MountPoint (..),
                     mkFsPath)
import           Ouroboros.Consensus.Storage.FS.IO (ioHasFS)

import           Ouroboros.Consensus.Storage.ImmutableDB (HashInfo (..),
                     ImmutableDB, TipInfo (..))
import qualified Ouroboros.Consensus.Storage.ImmutableDB as ImmDB
import           Ouroboros.Consensus.Storage.ImmutableDB.Chunks
import qualified Ouroboros.Consensus.Storage.ImmutableDB.Impl.Index as Index
                     (CacheConfig (..))
import qualified Ouroboros.Consensus.Storage.ImmutableDB.Parser as ImmDB

import           Ouroboros.Consensus.Storage.ChainDB.API (ChainDB)
import           Ouroboros.Consensus.Storage.ChainDB.API hiding (ChainDB (..),
                     Iterator (..), closeDB)
import           Ouroboros.Consensus.Storage.ChainDB.Serialisation

-- | Thin wrapper around the ImmutableDB (opaque type)
data ImmDB m blk = ImmDB {
      immDB       :: !(ImmutableDB (HeaderHash blk) m)
    , codecConfig :: !(CodecConfig blk)
    , chunkInfo   :: !ChunkInfo
    }
  deriving (Generic)

deriving instance NoUnexpectedThunks (CodecConfig blk)
               => NoUnexpectedThunks (ImmDB m blk)

-- | 'EncodeDisk' and 'DecodeDisk' constraints needed for the ImmDB.
class ( EncodeDisk blk blk
      , DecodeDisk blk (Lazy.ByteString -> blk)
      , DecodeDiskDep (NestedCtxt Header) blk
      , ReconstructNestedCtxt Header blk
      , HasBinaryBlockInfo blk
      ) => ImmDbSerialiseConstraints blk

-- | Short-hand for events traced by the ImmDB wrapper.
type TraceEvent blk =
  ImmDB.TraceEvent (ImmDB.ChunkFileError (HeaderHash blk)) (HeaderHash blk)

{-------------------------------------------------------------------------------
  Initialization
-------------------------------------------------------------------------------}

-- | Arguments to initialize the ImmutableDB
--
-- See also 'defaultArgs'.
data ImmDbArgs m blk = ImmDbArgs {
      immCodecConfig    :: CodecConfig blk
    , immChunkInfo      :: ChunkInfo
    , immValidation     :: ImmDB.ValidationPolicy
    , immCheckIntegrity :: blk -> Bool
    , immHasFS          :: SomeHasFS m
    , immTracer         :: Tracer m (TraceEvent blk)
    , immCacheConfig    :: Index.CacheConfig
    , immRegistry       :: ResourceRegistry m
    }

-- | Default arguments when using the 'IO' monad
--
-- The following fields must still be defined:
--
-- * 'immCodecConfig'
-- * 'immChunkInfo'
-- * 'immValidation'
-- * 'immCheckIntegrity'
-- * 'immRegistry'
defaultArgs :: FilePath -> ImmDbArgs IO blk
defaultArgs fp = ImmDbArgs{
      immHasFS              = SomeHasFS $ ioHasFS $ MountPoint (fp </> "immutable")
    , immCacheConfig        = cacheConfig
    , immTracer             = nullTracer
      -- Fields without a default
    , immCodecConfig        = error "no default for immCodecConfig"
    , immChunkInfo          = error "no default for immChunkInfo"
    , immValidation         = error "no default for immValidation"
    , immCheckIntegrity     = error "no default for immCheckIntegrity"
    , immRegistry           = error "no default for immRegistry"
    }
  where
    -- Cache 250 past chunks by default. This will take roughly 250 MB of RAM.
    -- At the time of writing (1/2020), there are 166 epochs, and we store one
    -- epoch per chunk, so even one year from now, we will be able to cache all
    -- chunks' indices in the chain.
    --
    -- If this number were too low, i.e., less than the number of chunks that
    -- that clients are requesting blocks from, we would constantly evict and
    -- reparse indices, causing a much higher CPU load.
    cacheConfig = Index.CacheConfig
      { pastChunksToCache = 250
      , expireUnusedAfter = 5 * 60 -- Expire after 1 minute
      }

-- | Create a 'HashInfo' based on a 'ConvertRawHash' instance.
hashInfo
  :: forall blk. ConvertRawHash blk
  => Proxy blk -> HashInfo (HeaderHash blk)
hashInfo p = HashInfo { hashSize, getHash, putHash }
  where
    hashSize :: Word32
    hashSize = Block.hashSize p

    getHash :: Get (HeaderHash blk)
    getHash = do
      bytes <- Get.getByteString (fromIntegral hashSize)
      return $! fromRawHash p bytes

    putHash :: HeaderHash blk -> Put
    putHash = Put.putShortByteString . toShortRawHash p

withImmDB
  :: ( IOLike m
     , GetPrevHash blk
     , ConvertRawHash blk
     , ImmDbSerialiseConstraints blk
     )
  => ImmDbArgs m blk -> (ImmDB m blk -> m a) -> m a
withImmDB args = bracket (openDB args) closeDB

-- | For testing purposes
openDB
  :: forall m blk.
     ( IOLike m
     , GetPrevHash blk
     , ConvertRawHash blk
     , ImmDbSerialiseConstraints blk
     )
  => ImmDbArgs m blk
  -> m (ImmDB m blk)
openDB ImmDbArgs { immHasFS = SomeHasFS hasFS, ..} = do
    createDirectoryIfMissing hasFS True (mkFsPath [])
    (immDB, _internal) <- ImmDB.openDBInternal args
    return ImmDB
      { immDB              = immDB
      , codecConfig        = immCodecConfig
      , chunkInfo          = immChunkInfo
      }
  where
    args = ImmDB.ImmutableDbArgs
      { registry    = immRegistry
      , hasFS       = hasFS
      , chunkInfo   = immChunkInfo
      , hashInfo    = hashInfo (Proxy @blk)
      , tracer      = immTracer
      , cacheConfig = immCacheConfig
      , valPol      = immValidation
      , parser      = parser
      , prefixLen   = reconstructPrefixLen (Proxy @(Header blk))
      }
    parser = ImmDB.chunkFileParser
               hasFS
               (decodeDisk immCodecConfig)
               immCheckIntegrity

-- | For testing purposes
mkImmDB :: ImmutableDB (HeaderHash blk) m
        -> CodecConfig blk
        -> ChunkInfo
        -> ImmDB m blk
mkImmDB immDB codecConfig chunkInfo = ImmDB {..}

{-------------------------------------------------------------------------------
  Getting and parsing blocks
-------------------------------------------------------------------------------}

-- | Translate a 'BlockComponent' from ChainDB to ImmutableDB
translateBlockComponent ::
     forall m blk b.
     (HasHeader blk, ImmDbSerialiseConstraints blk, MonadThrow m)
  => ImmDB m blk
  -> BlockComponent (ChainDB m blk)                  b
  -> BlockComponent (ImmutableDB (HeaderHash blk) m) b
translateBlockComponent ImmDB { codecConfig = ccfg } = go
  where
    go ::
         forall b'. BlockComponent (ChainDB m blk) b'
      -> BlockComponent (ImmutableDB (HeaderHash blk) m) b'
    go = \case
      GetBlock      -> parseBlock ccfg <$> getBlockRef <*> GetRawBlock
      GetRawBlock   -> GetRawBlock
      GetHeader     -> parseHeader ccfg
                         <$> getBlockRef
                         <*> GetNestedCtxt
                         <*> GetBlockSize
                         <*> GetRawHeader
      GetRawHeader  -> GetRawHeader
      GetHash       -> GetHash
      GetSlot       -> GetSlot
      GetIsEBB      -> GetIsEBB
      GetBlockSize  -> GetBlockSize
      GetHeaderSize -> GetHeaderSize
      GetNestedCtxt -> reconstructNestedCtxt (Proxy @(Header blk))
                         <$> GetNestedCtxt
                         <*> GetBlockSize
      GetPure a     -> GetPure a
      GetApply f bc -> GetApply (go f) (go bc)

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
  -> RealPoint blk
  -> m Bool
hasBlock db (RealPoint slot hash) =
    withDB db $ \imm -> do
      immTip <- ImmDB.getTip imm

      let slotNoAtTip :: WithOrigin SlotNo
          slotNoAtTip = slotNoOfBlockOrEBB (chunkInfo db) . forgetTipInfo <$>
                          immTip

          ebbAtTip :: Maybe EpochNo
          ebbAtTip = case forgetTipInfo <$> immTip of
                       NotOrigin (ImmDB.EBB epochNo) -> Just epochNo
                       _otherwise                    -> Nothing

      case NotOrigin slot `compare` slotNoAtTip of
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
          -- If it's a slot that can contain an EBB, check if we have an EBB
          else case slotMightBeEBB (chunkInfo db) slot of
            Nothing      -> return False
            Just epochNo -> (== Just hash) <$>
                              ImmDB.getEBBComponent imm GetHash epochNo

getTipInfo :: forall m blk.
              (MonadCatch m, HasCallStack)
           => ImmDB m blk
           -> m (WithOrigin (SlotNo, HeaderHash blk, IsEBB, BlockNo))
getTipInfo db = withDB db $ \imm -> fmap conv <$> ImmDB.getTip imm
  where
    conv :: TipInfo (HeaderHash blk) ImmDB.BlockOrEBB
         -> (SlotNo, HeaderHash blk, IsEBB, BlockNo)
    conv (TipInfo hash blockOrEBB tipBlockNo) = (
          slotNoOfBlockOrEBB (chunkInfo db) blockOrEBB
        , hash
        , ImmDB.isBlockOrEBB blockOrEBB
        , tipBlockNo
        )

getPointAtTip :: forall m blk.
                 (MonadCatch m, HasCallStack)
              => ImmDB m blk -> m (Point blk)
getPointAtTip db = getTipInfo db <&> \case
    Origin                       -> GenesisPoint
    NotOrigin (slot, hash, _, _) -> BlockPoint slot hash

getAnchorForTip :: (MonadCatch m, HasCallStack)
                => ImmDB m blk -> m (AF.Anchor blk)
getAnchorForTip db = getTipInfo db <&> \case
   Origin                           -> AF.AnchorGenesis
   NotOrigin (slot, hash, _, block) -> AF.Anchor slot hash block

getSlotNoAtTip :: MonadCatch m => ImmDB m blk -> m (WithOrigin SlotNo)
getSlotNoAtTip db = pointSlot <$> getPointAtTip db

getKnownBlockComponent
  :: (MonadCatch m, HasHeader blk, ImmDbSerialiseConstraints blk, HasCallStack)
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
  :: forall m blk b.
     (MonadCatch m, HasHeader blk, ImmDbSerialiseConstraints blk, HasCallStack)
  => ImmDB m blk
  -> BlockComponent (ChainDB m blk) b
  -> Either EpochNo SlotNo
  -> m (Maybe b)
getBlockComponent db blockComponent epochOrSlot = withDB db $ \imm ->
    case epochOrSlot of
      Left epoch -> ImmDB.getEBBComponent   imm blockComponent' epoch
      Right slot -> ImmDB.getBlockComponent imm blockComponent' slot
  where
    blockComponent' = translateBlockComponent db blockComponent

-- | Return the block component of the block corresponding to the given point,
-- if it is part of the ImmutableDB.
--
-- If we have a block at the slot of the point, but its hash differs, we
-- return 'Nothing'.
--
-- If the point corresponds to some slot in the future, a
-- 'ReadFutureSlotError' wrapped in a 'ImmDbFailure' is thrown.
getBlockComponentWithPoint
  :: forall m blk b.
     (MonadCatch m, HasHeader blk, ImmDbSerialiseConstraints blk, HasCallStack)
  => ImmDB m blk
  -> BlockComponent (ChainDB m blk) b
  -> RealPoint blk
  -> m (Maybe b)
getBlockComponentWithPoint db blockComponent (RealPoint slot hash) =
    withDB db $ \imm ->
      ImmDB.getBlockOrEBBComponent imm blockComponent' slot hash
  where
    blockComponent' = translateBlockComponent db blockComponent

{-------------------------------------------------------------------------------
  Appending a block
-------------------------------------------------------------------------------}

-- | Appends the given block (as a regular block or EBB) to the ImmutableDB.
--
-- Does not check whether the block is in the past or not.
appendBlock
  :: ( MonadCatch m
     , HasHeader blk
     , GetHeader blk
     , ImmDbSerialiseConstraints blk
     , HasCallStack
     )
  => ImmDB m blk -> blk -> m ()
appendBlock db@ImmDB{..} b = withDB db $ \imm -> case blockIsEBB b of
    Nothing      ->
      ImmDB.appendBlock imm slotNo  blockNr hash builder binaryBlockInfo
    Just epochNo ->
      ImmDB.appendEBB   imm epochNo blockNr hash builder binaryBlockInfo
  where
    hash            = blockHash b
    slotNo          = blockSlot b
    blockNr         = blockNo   b
    builder         = CBOR.toBuilder $ encodeDisk codecConfig b
    binaryBlockInfo = getBinaryBlockInfo b

{-------------------------------------------------------------------------------
  Streaming
-------------------------------------------------------------------------------}

-- | Wrapper around 'ImmDB.stream' that translates the requested
-- 'BlockComponent' into the 'ImmDB.BlockComponent' the ImmutableDB
-- understands.
openIterator
  :: forall m blk b.
     (IOLike m, HasHeader blk, ImmDbSerialiseConstraints blk)
  => ImmDB m blk
  -> ResourceRegistry m
  -> BlockComponent (ChainDB m blk) b
  -> Maybe (SlotNo, HeaderHash blk)
  -> Maybe (SlotNo, HeaderHash blk)
  -> m (Either (ImmDB.WrongBoundError (HeaderHash blk))
               (ImmDB.Iterator (HeaderHash blk) m b))
openIterator db registry blockComponent start end =
    withDB db $ \imm -> ImmDB.stream imm registry blockComponent' start end
  where
    blockComponent' = translateBlockComponent db blockComponent

-- | Stream headers/blocks from the given 'StreamFrom' to the given
-- 'StreamTo'.
--
-- When passed @'StreamFromInclusive' pt@ where @pt@ refers to Genesis, a
-- 'NoGenesisBlock' exception will be thrown.
stream
  :: forall m blk b.
     (IOLike m, HasHeader blk, ImmDbSerialiseConstraints blk)
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
      StreamToInclusive pt@(RealPoint slot hash) -> do
        when (NotOrigin slot > slotNoAtTip) $ throwError $ MissingBlock pt
        return $ Just (slot, hash)

    case from of
      StreamFromExclusive pt@(BlockPoint slot hash) -> do
        when (pointSlot pt > slotNoAtTip) $ throwError $ MissingBlock (RealPoint slot hash)
        it <- openStream (Just (slot, hash)) end
        -- Skip the first block, as the bound is exclusive
        void $ lift $ iteratorNext db it
        return it
      StreamFromExclusive    GenesisPoint ->
        openStream Nothing end
      StreamFromInclusive pt@(RealPoint slot hash) -> do
        when (NotOrigin slot > slotNoAtTip) $ throwError $ MissingBlock pt
        openStream (Just (slot, hash)) end
  where
    openStream
      :: Maybe (SlotNo, HeaderHash blk)
      -> Maybe (SlotNo, HeaderHash blk)
      -> ExceptT (UnknownRange blk)
                 m
                 (ImmDB.Iterator (HeaderHash blk) m b)
    openStream start end = ExceptT $
        fmap (first toUnknownRange) $
          openIterator db registry blockComponent start end
      where
        toUnknownRange :: ImmDB.WrongBoundError (HeaderHash blk) -> UnknownRange blk
        toUnknownRange e
          | Just (startSlot, startHash) <- start
          , wrongBoundErrorSlotNo e == startSlot
          = MissingBlock (RealPoint startSlot startHash)
          | Just (endSlot, endHash) <- end
          , wrongBoundErrorSlotNo e == endSlot
          = MissingBlock (RealPoint endSlot endHash)
          | otherwise
          = error "WrongBoundError for a different bound than we gave"

        wrongBoundErrorSlotNo :: ImmDB.WrongBoundError (HeaderHash blk) -> SlotNo
        wrongBoundErrorSlotNo = \case
          ImmDB.EmptySlotError slot     -> slot
          ImmDB.WrongHashError slot _ _ -> slot

-- | Stream headers/blocks after the given point
--
-- Returns 'ImmDB.WrongBoundError' if the lower bound is not part of the
-- ImmutableDB.
streamAfter
  :: forall m blk b.
     (IOLike m, HasHeader blk, ImmDbSerialiseConstraints blk, HasCallStack)
  => ImmDB m blk
  -> ResourceRegistry m
  -> BlockComponent (ChainDB m blk) b
  -> Point blk -- ^ Exclusive lower bound
  -> m (Either
          (ImmDB.WrongBoundError (HeaderHash blk))
          (ImmDB.Iterator (HeaderHash blk) m b))
streamAfter db registry blockComponent low =
    openIterator db registry blockComponent low' Nothing >>= \case
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
  :: forall m blk b.
     (IOLike m, HasHeader blk, ImmDbSerialiseConstraints blk, HasCallStack)
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
withDB ImmDB{..} k = catch (k immDB) rethrow
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

{-------------------------------------------------------------------------------
  Wrappers
-------------------------------------------------------------------------------}

closeDB :: (MonadCatch m, HasCallStack) => ImmDB m blk -> m ()
closeDB db = withDB db ImmDB.closeDB

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

iteratorClose :: (HasCallStack, MonadCatch m)
              => ImmDB m blk
              -> ImmDB.Iterator (HeaderHash blk) m a
              -> m ()
iteratorClose db it = withDB db $ const $ ImmDB.iteratorClose it

{-------------------------------------------------------------------------------
  Parsing
-------------------------------------------------------------------------------}

getBlockRef :: DBHeaderHash db ~ HeaderHash blk
            => BlockComponent db (BlockRef blk)
getBlockRef = mkBlockRef <$> GetSlot <*> GetHash <*> GetIsEBB
  where
    mkBlockRef :: SlotNo -> HeaderHash blk -> IsEBB -> BlockRef blk
    mkBlockRef slot hash = BlockRef (BlockPoint slot hash)

parseBlock ::
     ( HasHeader blk
     , DecodeDisk blk (Lazy.ByteString -> blk)
     , MonadThrow m
     )
  => CodecConfig blk
  -> BlockRef blk
  -> Lazy.ByteString
  -> m blk
parseBlock ccfg blockRef bytes = throwParseErrors blockRef bytes $
      CBOR.deserialiseFromBytes (decodeDisk ccfg) bytes

parseHeader ::
     forall m blk.
     ( HasHeader blk
     , ReconstructNestedCtxt Header blk
     , DecodeDiskDep (NestedCtxt Header) blk
     , MonadThrow m
     )
  => CodecConfig blk
  -> BlockRef blk
  -> ShortByteString
  -> SizeInBytes
  -> Lazy.ByteString
  -> m (Header blk)
parseHeader ccfg blockRef prefix blockSize bytes =
    case reconstructNestedCtxt (Proxy @(Header blk)) prefix blockSize of
      SomeBlock ctxt ->
        throwParseErrors blockRef bytes $
          CBOR.deserialiseFromBytes (parser ctxt) bytes
  where
    parser :: NestedCtxt Header blk a -> Decoder s (Lazy.ByteString -> Header blk)
    parser ctxt = (\f -> nest . DepPair ctxt . f) <$> decodeDiskDep ccfg ctxt

throwParseErrors ::
     (HasHeader blk, MonadThrow m)
  => BlockRef blk
  -> Lazy.ByteString
  -> Either CBOR.DeserialiseFailure (Lazy.ByteString, Lazy.ByteString -> b)
  -> m b
throwParseErrors blockRef fullBytes = \case
    Right (trailing, f)
      | Lazy.null trailing -> return $ f fullBytes
      | otherwise          -> throwM $ ChainDbTrailingData blockRef trailing
    Left err               -> throwM $ ChainDbParseFailure blockRef err
