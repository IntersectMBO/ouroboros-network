{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TypeApplications          #-}

-- | Thin wrapper around the immutable DB
module Ouroboros.Storage.ChainDB.ImmDB (
    ImmDB(..)
    -- * Initialization
  , ImmDbArgs(..)
  , defaultArgs
  , openDB
    -- * Getting and parsing blocks
  , getBlockWithPoint
  , getBlockAtTip
  , getSlotNoAtTip
  , getKnownBlock
  , getBlock
  , getBlob
    -- * Streaming
  , streamBlocks
  , streamBlobs
    -- * Re-exports
  , Iterator(..)
  , IteratorResult(..)
  , ImmDB.ValidationPolicy(..)
  , ImmDB.ImmutableDBError
  ) where

import           Codec.CBOR.Decoding (Decoder)
import           Codec.CBOR.Encoding (Encoding)
import qualified Codec.CBOR.Read as CBOR
import           Control.Monad
import           Data.Bifunctor (first)
import qualified Data.ByteString.Lazy as Lazy
import           Data.Proxy
import           Data.Word
import           System.FilePath ((</>))

import           Control.Monad.Class.MonadST
import           Control.Monad.Class.MonadSTM
import           Control.Monad.Class.MonadThrow

import           Ouroboros.Network.Block (ChainHash (..), HasHeader (..),
                     HeaderHash, Point (..), SlotNo)
import           Ouroboros.Network.Chain (genesisSlotNo)

import qualified Ouroboros.Consensus.Util.CBOR as Util.CBOR

import           Ouroboros.Storage.ChainDB.API (ChainDbError (..),
                     ChainDbFailure (..), StreamFrom (..))
import           Ouroboros.Storage.Common
import           Ouroboros.Storage.EpochInfo (EpochInfo (..), newEpochInfo)
import           Ouroboros.Storage.FS.API (HasFS)
import           Ouroboros.Storage.FS.API.Types (MountPoint (..))
import           Ouroboros.Storage.FS.IO (ioHasFS)
import           Ouroboros.Storage.ImmutableDB (ImmutableDB, Iterator (..),
                     IteratorResult (..))
import qualified Ouroboros.Storage.ImmutableDB as ImmDB
import           Ouroboros.Storage.Util.ErrorHandling (ErrorHandling)
import qualified Ouroboros.Storage.Util.ErrorHandling as EH

-- | Thin wrapper around the immutable DB (opaque type)
data ImmDB m blk = ImmDB {
      immDB        :: ImmutableDB (HeaderHash blk) m
    , decBlock     :: forall s. Decoder s blk
    , immEpochInfo :: EpochInfo m
    }

{-------------------------------------------------------------------------------
  Initialization
-------------------------------------------------------------------------------}

-- | Arguments to initialize the immutable DB
--
-- See also 'defaultArgs'.
data ImmDbArgs m blk = forall h. ImmDbArgs {
      immDecodeHash  :: forall s. Decoder s (HeaderHash blk)
    , immDecodeBlock :: forall s. Decoder s blk
    , immEncodeHash  :: HeaderHash blk -> Encoding
    , immErr         :: ErrorHandling ImmDB.ImmutableDBError m
    , immEpochSize   :: EpochNo -> m EpochSize
    , immValidation  :: ImmDB.ValidationPolicy
    , immIsEBB       :: blk -> Maybe (HeaderHash blk)
    , immHasFS       :: HasFS m h
    }

-- | Default arguments when using the 'IO' monad
--
-- The following fields must still be defined:
--
-- * `decodeHash`
-- * `decodeBlock`
-- * `encodeHash`
-- * `epochSize`
-- * `validation`
-- * `isEBB`
defaultArgs :: FilePath -> ImmDbArgs IO blk
defaultArgs fp = ImmDbArgs{
      immErr   = EH.exceptions
    , immHasFS = ioHasFS $ MountPoint (fp </> "immutable")
      -- Fields without a default
    , immDecodeHash  = error "no default for immDecodeHash"
    , immDecodeBlock = error "no default for immDecodeBlock"
    , immEncodeHash  = error "no default for immEncodeHash"
    , immEpochSize   = error "no default for immEpochSize"
    , immValidation  = error "no default for immValidation"
    , immIsEBB       = error "no default for immIsEBB"
    }

openDB :: (MonadSTM m, MonadST m, MonadCatch m, HasHeader blk)
       => ImmDbArgs m blk -> m (ImmDB m blk)
openDB args@ImmDbArgs{..} = do
    immEpochInfo <- newEpochInfo immEpochSize
    immDB <- ImmDB.openDB
               immDecodeHash
               immEncodeHash
               immHasFS
               immErr
               immEpochInfo
               immValidation
               (epochFileParser args)
    return $ ImmDB immDB immDecodeBlock immEpochInfo

{-------------------------------------------------------------------------------
  Getting and parsing blocks
-------------------------------------------------------------------------------}

getBlockWithPoint :: (MonadCatch m, HasHeader blk)
                  => ImmDB m blk -> Point blk -> m (Maybe blk)
getBlockWithPoint db Point{..} = do
    -- Unfortunately a point does not give us enough information to determine
    -- whether this corresponds to a regular block or an EBB. We will
    -- optimistically assume it refers to a regular block, and only when that
    -- fails try to read the EBB instead. This means that
    --
    -- * If it is indeed a regular block performance is optimal
    -- * If it is an EBB we would needlessly have read a regular block from
    --   disk first, but EBBs are rare (1:20000 before the hard fork, and
    --   non-existent after)
    -- * If the block does not exist in the database at all, we'd have done two
    --   reads before returning 'Nothing', but both of those reads are cheap
    mbImmBlock <- getBlock db (Right pointSlot)
    case mbImmBlock of
      Just block -> return $ Just block
      Nothing    -> do
        epochNo <- epochInfoEpoch pointSlot
        getBlock db (Left epochNo)
  where
    EpochInfo{..} = immEpochInfo db

getBlockAtTip :: (MonadCatch m, HasHeader blk)
              => ImmDB m blk -> m (Maybe blk)
getBlockAtTip db = do
    immTip <- withDB db $ \imm -> ImmDB.getTip imm
    case immTip of
      TipGen             -> return Nothing
      Tip (Left epochNo) -> Just <$> getKnownBlock db (Left epochNo)
      Tip (Right slotNo) -> Just <$> getKnownBlock db (Right slotNo)

getSlotNoAtTip :: (MonadCatch m, HasHeader blk)
               => ImmDB m blk -> m SlotNo
getSlotNoAtTip db = do
    immTip <- withDB db $ \imm -> ImmDB.getTip imm
    case immTip of
      TipGen             -> return genesisSlotNo
      Tip (Left epochNo) -> epochInfoFirst epochNo
      Tip (Right slotNo) -> return slotNo
  where
    EpochInfo{..} = immEpochInfo db

-- | Get known block from the immutable DB that we know should exist
--
-- If it does not exist, we are dealing with data corruption.
getKnownBlock :: (MonadCatch m, HasHeader blk)
              => ImmDB m blk -> Either EpochNo SlotNo -> m blk
getKnownBlock db epochOrSlot = do
    mBlock <- mustExist epochOrSlot <$> getBlock db epochOrSlot
    case mBlock of
      Right b  -> return b
      Left err -> throwM err

-- | Get block from the immutable DB
getBlock :: (MonadCatch m, HasHeader blk)
         => ImmDB m blk -> Either EpochNo SlotNo -> m (Maybe blk)
getBlock db epochOrSlot = do
    mBlob <- fmap (parse (decBlock db) epochOrSlot) <$> getBlob db epochOrSlot
    case mBlob of
      Nothing         -> return $ Nothing
      Just (Right b)  -> return $ Just b
      Just (Left err) -> throwM $ err

getBlob :: (MonadCatch m, HasHeader blk)
        => ImmDB m blk -> Either EpochNo SlotNo -> m (Maybe Lazy.ByteString)
getBlob db epochOrSlot = withDB db $ \imm ->
    case epochOrSlot of
      Left epochNo -> fmap snd <$> ImmDB.getEBB imm epochNo
      Right slotNo -> ImmDB.getBinaryBlob imm slotNo

{-------------------------------------------------------------------------------
  Streaming
-------------------------------------------------------------------------------}

-- | Stream blocks
--
-- See also 'streamBlobs'.
streamBlocks :: forall m blk. (MonadCatch m, HasHeader blk)
             => ImmDB m blk
             -> StreamFrom blk
             -> m (Iterator (HeaderHash blk) m blk)
streamBlocks db low = wrap <$> streamBlobs db low
  where
    wrap :: Iterator (HeaderHash blk) m Lazy.ByteString
         -> Iterator (HeaderHash blk) m blk
    wrap itr = Iterator {
          iteratorNext    = parseIteratorResult db =<< iteratorNext itr
        , iteratorPeek    = parseIteratorResult db =<< iteratorPeek itr
        , iteratorHasNext = iteratorHasNext itr
        , iteratorClose   = iteratorClose   itr
        , iteratorID      = ImmDB.DerivedIteratorID $ iteratorID itr
        }

-- | Stream blobs
streamBlobs :: forall m blk. (MonadCatch m, HasHeader blk)
            => ImmDB m blk
            -> StreamFrom blk
            -> m (Iterator (HeaderHash blk) m Lazy.ByteString)
streamBlobs db low = case low of
    StreamFromExclusive pt -> streamBlobsAfter db pt
    StreamFromGenesis      -> withDB db $ \imm ->
      ImmDB.streamBinaryBlobs imm Nothing Nothing
    StreamFromInclusive pt -> case pointHash pt of
      GenesisHash -> throwM $ NoGenesisBlock @blk
      BlockHash h -> withDB db $ \imm ->
        ImmDB.streamBinaryBlobs imm (Just (pointSlot pt, h)) Nothing

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
streamBlobsAfter :: forall m blk. (MonadCatch m, HasHeader blk)
                 => ImmDB m blk
                 -> Point blk -- ^ Exclusive lower bound
                 -> m (Iterator (HeaderHash blk) m Lazy.ByteString)
streamBlobsAfter db low = withDB db $ \imm -> do
    itr   <- ImmDB.streamBinaryBlobs imm low' Nothing
    skipAndCheck itr
    return itr
  where
    low' :: Maybe (SlotNo, HeaderHash blk)
    low' = case pointHash low of
             GenesisHash -> Nothing
             BlockHash h -> Just (pointSlot low, h)

    -- Skip the first block (if any) to provide an /exclusive/ lower bound
    --
    -- Take advantage of the opportunity to also verify the hash (which the
    -- immutable DB can't, since it cannot compute hashes)
    skipAndCheck :: Iterator (HeaderHash blk) m Lazy.ByteString -> m ()
    skipAndCheck itr =
        case low' of
          Nothing           -> return ()
          Just (slot, hash) -> do
            skipped <- parseIteratorResult db =<< iteratorNext itr
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

-- | Wrap calls to the immutable DB and rethrow exceptions that may indicate
-- disk failure and should therefore trigger recovery
withDB :: forall m blk x. (MonadCatch m, HasHeader blk)
       => ImmDB m blk -> (ImmutableDB (HeaderHash blk) m -> m x) -> m x
withDB ImmDB{..} k = catch (k immDB) rethrow
  where
    rethrow :: ImmDB.ImmutableDBError -> m x
    rethrow err = case wrap err of
                    Just err' -> throwM err'
                    Nothing   -> throwM err

    wrap :: ImmDB.ImmutableDBError -> Maybe (ChainDbFailure blk)
    wrap (ImmDB.UnexpectedError err) = Just (ImmDbFailure err)
    wrap ImmDB.UserError{}           = Nothing

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
