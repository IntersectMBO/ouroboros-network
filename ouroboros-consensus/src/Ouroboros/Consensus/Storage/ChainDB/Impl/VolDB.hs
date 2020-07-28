{-# LANGUAGE BangPatterns         #-}
{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE NamedFieldPuns       #-}
{-# LANGUAGE PatternSynonyms      #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Thin wrapper around the VolatileDB
module Ouroboros.Consensus.Storage.ChainDB.Impl.VolDB (
    VolDB -- Opaque
  , VolDbSerialiseConstraints
    -- * Initialization
  , VolDbArgs(..)
  , defaultArgs
  , openDB
    -- * Getting and parsing blocks
  , BlockFileParserError
  , getKnownBlock
  , getKnownHeader
  , getKnownBlockComponent
  , getBlockComponent
    -- * Wrappers
  , getBlockInfo
  , getIsMember
  , getPredecessor
  , filterByPredecessor
  , getMaxSlotNo
  , putBlock
  , closeDB
  , garbageCollect
    -- * Tracing
  , TraceEvent
    -- * Re-exports
  , VolatileDBError
  , BlockValidationPolicy
  , BlocksPerFile
    -- * Exported for testing purposes
  , mkVolDB
  , blockFileParser'
  , fromChainHash
  ) where

import           Codec.CBOR.Decoding (Decoder)
import qualified Codec.CBOR.Write as CBOR
import           Control.Monad (join)
import           Control.Tracer (Tracer, nullTracer)
import           Data.Bifunctor (first)
import qualified Data.ByteString.Lazy as Lazy
import           Data.ByteString.Short (ShortByteString)
import           Data.Maybe (isJust)
import           Data.Set (Set)
import           Data.Typeable (Typeable)
import           Data.Word (Word64)
import           GHC.Generics (Generic)
import           GHC.Stack (HasCallStack)
import           Streaming.Prelude (Of (..), Stream)
import qualified Streaming.Prelude as S
import           System.FilePath ((</>))

import           Ouroboros.Network.Block (MaxSlotNo)

import           Ouroboros.Consensus.Block
import qualified Ouroboros.Consensus.Util.CBOR as Util.CBOR
import           Ouroboros.Consensus.Util.IOLike

import           Ouroboros.Consensus.Storage.Common
import           Ouroboros.Consensus.Storage.FS.API (HasFS, SomeHasFS (..),
                     createDirectoryIfMissing)
import           Ouroboros.Consensus.Storage.FS.API.Types (MountPoint (..),
                     mkFsPath)
import           Ouroboros.Consensus.Storage.FS.IO (ioHasFS)
import           Ouroboros.Consensus.Storage.VolatileDB
                     (BlockValidationPolicy (..), BlocksPerFile, VolatileDB,
                     VolatileDBError)
import qualified Ouroboros.Consensus.Storage.VolatileDB as VolDB

import           Ouroboros.Consensus.Storage.ChainDB.API (ChainDB)
import           Ouroboros.Consensus.Storage.ChainDB.API hiding (ChainDB (..),
                     closeDB, getMaxSlotNo)
import           Ouroboros.Consensus.Storage.ChainDB.Impl.BlockComponent
import           Ouroboros.Consensus.Storage.ChainDB.Impl.Paths
import           Ouroboros.Consensus.Storage.ChainDB.Serialisation

-- | Thin wrapper around the VolatileDB (opaque type)
--
-- The intention is that all interaction with the VolatileDB goes through this
-- module.
data VolDB m blk = VolDB {
      volDB       :: !(VolatileDB (HeaderHash blk) m)
    , codecConfig :: !(CodecConfig blk)
    }
  deriving (Generic)

deriving instance NoUnexpectedThunks (CodecConfig blk)
               => NoUnexpectedThunks (VolDB m blk)

-- | 'EncodeDisk' and 'DecodeDisk' constraints needed for the VolDB.
class ( EncodeDisk blk blk
      , DecodeDisk blk (Lazy.ByteString -> blk)
      , DecodeDiskDep (NestedCtxt Header) blk
      , ReconstructNestedCtxt Header blk
      , HasBinaryBlockInfo blk
      ) => VolDbSerialiseConstraints blk

-- | Short-hand for events traced by the VolDB wrapper.
type TraceEvent blk =
  VolDB.TraceEvent Util.CBOR.ReadIncrementalErr (HeaderHash blk)

{-------------------------------------------------------------------------------
  Initialization
-------------------------------------------------------------------------------}

data VolDbArgs m blk = VolDbArgs {
      volHasFS          :: SomeHasFS m
    , volCheckIntegrity :: blk -> Bool
    , volBlocksPerFile  :: BlocksPerFile
    , volCodecConfig    :: CodecConfig blk
    , volValidation     :: VolDB.BlockValidationPolicy
    , volTracer         :: Tracer m (TraceEvent blk)
    }

-- | Default arguments when using the 'IO' monad
--
-- The following fields must still be defined:
--
-- * 'volCheckIntegrity'
-- * 'volBlocksPerFile'
-- * 'volCodecConfig'
-- * 'volValidation'
defaultArgs :: FilePath -> VolDbArgs IO blk
defaultArgs fp = VolDbArgs {
      volHasFS              = SomeHasFS $ ioHasFS $ MountPoint (fp </> "volatile")
    , volTracer             = nullTracer
      -- Fields without a default
    , volCheckIntegrity     = error "no default for volCheckIntegrity"
    , volBlocksPerFile      = error "no default for volBlocksPerFile"
    , volCodecConfig        = error "no default for volCodecConfig"
    , volValidation         = error "no default for volValidation"
    }

openDB
  :: forall m blk.
     (IOLike m, GetPrevHash blk, VolDbSerialiseConstraints blk)
  => VolDbArgs m blk -> m (VolDB m blk)
openDB args@VolDbArgs{ volHasFS = SomeHasFS hasFS, ..} = do
    createDirectoryIfMissing hasFS True (mkFsPath [])
    volDB <- VolDB.openDB volatileDbArgs
    return VolDB
      { volDB              = volDB
      , codecConfig        = volCodecConfig
      }
  where
    volatileDbArgs = VolDB.VolatileDbArgs
      { hasFS            = hasFS
      , maxBlocksPerFile = volBlocksPerFile
      , tracer           = volTracer
      , parser           = blockFileParser args
      , prefixLen        = reconstructPrefixLen (Proxy @(Header blk))
      }

-- | For testing purposes
mkVolDB :: VolatileDB (HeaderHash blk) m
        -> CodecConfig blk
        -> VolDB m blk
mkVolDB volDB codecConfig = VolDB {..}

{-------------------------------------------------------------------------------
  Wrappers
-------------------------------------------------------------------------------}

getBlockInfo
  :: VolDB m blk
  -> STM m (LookupBlockInfo blk)
getBlockInfo db = withSTM db VolDB.getBlockInfo

getIsMember :: Functor (STM m) => VolDB m blk -> STM m (HeaderHash blk -> Bool)
getIsMember = fmap (isJust .) . getBlockInfo

getPredecessor :: Functor (STM m)
               => VolDB m blk
               -> STM m (HeaderHash blk -> Maybe (WithOrigin (HeaderHash blk)))
getPredecessor = fmap (fmap VolDB.bpreBid .) . getBlockInfo

filterByPredecessor :: VolDB m blk
                    -> STM m (WithOrigin (HeaderHash blk)
                    -> Set (HeaderHash blk))
filterByPredecessor db = withSTM db VolDB.filterByPredecessor

getMaxSlotNo :: VolDB m blk
             -> STM m MaxSlotNo
getMaxSlotNo db = withSTM db VolDB.getMaxSlotNo

putBlock
  :: (MonadCatch m, GetPrevHash blk, VolDbSerialiseConstraints blk)
  => VolDB m blk -> blk -> m ()
putBlock db@VolDB{..} b = withDB db $ \vol ->
    VolDB.putBlock vol (extractInfo b binaryBlockInfo) binaryBlob
  where
    binaryBlockInfo = getBinaryBlockInfo b
    binaryBlob      = CBOR.toBuilder $ encodeDisk codecConfig b

closeDB :: (MonadCatch m, HasCallStack) => VolDB m blk -> m ()
closeDB db = withDB db VolDB.closeDB

garbageCollect :: MonadCatch m => VolDB m blk -> SlotNo -> m ()
garbageCollect db slotNo = withDB db $ \vol ->
    VolDB.garbageCollect vol slotNo

{-------------------------------------------------------------------------------
  Getting and parsing blocks
-------------------------------------------------------------------------------}

-- | Translate a 'BlockComponent' from ChainDB to VolatileDB
translateBlockComponent
  :: (HasHeader blk, VolDbSerialiseConstraints blk, MonadThrow m)
  => VolDB m blk
  -> BlockComponent (ChainDB m blk)                  b
  -> BlockComponent (VolatileDB (HeaderHash blk) m) b
translateBlockComponent VolDB { codecConfig } = translateToRawDB codecConfig

getKnownBlock
  :: (MonadCatch m, HasHeader blk, VolDbSerialiseConstraints blk)
  => VolDB m blk
  -> HeaderHash blk
  -> m blk
getKnownBlock volDB = join . getKnownBlockComponent volDB GetBlock

getKnownHeader
  :: (MonadCatch m, HasHeader blk, VolDbSerialiseConstraints blk)
  => VolDB m blk
  -> HeaderHash blk
  -> m (Header blk)
getKnownHeader volDB = join . getKnownBlockComponent volDB GetHeader

getKnownBlockComponent
  :: (MonadCatch m, HasHeader blk, VolDbSerialiseConstraints blk)
  => VolDB m blk
  -> BlockComponent (ChainDB m blk) b
  -> HeaderHash blk
  -> m b
getKnownBlockComponent db blockComponent hash = do
    mBlock <- mustExist db hash <$>
      getBlockComponent db blockComponent hash
    case mBlock of
      Right b  -> return b
      Left err -> throwM err

getBlockComponent
  :: forall m blk b.
     (MonadCatch m, HasHeader blk, VolDbSerialiseConstraints blk)
  => VolDB m blk
  -> BlockComponent (ChainDB m blk) b
  -> HeaderHash blk
  -> m (Maybe b)
getBlockComponent db blockComponent hash = withDB db $ \vol ->
    VolDB.getBlockComponent vol blockComponent' hash
  where
    blockComponent' = translateBlockComponent db blockComponent

{-------------------------------------------------------------------------------
  Parsing
-------------------------------------------------------------------------------}

type BlockFileParserError hash =
    VolDB.ParserError hash Util.CBOR.ReadIncrementalErr

blockFileParser
  :: forall m blk.
     ( IOLike m
     , GetPrevHash blk
     , VolDbSerialiseConstraints blk
     )
  => VolDbArgs m blk
  -> VolDB.Parser
       Util.CBOR.ReadIncrementalErr
       m
       (HeaderHash blk)
blockFileParser VolDbArgs{ volHasFS = SomeHasFS hasFS, .. } =
    blockFileParser'
      hasFS
      decodeNestedCtxtAndBlock
      volCheckIntegrity
      volValidation
  where
    prefixLen :: PrefixLen
    prefixLen = reconstructPrefixLen (Proxy @(Header blk))

    -- The decoder for a block is given the ByteString corresponding to the
    -- decoded block by the blockFileParser'@. We take advantage of this to
    -- extract the nested context from that same bytestring.
    decodeNestedCtxtAndBlock
      :: forall s. Decoder s (Lazy.ByteString -> (ShortByteString, blk))
    decodeNestedCtxtAndBlock =
      (\f bytes -> (takePrefix prefixLen bytes, f bytes)) <$>
        decodeDisk volCodecConfig

-- | A version which is easier to use for tests, since it does not require
-- the whole @VolDbArgs@.
blockFileParser'
  :: forall m blk h. (IOLike m, GetPrevHash blk, HasBinaryBlockInfo blk)
  => HasFS m h
  -> (forall s. Decoder s (Lazy.ByteString -> (ShortByteString, blk)))
  -> (blk -> Bool)
  -> VolDB.BlockValidationPolicy
  -> VolDB.Parser
       Util.CBOR.ReadIncrementalErr
       m
       (HeaderHash blk)
blockFileParser' hasFS
                 decodeNestedCtxtAndBlock
                 isNotCorrupt
                 validationPolicy =
    VolDB.Parser $ \fsPath -> Util.CBOR.withStreamIncrementalOffsets
      hasFS decodeNestedCtxtAndBlock fsPath (checkEntries [])
  where
    extractInfo' :: blk -> VolDB.BlockInfo (HeaderHash blk)
    extractInfo' blk = extractInfo blk (getBinaryBlockInfo blk)

    noValidation :: Bool
    noValidation = validationPolicy == VolDB.NoValidation

    checkEntries :: VolDB.ParsedInfo (HeaderHash blk)
                 -> Stream (Of (Word64, (Word64, (ShortByteString, blk))))
                    m
                    (Maybe (Util.CBOR.ReadIncrementalErr, Word64))
                 -> m ( VolDB.ParsedInfo (HeaderHash blk)
                      , Maybe (BlockFileParserError (HeaderHash blk), VolDB.BlockOffset)
                      )
    checkEntries parsed stream = S.next stream >>= \case
      Left mbErr
        -> return (reverse parsed, first VolDB.BlockReadErr <$> mbErr)
      Right ((offset, (size, (nestedCtxt, blk))), stream')
        | noValidation || isNotCorrupt blk
        -> let !blockInfo = extractInfo' blk
               !newParsed = VolDB.ParsedBlockInfo  {
                   pbiBlockOffset = offset
                 , pbiBlockSize   = VolDB.BlockSize size
                 , pbiBlockInfo   = blockInfo
                 , pbiNestedCtxt  = nestedCtxt
                 }
           in checkEntries (newParsed : parsed) stream'
        | otherwise  -- The block was invalid
        -> let !bid = VolDB.bbid $ extractInfo' blk
           in return (reverse parsed, Just (VolDB.BlockCorruptedErr bid, offset))

{-------------------------------------------------------------------------------
  Error handling
-------------------------------------------------------------------------------}

-- | Wrap calls to the VolatileDB and rethrow exceptions that may indicate
-- disk failure and should therefore trigger recovery
withDB :: forall m blk x. MonadCatch m
       => VolDB m blk
       -> (VolatileDB (HeaderHash blk) m -> m x)
       -> m x
withDB VolDB{..} k = catch (k volDB) rethrow
  where
    rethrow :: VolatileDBError -> m x
    rethrow e = case wrap e of
                  Just e' -> throwM e'
                  Nothing -> throwM e

    wrap :: VolatileDBError -> Maybe ChainDbFailure
    wrap (VolDB.UnexpectedError e) = Just (VolDbFailure e)
    wrap VolDB.UserError{}         = Nothing

-- | STM actions, by definition, cannot access the disk and therefore we don't
-- have to worry about catching exceptions here: any exceptions that may be
-- thrown indicate bugs, either in the ChainDB or in the VolatileDB
withSTM :: VolDB m blk
        -> (VolatileDB (HeaderHash blk) m -> STM m x)
        -> STM m x
withSTM VolDB{..} k = k volDB

mustExist :: forall proxy blk b. (StandardHash blk, Typeable blk)
          => proxy blk
          -> HeaderHash blk
          -> Maybe b
          -> Either ChainDbFailure b
mustExist _ hash Nothing  = Left  $ VolDbMissingBlock (Proxy @blk) hash
mustExist _ _    (Just b) = Right $ b

{-------------------------------------------------------------------------------
  Auxiliary
-------------------------------------------------------------------------------}

extractInfo :: GetPrevHash blk
            => blk
            -> BinaryBlockInfo
            -> VolDB.BlockInfo (HeaderHash blk)
extractInfo b BinaryBlockInfo{..} = VolDB.BlockInfo {
      bbid          = blockHash b
    , bslot         = blockSlot b
    , bbno          = blockNo   b
    , bpreBid       = fromChainHash (blockPrevHash b)
    , bisEBB        = blockToIsEBB b
    , bheaderOffset = headerOffset
    , bheaderSize   = headerSize
    }
