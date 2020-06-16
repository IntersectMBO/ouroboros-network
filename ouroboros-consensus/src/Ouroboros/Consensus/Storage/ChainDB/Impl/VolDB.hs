{-# LANGUAGE BangPatterns              #-}
{-# LANGUAGE DeriveAnyClass            #-}
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE NamedFieldPuns            #-}
{-# LANGUAGE PatternSynonyms           #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE StandaloneDeriving        #-}
{-# LANGUAGE TypeApplications          #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE TypeOperators             #-}
{-# LANGUAGE UndecidableInstances      #-}

-- | Thin wrapper around the VolatileDB
module Ouroboros.Consensus.Storage.ChainDB.Impl.VolDB (
    VolDB -- Opaque
  , VolDbSerialiseConstraints
    -- * Initialization
  , VolDbArgs(..)
  , defaultArgs
  , openDB
    -- * Candidates
  , candidates
  , isReachable
  , isReachableSTM
    -- * Paths
  , Path(..)
  , computePath
  , computePathSTM
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
  ) where

import           Codec.CBOR.Decoding (Decoder)
import qualified Codec.CBOR.Write as CBOR
import           Control.Monad (join)
import           Control.Tracer (Tracer, nullTracer)
import           Data.Bifunctor (first)
import qualified Data.ByteString.Lazy as Lazy
import           Data.ByteString.Short (ShortByteString)
import           Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import           Data.Maybe (isJust, mapMaybe)
import           Data.Proxy (Proxy (..))
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Typeable (Typeable)
import           Data.Word (Word64)
import           GHC.Generics (Generic)
import           GHC.Stack (HasCallStack)
import           Streaming.Prelude (Of (..), Stream)
import qualified Streaming.Prelude as S
import           System.FilePath ((</>))

import           Ouroboros.Network.Block (pattern BlockPoint, ChainHash (..),
                     pattern GenesisPoint, HasHeader (..), HeaderHash,
                     MaxSlotNo, Point, SlotNo, StandardHash, pointHash)
import qualified Ouroboros.Network.Block as Block
import           Ouroboros.Network.Point (WithOrigin (..))

import           Ouroboros.Consensus.Block
import qualified Ouroboros.Consensus.Util.CBOR as Util.CBOR
import           Ouroboros.Consensus.Util.IOLike

import           Ouroboros.Consensus.Storage.Common
import           Ouroboros.Consensus.Storage.FS.API (HasFS,
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
import           Ouroboros.Consensus.Storage.ChainDB.Serialisation

-- | Thin wrapper around the VolatileDB (opaque type)
--
-- The intention is that all interaction with the VolatileDB goes through this
-- module.
data VolDB m blk = VolDB {
      volDB              :: !(VolatileDB (HeaderHash blk) m)
    , getBinaryBlockInfo :: !(blk -> BinaryBlockInfo)
    , codecConfig        :: !(CodecConfig blk)
    }
  deriving (Generic)

deriving instance HasCodecConfig blk => NoUnexpectedThunks (VolDB m blk)
  -- use generic instance

-- | 'EncodeDisk' and 'DecodeDisk' constraints needed for the VolDB.
class ( EncodeDisk blk blk
      , DecodeDisk blk (Lazy.ByteString -> blk)
      , DecodeDiskDep (NestedCtxt Header) blk
      , ReconstructNestedCtxt Header blk
      ) => VolDbSerialiseConstraints blk

-- | Short-hand for events traced by the VolDB wrapper.
type TraceEvent blk =
  VolDB.TraceEvent Util.CBOR.ReadIncrementalErr (HeaderHash blk)

{-------------------------------------------------------------------------------
  Initialization
-------------------------------------------------------------------------------}

data VolDbArgs m blk = forall h. Eq h => VolDbArgs {
      volHasFS              :: HasFS m h
    , volCheckIntegrity     :: blk -> Bool
    , volBlocksPerFile      :: BlocksPerFile
    , volGetBinaryBlockInfo :: blk -> BinaryBlockInfo
    , volCodecConfig        :: CodecConfig blk
    , volValidation         :: VolDB.BlockValidationPolicy
    , volTracer             :: Tracer m (TraceEvent blk)
    }

-- | Default arguments when using the 'IO' monad
--
-- The following fields must still be defined:
--
-- * 'volCheckIntegrity'
-- * 'volBlocksPerFile'
-- * 'volGetBinaryBlockInfo'
-- * 'volCodecConfig'
-- * 'volValidation'
defaultArgs :: FilePath -> VolDbArgs IO blk
defaultArgs fp = VolDbArgs {
      volHasFS              = ioHasFS $ MountPoint (fp </> "volatile")
    , volTracer             = nullTracer
      -- Fields without a default
    , volCheckIntegrity     = error "no default for volCheckIntegrity"
    , volBlocksPerFile      = error "no default for volBlocksPerFile"
    , volGetBinaryBlockInfo = error "no default for volGetBinaryBlockInfo"
    , volCodecConfig        = error "no default for volCodecConfig"
    , volValidation         = error "no default for volValidation"
    }

openDB
  :: forall m blk.
     (IOLike m, HasHeader blk, GetHeader blk, VolDbSerialiseConstraints blk)
  => VolDbArgs m blk -> m (VolDB m blk)
openDB args@VolDbArgs{..} = do
    createDirectoryIfMissing volHasFS True (mkFsPath [])
    volDB <- VolDB.openDB volatileDbArgs
    return VolDB
      { volDB              = volDB
      , getBinaryBlockInfo = volGetBinaryBlockInfo
      , codecConfig        = volCodecConfig
      }
  where
    volatileDbArgs = VolDB.VolatileDbArgs
      { hasFS            = volHasFS
      , maxBlocksPerFile = volBlocksPerFile
      , tracer           = volTracer
      , parser           = blockFileParser args
      , prefixLen        = reconstructPrefixLen (Proxy @(Header blk))
      }

-- | For testing purposes
mkVolDB :: VolatileDB (HeaderHash blk) m
        -> (blk -> BinaryBlockInfo)
        -> CodecConfig blk
        -> VolDB m blk
mkVolDB volDB getBinaryBlockInfo codecConfig = VolDB {..}

{-------------------------------------------------------------------------------
  Wrappers
-------------------------------------------------------------------------------}

getBlockInfo
  :: VolDB m blk
  -> STM m (HeaderHash blk -> Maybe (VolDB.BlockInfo (HeaderHash blk)))
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
  :: (MonadCatch m, HasHeader blk, GetHeader blk, VolDbSerialiseConstraints blk)
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
  Compute candidates
-------------------------------------------------------------------------------}

-- | Compute all candidates starting at the specified hash
--
-- PRECONDITION: the block to which the given point corresponds is part of the
-- VolatileDB.
--
-- The first element in each list of hashes is the hash /after/ the specified
-- hash. Thus, when building fragments from these lists of hashes, they
-- fragments must be /anchored/ at the specified hash, but not contain it.
--
-- NOTE: it is possible that no candidates are found, but don't forget that
-- the chain (fragment) ending with @B@ is also a potential candidate.
candidates
  :: forall blk.
     (WithOrigin (HeaderHash blk) -> Set (HeaderHash blk))
     -- ^ @filterByPredecessor@
  -> Point blk -- ^ @B@
  -> [NonEmpty (HeaderHash blk)]
     -- ^ Each element in the list is a list of hashes from which we can
     -- construct a fragment anchored at the point @B@.
candidates succsOf b = mapMaybe NE.nonEmpty $ go (fromChainHash (pointHash b))
  where
    go :: WithOrigin (HeaderHash blk) -> [[HeaderHash blk]]
    go mbHash = case Set.toList $ succsOf mbHash of
      []    -> [[]]
      succs -> [ next : candidate
               | next <- succs
               , candidate <- go (At next)
               ]

-- | Variant of 'isReachable' that obtains its arguments in the same 'STM'
-- transaction.
isReachableSTM :: (IOLike m, HasHeader blk)
               => VolDB m blk
               -> STM m (Point blk) -- ^ The tip of the ImmutableDB (@I@).
               -> RealPoint blk     -- ^ The point of the block (@B@) to check
                                    -- the reachability of
               -> STM m (Maybe (NonEmpty (HeaderHash blk)))
isReachableSTM volDB getI b = do
    (predecessor, i) <- (,) <$> getPredecessor volDB <*> getI
    return $ isReachable predecessor i b

-- | Check whether the given point, corresponding to a block @B@, is reachable
-- from the tip of the ImmutableDB (@I@) by chasing the predecessors.
--
-- Returns a path (of hashes) from @I@ (excluded) to @B@ (included).
--
-- PRECONDITION: @B ∈ V@, i.e. @B@ is part of the VolatileDB.
--
-- 'True' <=> for all transitive predecessors @B'@ of @B@ we have @B' ∈ V@ or
-- @B' = I@.
isReachable :: forall blk. (HasHeader blk, HasCallStack)
            => (HeaderHash blk -> Maybe (WithOrigin (HeaderHash blk)))
            -> Point blk                                        -- ^ @I@
            -> RealPoint blk                                    -- ^ @B@
            -> Maybe (NonEmpty (HeaderHash blk))
isReachable predecessor i b =
    case computePath predecessor from to of
      -- Bounds are not on the same fork
      Nothing   -> Nothing
      Just path -> case path of
        CompletelyInVolDB hashes -> case NE.nonEmpty hashes of
          Just hashes' -> Just hashes'
          Nothing      ->
            error "impossible: empty list of hashes with an inclusive bound"
        PartiallyInVolDB  {}     -> Nothing
        NotInVolDB        _      ->
          error "impossible: block just added to VolatileDB missing"
  where
    from = StreamFromExclusive i
    to   = StreamToInclusive   b

{-------------------------------------------------------------------------------
  Compute paths
-------------------------------------------------------------------------------}

-- | Construct a path backwards through the VolatileDB.
--
-- We walk backwards through the VolatileDB, constructing a 'Path' from the
-- 'StreamTo' to the 'StreamFrom'.
--
-- If the range is invalid, 'Nothing' is returned.
--
-- See the documentation of 'Path'.
computePath
  :: forall blk. HasHeader blk
  => (HeaderHash blk -> Maybe (WithOrigin (HeaderHash blk)))
     -- Return the predecessor
  -> StreamFrom blk
  -> StreamTo   blk
  -> Maybe (Path blk)
computePath predecessor from to = case to of
    StreamToInclusive (RealPoint _ end)
      | Just prev <- predecessor end -> case from of
          -- Easier to handle this special case (@StreamFromInclusive start,
          -- StreamToInclusive end, start == end@) here:
          StreamFromInclusive (RealPoint _ start)
            | start == end -> return $ CompletelyInVolDB [end]
          _                -> go [end] prev
      | otherwise        -> return $ NotInVolDB end
    StreamToExclusive (RealPoint _ end)
      | Just prev <- predecessor end -> go [] prev
      | otherwise                    -> return $ NotInVolDB end
  where
    -- | It only needs the predecessor @prev@ and not the actual hash, because
    -- the hash is already added to @acc@ (if allowed by the bounds).
    go :: [HeaderHash blk] -> WithOrigin (HeaderHash blk) -> Maybe (Path blk)
    go acc prev = case prev of
      -- Found genesis
      Origin -> case from of
        StreamFromInclusive _
          -> Nothing
        StreamFromExclusive GenesisPoint
          -> return $ CompletelyInVolDB acc
        StreamFromExclusive (BlockPoint {})
          -> Nothing

      At predHash
        | Just prev' <- predecessor predHash -> case from of
          StreamFromInclusive pt
            | predHash == realPointHash pt
            -> return $ CompletelyInVolDB (predHash : acc)
          StreamFromExclusive pt
            | BlockHash predHash == Block.pointHash pt
            -> return $ CompletelyInVolDB acc
          -- Bound not yet reached, invariants both ok!
          _ -> go (predHash : acc) prev'
        -- Predecessor not in the VolatileDB
        | StreamFromExclusive pt <- from
        , BlockHash predHash == Block.pointHash pt
          -- That's fine if we don't need to include it in the path.
        -> return $ CompletelyInVolDB acc
        | otherwise
        -> return $ PartiallyInVolDB predHash acc

-- | Variant of 'computePath' that obtains its arguments in the same 'STM'
-- transaction. Throws an 'InvalidIteratorRange' exception when the range is
-- invalid (i.e., 'computePath' returned 'Nothing').
computePathSTM
  :: forall m blk. (IOLike m, HasHeader blk)
  => VolDB m blk
  -> StreamFrom blk
  -> StreamTo   blk
  -> STM m (Path blk)
computePathSTM volDB from to = do
    predecessor <- getPredecessor volDB
    case computePath predecessor from to of
      Just path -> return path
      Nothing   -> throwM $ InvalidIteratorRange from to

-- | A path through the VolatileDB from a 'StreamFrom' to a 'StreamTo'.
--
-- Invariant: the @ChainFragment@ (oldest first) constructed using the blocks
-- corresponding to the hashes in the path will be valid, i.e. the blocks will
-- fit onto each other.
data Path blk
  = NotInVolDB (HeaderHash blk)
    -- ^ The @end@ point (@'StreamToInclusive' end@ or @'StreamToExclusive'
    -- end@) was not part of the VolatileDB.
  | CompletelyInVolDB [HeaderHash blk]
    -- ^ A complete path, from start point to end point was constructed from
    -- the VolatileDB. The list contains the hashes from oldest to newest.
    --
    -- * If the lower bound was @'StreamFromInclusive' pt@, then @pt@ will be
    --   the first element of the list.
    -- * If the lower bound was @'StreamFromExclusive' pt@, then the first
    --   element of the list will correspond to the first block after @pt@.
    --
    -- * If the upper bound was @'StreamToInclusive' pt@, then @pt@ will be
    --   the last element of the list.
    -- * If the upper bound was @'StreamToExclusive' pt@, then the last
    --   element of the list will correspond to the first block before @pt@.
  | PartiallyInVolDB (HeaderHash blk) [HeaderHash blk]
    -- ^ Only a partial path could be constructed from the VolatileDB. The
    -- missing predecessor could still be in the ImmutableDB. The list
    -- contains the hashes from oldest to newest.
    --
    -- * The first element in the list is the point for which no predecessor
    --   is available in the VolatileDB. The block corresponding to the point
    --   itself, /is/ available in the VolatileDB.
    -- * The first argument is the hash of predecessor, the block that is not
    --   available in the VolatileDB.
    --
    -- Note: if the lower bound is exclusive, the block corresponding to it
    -- doesn't have to be part of the VolatileDB, it will result in a
    -- 'StartToEnd'.
    --
    -- The same invariants hold for the upper bound as for 'StartToEnd'.

deriving instance Show (HeaderHash blk) => Show (Path blk)

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
     , HasHeader blk
     , GetHeader blk
     , VolDbSerialiseConstraints blk
     )
  => VolDbArgs m blk
  -> VolDB.Parser
       Util.CBOR.ReadIncrementalErr
       m
       (HeaderHash blk)
blockFileParser VolDbArgs{..} =
    blockFileParser'
      volHasFS
      volGetBinaryBlockInfo
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
  :: forall m blk h. (IOLike m, HasHeader blk, GetHeader blk)
  => HasFS m h
  -> (blk -> BinaryBlockInfo)
  -> (forall s. Decoder s (Lazy.ByteString -> (ShortByteString, blk)))
  -> (blk -> Bool)
  -> VolDB.BlockValidationPolicy
  -> VolDB.Parser
       Util.CBOR.ReadIncrementalErr
       m
       (HeaderHash blk)
blockFileParser' hasFS getBinaryBlockInfo decodeNestedCtxtAndBlock isNotCorrupt validationPolicy =
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

fromChainHash :: ChainHash blk -> WithOrigin (HeaderHash blk)
fromChainHash GenesisHash      = Origin
fromChainHash (BlockHash hash) = At hash

extractInfo :: (HasHeader blk, GetHeader blk)
            => blk
            -> BinaryBlockInfo
            -> VolDB.BlockInfo (HeaderHash blk)
extractInfo b BinaryBlockInfo{..} = VolDB.BlockInfo {
      bbid          = blockHash b
    , bslot         = blockSlot b
    , bpreBid       = fromChainHash (blockPrevHash b)
    , bisEBB        = blockToIsEBB b
    , bheaderOffset = headerOffset
    , bheaderSize   = headerSize
    }
