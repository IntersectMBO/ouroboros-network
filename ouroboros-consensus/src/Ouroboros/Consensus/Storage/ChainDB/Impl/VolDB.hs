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
    -- * Candidates
  , LookupBlockInfo
  , candidates
  , extendWithSuccessors
  , isReachable
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
  , fromChainHash
  ) where

import           Codec.CBOR.Decoding (Decoder)
import qualified Codec.CBOR.Write as CBOR
import           Control.Monad (join)
import           Control.Tracer (Tracer, nullTracer)
import           Data.Bifunctor (first)
import qualified Data.ByteString.Lazy as Lazy
import           Data.ByteString.Short (ShortByteString)
import           Data.Foldable (foldl')
import           Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import           Data.Maybe (fromMaybe, isJust, mapMaybe)
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Typeable (Typeable)
import           Data.Word (Word64)
import           GHC.Generics (Generic)
import           GHC.Stack (HasCallStack)
import           Streaming.Prelude (Of (..), Stream)
import qualified Streaming.Prelude as S
import           System.FilePath ((</>))

import           Ouroboros.Network.AnchoredFragment (AnchoredFragment (..))
import qualified Ouroboros.Network.AnchoredFragment as AF
import           Ouroboros.Network.Block (MaxSlotNo)

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Fragment.Diff (ChainDiff (..))
import qualified Ouroboros.Consensus.Fragment.Diff as Diff
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
  Compute candidates
-------------------------------------------------------------------------------}

-- | Compute all candidates starting at the specified point
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
               , candidate <- go (NotOrigin next)
               ]

-- | Extend the 'ChainDiff' with the successors found by 'candidates'.
--
-- In case no successors were found, the original 'ChainDiff' is returned as a
-- singleton.
--
-- In case successors /were/ found, the original 'ChainDiff' is /not/
-- included, only its extensions.
--
-- Only the longest possible extensions are returned, no intermediary prefixes
-- of extensions.
extendWithSuccessors ::
     forall blk. HasHeader blk
  => (WithOrigin (HeaderHash blk) -> Set (HeaderHash blk))
  -> LookupBlockInfo blk
  -> ChainDiff (HeaderFields blk)
  -> NonEmpty (ChainDiff (HeaderFields blk))
extendWithSuccessors succsOf lookupBlockInfo diff =
    case NE.nonEmpty extensions of
      Nothing          -> diff NE.:| []
      Just extensions' -> extensions'
  where
    extensions =
        [ foldl' Diff.append diff (lookupHeaderFields <$> candHashes)
        | candHashes <- candidates succsOf (Diff.getTip diff)
        ]

    lookupHeaderFields :: HeaderHash blk -> HeaderFields blk
    lookupHeaderFields =
          headerFieldsFromBlockInfo
          -- The successor mapping is populated with the blocks in the
          -- VolatileDB, so looking up the block info of a successor /must/
          -- succeed.
        . fromMaybe (error "successor must in the VolatileDB")
        . lookupBlockInfo

{-------------------------------------------------------------------------------
  Paths through the VolatileDB
-------------------------------------------------------------------------------}

-- | Return the block info for the block with the given hash. Return 'Nothing'
-- when not in the VolatileDB.
type LookupBlockInfo blk =
  HeaderHash blk -> Maybe (VolDB.BlockInfo (HeaderHash blk))

headerFieldsFromBlockInfo ::
     VolDB.BlockInfo (HeaderHash blk)
  -> HeaderFields blk
headerFieldsFromBlockInfo VolDB.BlockInfo { bslot, bbid, bbno } =
    HeaderFields {
        headerFieldHash    = bbid
      , headerFieldSlot    = bslot
      , headerFieldBlockNo = bbno
      }

-- | A reverse path through the VolatileDB starting at a block in the
-- VolatileDB until we reach genesis or leave the VolatileDB.
data ReversePath blk =
      -- | The path stopped at genesis
      StoppedAtGenesis

      -- | The path stopped at this hash, which is the hash of the predecessor
      -- of the last block in the path (that was still stored in the
      -- VolatileDB).
      --
      -- The block corresponding to the predecessor is /not/ stored in the
      -- VolatileDB. Either because it is missing, or because it is old and
      -- has been garbage collected.
      --
      -- Since block numbers are consecutive, we subtract 1 from the block
      -- number of the last block to obtain the block number corresponding to
      -- this hash.
      --
      -- EBBs share their block number with their predecessor:
      --
      -- > block:         regular block 1 | EBB | regular block 2
      -- > block number:                X |   X | X + 1
      --
      -- So when the hash refers to regular block 1, we see that the successor
      -- block is an EBB and use its block number without subtracting 1.
      --
      -- Edge case: if there are two or more consecutive EBBs, we might
      -- predict the wrong block number, but there are no consecutive EBBs in
      -- practice, they are one epoch apart.
    | StoppedAt (HeaderHash blk) BlockNo

      -- | Snoc: the block with the given 'HeaderFields' is in the VolatileDB.
      -- We also track whether it is an EBB or not.
      --
      -- NOTE: we are intentionally lazy in the spine, as constructing the
      -- path requires lookups in the VolatileDB's in-memory indices, which
      -- are logarithmic in the size of the index.
    | (ReversePath blk) ::> (HeaderFields blk, IsEBB)

-- | Lazily compute the 'ReversePath' that starts (i.e., ends) with the given
-- 'HeaderHash'.
computeReversePath
  :: forall blk.
     LookupBlockInfo blk
  -> HeaderHash blk
     -- ^ End hash
  -> Maybe (ReversePath blk)
     -- ^ Reverse path from the end point to genesis or the first predecessor
     -- not in the VolatileDB. Nothing when the end hash is not in the
     -- VolatileDB.
computeReversePath lookupBlockInfo endHash =
    case lookupBlockInfo endHash of
      Nothing                                               -> Nothing
      Just blockInfo@VolDB.BlockInfo { bbno, bisEBB, bpreBid } -> Just $
        go bpreBid bbno bisEBB ::> (headerFieldsFromBlockInfo blockInfo, bisEBB)
  where
    go ::
         WithOrigin (HeaderHash blk)
         -- ^ The predecessor of the last block added to the path. Not
         -- necessarily in the VolatileDB.
      -> BlockNo  -- ^ The block number of the last block
      -> IsEBB    -- ^ Whether the last block is an EBB or not
      -> ReversePath blk
    go predecessor lastBlockNo lastIsEBB = case predecessor of
      Origin             -> StoppedAtGenesis
      NotOrigin prevHash -> case lookupBlockInfo prevHash of
        Nothing ->
          StoppedAt prevHash (prevBlockNo lastBlockNo lastIsEBB)
        Just blockInfo@VolDB.BlockInfo { bbno, bisEBB, bpreBid } ->
          go bpreBid bbno bisEBB ::> (headerFieldsFromBlockInfo blockInfo, bisEBB)

    -- | Predict the block number of the missing predecessor.
    --
    -- PRECONDITION: the block number and 'IsEBB' correspond to a block that
    -- has a predecessor.
    --
    -- For regular blocks, this is just block number - 1, EBBs are special of
    -- course: they share their block number with their predecessor:
    --
    -- > block:         regular block 1 | EBB | regular block 2
    -- > block number:                X |   X | X + 1
    --
    -- Edge case: if there are two or more consecutive EBBs, we might predict
    -- the wrong block number, but there are no consecutive EBBs in practice
    -- (nor in the tests), they are one epoch apart.
    prevBlockNo :: BlockNo -> IsEBB -> BlockNo
    prevBlockNo bno isEBB = case (bno, isEBB) of
      (0, IsNotEBB) -> error "precondition violated"
      (_, IsNotEBB) -> bno - 1
      (_, IsEBB)    -> bno

{-------------------------------------------------------------------------------
  Reachability
-------------------------------------------------------------------------------}

-- | Try to connect the point @P@ to the chain fragment by chasing the
-- predecessors.
--
-- When successful, return a 'ChainDiff': the number of blocks to roll back
-- the chain fragment to the intersection point and a fragment anchored at the
-- intersection point containing the 'HeaderFields' corresponding to the
-- blocks needed to connect to @P@. The intersection point will be the most
-- recent intersection point.
--
-- Returns 'Nothing' when @P@ is not in the VolatileDB or when @P@ is not
-- connected to the given chain fragment.
--
-- POSTCONDITION: the returned number of blocks to roll back is less than or
-- equal to the length of the given chain fragment.
--
-- Note that the number of returned points can be smaller than the number of
-- blocks to roll back. This means @P@ is on a fork shorter than the given
-- chain fragment.
--
-- A 'ChainDiff' is returned iff @P@ is on the chain fragment. Moreover, when
-- the number of blocks to roll back is also 0, it must be that @P@ is the tip
-- of the chain fragment.
--
-- When the suffix of the 'ChainDiff' is non-empty, @P@ will be the last point
-- in the suffix.
isReachable
  :: forall blk. (HasHeader blk, GetHeader blk, HasCallStack)
  => LookupBlockInfo blk
  -> AnchoredFragment (Header blk) -- ^ Chain fragment to connect the point to
  -> RealPoint blk
  -> Maybe (ChainDiff (HeaderFields blk))
isReachable lookupBlockInfo = \chain b ->
    case computeReversePath lookupBlockInfo (realPointHash b) of
      -- Block not in the VolatileDB, so it's unreachable
      Nothing          -> Nothing
      Just reversePath -> go chain reversePath 0 []
  where
    -- | NOTE: the 'ReversePath' is lazy in its spine. We will only force as
    -- many elements as 'RealPoint's we return. In the worst case, the path is
    -- not connected to the current chain at all, in which case we do force
    -- the entire path.
    --
    -- We're trying to find a common block, i.e., one with the same point and
    -- thus the same slot. Both the chain and the path are ordered by slots,
    -- so we compare the slots and drop the largest one until we have a match
    -- in slot, then we check hashes. If those don't match, we drop both.
    -- Note: EBBs complicate things, see 'ebbAwareCompare'.
    go
      :: AnchoredFragment (Header blk)
         -- ^ Prefix of the current chain
      -> ReversePath blk
         -- ^ Prefix of the path through the VolatileDB
      -> Word64
         -- ^ Number of blocks we have had to roll back from the current chain
      -> [HeaderFields blk]
         -- ^ Accumulator for the suffix, from oldest to newest
      -> Maybe (ChainDiff (HeaderFields blk))
    go chain path !rollback acc = case (chain, path) of
        (AF.Empty anchor, StoppedAt hash bno)
          | AF.anchorToBlockNo anchor == NotOrigin bno
          , AF.anchorToHash anchor == BlockHash hash
          -> Just (ChainDiff rollback (AF.fromOldestFirst (AF.castAnchor anchor) acc))
          | otherwise
          -> Nothing

        (AF.Empty anchor, path' ::> (flds, _))
          | AF.anchorToHeaderFields (AF.castAnchor anchor) == NotOrigin flds
          -> Just (ChainDiff rollback (AF.fromOldestFirst (AF.castAnchor anchor) acc))
          | AF.anchorToBlockNo anchor > NotOrigin (headerFieldBlockNo flds)
          -> Nothing
          | otherwise
          -> go chain path' rollback (flds:acc)

        (chain' AF.:> hdr, StoppedAt hash bno)
          | blockNo hdr == bno
          , headerHash hdr == hash
          , let anchor = AF.castAnchor (AF.anchorFromBlock hdr)
          -> Just (ChainDiff rollback (AF.fromOldestFirst anchor acc))
          | blockNo hdr < bno
          -> Nothing
          | otherwise
          -> go chain' path (rollback + 1) acc

        (_, StoppedAtGenesis)
          | AF.anchorIsGenesis (AF.anchor chain)
          -> let !rollback' = rollback + fromIntegral (AF.length chain)
             in Just (ChainDiff rollback' (AF.fromOldestFirst AF.AnchorGenesis acc))
          | otherwise
          -> Nothing

        (chain' AF.:> hdr, path' ::> (flds, ptIsEBB)) ->
          case hdr `ebbAwareCompare` (headerFieldBlockNo flds, ptIsEBB) of
            -- Drop from the path
            LT -> go chain path' rollback (flds:acc)
            -- Drop from the current chain fragment
            GT -> go chain' path (rollback + 1) acc
            -- Same slot and value for 'IsEBB'
            EQ | blockHash hdr == headerFieldHash flds
               , let anchor = AF.castAnchor (AF.anchorFromBlock hdr)
               -- Found a match
               -> Just (ChainDiff rollback (AF.fromOldestFirst anchor acc))
               -- Different hashes, drop both
               | otherwise
               -> go chain' path' (rollback + 1) (flds:acc)

    -- | EBBs have the same block number as their predecessor, which means
    -- that in case we have an EBB and a regular block with the same slot, the
    -- EBB comes /after/ the regular block.
    ebbAwareCompare :: Header blk -> (BlockNo, IsEBB) -> Ordering
    ebbAwareCompare hdr (ptBlockNo, ptIsEBB) =
      compare (blockNo hdr) ptBlockNo `mappend`
      case (headerToIsEBB hdr, ptIsEBB) of
        (IsEBB,    IsNotEBB) -> GT
        (IsNotEBB, IsEBB)    -> LT
        (IsEBB,    IsEBB)    -> EQ
        (IsNotEBB, IsNotEBB) -> EQ

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
computePath ::
     forall blk. HasHeader blk
  => LookupBlockInfo blk
  -> StreamFrom blk
  -> StreamTo   blk
  -> Maybe (Path blk)
computePath lookupBlockInfo from to =
    case computeReversePath lookupBlockInfo (realPointHash endPt) of
      Nothing
        -> Just $ NotInVolDB endPt
      Just volPath
        | exclUpperBound
        -- When there's an exclusive upper bound, we must exclude the first
        -- point of the reverse path from the returned 'Path'. We simply drop
        -- it from the path before starting the main loop (@go@).
        -> case volPath of
             volPath' ::> _   -> go [] volPath'
             -- If we're immediately stopped, the accumulator will be empty,
             -- so no need to drop the last element. Reuse the termination
             -- logic of @go@.
             StoppedAt {}     -> go [] volPath
             -- The exclusive end bound cannot be genesis, as it must be a
             -- 'RealPoint', so the reverse path cannot start with
             -- 'StoppedAtGenesis'.
             StoppedAtGenesis -> error "exclusive end bound cannot be genesis"
        | otherwise
        -> go [] volPath
  where
    (endPt, exclUpperBound) = case to of
        StreamToInclusive pt -> (pt, False)
        StreamToExclusive pt -> (pt, True)

    fieldsToRealPoint :: HeaderFields blk -> RealPoint blk
    fieldsToRealPoint flds =
        RealPoint (headerFieldSlot flds) (headerFieldHash flds)

    -- | Convert the 'HeaderFields' to a 'RealPoint' and prepend that to the
    -- accumulator.
    --
    -- NOTE: we will store the returned list in the state of a ChainDB
    -- iterator as a lazy non-empty list. To avoid thunks, we force the
    -- elements now, when adding them to the accumulator. TODO #2341
    addToAcc :: HeaderFields blk -> [RealPoint blk] -> [RealPoint blk]
    addToAcc flds pts = pt : pts
        -- When the returned list is forced, @pt@ is forced. The returned list
        -- is forced because the accumulator is forced in @go@.
      where
        !pt = fieldsToRealPoint flds

    go ::
         [RealPoint blk]  -- ^ Accumulator for the 'Path'
      -> ReversePath blk  -- ^ Prefix of the path to 'StreamFrom'
      -> Maybe (Path blk)
    go !acc = \case
        StoppedAtGenesis
          | StreamFromExclusive GenesisPoint <- from
          -> Just $ CompletelyInVolDB acc
          | otherwise
            -- If 'StreamFrom' was not from genesis, then the range must be
            -- invalid.
          -> Nothing

        StoppedAt hash _bno
          | StreamFromExclusive GenesisPoint <- from
          -> Just $ PartiallyInVolDB hash acc
          | StreamFromExclusive (BlockPoint _ hash') <- from
          , hash == hash'
          -> Just $ CompletelyInVolDB acc
          | StreamFromExclusive (BlockPoint _ _) <- from
          -> Just $ PartiallyInVolDB hash acc
          | StreamFromInclusive _ <- from
          -> Just $ PartiallyInVolDB hash acc

        volPath' ::> (flds, _isEBB)
          | StreamFromExclusive GenesisPoint <- from
          -> go (addToAcc flds acc) volPath'
          | StreamFromExclusive (BlockPoint _ hash') <- from
          , headerFieldHash flds == hash'
          -> Just $ CompletelyInVolDB acc
          | StreamFromExclusive (BlockPoint _ _) <- from
          -> go (addToAcc flds acc) volPath'
          | StreamFromInclusive pt' <- from
          , fieldsToRealPoint flds == pt'
          -> Just $ CompletelyInVolDB (addToAcc flds acc)
          | StreamFromInclusive _ <- from
          -> go (addToAcc flds acc) volPath'

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
    lookupBlockInfo <- getBlockInfo volDB
    case computePath lookupBlockInfo from to of
      Just path -> return path
      Nothing   -> throwM $ InvalidIteratorRange from to

-- | A path through the VolatileDB from a 'StreamFrom' to a 'StreamTo'.
--
-- Invariant: the @ChainFragment@ (oldest first) constructed using the blocks
-- corresponding to the points in the path will be valid, i.e., the blocks
-- will fit onto each other.
data Path blk =
    NotInVolDB (RealPoint blk)
    -- ^ The @end@ point (@'StreamToInclusive' end@ or @'StreamToExclusive'
    -- end@) was not part of the VolatileDB.
  | CompletelyInVolDB [RealPoint blk]
    -- ^ A complete path, from start point to end point was constructed from
    -- the VolatileDB. The list contains the points from oldest to newest.
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
  | PartiallyInVolDB (HeaderHash blk) [RealPoint blk]
    -- ^ Only a partial path could be constructed from the VolatileDB. The
    -- missing predecessor could still be in the ImmutableDB. The list
    -- contains the points from oldest to newest.
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

deriving instance HasHeader blk => Eq   (Path blk)
deriving instance HasHeader blk => Show (Path blk)

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

fromChainHash :: ChainHash blk -> WithOrigin (HeaderHash blk)
fromChainHash GenesisHash      = Origin
fromChainHash (BlockHash hash) = NotOrigin hash

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
