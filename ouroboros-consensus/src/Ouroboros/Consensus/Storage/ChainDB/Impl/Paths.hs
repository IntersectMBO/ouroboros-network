{-# LANGUAGE BangPatterns             #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE LambdaCase               #-}
{-# LANGUAGE NamedFieldPuns           #-}
{-# LANGUAGE ScopedTypeVariables      #-}
{-# LANGUAGE StandaloneDeriving       #-}
module Ouroboros.Consensus.Storage.ChainDB.Impl.Paths (
    -- * LookupBlockInfo
    LookupBlockInfo
    -- * Candidates
  , extendWithSuccessors
  , maximalCandidates
    -- * Path
  , Path (..)
  , computePath
    -- * Reverse path
  , ReversePath (..)
  , computeReversePath
    -- * Reachability
  , isReachable
  ) where

import           Data.Foldable (foldl')
import           Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import           Data.Maybe (fromMaybe, mapMaybe)
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Word (Word64)

import           Ouroboros.Network.AnchoredFragment (AnchoredFragment)
import qualified Ouroboros.Network.AnchoredFragment as AF

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Fragment.Diff (ChainDiff (..))
import qualified Ouroboros.Consensus.Fragment.Diff as Diff

import qualified Ouroboros.Consensus.Storage.VolatileDB as VolatileDB

import           Ouroboros.Consensus.Storage.ChainDB.API hiding (ChainDB (..),
                     closeDB, getMaxSlotNo)

{-------------------------------------------------------------------------------
  LookupBlockInfo
-------------------------------------------------------------------------------}

-- | Return the block info for the block with the given hash. Return 'Nothing'
-- when not in the VolatileDB.
type LookupBlockInfo blk = HeaderHash blk -> Maybe (VolatileDB.BlockInfo blk)

{-------------------------------------------------------------------------------
  Candidates
-------------------------------------------------------------------------------}

-- | Compute the /maximal/ candidates starting at the specified point
--
-- As discussed in the Consensus Report, the set of /maximal/ candidates doesn't
-- include prefixes.
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
maximalCandidates ::
     forall blk.
     (ChainHash blk -> Set (HeaderHash blk))
     -- ^ @filterByPredecessor@
  -> Point blk -- ^ @B@
  -> [NonEmpty (HeaderHash blk)]
     -- ^ Each element in the list is a list of hashes from which we can
     -- construct a fragment anchored at the point @B@.
maximalCandidates succsOf b = mapMaybe NE.nonEmpty $ go (pointHash b)
  where
    go :: ChainHash blk -> [[HeaderHash blk]]
    go mbHash = case Set.toList $ succsOf mbHash of
      []    -> [[]]
      succs -> [ next : candidate
               | next <- succs
               , candidate <- go (BlockHash next)
               ]

-- | Extend the 'ChainDiff' with the successors found by 'maximalCandidates'.
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
  => (ChainHash blk -> Set (HeaderHash blk))
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
        | candHashes <- maximalCandidates succsOf (castPoint (Diff.getTip diff))
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
  Paths
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
      Nothing      -> Just $ NotInVolatileDB endPt
      Just volPath -> go [] volPath
  where
    endPt :: RealPoint blk
    endPt = case to of
        StreamToInclusive pt -> pt

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
          -> Just $ CompletelyInVolatileDB acc
          | otherwise
            -- If 'StreamFrom' was not from genesis, then the range must be
            -- invalid.
          -> Nothing

        StoppedAt hash _bno
          | StreamFromExclusive GenesisPoint <- from
          -> Just $ PartiallyInVolatileDB hash acc
          | StreamFromExclusive (BlockPoint _ hash') <- from
          , hash == hash'
          -> Just $ CompletelyInVolatileDB acc
          | StreamFromExclusive (BlockPoint _ _) <- from
          -> Just $ PartiallyInVolatileDB hash acc
          | StreamFromInclusive _ <- from
          -> Just $ PartiallyInVolatileDB hash acc

        volPath' ::> (flds, _isEBB)
          | StreamFromExclusive GenesisPoint <- from
          -> go (addToAcc flds acc) volPath'
          | StreamFromExclusive (BlockPoint _ hash') <- from
          , headerFieldHash flds == hash'
          -> Just $ CompletelyInVolatileDB acc
          | StreamFromExclusive (BlockPoint _ _) <- from
          -> go (addToAcc flds acc) volPath'
          | StreamFromInclusive pt' <- from
          , fieldsToRealPoint flds == pt'
          -> Just $ CompletelyInVolatileDB (addToAcc flds acc)
          | StreamFromInclusive _ <- from
          -> go (addToAcc flds acc) volPath'

-- | A path through the VolatileDB from a 'StreamFrom' to a 'StreamTo'.
--
-- Invariant: the @AnchoredFragment@ (oldest first) constructed using the blocks
-- corresponding to the points in the path will be valid, i.e., the blocks
-- will fit onto each other.
data Path blk =
    NotInVolatileDB (RealPoint blk)
    -- ^ The @end@ point (@'StreamToInclusive' end@) was not part of the
    -- VolatileDB.
  | CompletelyInVolatileDB [RealPoint blk]
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
  | PartiallyInVolatileDB (HeaderHash blk) [RealPoint blk]
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
  Reverse path
-------------------------------------------------------------------------------}

headerFieldsFromBlockInfo :: VolatileDB.BlockInfo blk -> HeaderFields blk
headerFieldsFromBlockInfo VolatileDB.BlockInfo { biSlotNo, biHash, biBlockNo } =
    HeaderFields {
        headerFieldHash    = biHash
      , headerFieldSlot    = biSlotNo
      , headerFieldBlockNo = biBlockNo
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
      Just blockInfo@VolatileDB.BlockInfo { biBlockNo, biIsEBB, biPrevHash } -> Just $
        go biPrevHash biBlockNo biIsEBB ::> (headerFieldsFromBlockInfo blockInfo, biIsEBB)
  where
    go ::
         ChainHash blk
         -- ^ The predecessor of the last block added to the path. Not
         -- necessarily in the VolatileDB.
      -> BlockNo  -- ^ The block number of the last block
      -> IsEBB    -- ^ Whether the last block is an EBB or not
      -> ReversePath blk
    go predecessor lastBlockNo lastIsEBB = case predecessor of
      GenesisHash        -> StoppedAtGenesis
      BlockHash prevHash -> case lookupBlockInfo prevHash of
        Nothing ->
          StoppedAt prevHash (prevBlockNo lastBlockNo lastIsEBB)
        Just blockInfo@VolatileDB.BlockInfo { biBlockNo, biIsEBB, biPrevHash } ->
          go biPrevHash biBlockNo biIsEBB ::> (headerFieldsFromBlockInfo blockInfo, biIsEBB)

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
  :: forall blk. (HasHeader blk, GetHeader blk)
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
