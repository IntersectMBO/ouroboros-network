{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE PatternSynonyms           #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE StandaloneDeriving        #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE UndecidableInstances      #-}

{-# OPTIONS_GHC -Wredundant-constraints #-}
-- | Thin wrapper around the VolatileDB
module Ouroboros.Storage.ChainDB.Impl.VolDB (
    VolDB -- Opaque
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
  , getKnownHeader
  , getKnownBlock
  , getHeader
  , getBlock
    -- * Wrappers
  , getIsMember
  , getPredecessor
  , getSuccessors
  , putBlock
  , closeDB
  , reopen
  , garbageCollect
    -- * Re-exports
  , VolatileDBError
    -- * Exported for testing purposes
  , mkVolDB
  ) where

import           Codec.CBOR.Decoding (Decoder)
import           Codec.CBOR.Encoding (Encoding)
import qualified Codec.CBOR.Read as CBOR
import qualified Codec.CBOR.Write as CBOR
import qualified Data.ByteString.Lazy as Lazy
import           Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import           Data.Maybe (mapMaybe)
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Typeable (Typeable)
import           GHC.Stack (HasCallStack)
import           System.FilePath ((</>))

import           Control.Monad.Class.MonadST
import           Control.Monad.Class.MonadSTM
import           Control.Monad.Class.MonadThrow

import           Ouroboros.Network.Block (pattern BlockPoint, ChainHash (..),
                     pattern GenesisPoint, HasHeader (..), HeaderHash, Point,
                     SlotNo, StandardHash, pointHash, withHash)
import qualified Ouroboros.Network.Block as Block

import           Ouroboros.Consensus.Block (GetHeader, Header)
import qualified Ouroboros.Consensus.Block as Block
import qualified Ouroboros.Consensus.Util.CBOR as Util.CBOR

import           Ouroboros.Storage.ChainDB.API (ChainDbError (..),
                     ChainDbFailure (..), StreamFrom (..), StreamTo (..))
import           Ouroboros.Storage.FS.API (HasFS, createDirectoryIfMissing)
import           Ouroboros.Storage.FS.API.Types (MountPoint (..), mkFsPath)
import           Ouroboros.Storage.FS.IO (ioHasFS)
import           Ouroboros.Storage.Util.ErrorHandling (ErrorHandling,
                     ThrowCantCatch)
import qualified Ouroboros.Storage.Util.ErrorHandling as EH
import           Ouroboros.Storage.VolatileDB (VolatileDB, VolatileDBError)
import qualified Ouroboros.Storage.VolatileDB as VolDB

-- | Thin wrapper around the VolatileDB (opaque type)
--
-- The intention is that all interaction with the VolatileDB goes through this
-- module.
data VolDB m blk = VolDB {
      volDB    :: VolatileDB (HeaderHash blk) m
    , decBlock :: forall s. Decoder s (Lazy.ByteString -> blk)
    , encBlock :: blk -> Encoding
    , err      :: ErrorHandling (VolatileDBError (HeaderHash blk)) m
    , errSTM   :: ThrowCantCatch (VolatileDBError (HeaderHash blk)) (STM m)
    }

{-------------------------------------------------------------------------------
  Initialization
-------------------------------------------------------------------------------}

data VolDbArgs m blk = forall h. VolDbArgs {
      volHasFS         :: HasFS m h
    , volErr           :: ErrorHandling (VolatileDBError (HeaderHash blk)) m
    , volErrSTM        :: ThrowCantCatch (VolatileDBError (HeaderHash blk)) (STM m)
    , volBlocksPerFile :: Int
    , volDecodeBlock   :: forall s. Decoder s (Lazy.ByteString -> blk)
    , volEncodeBlock   :: blk -> Encoding
    }

-- | Default arguments when using the 'IO' monad
--
-- The following fields must still be defined:
--
-- * 'volBlocksPerFile'
-- * 'volDecodeBlock'
-- * 'volEncodeBlock'
defaultArgs :: StandardHash blk => FilePath -> VolDbArgs IO blk
defaultArgs fp = VolDbArgs {
      volErr    = EH.exceptions
    , volErrSTM = EH.throwSTM
    , volHasFS  = ioHasFS $ MountPoint (fp </> "volatile")
      -- Fields without a default
    , volBlocksPerFile = error "no default for volBlocksPerFile"
    , volDecodeBlock   = error "no default for volDecodeBlock"
    , volEncodeBlock   = error "no default for volEncodeBlock"
    }

openDB :: (MonadCatch m, MonadSTM m, MonadST m, HasHeader blk)
       => VolDbArgs m blk -> m (VolDB m blk)
openDB args@VolDbArgs{..} = do
    createDirectoryIfMissing volHasFS True (mkFsPath [])
    volDB <- VolDB.openDB
               volHasFS
               volErr
               volErrSTM
               (blockFileParser args)
               volBlocksPerFile
    return VolDB
      { volDB    = volDB
      , err      = volErr
      , errSTM   = volErrSTM
      , decBlock = volDecodeBlock
      , encBlock = volEncodeBlock
      }

-- | For testing purposes
mkVolDB :: VolatileDB (HeaderHash blk) m
        -> (forall s. Decoder s (Lazy.ByteString -> blk))
        -> (blk -> Encoding)
        -> ErrorHandling (VolatileDBError (HeaderHash blk)) m
        -> ThrowCantCatch (VolatileDBError (HeaderHash blk)) (STM m)
        -> VolDB m blk
mkVolDB volDB decBlock encBlock err errSTM = VolDB {..}

{-------------------------------------------------------------------------------
  Wrappers
-------------------------------------------------------------------------------}

getIsMember :: VolDB m blk -> STM m (HeaderHash blk -> Bool)
getIsMember db = withSTM db VolDB.getIsMember

getPredecessor :: VolDB m blk
               -> STM m (HeaderHash blk -> Maybe (HeaderHash blk))
getPredecessor db = withSTM db VolDB.getPredecessor

getSuccessors :: VolDB m blk
              -> STM m (Maybe (HeaderHash blk) -> Set (HeaderHash blk))
getSuccessors db = withSTM db VolDB.getSuccessors

putBlock :: (MonadCatch m, HasHeader blk) => VolDB m blk -> blk -> m ()
putBlock db@VolDB{..} b = withDB db $ \vol ->
    VolDB.putBlock vol (extractInfo b) (CBOR.toBuilder (encBlock b))

closeDB :: (MonadCatch m, HasHeader blk, HasCallStack)
        => VolDB m blk -> m ()
closeDB db = withDB db VolDB.closeDB

reopen :: (MonadCatch m, HasHeader blk, HasCallStack)
       => VolDB m blk -> m ()
reopen db = withDB db VolDB.reOpenDB

garbageCollect :: (MonadCatch m, HasHeader blk)
               => VolDB m blk -> SlotNo -> m ()
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
     (Maybe (HeaderHash blk) -> Set (HeaderHash blk)) -- ^ @getSuccessors@
  -> Point blk                                        -- ^ @B@
  -> [NonEmpty (HeaderHash blk)]
     -- ^ Each element in the list is a list of hashes from which we can
     -- construct a fragment anchored at the point @B@.
candidates succsOf b = mapMaybe NE.nonEmpty $ go (fromChainHash (pointHash b))
  where
    go :: Maybe (HeaderHash blk) -> [[HeaderHash blk]]
    go mbHash = case Set.toList $ succsOf mbHash of
      []    -> [[]]
      succs -> [ next : candidate
               | next <- succs
               , candidate <- go (Just next)
               ]

-- | Variant of 'isReachable' that obtains its arguments in the same 'STM'
-- transaction.
isReachableSTM :: (MonadSTM m, HasHeader blk)
               => VolDB m blk
               -> STM m (Point blk) -- ^ The tip of the ImmutableDB (@I@).
               -> Point blk         -- ^ The point of the block (@B@) to check
                                    -- the reachability of
               -> STM m (Maybe (NonEmpty (HeaderHash blk)))
isReachableSTM volDB getI b = do
    (predecessor, isMember, i) <- withSTM volDB $ \db ->
      (,,) <$> VolDB.getPredecessor db <*> VolDB.getIsMember db <*> getI
    return $ isReachable predecessor isMember i b

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
            => (HeaderHash blk -> Maybe (HeaderHash blk))  -- ^ @getPredecessor@
            -> (HeaderHash blk -> Bool)                    -- ^ @isMember@
            -> Point blk                                   -- ^ @I@
            -> Point blk                                   -- ^ @B@
            -> Maybe (NonEmpty (HeaderHash blk))
isReachable predecessor isMember i b =
    case computePath predecessor isMember from to of
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
  => (HeaderHash blk -> Maybe (HeaderHash blk)) -- ^ @getPredecessor@
  -> (HeaderHash blk -> Bool)                   -- ^ @isMember@
  -> StreamFrom blk
  -> StreamTo   blk
  -> Maybe (Path blk)
computePath predecessor isMember from to = case to of
    StreamToInclusive GenesisPoint
      -> Nothing
    StreamToExclusive GenesisPoint
      -> Nothing
    StreamToInclusive (BlockPoint { withHash = end })
      | isMember end     -> case from of
          -- Easier to handle this special case (@StreamFromInclusive start,
          -- StreamToInclusive end, start == end@) here:
          StreamFromInclusive (BlockPoint { withHash = start })
            | start == end -> return $ CompletelyInVolDB [end]
          _                -> go [end] end
      | otherwise        -> return $ NotInVolDB end
    StreamToExclusive (BlockPoint { withHash = end })
      | isMember end     -> go [] end
      | otherwise        -> return $ NotInVolDB end
  where
    -- Invariant: @isMember hash@ and @hash@ has been added to @acc@ (if
    -- allowed by the bounds)
    go :: [HeaderHash blk] -> HeaderHash blk -> Maybe (Path blk)
    go acc hash = case predecessor hash of
      -- Found genesis
      Nothing -> case from of
        StreamFromInclusive _
          -> Nothing
        StreamFromExclusive GenesisPoint
          -> return $ CompletelyInVolDB acc
        StreamFromExclusive (BlockPoint {})
          -> Nothing

      Just predHash
        | isMember predHash -> case from of
          StreamFromInclusive pt
            | BlockHash predHash == Block.pointHash pt
            -> return $ CompletelyInVolDB (predHash : acc)
          StreamFromExclusive pt
            | BlockHash predHash == Block.pointHash pt
            -> return $ CompletelyInVolDB acc
          -- Bound not yet reached, invariants both ok!
          _ -> go (predHash : acc) predHash
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
  :: forall m blk.
     ( MonadSTM m
     , MonadThrow (STM m)
     , HasHeader blk
     )
  => VolDB m blk
  -> StreamFrom blk
  -> StreamTo   blk
  -> STM m (Path blk)
computePathSTM volDB from to = do
    (predecessor, isMember) <- withSTM volDB $ \db ->
      (,) <$> VolDB.getPredecessor db <*> VolDB.getIsMember db
    case computePath predecessor isMember from to of
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

getKnownHeader :: (MonadCatch m, StandardHash blk, Typeable blk, GetHeader blk)
               => VolDB m blk -> HeaderHash blk -> m (Header blk)
getKnownHeader db@VolDB{..} hash =
    Block.getHeader <$> getKnownBlock db hash

getKnownBlock :: (MonadCatch m, StandardHash blk, Typeable blk)
              => VolDB m blk -> HeaderHash blk -> m blk
getKnownBlock db hash = do
    mBlock <- mustExist hash <$> getBlock db hash
    case mBlock of
      Right b  -> return b
      Left err -> throwM err

getHeader :: (MonadCatch m, StandardHash blk, Typeable blk, GetHeader blk)
               => VolDB m blk -> HeaderHash blk -> m (Maybe (Header blk))
getHeader db@VolDB{..} hash =
    fmap Block.getHeader <$> getBlock db hash

getBlock :: (MonadCatch m, StandardHash blk, Typeable blk)
         => VolDB m blk -> HeaderHash blk -> m (Maybe blk)
getBlock db hash = do
    mBlob <- withDB db $ \vol ->
               fmap (parse (decBlock db) hash) <$> VolDB.getBlock vol hash
    case mBlob of
      Nothing         -> return $ Nothing
      Just (Right b)  -> return $ Just b
      Just (Left err) -> throwM $ err

{-------------------------------------------------------------------------------
  Auxiliary: parsing
-------------------------------------------------------------------------------}

blockFileParser :: forall m blk. (MonadST m, MonadThrow m, HasHeader blk)
                => VolDbArgs m blk
                -> VolDB.Parser
                     Util.CBOR.ReadIncrementalErr
                     m
                     (HeaderHash blk)
blockFileParser VolDbArgs{..} =
    VolDB.Parser $ Util.CBOR.readIncrementalOffsets volHasFS decoder'
  where
    decoder' :: forall s. Decoder s (Lazy.ByteString -> VolDB.BlockInfo (HeaderHash blk))
    decoder' = (extractInfo .) <$> volDecodeBlock

{-------------------------------------------------------------------------------
  Error handling
-------------------------------------------------------------------------------}

-- | Wrap calls to the VolatileDB and rethrow exceptions that may indicate
-- disk failure and should therefore trigger recovery
withDB :: forall m blk x. (MonadThrow m, StandardHash blk, Typeable blk)
       => VolDB m blk
       -> (VolatileDB (HeaderHash blk) m -> m x)
       -> m x
withDB VolDB{..} k = EH.catchError err (k volDB) rethrow
  where
    rethrow :: VolatileDBError (HeaderHash blk) -> m x
    rethrow e = case wrap e of
                  Just e' -> throwM e'
                  Nothing -> throwM e

    wrap :: VolatileDBError (HeaderHash blk) -> Maybe (ChainDbFailure blk)
    wrap (VolDB.UnexpectedError e) = Just (VolDbFailure e)
    wrap VolDB.UserError{}         = Nothing

-- | STM actions, by definition, cannot access the disk and therefore we don't
-- have to worry about catching exceptions here: any exceptions that may be
-- thrown indicate bugs, either in the ChainDB or in the VolatileDB
withSTM :: VolDB m blk
        -> (VolatileDB (HeaderHash blk) m -> STM m x)
        -> STM m x
withSTM VolDB{..} k = k volDB

mustExist :: HeaderHash blk
          -> Maybe blk
          -> Either (ChainDbFailure blk) blk
mustExist hash Nothing  = Left  $ VolDbMissingBlock hash
mustExist _    (Just b) = Right $ b

parse :: forall blk.
         (forall s. Decoder s (Lazy.ByteString -> blk))
      -> HeaderHash blk
      -> Lazy.ByteString
      -> Either (ChainDbFailure blk) blk
parse dec hash bytes =
    aux (CBOR.deserialiseFromBytes dec bytes)
  where
    aux :: Either CBOR.DeserialiseFailure
                  (Lazy.ByteString, Lazy.ByteString -> blk)
        -> Either (ChainDbFailure blk) blk
    aux (Right (bs, blk))
      | Lazy.null bs = Right (blk bytes)
      | otherwise    = Left $ VolDbTrailingData hash bs
    aux (Left err)   = Left $ VolDbParseFailure hash err

{-------------------------------------------------------------------------------
  Auxiliary
-------------------------------------------------------------------------------}

fromChainHash :: ChainHash blk -> Maybe (HeaderHash blk)
fromChainHash GenesisHash      = Nothing
fromChainHash (BlockHash hash) = Just hash

extractInfo :: HasHeader blk => blk -> VolDB.BlockInfo (HeaderHash blk)
extractInfo b = VolDB.BlockInfo {
      bbid    = blockHash b
    , bslot   = blockSlot b
    , bpreBid = fromChainHash (blockPrevHash b)
    }
