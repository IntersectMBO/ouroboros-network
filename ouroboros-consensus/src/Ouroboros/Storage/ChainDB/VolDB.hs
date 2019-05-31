{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE StandaloneDeriving        #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE UndecidableInstances      #-}

-- | Thin wrapper around the volatile DB
module Ouroboros.Storage.ChainDB.VolDB (
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
    -- * Re-exports
  , VolatileDBError
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

import           Ouroboros.Network.Block (ChainHash (..), HasHeader (..),
                     HeaderHash, Point (..), StandardHash)
import qualified Ouroboros.Network.Block as Block

import qualified Ouroboros.Consensus.Util.CBOR as Util.CBOR

import           Ouroboros.Storage.ChainDB.API (ChainDbError (..),
                     ChainDbFailure (..), StreamFrom (..), StreamTo (..))
import           Ouroboros.Storage.FS.API (HasFS, createDirectoryIfMissing)
import           Ouroboros.Storage.FS.API.Types (MountPoint (..))
import           Ouroboros.Storage.FS.IO (ioHasFS)
import           Ouroboros.Storage.Util.ErrorHandling (ErrorHandling,
                     ThrowCantCatch)
import qualified Ouroboros.Storage.Util.ErrorHandling as EH
import           Ouroboros.Storage.VolatileDB (VolatileDB, VolatileDBError)
import qualified Ouroboros.Storage.VolatileDB as VolDB

-- | Thin wrapper around the volatile DB (opaque type)
--
-- The intention is that all interaction with the volatile DB goes through
-- this module.
data VolDB m blk hdr = VolDB {
      volDB    :: VolatileDB (HeaderHash blk) m
    , decBlock :: forall s. Decoder s blk
    , encBlock :: blk -> Encoding
    , header   :: blk -> hdr
    }

{-------------------------------------------------------------------------------
  Initialization
-------------------------------------------------------------------------------}

data VolDbArgs m blk hdr = forall h. VolDbArgs {
      volHasFS         :: HasFS m h
    , volErr           :: ErrorHandling (VolatileDBError (HeaderHash blk)) m
    , volErrSTM        :: ThrowCantCatch (VolatileDBError (HeaderHash blk)) (STM m)
    , volBlocksPerFile :: Int
    , volDecodeBlock   :: forall s. Decoder s blk
    , volEncodeBlock   :: blk -> Encoding
    , volGetHeader     :: blk -> hdr
    }

-- | Default arguments when using the 'IO' monad
--
-- The following fields must still be defined:
--
-- * blocksPerFile
-- * decodeBlock
-- * getHeader
defaultArgs :: StandardHash blk => FilePath -> VolDbArgs IO blk hdr
defaultArgs fp = VolDbArgs {
      volErr    = EH.exceptions
    , volErrSTM = EH.throwSTM
    , volHasFS  = ioHasFS $ MountPoint (fp </> "volatile")
      -- Fields without a default
    , volBlocksPerFile = error "no default for volBlocksPerFile"
    , volDecodeBlock   = error "no default for volDecodeBlock"
    , volEncodeBlock   = error "no default for volEncodeBlock"
    , volGetHeader     = error "no default for volGetHeader"
    }

openDB :: (MonadCatch m, MonadSTM m, MonadST m, HasHeader blk)
       => VolDbArgs m blk hdr -> m (VolDB m blk hdr)
openDB args@VolDbArgs{..} = do
    createDirectoryIfMissing volHasFS True []
    volDB <- VolDB.openDB
               volHasFS
               volErr
               volErrSTM
               (blockFileParser args)
               volBlocksPerFile
    return $ VolDB volDB volDecodeBlock volEncodeBlock volGetHeader

{-------------------------------------------------------------------------------
  Wrappers
-------------------------------------------------------------------------------}

getIsMember :: VolDB m blk hdr -> STM m (HeaderHash blk -> Bool)
getIsMember db = withSTM db VolDB.getIsMember

getPredecessor :: VolDB m blk hdr
               -> STM m (HeaderHash blk -> Maybe (HeaderHash blk))
getPredecessor db = withSTM db VolDB.getPredecessor

getSuccessors :: VolDB m blk hdr
              -> STM m (Maybe (HeaderHash blk) -> Set (HeaderHash blk))
getSuccessors db = withSTM db VolDB.getSuccessors

putBlock :: (MonadCatch m, HasHeader blk) => VolDB m blk hdr -> blk -> m ()
putBlock db@VolDB{..} b = withDB db $ \vol ->
    VolDB.putBlock vol (extractInfo b) (CBOR.toBuilder (encBlock b))

closeDB :: (MonadCatch m, HasHeader blk) => VolDB m blk hdr -> m ()
closeDB db = withDB db VolDB.closeDB

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
               => VolDB m blk hdr
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
      Nothing   -> error "impossible: invalid bounds"
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
    StreamToInclusive (Point { pointHash = GenesisHash })
      -> Nothing
    StreamToExclusive (Point { pointHash = GenesisHash })
      -> Nothing
    StreamToInclusive (Point { pointHash = BlockHash end })
      | isMember end     -> go [end] end
      | otherwise        -> return $ NotInVolDB end
    StreamToExclusive (Point { pointHash = BlockHash end })
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
        StreamFromExclusive (Point { pointHash = GenesisHash })
          -> return $ CompletelyInVolDB acc
        StreamFromExclusive (Point { pointHash = BlockHash _ })
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
  :: forall m blk hdr.
     ( MonadSTM m
     , MonadThrow (STM m)
     , HasHeader blk
     )
  => VolDB m blk hdr
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

getKnownHeader :: (MonadCatch m, StandardHash blk, Typeable blk)
               => VolDB m blk hdr -> HeaderHash blk -> m hdr
getKnownHeader db@VolDB{..} hash =
    header <$> getKnownBlock db hash

getKnownBlock :: (MonadCatch m, StandardHash blk, Typeable blk)
              => VolDB m blk hdr -> HeaderHash blk -> m blk
getKnownBlock db hash = do
    mBlock <- mustExist hash <$> getBlock db hash
    case mBlock of
      Right b  -> return b
      Left err -> throwM err

getHeader :: (MonadCatch m, StandardHash blk, Typeable blk)
          => VolDB m blk hdr -> HeaderHash blk -> m (Maybe hdr)
getHeader db@VolDB{..} hash =
    fmap header <$> getBlock db hash

getBlock :: (MonadCatch m, StandardHash blk, Typeable blk)
         => VolDB m blk hdr -> HeaderHash blk -> m (Maybe blk)
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

blockFileParser :: forall m blk hdr. (MonadST m, MonadThrow m, HasHeader blk)
                => VolDbArgs m blk hdr
                -> VolDB.Parser
                     Util.CBOR.ReadIncrementalErr
                     m
                     (HeaderHash blk)
blockFileParser VolDbArgs{..} =
    VolDB.Parser $ Util.CBOR.readIncrementalOffsets volHasFS decoder'
  where
    decoder' :: forall s. Decoder s (VolDB.BlockInfo (HeaderHash blk))
    decoder' = extractInfo <$> volDecodeBlock

{-------------------------------------------------------------------------------
  Error handling
-------------------------------------------------------------------------------}

-- | Wrap calls to the volatile DB and rethrow exceptions that may indicate
-- disk failure and should therefore trigger recovery
withDB :: forall m blk hdr x. (MonadCatch m, StandardHash blk, Typeable blk)
       => VolDB m blk hdr
       -> (VolatileDB (HeaderHash blk) m -> m x)
       -> m x
withDB VolDB{..} k = catch (k volDB) rethrow
  where
    rethrow :: VolatileDBError (HeaderHash blk) -> m x
    rethrow err = case wrap err of
                    Just err' -> throwM err'
                    Nothing   -> throwM err

    wrap :: VolatileDBError (HeaderHash blk) -> Maybe (ChainDbFailure blk)
    wrap (VolDB.UnexpectedError err) = Just (VolDbFailure err)
    wrap VolDB.UserError{}           = Nothing

-- | STM actions, by definition, cannot access the disk and therefore we don't
-- have to worry about catching exceptions here: any exceptions that may be
-- thrown indicate bugs, either in the ChainDB or in the volatile DB
withSTM :: VolDB m blk hdr
        -> (VolatileDB (HeaderHash blk) m -> STM m x)
        -> STM m x
withSTM VolDB{..} k = k volDB

mustExist :: HeaderHash blk
          -> Maybe blk
          -> Either (ChainDbFailure blk) blk
mustExist hash Nothing  = Left  $ VolDbMissingBlock hash
mustExist _    (Just b) = Right $ b

parse :: forall blk.
         (forall s. Decoder s blk)
      -> HeaderHash blk
      -> Lazy.ByteString
      -> Either (ChainDbFailure blk) blk
parse dec hash =
    aux . CBOR.deserialiseFromBytes dec
  where
    aux :: Either CBOR.DeserialiseFailure (Lazy.ByteString, blk)
        -> Either (ChainDbFailure blk) blk
    aux (Right (bs, b))
      | Lazy.null bs = Right b
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
