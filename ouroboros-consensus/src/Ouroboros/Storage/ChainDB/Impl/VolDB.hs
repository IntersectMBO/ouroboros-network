{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
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
  , getKnownBlock
  , getKnownHeader
  , getKnownBlockComponent
  , getBlockComponent
    -- * Wrappers
  , getIsMember
  , getPredecessor
  , getSuccessors
  , getMaxSlotNo
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
import           Control.Monad (join)
import qualified Data.ByteString.Lazy as Lazy
import           Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import           Data.Maybe (mapMaybe)
import           Data.Proxy (Proxy (..))
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Typeable (Typeable)
import           Data.Word (Word32)
import           GHC.Stack (HasCallStack)
import           System.FilePath ((</>))

import           Cardano.Prelude (allNoUnexpectedThunks)

import           Control.Monad.Class.MonadThrow

import           Ouroboros.Network.Block (pattern BlockPoint, ChainHash (..),
                     pattern GenesisPoint, HasHeader (..), HeaderHash,
                     MaxSlotNo, Point, SlotNo, StandardHash, pointHash,
                     withHash)
import qualified Ouroboros.Network.Block as Block
import           Ouroboros.Network.Point (WithOrigin (..))

import           Ouroboros.Consensus.Block (Header, IsEBB)
import qualified Ouroboros.Consensus.Util.CBOR as Util.CBOR
import           Ouroboros.Consensus.Util.IOLike

import           Ouroboros.Storage.ChainDB.API (ChainDB)
import           Ouroboros.Storage.ChainDB.API hiding (ChainDB (..), closeDB,
                     getMaxSlotNo)
import           Ouroboros.Storage.ChainDB.Impl.BlockComponent
import           Ouroboros.Storage.Common (BinaryInfo (..))
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
      volDB     :: VolatileDB (HeaderHash blk) m
    , decHeader :: forall s. Decoder s (Lazy.ByteString -> Header blk)
    , decBlock  :: forall s. Decoder s (Lazy.ByteString -> blk)
      -- ^ TODO introduce a newtype wrapper around the @s@ so we can use
      -- generics to derive the NoUnexpectedThunks instance.
    , encBlock  :: blk -> BinaryInfo Encoding
    , isEBB     :: blk -> IsEBB
    , addHdrEnv :: !(IsEBB -> Word32 -> Lazy.ByteString -> Lazy.ByteString)
    , err       :: ErrorHandling VolatileDBError m
    , errSTM    :: ThrowCantCatch VolatileDBError (STM m)
    }

-- Universal type; we can't use generics
instance NoUnexpectedThunks (VolDB m blk) where
  showTypeOf _ = "VolDB"
  whnfNoUnexpectedThunks ctxt VolDB {..} = allNoUnexpectedThunks
    [ noUnexpectedThunks ctxt volDB
    , noUnexpectedThunks ctxt decHeader
    , noUnexpectedThunks ctxt decBlock
    , noUnexpectedThunks ctxt encBlock
    , noUnexpectedThunks ctxt isEBB
    , noUnexpectedThunks ctxt addHdrEnv
    , noUnexpectedThunks ctxt err
    , noUnexpectedThunks ctxt errSTM
    ]

{-------------------------------------------------------------------------------
  Initialization
-------------------------------------------------------------------------------}

data VolDbArgs m blk = forall h. VolDbArgs {
      volHasFS         :: HasFS m h
    , volErr           :: ErrorHandling VolatileDBError m
    , volErrSTM        :: ThrowCantCatch VolatileDBError (STM m)
    , volBlocksPerFile :: Int
    , volDecodeHeader  :: forall s. Decoder s (Lazy.ByteString -> Header blk)
    , volDecodeBlock   :: forall s. Decoder s (Lazy.ByteString -> blk)
    , volEncodeBlock   :: blk -> BinaryInfo Encoding
    , volIsEBB         :: blk -> IsEBB
    , volAddHdrEnv     :: IsEBB -> Word32 -> Lazy.ByteString -> Lazy.ByteString
    }

-- | Default arguments when using the 'IO' monad
--
-- The following fields must still be defined:
--
-- * 'volBlocksPerFile'
-- * 'volDecodeHeader'
-- * 'volDecodeBlock'
-- * 'volEncodeBlock'
-- * 'volIsEBB'
-- * 'volAddHdrEnv'
defaultArgs :: FilePath -> VolDbArgs IO blk
defaultArgs fp = VolDbArgs {
      volErr    = EH.exceptions
    , volErrSTM = EH.throwSTM
    , volHasFS  = ioHasFS $ MountPoint (fp </> "volatile")
      -- Fields without a default
    , volBlocksPerFile = error "no default for volBlocksPerFile"
    , volDecodeHeader  = error "no default for volDecodeHeader"
    , volDecodeBlock   = error "no default for volDecodeBlock"
    , volEncodeBlock   = error "no default for volEncodeBlock"
    , volIsEBB         = error "no default for volIsEBB"
    , volAddHdrEnv     = error "no default for volAddHdrEnv"
    }

openDB :: (IOLike m, HasHeader blk) => VolDbArgs m blk -> m (VolDB m blk)
openDB args@VolDbArgs{..} = do
    createDirectoryIfMissing volHasFS True (mkFsPath [])
    volDB <- VolDB.openDB
               volHasFS
               volErr
               volErrSTM
               (blockFileParser args)
               volBlocksPerFile
    return VolDB
      { volDB     = volDB
      , err       = volErr
      , errSTM    = volErrSTM
      , decHeader = volDecodeHeader
      , decBlock  = volDecodeBlock
      , encBlock  = volEncodeBlock
      , addHdrEnv = volAddHdrEnv
      , isEBB     = volIsEBB
      }

-- | For testing purposes
mkVolDB :: VolatileDB (HeaderHash blk) m
        -> (forall s. Decoder s (Lazy.ByteString -> Header blk))
        -> (forall s. Decoder s (Lazy.ByteString -> blk))
        -> (blk -> BinaryInfo Encoding)
        -> (blk -> IsEBB)
        -> (IsEBB -> Word32 -> Lazy.ByteString -> Lazy.ByteString)
        -> ErrorHandling VolatileDBError m
        -> ThrowCantCatch VolatileDBError (STM m)
        -> VolDB m blk
mkVolDB volDB decHeader decBlock encBlock isEBB addHdrEnv err errSTM = VolDB {..}

{-------------------------------------------------------------------------------
  Wrappers
-------------------------------------------------------------------------------}

getIsMember :: VolDB m blk -> STM m (HeaderHash blk -> Bool)
getIsMember db = withSTM db VolDB.getIsMember

getPredecessor :: VolDB m blk
               -> STM m (HeaderHash blk -> WithOrigin (HeaderHash blk))
getPredecessor db = withSTM db VolDB.getPredecessor

getSuccessors :: VolDB m blk
              -> STM m (WithOrigin (HeaderHash blk) -> Set (HeaderHash blk))
getSuccessors db = withSTM db VolDB.getSuccessors

getMaxSlotNo :: VolDB m blk
             -> STM m MaxSlotNo
getMaxSlotNo db = withSTM db VolDB.getMaxSlotNo

putBlock :: (MonadCatch m, HasHeader blk) => VolDB m blk -> blk -> m ()
putBlock db@VolDB{..} b = withDB db $ \vol ->
    VolDB.putBlock vol (extractInfo isEBB binInfo b) binaryBlob
  where
    binInfo@BinaryInfo { binaryBlob } = CBOR.toBuilder <$> encBlock b

closeDB :: (MonadCatch m, HasCallStack) => VolDB m blk -> m ()
closeDB db = withDB db VolDB.closeDB

reopen :: (MonadCatch m, HasCallStack) => VolDB m blk -> m ()
reopen db = withDB db VolDB.reOpenDB

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
     (WithOrigin (HeaderHash blk) -> Set (HeaderHash blk)) -- ^ @getSuccessors@
  -> Point blk                                             -- ^ @B@
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
            => (HeaderHash blk -> WithOrigin (HeaderHash blk))  -- ^ @getPredecessor@
            -> (HeaderHash blk -> Bool)                         -- ^ @isMember@
            -> Point blk                                        -- ^ @I@
            -> Point blk                                        -- ^ @B@
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
  => (HeaderHash blk -> WithOrigin (HeaderHash blk)) -- ^ @getPredecessor@
  -> (HeaderHash blk -> Bool)                        -- ^ @isMember@
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
      Origin -> case from of
        StreamFromInclusive _
          -> Nothing
        StreamFromExclusive GenesisPoint
          -> return $ CompletelyInVolDB acc
        StreamFromExclusive (BlockPoint {})
          -> Nothing

      At predHash
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
  :: forall m blk. (IOLike m, HasHeader blk)
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

getKnownBlock
  :: (MonadCatch m, HasHeader blk)
  => VolDB m blk
  -> HeaderHash blk
  -> m blk
getKnownBlock volDB = join . getKnownBlockComponent volDB GetBlock

getKnownHeader
  :: (MonadCatch m, HasHeader blk)
  => VolDB m blk
  -> HeaderHash blk
  -> m (Header blk)
getKnownHeader volDB = join . getKnownBlockComponent volDB GetHeader

getKnownBlockComponent
  :: (MonadCatch m, HasHeader blk)
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
  :: forall m blk b. (MonadCatch m, HasHeader blk)
  => VolDB m blk
  -> BlockComponent (ChainDB m blk) b
  -> HeaderHash blk
  -> m (Maybe b)
getBlockComponent db blockComponent hash = withDB db $ \vol ->
    VolDB.getBlockComponent vol blockComponent' hash
  where
    blockComponent' = translateToRawDB (parse db) (addHdrEnv db) blockComponent

{-------------------------------------------------------------------------------
  Auxiliary: parsing
-------------------------------------------------------------------------------}

blockFileParser :: forall m blk. (IOLike m, HasHeader blk)
                => VolDbArgs m blk
                -> VolDB.Parser
                     Util.CBOR.ReadIncrementalErr
                     m
                     (HeaderHash blk)
blockFileParser VolDbArgs{..} = VolDB.Parser $
       fmap (fmap (fmap fst)) -- Drop the offset of the error
     . Util.CBOR.readIncrementalOffsets volHasFS decoder'
  where
    -- TODO: It looks weird that we use an encoding function 'volEncodeBlock'
    -- during parsing, but this is quite cheap, since the encoding is already
    -- cached. We should consider improving this, so that it does not create
    -- confusion.
    decoder' :: forall s. Decoder s (Lazy.ByteString
             -> VolDB.BlockInfo (HeaderHash blk))
    decoder' = ((\blk -> extractInfo volIsEBB (volEncodeBlock blk) blk) .)
      <$> volDecodeBlock

{-------------------------------------------------------------------------------
  Error handling
-------------------------------------------------------------------------------}

-- | Wrap calls to the VolatileDB and rethrow exceptions that may indicate
-- disk failure and should therefore trigger recovery
withDB :: forall m blk x. MonadThrow m
       => VolDB m blk
       -> (VolatileDB (HeaderHash blk) m -> m x)
       -> m x
withDB VolDB{..} k = EH.catchError err (k volDB) rethrow
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

-- TODO unify with ImmDB.parse
parse :: forall m blk b. (HasHeader blk, MonadThrow m)
      => VolDB m blk
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
      | otherwise    = throwM $ VolDbTrailingData blockRef bs
    aux (Left err)   = throwM $ VolDbParseFailure blockRef err

{-------------------------------------------------------------------------------
  Auxiliary
-------------------------------------------------------------------------------}

fromChainHash :: ChainHash blk -> WithOrigin (HeaderHash blk)
fromChainHash GenesisHash      = Origin
fromChainHash (BlockHash hash) = At hash

extractInfo :: HasHeader blk
            => (blk -> IsEBB)
            -> BinaryInfo a
            -> blk
            -> VolDB.BlockInfo (HeaderHash blk)
extractInfo isEBB BinaryInfo{..} b = VolDB.BlockInfo {
      bbid          = blockHash b
    , bslot         = blockSlot b
    , bpreBid       = fromChainHash (blockPrevHash b)
    , bisEBB        = isEBB b
    , bheaderOffset = headerOffset
    , bheaderSize   = headerSize
    }
