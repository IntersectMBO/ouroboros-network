{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE DuplicateRecordFields      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiWayIf                 #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}

module Ouroboros.Consensus.Storage.ImmutableDB.Impl.Index.Cache
  ( -- * Environment
    CacheEnv
  , newEnv
  , CacheConfig (..)
  , checkInvariants
    -- * Background thread
  , expireUnusedChunks
    -- * Operations
  , close
  , restart
    -- ** On the primary index
  , readOffsets
  , readFirstFilledSlot
  , openPrimaryIndex
  , appendOffsets
    -- ** On the secondary index
  , readEntries
  , readAllEntries
  , appendEntry
  ) where

import           Control.Exception (assert)
import           Control.Monad (forM, forM_, forever, mplus, unless, void, when)
import           Control.Monad.Except (throwError)
import           Control.Tracer (Tracer, traceWith)
import           Data.Foldable (toList)
import           Data.Functor ((<&>))
import           Data.IntPSQ (IntPSQ)
import qualified Data.IntPSQ as PSQ
import           Data.Maybe (fromMaybe)
import           Data.Sequence.Strict (StrictSeq)
import qualified Data.Sequence.Strict as Seq
import           Data.Vector (Vector)
import qualified Data.Vector as Vector
import           Data.Void (Void)
import           Data.Word (Word32, Word64)
import           GHC.Generics (Generic)
import           GHC.Stack (HasCallStack, callStack)

import           Cardano.Prelude (NoUnexpectedThunks (..), forceElemsToWHNF,
                     unsafeNoUnexpectedThunks)

import           Ouroboros.Consensus.Block (IsEBB (..))
import           Ouroboros.Consensus.Util (whenJust)
import           Ouroboros.Consensus.Util.IOLike
import           Ouroboros.Consensus.Util.MonadSTM.NormalForm (tryPutMVar)
import qualified Ouroboros.Consensus.Util.MonadSTM.StrictMVar as Strict
import           Ouroboros.Consensus.Util.ResourceRegistry

import           Ouroboros.Consensus.Storage.FS.API (HasFS (..), withFile)
import           Ouroboros.Consensus.Storage.FS.API.Types (AllowExisting (..),
                     Handle, OpenMode (ReadMode))

import           Ouroboros.Consensus.Storage.ImmutableDB.Chunks.Internal
import           Ouroboros.Consensus.Storage.ImmutableDB.Chunks.Layout
import           Ouroboros.Consensus.Storage.ImmutableDB.Impl.Index.Primary
                     (PrimaryIndex, SecondaryOffset)
import qualified Ouroboros.Consensus.Storage.ImmutableDB.Impl.Index.Primary as Primary
import           Ouroboros.Consensus.Storage.ImmutableDB.Impl.Index.Secondary
                     (BlockSize (..))
import qualified Ouroboros.Consensus.Storage.ImmutableDB.Impl.Index.Secondary as Secondary
import           Ouroboros.Consensus.Storage.ImmutableDB.Impl.Util
                     (fsPathChunkFile, fsPathPrimaryIndexFile,
                     fsPathSecondaryIndexFile, throwUnexpectedError)
import           Ouroboros.Consensus.Storage.ImmutableDB.Types (HashInfo (..),
                     TraceCacheEvent (..), UnexpectedError (..),
                     WithBlockSize (..))

-- TODO property and/or q-s-m tests comparing with 'fileBackedIndex'

{------------------------------------------------------------------------------
  Environment
------------------------------------------------------------------------------}

data CacheConfig = CacheConfig
  { pastChunksToCache :: Word32
    -- ^ Maximum number of past chunks to cache, excluding the current chunk.
    --
    -- NOTE: must be > 0
  , expireUnusedAfter :: DiffTime
    -- ^ Expire past chunks that haven't been used for 'expireUnusedAfter'
    -- from the cache, regardless the number of past chunks in the cache.
  }
  deriving (Eq, Show)

-- | Short-hand we use internally
type Entry hash = WithBlockSize (Secondary.Entry hash)

-- | The cached primary and secondary indices of the current chunk.
--
-- We use sequences (as opposed to vectors) to allow for efficient appending
-- in addition to (reasonably) efficient indexing.
data CurrentChunkInfo hash = CurrentChunkInfo
  { currentChunkNo      :: !ChunkNo
  , currentChunkOffsets :: !(StrictSeq SecondaryOffset)
  , currentChunkEntries :: !(StrictSeq (Entry hash))
  }
  deriving (Generic, NoUnexpectedThunks, Show)

emptyCurrentChunkInfo :: ChunkNo -> CurrentChunkInfo hash
emptyCurrentChunkInfo chunk = CurrentChunkInfo
  { currentChunkNo      = chunk
  , currentChunkOffsets = Seq.singleton 0
  , currentChunkEntries = Seq.empty
  }

-- | Convert a 'CurrentChunkInfo' to a 'PastChunkInfo'
--
-- TODO don't bother with the conversion? Use vectors for past chunks at start
-- up. Chunks that become past chunks because we advance to new chunks, we can
-- just leave in memory as seqs?
toPastChunkInfo :: CurrentChunkInfo hash -> PastChunkInfo hash
toPastChunkInfo CurrentChunkInfo{..} =
    PastChunkInfo
      { pastChunkOffsets =
          fromMaybe (error "invalid current chunk") $
          Primary.mk currentChunkNo (toList currentChunkOffsets)
      , pastChunkEntries =
          -- TODO optimise this
          Vector.fromList $ toList currentChunkEntries
      }

-- | The cached primary and secondary indices of an chunk in the past.
--
-- We use vectors to allow for efficient indexing. We don't need to append to
-- them, as they are in the past and thus immutable.
data PastChunkInfo hash = PastChunkInfo
  { pastChunkOffsets :: !PrimaryIndex
  , pastChunkEntries :: !(Vector (Entry hash))
  }
  deriving (Generic, NoUnexpectedThunks)

-- | The last time a cached past chunk was accessed.
--
-- We care about the ordering /and/ the absolute times so we can also evict
-- chunks when they haven't been used for @x@ seconds or minutes.
newtype LastUsed = LastUsed Time
  deriving newtype (Eq, Ord, Show, NoUnexpectedThunks)

-- | The data stored in the cache.
data Cached hash = Cached
  { currentChunk     :: !ChunkNo
    -- ^ The current chunk of the ImmutableDB, i.e., the chunk we're still
    -- appending entries too.
  , currentChunkInfo :: !(CurrentChunkInfo hash)
    -- ^ We always cache the current chunk.
    --
    -- When clients are in sync with our chain, they will only request blocks
    -- from the current chunk, so it is worth optimising this case.
    -- Additionally, by appending to the current chunk through the cache, we
    -- are sure the current chunk info is never stale.
    --
    -- We use an 'IntPSQ' here, where the keys are in fact chunk numbers. Since
    -- chunk numbers are internally represented by a 'Word64', one might be worried
    -- about a potential overflow here. While possible, it's not worth worrying about:
    -- - Whilst guaranteed to be only at least 30 bits, in practice, 64-bit GHC has 64-bit
    --   integers, so the conversion is bijective.
    -- - An chunk currently lasts around a week. Systems using a smaller representation
    --   might need to worry in a million years or so.
    -- - In the event of running for a million years, we're unlikely to have a problem anyway,
    --   since we only really cache _recent_ chunks. So the fact that they clash with the
    --   chunks from a million years ago isn't likely to be an issue.
  , pastChunksInfo   :: !(IntPSQ LastUsed (PastChunkInfo hash))
    -- ^ Cached chunks from the past.
    --
    -- A LRU-cache (least recently used). Whenever a we get a cache hit
    -- ('getChunkInfo') for a past chunk, we change its 'LastUsed' priority to
    -- the current time. When the cache is full, see 'pastChunksToCache', we
    -- will remove the chunk with the lowest priority, i.e. the least recently
    -- used past chunk.
    --
    -- INVARIANT: all past chunks are < 'currentChunk'
    --
    -- INVARIANT: @'PSQ.size' 'pastChunksInfo' <= 'pastChunksToCache'@
  , nbPastChunks     :: !Word32
    -- ^ Cached size of 'pastChunksInfo', as an 'IntPSQ' only provides a \(O(n)
    -- \) 'PSQ.size' operation.
    --
    -- INVARIANT: 'nbPastChunks' == @'PSQ.size' 'pastChunksInfo'@
  }
  deriving (Generic, NoUnexpectedThunks)

checkInvariants
  :: Word32  -- ^ Maximum number of past chunks to cache
  -> Cached hash
  -> Maybe String
checkInvariants pastChunksToCache Cached {..} = either Just (const Nothing) $ do
    forM_ (PSQ.keys pastChunksInfo) $ \pastChunk ->
      unless (pastChunk < chunkNoToInt currentChunk) $
        throwError $
          "past chunk (" <> show pastChunk <> ") >= current chunk (" <>
          show currentChunk <> ")"

    unless (PSQ.size pastChunksInfo <= fromIntegral pastChunksToCache) $
      throwError $
        "PSQ.size pastChunksInfo (" <> show (PSQ.size pastChunksInfo) <>
        ") > pastChunksToCache (" <> show pastChunksToCache <> ")"

    unless (nbPastChunks == fromIntegral (PSQ.size pastChunksInfo)) $
      throwError $
        "nbPastChunks (" <> show nbPastChunks <>
        ") /= PSQ.size pastChunksInfo (" <> show (PSQ.size pastChunksInfo) <>
        ")"


-- | Store the 'PastChunkInfo' for the given 'ChunkNo' in 'Cached'.
--
-- Uses the 'LastUsed' as the priority.
--
-- NOTE: does not trim the cache.
--
-- PRECONDITION: the given 'ChunkNo' is < the 'currentChunk'.
addPastChunkInfo
  :: ChunkNo
  -> LastUsed
  -> PastChunkInfo hash
  -> Cached hash
  -> Cached hash
addPastChunkInfo chunk lastUsed pastChunkInfo cached =
    assert (chunk < currentChunk cached) $
    -- NOTE: in case of multiple concurrent cache misses of the same chunk,
    -- we might add the same past chunk multiple times to the cache. This
    -- means the following cannot be a precondition:
    -- assert (not (PSQ.member chunk pastChunksInfo)) $
    cached
      { pastChunksInfo = pastChunksInfo'
      , nbPastChunks   = nbPastChunks'
      }
  where
    Cached { pastChunksInfo, nbPastChunks } = cached

    -- In case of multiple concurrent cache misses of the same chunk, the
    -- chunk might already be in there.
    (mbAlreadyPresent, pastChunksInfo') =
      PSQ.insertView (chunkNoToInt chunk) lastUsed pastChunkInfo pastChunksInfo

    nbPastChunks'
      | Just _ <- mbAlreadyPresent
      = nbPastChunks
      | otherwise
      = succ nbPastChunks

-- | Remove the least recently used past chunk from the cache when 'Cached'
-- contains more chunks than the given maximum.
--
-- PRECONDITION: 'nbPastChunks' + 1 <= given maximum. In other words, 'Cached'
-- contains at most one chunk too many. We ensure this by calling this
-- function directly after adding a past chunk to 'Cached'.
--
-- If a past chunk was evicted, its chunk number is returned.
evictIfNecessary
  :: Word32  -- ^ Maximum number of past chunks to cache
  -> Cached hash
  -> (Cached hash, Maybe ChunkNo)
evictIfNecessary maxNbPastChunks cached
    | nbPastChunks > maxNbPastChunks
    = assert (nbPastChunks == maxNbPastChunks + 1) $
      case PSQ.minView pastChunksInfo of
        Nothing                                 -> error
          "nbPastChunks > maxNbPastChunks but pastChunksInfo was empty"
        Just (chunkNo, _p, _v, pastChunksInfo') ->
            (cached', Just $ chunkNoFromInt chunkNo)
          where
            cached' = cached
              { nbPastChunks   = maxNbPastChunks
              , pastChunksInfo = pastChunksInfo'
              }
    | otherwise
    = (cached, Nothing)
  where
    Cached { nbPastChunks, pastChunksInfo } = cached
-- NOTE: we must inline 'evictIfNecessary' otherwise we get unexplained thunks
-- in 'Cached' and thus a space leak. Alternatively, we could disable the
-- @-fstrictness@ optimisation (enabled by default for -O1).
{-# INLINE evictIfNecessary #-}

lookupPastChunkInfo
  :: ChunkNo
  -> LastUsed
  -> Cached hash
  -> Maybe (PastChunkInfo hash, Cached hash)
lookupPastChunkInfo chunk lastUsed cached@Cached { pastChunksInfo } =
    case PSQ.alter lookupAndUpdateLastUsed (chunkNoToInt chunk) pastChunksInfo of
      (Nothing, _) -> Nothing
      (Just pastChunkInfo, pastChunksInfo') -> Just (pastChunkInfo, cached')
        where
          cached' = cached { pastChunksInfo = pastChunksInfo' }
  where
    lookupAndUpdateLastUsed
      :: Maybe (LastUsed, PastChunkInfo hash)
      -> (Maybe (PastChunkInfo hash), Maybe (LastUsed, PastChunkInfo hash))
    lookupAndUpdateLastUsed = \case
      Nothing                -> (Nothing, Nothing)
      Just (_lastUsed, info) -> (Just info, Just (lastUsed, info))

openChunk
  :: ChunkNo
  -> LastUsed
  -> CurrentChunkInfo hash
  -> Cached hash
  -> Cached hash
openChunk chunk lastUsed newCurrentChunkInfo cached
    | currentChunk == chunk
    = cached
        { currentChunkInfo = newCurrentChunkInfo }

    | nextChunkNo currentChunk == chunk
    = Cached
        { currentChunk     = chunk
        , currentChunkInfo = newCurrentChunkInfo
          -- We use 'lastUsed' for the current chunk that has now become a
          -- "past" chunk, which means that that chunk is most recently used
          -- one. When clients are roughly in sync with us, when we switch to a
          -- new chunk, they might still request blocks from the previous one.
          -- So to avoid throwing away that cached information, we give it the
          -- highest priority.
        , pastChunksInfo   = PSQ.insert (chunkNoToInt currentChunk) lastUsed
            (toPastChunkInfo currentChunkInfo) pastChunksInfo
        , nbPastChunks     = succ nbPastChunks
        }

    | otherwise
    = error $ "Going from chunk " <> show currentChunk <> " to " <> show chunk
  where
    Cached
      { currentChunk, currentChunkInfo, pastChunksInfo, nbPastChunks
      } = cached

emptyCached
  :: ChunkNo -- ^ The current chunk
  -> CurrentChunkInfo hash
  -> Cached hash
emptyCached currentChunk currentChunkInfo = Cached
    { currentChunk
    , currentChunkInfo
    , pastChunksInfo = PSQ.empty
    , nbPastChunks   = 0
    }

-- | Environment used by functions operating on the cached index.
data CacheEnv m hash h = CacheEnv
  { hasFS       :: HasFS m h
  , hashInfo    :: HashInfo hash
  , registry    :: ResourceRegistry m
  , tracer      :: Tracer m TraceCacheEvent
  , cacheVar    :: StrictMVar m (Cached hash)
  , cacheConfig :: CacheConfig
  , bgThreadVar :: StrictMVar m (Maybe (Thread m Void))
    -- ^ Nothing if no thread running
  , chunkInfo   :: ChunkInfo
  }

-- | Creates a new 'CacheEnv' and launches a background thread that expires
-- unused past chunks ('expireUnusedChunks').
--
-- PRECONDITION: 'pastChunksToCache' (in 'CacheConfig') > 0
newEnv
  :: (HasCallStack, IOLike m, NoUnexpectedThunks hash)
  => HasFS m h
  -> HashInfo hash
  -> ResourceRegistry m
  -> Tracer m TraceCacheEvent
  -> CacheConfig
  -> ChunkInfo
  -> ChunkNo  -- ^ Current chunk
  -> m (CacheEnv m hash h)
newEnv hasFS hashInfo registry tracer cacheConfig chunkInfo chunk = do
    when (pastChunksToCache == 0) $
      error "pastChunksToCache must be > 0"

    currentChunkInfo <- loadCurrentChunkInfo hasFS chunkInfo hashInfo chunk
    cacheVar <- newMVarWithInvariants $ emptyCached chunk currentChunkInfo
    bgThreadVar <- newMVar Nothing
    let cacheEnv = CacheEnv {..}
    mask_ $ modifyMVar_ bgThreadVar $ \_mustBeNothing -> do
      !bgThread <- forkLinkedThread registry "ImmutableDB.expireUnusedChunks" $
        expireUnusedChunks cacheEnv
      return $ Just bgThread
    return cacheEnv
  where
    CacheConfig { pastChunksToCache } = cacheConfig

    -- When checking invariants, check both our invariants and for thunks.
    -- Note that this is only done when the corresponding flag is enabled.
    newMVarWithInvariants =
      Strict.newMVarWithInvariant $ \cached ->
        checkInvariants pastChunksToCache cached
        `mplus`
        unsafeNoUnexpectedThunks cached

{------------------------------------------------------------------------------
  Background thread
------------------------------------------------------------------------------}

-- | Intended to run as a background thread.
--
-- Will expire past chunks that haven't been used for 'expireUnusedAfter' from
-- the cache.
expireUnusedChunks
  :: (HasCallStack, IOLike m)
  => CacheEnv m hash h
  -> m Void
expireUnusedChunks CacheEnv { cacheVar, cacheConfig, tracer } =
    forever $ do
      now <- getMonotonicTime
      mbTraceMsg <- updateMVar cacheVar $ garbageCollect now
      mapM_ (traceWith tracer) mbTraceMsg
      threadDelay expireUnusedAfter
  where
    CacheConfig { expireUnusedAfter } = cacheConfig

    -- | Remove the least recently used past chunk from 'Cached' /if/ it
    -- hasn't been used for 'expireUnusedAfter', otherwise the original
    -- 'Cached' is returned.
    --
    -- In case a 'TracePastChunksExpired' event should be traced, it is
    -- returned as a 'Just'.
    garbageCollect
      :: Time
      -> Cached hash
      -> (Cached hash, Maybe TraceCacheEvent)
    garbageCollect now cached@Cached { pastChunksInfo, nbPastChunks } =
        case expiredPastChunks of
          [] -> (cached,  Nothing)
          _  -> (cached', Just traceMsg)
      where
        -- Every past chunk last used before (or at) this time, must be
        -- expired.
        expiredLastUsedTime :: LastUsed
        expiredLastUsedTime = LastUsed $
          Time (now `diffTime` Time expireUnusedAfter)

        (expiredPastChunks, pastChunksInfo') =
          PSQ.atMostView expiredLastUsedTime pastChunksInfo

        nbPastChunks' = nbPastChunks - fromIntegral (length expiredPastChunks)

        cached' = cached
          { pastChunksInfo = pastChunksInfo'
          , nbPastChunks   = nbPastChunks'
          }

        !traceMsg = TracePastChunksExpired
          -- Force this list, otherwise the traced message holds onto to the
          -- past chunk indices.
          (forceElemsToWHNF
            [ chunkNoFromInt $ chunk
            | (chunk, _, _) <- expiredPastChunks
            ])
          nbPastChunks'

{------------------------------------------------------------------------------
  Reading indices
------------------------------------------------------------------------------}

readPrimaryIndex
  :: (HasCallStack, IOLike m)
  => HasFS m h
  -> ChunkInfo
  -> ChunkNo
  -> m (PrimaryIndex, IsEBB)
     -- ^ The primary index and whether it starts with an EBB or not
readPrimaryIndex hasFS chunkInfo chunk = do
    primaryIndex <- Primary.load hasFS chunk
    let firstIsEBB
          | Primary.containsSlot primaryIndex firstRelativeSlot
          , Primary.isFilledSlot primaryIndex firstRelativeSlot
          = relativeSlotIsEBB firstRelativeSlot
          | otherwise
          = IsNotEBB
    return (primaryIndex, firstIsEBB)
  where
    firstRelativeSlot :: RelativeSlot
    firstRelativeSlot = firstBlockOrEBB chunkInfo chunk

readSecondaryIndex
  :: (HasCallStack, IOLike m)
  => HasFS m h
  -> HashInfo hash
  -> ChunkNo
  -> IsEBB
  -> m [Entry hash]
readSecondaryIndex hasFS@HasFS { hGetSize } hashInfo chunk firstIsEBB = do
    !chunkFileSize <- withFile hasFS chunkFile ReadMode hGetSize
    Secondary.readAllEntries hasFS hashInfo secondaryOffset
      chunk stopCondition chunkFileSize firstIsEBB
  where
    chunkFile = fsPathChunkFile chunk
    -- Read from the start
    secondaryOffset = 0
    -- Don't stop until the end
    stopCondition = const False

loadCurrentChunkInfo
  :: (HasCallStack, IOLike m)
  => HasFS m h
  -> ChunkInfo
  -> HashInfo hash
  -> ChunkNo
  -> m (CurrentChunkInfo hash)
loadCurrentChunkInfo hasFS chunkInfo hashInfo chunk = do
    -- We're assuming that when the primary index file exists, the secondary
    -- index file will also exist
    chunkExists <- doesFileExist hasFS primaryIndexFile
    if chunkExists then do
      (primaryIndex, firstIsEBB) <- readPrimaryIndex hasFS chunkInfo chunk
      entries <- readSecondaryIndex hasFS hashInfo chunk firstIsEBB
      return CurrentChunkInfo
        { currentChunkNo      = chunk
        , currentChunkOffsets =
          -- TODO optimise this
            Seq.fromList . Primary.toSecondaryOffsets $ primaryIndex
        , currentChunkEntries = Seq.fromList entries
        }
    else
      return $ emptyCurrentChunkInfo chunk
  where
    primaryIndexFile = fsPathPrimaryIndexFile chunk

loadPastChunkInfo
  :: (HasCallStack, IOLike m)
  => HasFS m h
  -> ChunkInfo
  -> HashInfo hash
  -> ChunkNo
  -> m (PastChunkInfo hash)
loadPastChunkInfo hasFS chunkInfo hashInfo chunk = do
    (primaryIndex, firstIsEBB) <- readPrimaryIndex hasFS chunkInfo chunk
    entries <- readSecondaryIndex hasFS hashInfo chunk firstIsEBB
    return PastChunkInfo
      { pastChunkOffsets = primaryIndex
      , pastChunkEntries = Vector.fromList $ forceElemsToWHNF entries
      }

getChunkInfo
  :: (HasCallStack, IOLike m)
  => CacheEnv m hash h
  -> ChunkNo
  -> m (Either (CurrentChunkInfo hash) (PastChunkInfo hash))
getChunkInfo cacheEnv chunk = do
    lastUsed <- LastUsed <$> getMonotonicTime
    -- Make sure we don't leave an empty MVar in case of an exception.
    mbCacheHit <- bracketOnError (takeMVar cacheVar) (tryPutMVar cacheVar) $
      \cached@Cached { currentChunk, currentChunkInfo, nbPastChunks } -> if
        | chunk == currentChunk -> do
          -- Cache hit for the current chunk
          putMVar cacheVar cached
          traceWith tracer $ TraceCurrentChunkHit chunk nbPastChunks
          return $ Just $ Left currentChunkInfo
        | Just (pastChunkInfo, cached') <- lookupPastChunkInfo chunk lastUsed cached -> do
          -- Cache hit for an chunk in the past
          putMVar cacheVar cached'
          traceWith tracer $ TracePastChunkHit chunk nbPastChunks
          return $ Just $ Right pastChunkInfo
        | otherwise -> do
          -- Cache miss for an chunk in the past. We don't want to hold on to
          -- the 'cacheVar' MVar, blocking all other access to the cace, while
          -- we're reading things from disk, so put it back now and update the
          -- cache afterwards.
          putMVar cacheVar cached
          traceWith tracer $ TracePastChunkMiss chunk nbPastChunks
          return Nothing
    case mbCacheHit of
      Just hit -> return hit
      Nothing  -> do
        -- Cache miss, load both entire indices for the chunk from disk.
        pastChunkInfo <- loadPastChunkInfo hasFS chunkInfo hashInfo chunk
        -- Loading the chunk might have taken some time, so obtain the time
        -- again.
        lastUsed' <- LastUsed <$> getMonotonicTime
        mbEvicted <- updateMVar cacheVar $
          evictIfNecessary pastChunksToCache .
          addPastChunkInfo chunk lastUsed' pastChunkInfo
        whenJust mbEvicted $ \evicted ->
          -- If we had to evict, we are at 'pastChunksToCache'
          traceWith tracer $ TracePastChunkEvict evicted pastChunksToCache
        return $ Right pastChunkInfo
  where
    CacheEnv { hasFS, hashInfo, cacheVar, cacheConfig, tracer, chunkInfo } = cacheEnv
    CacheConfig { pastChunksToCache } = cacheConfig

{------------------------------------------------------------------------------
  Operations
------------------------------------------------------------------------------}

-- | Stops the background expiration thread.
--
-- This operation is idempotent.
close :: IOLike m => CacheEnv m hash h -> m ()
close CacheEnv { bgThreadVar } =
    mask_ $ modifyMVar_ bgThreadVar $ \mbBgThread -> do
      mapM_ cancelThread mbBgThread
      return Nothing

-- | Restarts the background expiration thread, drops all previously cached
-- information, loads the given chunk.
--
-- PRECONDITION: the background thread expiring unused past chunks must have
-- been terminated.
restart
  :: IOLike m
  => CacheEnv m hash h
  -> ChunkNo  -- ^ The new current chunk
  -> m ()
restart cacheEnv chunk = do
    currentChunkInfo <- loadCurrentChunkInfo hasFS chunkInfo hashInfo chunk
    void $ swapMVar cacheVar $ emptyCached chunk currentChunkInfo
    mask_ $ modifyMVar_ bgThreadVar $ \mbBgThread ->
      case mbBgThread of
        Just _  -> throwM $ userError "background thread still running"
        Nothing -> do
          !bgThread <- forkLinkedThread registry "ImmutableDB.expireUnusedChunks" $
            expireUnusedChunks cacheEnv
          return $ Just bgThread
  where
    CacheEnv { hasFS, hashInfo, registry, cacheVar, bgThreadVar, chunkInfo } = cacheEnv

{------------------------------------------------------------------------------
  On the primary index
------------------------------------------------------------------------------}

readOffsets
  :: (HasCallStack, Traversable t, IOLike m)
  => CacheEnv m hash h
  -> ChunkNo
  -> t RelativeSlot
  -> m (t (Maybe SecondaryOffset))
readOffsets cacheEnv chunk relSlots =
    getChunkInfo cacheEnv chunk <&> \case
      Left CurrentChunkInfo { currentChunkOffsets } ->
        getOffsetFromSecondaryOffsets currentChunkOffsets <$> relSlots
      Right PastChunkInfo { pastChunkOffsets } ->
        getOffsetFromPrimaryIndex pastChunkOffsets <$> relSlots
  where
    getOffsetFromSecondaryOffsets
      :: StrictSeq SecondaryOffset
      -> RelativeSlot
      -> Maybe SecondaryOffset
    getOffsetFromSecondaryOffsets offsets relSlot =
      let s = assertRelativeSlotInChunk chunk relSlot in
      case Seq.splitAt (fromIntegral s + 1) offsets of
        (_ Seq.:|> offset, offsetAfter Seq.:<| _)
          | offset /= offsetAfter
            -- The slot is not empty
          -> Just offset
        _ -> Nothing

    getOffsetFromPrimaryIndex
      :: PrimaryIndex
      -> RelativeSlot
      -> Maybe SecondaryOffset
    getOffsetFromPrimaryIndex index relSlot
      | Primary.containsSlot  index relSlot
      , Primary.isFilledSlot  index relSlot
      = Just $ Primary.offsetOfSlot index relSlot
      | otherwise
      = Nothing

readFirstFilledSlot
  :: (HasCallStack, IOLike m)
  => CacheEnv m hash h
  -> ChunkNo
  -> m (Maybe RelativeSlot)
readFirstFilledSlot cacheEnv chunk =
    getChunkInfo cacheEnv chunk <&> \case
      Left CurrentChunkInfo { currentChunkOffsets } ->
        firstFilledSlotInSeq currentChunkOffsets
      Right PastChunkInfo { pastChunkOffsets } ->
        Primary.firstFilledSlot chunkInfo pastChunkOffsets
  where
    CacheEnv { chunkInfo } = cacheEnv

    firstFilledSlotInSeq :: StrictSeq SecondaryOffset -> Maybe RelativeSlot
    firstFilledSlotInSeq = fmap indexToRelativeSlot . Seq.findIndexL (/= 0)
      where
        indexToRelativeSlot :: Int -> RelativeSlot
        indexToRelativeSlot = mkRelativeSlot chunkInfo chunk . fromIntegral . pred

-- | This is called when a new chunk is started, which means we need to update
-- 'Cached' to reflect this.
openPrimaryIndex
  :: (HasCallStack, IOLike m)
  => CacheEnv m hash h
  -> ChunkNo
  -> AllowExisting
  -> m (Handle h)
openPrimaryIndex cacheEnv chunk allowExisting = do
    lastUsed <- LastUsed <$> getMonotonicTime
    pHnd <- Primary.open hasFS chunk allowExisting
    -- Don't leak the handle in case of an exception
    flip onException (hClose pHnd) $ do
      newCurrentChunkInfo <- case allowExisting of
        MustBeNew     -> return $ emptyCurrentChunkInfo chunk
        AllowExisting -> loadCurrentChunkInfo hasFS chunkInfo hashInfo chunk
      mbEvicted <- updateMVar cacheVar $
        evictIfNecessary pastChunksToCache .
        openChunk chunk lastUsed newCurrentChunkInfo
      whenJust mbEvicted $ \evicted ->
        -- If we had to evict, we are at 'pastChunksToCache'
        traceWith tracer $ TracePastChunkEvict evicted pastChunksToCache
      return pHnd
  where
    CacheEnv { hasFS, hashInfo, cacheVar, cacheConfig, tracer, chunkInfo } = cacheEnv
    HasFS { hClose } = hasFS
    CacheConfig { pastChunksToCache } = cacheConfig

appendOffsets
  :: (HasCallStack, Foldable f, IOLike m)
  => CacheEnv m hash h
  -> Handle h
  -> f SecondaryOffset
  -> m ()
appendOffsets CacheEnv { hasFS, cacheVar } pHnd offsets = do
    Primary.appendOffsets hasFS pHnd offsets
    updateMVar_ cacheVar addCurrentChunkOffsets
  where
    -- Lenses would be nice here
    addCurrentChunkOffsets :: Cached hash -> Cached hash
    addCurrentChunkOffsets cached@Cached { currentChunkInfo } = cached
      { currentChunkInfo = currentChunkInfo
        { currentChunkOffsets = currentChunkOffsets currentChunkInfo <>
                                Seq.fromList (toList offsets)
        }
      }

{------------------------------------------------------------------------------
  On the secondary index
------------------------------------------------------------------------------}

readEntries
  :: forall m hash h t. (HasCallStack, Traversable t, IOLike m)
  => CacheEnv m hash h
  -> ChunkNo
  -> t (IsEBB, SecondaryOffset)
  -> m (t (Secondary.Entry hash, BlockSize))
readEntries cacheEnv@CacheEnv { hashInfo } chunk toRead =
    getChunkInfo cacheEnv chunk >>= \case
      Left CurrentChunkInfo { currentChunkEntries } ->
        forM toRead $ \(_isEBB, secondaryOffset) ->
          case currentChunkEntries Seq.!? indexForOffset secondaryOffset of
            Just (WithBlockSize size entry) -> return (entry, BlockSize size)
            Nothing                         -> noEntry secondaryOffset
      Right PastChunkInfo { pastChunkEntries } ->
        forM toRead $ \(_isEBB, secondaryOffset) ->
          case pastChunkEntries Vector.!? indexForOffset secondaryOffset of
            Just (WithBlockSize size entry) -> return (entry, BlockSize size)
            Nothing                         -> noEntry secondaryOffset
  where
    indexForOffset :: SecondaryOffset -> Int
    indexForOffset secondaryOffset = fromIntegral $
      secondaryOffset `div` Secondary.entrySize (hashSize hashInfo)

    -- There was no entry in the secondary index for the given
    -- 'SecondaryOffset'. Either the secondary index is incomplete, /or/, the
    -- primary index from which we read the 'SecondaryOffset' got corrupted.
    -- We don't know which of the two things happened, but the former is more
    -- likely, so we mention that file in the error message.
    noEntry :: SecondaryOffset -> m a
    noEntry secondaryOffset = throwUnexpectedError $ InvalidFileError
      (fsPathSecondaryIndexFile chunk)
      ("no entry missing for " <> show secondaryOffset)
      callStack

readAllEntries
  :: (HasCallStack, IOLike m)
  => CacheEnv m hash h
  -> SecondaryOffset
  -> ChunkNo
  -> (Secondary.Entry hash -> Bool)
  -> Word64
  -> IsEBB
  -> m [WithBlockSize (Secondary.Entry hash)]
readAllEntries cacheEnv@CacheEnv { hashInfo } secondaryOffset chunk stopCondition
               _chunkFileSize _firstIsEBB =
    getChunkInfo cacheEnv chunk <&> \case
      Left CurrentChunkInfo { currentChunkEntries } ->
        takeUntil (stopCondition . withoutBlockSize) $
        toList $ Seq.drop toDrop currentChunkEntries
      Right PastChunkInfo { pastChunkEntries } ->
        takeUntil (stopCondition . withoutBlockSize) $
        toList $ Vector.drop toDrop pastChunkEntries
  where
    toDrop :: Int
    toDrop = fromIntegral $
      secondaryOffset `div` Secondary.entrySize (hashSize hashInfo)

appendEntry
  :: forall m hash h. (HasCallStack, IOLike m)
  => CacheEnv m hash h
  -> ChunkNo
  -> Handle h
  -> Entry hash
  -> m Word64
appendEntry CacheEnv { hasFS, hashInfo, cacheVar } chunk sHnd entry = do
    nbBytes <- Secondary.appendEntry hasFS hashInfo sHnd (withoutBlockSize entry)
    updateMVar_ cacheVar addCurrentChunkEntry
    return nbBytes
  where
    -- Lenses would be nice here
    addCurrentChunkEntry :: Cached hash -> Cached hash
    addCurrentChunkEntry cached@Cached { currentChunk, currentChunkInfo }
      | currentChunk /= chunk
      = error $
          "Appending to chunk " <> show chunk <>
          " while the index is still in " <> show currentChunk
      | otherwise
      = cached
          { currentChunkInfo = currentChunkInfo
            { currentChunkEntries =
                currentChunkEntries currentChunkInfo Seq.|> entry
            }
          }

{------------------------------------------------------------------------------
  Helpers
------------------------------------------------------------------------------}

-- | Take items until the condition is true. If the condition is true for an
-- item, include that item as the last item in the returned list. If the
-- condition was never true, the original list is returned.
--
-- > takeUntil (== 3) [1,2,3,4]
-- [1,2,3]
-- > takeUntil (== 2) [0,1,0]
-- [0,1,0]
-- > takeUntil (== 2) [2,2,3]
-- [2]
takeUntil :: (a -> Bool) -> [a] -> [a]
takeUntil p = \case
  []
    -> []
  x:xs
    | p x
    -> [x]
    | otherwise
    -> x:takeUntil p xs
