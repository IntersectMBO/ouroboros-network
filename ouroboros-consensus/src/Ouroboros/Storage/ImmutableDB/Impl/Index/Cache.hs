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
{-# LANGUAGE ViewPatterns               #-}

{-# OPTIONS_GHC -Wredundant-constraints #-}
module Ouroboros.Storage.ImmutableDB.Impl.Index.Cache
  ( -- * Environment
    CacheEnv
  , newEnv
  , CacheConfig (..)
  , checkInvariants
    -- * Background thread
  , expireUnusedEpochs
    -- * Operations
  , reset
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

import           Control.Monad.Class.MonadThrow (bracketOnError)
import           Control.Monad.Class.MonadTime (Time (..))

import           Cardano.Prelude (NoUnexpectedThunks (..), forceElemsToWHNF)

import           Ouroboros.Consensus.Block (IsEBB (..))
import           Ouroboros.Consensus.Util (whenJust)
import           Ouroboros.Consensus.Util.IOLike
import           Ouroboros.Consensus.Util.MonadSTM.NormalForm (tryPutMVar,
                     unsafeNoThunks)
import qualified Ouroboros.Consensus.Util.MonadSTM.StrictMVar as Strict
import           Ouroboros.Consensus.Util.ResourceRegistry

import           Ouroboros.Storage.Common (EpochNo (..))
import           Ouroboros.Storage.FS.API (HasFS (..), withFile)
import           Ouroboros.Storage.FS.API.Types (AllowExisting (..), Handle,
                     OpenMode (ReadMode))
import           Ouroboros.Storage.Util.ErrorHandling (ErrorHandling)

import           Ouroboros.Storage.ImmutableDB.Impl.Index.Primary (PrimaryIndex,
                     SecondaryOffset)
import qualified Ouroboros.Storage.ImmutableDB.Impl.Index.Primary as Primary
import           Ouroboros.Storage.ImmutableDB.Impl.Index.Secondary
                     (BlockSize (..))
import qualified Ouroboros.Storage.ImmutableDB.Impl.Index.Secondary as Secondary
import           Ouroboros.Storage.ImmutableDB.Impl.Util (onException,
                     renderFile, throwUnexpectedError)
import           Ouroboros.Storage.ImmutableDB.Layout (RelativeSlot (..))
import           Ouroboros.Storage.ImmutableDB.Types (HashInfo (..),
                     ImmutableDBError, TraceCacheEvent (..),
                     UnexpectedError (..), WithBlockSize (..))

-- TODO property and/or q-s-m tests comparing with 'fileBackedIndex'

{------------------------------------------------------------------------------
  Environment
------------------------------------------------------------------------------}

data CacheConfig = CacheConfig
  { pastEpochsToCache :: Word32
    -- ^ Maximum number of past epochs to cache, excluding the current epoch.
    --
    -- NOTE: must be > 0
  , expireUnusedAfter :: DiffTime
    -- ^ Expire past epochs that haven't been used for 'expireUnusedAfter'
    -- from the cache, regardless the number of past epochs in the cache.
  }
  deriving (Eq, Show)

-- | Short-hand we use internally
type Entry hash = WithBlockSize (Secondary.Entry hash)

-- | The cached primary and secondary indices of the current epoch.
--
-- We use sequences (as opposed to vectors) to allow for efficient appending
-- in addition to (reasonably) efficient indexing.
data CurrentEpochInfo hash = CurrentEpochInfo
  { currentEpochOffsets :: !(StrictSeq SecondaryOffset)
  , currentEpochEntries :: !(StrictSeq (Entry hash))
  }
  deriving (Generic, NoUnexpectedThunks, Show)

emptyCurrentEpochInfo :: CurrentEpochInfo hash
emptyCurrentEpochInfo = CurrentEpochInfo (Seq.singleton 0) Seq.empty

-- | Convert a 'CurrentEpochInfo' to a 'PastEpochInfo'
--
-- TODO don't bother with the conversion? Use vectors for past epochs at start
-- up. Epochs that become past epochs because we advance to new epochs, we can
-- just leave in memory as seqs?
toPastEpochInfo :: CurrentEpochInfo hash -> PastEpochInfo hash
toPastEpochInfo CurrentEpochInfo { currentEpochOffsets, currentEpochEntries } =
    PastEpochInfo
      { pastEpochOffsets =
          fromMaybe (error "invalid current epoch") $
          Primary.mk (toList currentEpochOffsets)
      , pastEpochEntries =
          -- TODO optimise this
          Vector.fromList $ toList currentEpochEntries
      }

-- | The cached primary and secondary indices of an epoch in the past.
--
-- We use vectors to allow for efficient indexing. We don't need to append to
-- them, as they are in the past and thus immutable.
data PastEpochInfo hash = PastEpochInfo
  { pastEpochOffsets :: !PrimaryIndex
  , pastEpochEntries :: !(Vector (Entry hash))
  }
  deriving (Generic, NoUnexpectedThunks)

-- | The last time a cached past epoch was accessed.
--
-- We care about the ordering /and/ the absolute times so we can also evict
-- epochs when they haven't been used for @x@ seconds or minutes.
newtype LastUsed = LastUsed Time
  deriving newtype (Eq, Ord, Show, NoUnexpectedThunks)

-- | The data stored in the cache.
data Cached hash = Cached
  { currentEpoch     :: !EpochNo
    -- ^ The current epoch of the ImmutableDB, i.e., the epoch we're still
    -- appending entries too.
  , currentEpochInfo :: !(CurrentEpochInfo hash)
    -- ^ We always cache the current epoch.
    --
    -- When clients are in sync with our chain, they will only request blocks
    -- from the current epoch, so it is worth optimising this case.
    -- Additionally, by appending to the current epoch through the cache, we
    -- are sure the current epoch info is never stale.
    --
    -- We use an 'IntPSQ' here, where the keys are in fact epoch numbers. Since
    -- epoch numbers are internally represented by a 'Word64', one might be worried
    -- about a potential overflow here. While possible, it's not worth worrying about:
    -- - Whilst guaranteed to be only at least 30 bits, in practice, 64-bit GHC has 64-bit
    --   integers, so the conversion is bijective.
    -- - An epoch currently lasts around a week. Systems using a smaller representation
    --   might need to worry in a million years or so.
    -- - In the event of running for a million years, we're unlikely to have a problem anyway,
    --   since we only really cache _recent_ epochs. So the fact that they clash with the
    --   epochs from a million years ago isn't likely to be an issue.
  , pastEpochsInfo   :: !(IntPSQ LastUsed (PastEpochInfo hash))
    -- ^ Cached epochs from the past.
    --
    -- A LRU-cache (least recently used). Whenever a we get a cache hit
    -- ('getEpochInfo') for a past epoch, we change its 'LastUsed' priority to
    -- the current time. When the cache is full, see 'pastEpochsToCache', we
    -- will remove the epoch with the lowest priority, i.e. the least recently
    -- used past epoch.
    --
    -- INVARIANT: all past epochs are < 'currentEpoch'
    --
    -- INVARIANT: @'PSQ.size' 'pastEpochsInfo' <= 'pastEpochsToCache'@
  , nbPastEpochs     :: !Word32
    -- ^ Cached size of 'pastEpochsInfo', as an 'IntPSQ' only provides a \(O(n)
    -- \) 'PSQ.size' operation.
    --
    -- INVARIANT: 'nbPastEpochs' == @'PSQ.size' 'pastEpochsInfo'@
  }
  deriving (Generic, NoUnexpectedThunks)

checkInvariants
  :: Word32  -- ^ Maximum number of past epochs to cache
  -> Cached hash
  -> Maybe String
checkInvariants pastEpochsToCache Cached {..} = either Just (const Nothing) $ do
    forM_ (PSQ.keys pastEpochsInfo) $ \pastEpoch ->
      unless (pastEpoch < currentEpochInt) $
        throwError $
          "past epoch (" <> show pastEpoch <> ") >= current epoch (" <>
          show currentEpoch <> ")"

    unless (PSQ.size pastEpochsInfo <= fromIntegral pastEpochsToCache) $
      throwError $
        "PSQ.size pastEpochsInfo (" <> show (PSQ.size pastEpochsInfo) <>
        ") > pastEpochsToCache (" <> show pastEpochsToCache <> ")"

    unless (nbPastEpochs == fromIntegral (PSQ.size pastEpochsInfo)) $
      throwError $
        "nbPastEpochs (" <> show nbPastEpochs <>
        ") /= PSQ.size pastEpochsInfo (" <> show (PSQ.size pastEpochsInfo) <>
        ")"
  where
    EpochNo (fromIntegral -> currentEpochInt) = currentEpoch


-- | Store the 'PastEpochInfo' for the given 'EpochNo' in 'Cached'.
--
-- Uses the 'LastUsed' as the priority.
--
-- NOTE: does not trim the cache.
--
-- PRECONDITION: the given 'EpochNo' is < the 'currentEpoch'.
addPastEpochInfo
  :: EpochNo
  -> LastUsed
  -> PastEpochInfo hash
  -> Cached hash
  -> Cached hash
addPastEpochInfo epoch lastUsed pastEpochInfo cached =
    assert (epoch < currentEpoch cached) $
    -- NOTE: in case of multiple concurrent cache misses of the same epoch,
    -- we might add the same past epoch multiple times to the cache. This
    -- means the following cannot be a precondition:
    -- assert (not (PSQ.member epoch pastEpochsInfo)) $
    cached
      { pastEpochsInfo = PSQ.insert epochInt lastUsed pastEpochInfo pastEpochsInfo
      , nbPastEpochs   = succ nbPastEpochs
      }
  where
    Cached { pastEpochsInfo, nbPastEpochs } = cached
    EpochNo (fromIntegral -> epochInt) = epoch

-- | Remove the least recently used past epoch from the cache when 'Cached'
-- contains more epochs than the given maximum.
--
-- PRECONDITION: 'nbPastEpochs' + 1 <= given maximum. In other words, 'Cached'
-- contains at most one epoch too many. We ensure this by calling this
-- function directly after adding a past epoch to 'Cached'.
--
-- If a past epoch was evicted, its epoch number is returned.
evictIfNecessary
  :: Word32  -- ^ Maximum number of past epochs to cache
  -> Cached hash
  -> (Cached hash, Maybe EpochNo)
evictIfNecessary maxNbPastEpochs cached
    | nbPastEpochs > maxNbPastEpochs
    = assert (nbPastEpochs == maxNbPastEpochs + 1) $
      case PSQ.minView pastEpochsInfo of
        Nothing                                 -> error
          "nbPastEpochs > maxNbPastEpochs but pastEpochsInfo was empty"
        Just (epochNo, _p, _v, pastEpochsInfo') ->
            (cached', Just . EpochNo $ fromIntegral epochNo)
          where
            cached' = cached
              { nbPastEpochs   = maxNbPastEpochs
              , pastEpochsInfo = pastEpochsInfo'
              }
    | otherwise
    = (cached, Nothing)
  where
    Cached { nbPastEpochs, pastEpochsInfo } = cached
-- NOTE: we must inline 'evictIfNecessary' otherwise we get unexplained thunks
-- in 'Cached' and thus a space leak. Alternatively, we could disable the
-- @-fstrictness@ optimisation (enabled by default for -O1).
{-# INLINE evictIfNecessary #-}

lookupPastEpochInfo
  :: EpochNo
  -> LastUsed
  -> Cached hash
  -> Maybe (PastEpochInfo hash, Cached hash)
lookupPastEpochInfo epoch lastUsed cached@Cached { pastEpochsInfo } =
    case PSQ.alter lookupAndUpdateLastUsed epochInt pastEpochsInfo of
      (Nothing, _) -> Nothing
      (Just pastEpochInfo, pastEpochsInfo') -> Just (pastEpochInfo, cached')
        where
          cached' = cached { pastEpochsInfo = pastEpochsInfo' }
  where
    EpochNo (fromIntegral -> epochInt) = epoch
    lookupAndUpdateLastUsed
      :: Maybe (LastUsed, PastEpochInfo hash)
      -> (Maybe (PastEpochInfo hash), Maybe (LastUsed, PastEpochInfo hash))
    lookupAndUpdateLastUsed = \case
      Nothing                -> (Nothing, Nothing)
      Just (_lastUsed, info) -> (Just info, Just (lastUsed, info))

openEpoch
  :: EpochNo
  -> LastUsed
  -> CurrentEpochInfo hash
  -> Cached hash
  -> Cached hash
openEpoch epoch lastUsed newCurrentEpochInfo cached
    | currentEpoch == epoch
    = cached
        { currentEpochInfo = newCurrentEpochInfo }

    | currentEpoch + 1 == epoch
    = Cached
        { currentEpoch     = epoch
        , currentEpochInfo = newCurrentEpochInfo
          -- We use 'lastUsed' for the current epoch that has now become a
          -- "past" epoch, which means that that epoch is most recently used
          -- one. When clients are roughly in sync with us, when we switch to a
          -- new epoch, they might still request blocks from the previous one.
          -- So to avoid throwing away that cached information, we give it the
          -- highest priority.
        , pastEpochsInfo   = PSQ.insert currentEpochInt lastUsed
            (toPastEpochInfo currentEpochInfo) pastEpochsInfo
        , nbPastEpochs     = succ nbPastEpochs
        }

    | otherwise
    = error $ "Going from epoch " <> show currentEpoch <> " to " <> show epoch
  where
    EpochNo (fromIntegral -> currentEpochInt) = currentEpoch
    Cached
      { currentEpoch, currentEpochInfo, pastEpochsInfo, nbPastEpochs
      } = cached

emptyCached
  :: EpochNo -- ^ The current epoch
  -> CurrentEpochInfo hash
  -> Cached hash
emptyCached currentEpoch currentEpochInfo = Cached
    { currentEpoch
    , currentEpochInfo
    , pastEpochsInfo = PSQ.empty
    , nbPastEpochs   = 0
    }

-- | Environment used by functions operating on the cached index.
data CacheEnv m hash h = CacheEnv
  { hasFS       :: HasFS m h
  , err         :: ErrorHandling ImmutableDBError m
  , hashInfo    :: HashInfo hash
  , registry    :: ResourceRegistry m
  , tracer      :: Tracer m TraceCacheEvent
  , cacheVar    :: StrictMVar m (Cached hash)
  , cacheConfig :: CacheConfig
  }

-- | Creates a new 'CacheEnv' and launches a background thread that expires
-- unused past epochs ('expireUnusedEpochs').
--
-- PRECONDITION: 'pastEpochsToCache' (in 'CacheConfig') > 0
newEnv
  :: (HasCallStack, IOLike m, NoUnexpectedThunks hash)
  => HasFS m h
  -> ErrorHandling ImmutableDBError m
  -> HashInfo hash
  -> ResourceRegistry m
  -> Tracer m TraceCacheEvent
  -> CacheConfig
  -> EpochNo  -- ^ Current epoch
  -> m (CacheEnv m hash h)
newEnv hasFS err hashInfo registry tracer cacheConfig epoch = do
    when (pastEpochsToCache == 0) $
      error "pastEpochsToCache must be > 0"

    currentEpochInfo <- loadCurrentEpochInfo hasFS err hashInfo epoch
    cacheVar <- newMVarWithInvariants $ emptyCached epoch currentEpochInfo
    let cacheEnv = CacheEnv {..}
    void $ forkLinkedThread registry $ expireUnusedEpochs cacheEnv
    return cacheEnv
  where
    CacheConfig { pastEpochsToCache } = cacheConfig

    -- When checking invariants, check both our invariants and for thunks.
    -- Note that this is only done when the corresponding flag is enabled.
    newMVarWithInvariants =
      Strict.newMVarWithInvariant $ \cached ->
        checkInvariants pastEpochsToCache cached
        `mplus`
        unsafeNoThunks cached

{------------------------------------------------------------------------------
  Background thread
------------------------------------------------------------------------------}

-- | Intended to run as a background thread.
--
-- Will expire past epochs that haven't been used for 'expireUnusedAfter' from
-- the cache.
expireUnusedEpochs
  :: (HasCallStack, IOLike m)
  => CacheEnv m hash h
  -> m Void
expireUnusedEpochs CacheEnv { cacheVar, cacheConfig, tracer } =
    forever $ do
      now <- getMonotonicTime
      mbTraceMsg <- updateMVar cacheVar $ garbageCollect now
      mapM_ (traceWith tracer) mbTraceMsg
      threadDelay expireUnusedAfter
  where
    CacheConfig { expireUnusedAfter } = cacheConfig

    -- | Remove the least recently used past epoch from 'Cached' /if/ it
    -- hasn't been used for 'expireUnusedAfter', otherwise the original
    -- 'Cached' is returned.
    --
    -- In case a 'TracePastEpochsExpired' event should be traced, it is
    -- returned as a 'Just'.
    garbageCollect
      :: Time
      -> Cached hash
      -> (Cached hash, Maybe TraceCacheEvent)
    garbageCollect now cached@Cached { pastEpochsInfo, nbPastEpochs } =
        case expiredPastEpochs of
          [] -> (cached,  Nothing)
          _  -> (cached', Just traceMsg)
      where
        -- Every past epoch last used before (or at) this time, must be
        -- expired.
        expiredLastUsedTime :: LastUsed
        expiredLastUsedTime = LastUsed $
          Time (now `diffTime` Time expireUnusedAfter)

        (expiredPastEpochs, pastEpochsInfo') =
          PSQ.atMostView expiredLastUsedTime pastEpochsInfo

        nbPastEpochs' = nbPastEpochs - fromIntegral (length expiredPastEpochs)

        cached' = cached
          { pastEpochsInfo = pastEpochsInfo'
          , nbPastEpochs   = nbPastEpochs'
          }

        !traceMsg = TracePastEpochsExpired
          -- Force this list, otherwise the traced message holds onto to the
          -- past epoch indices.
          (forceElemsToWHNF
            [EpochNo . fromIntegral $ epoch
            | (epoch, _, _) <- expiredPastEpochs
            ])
          nbPastEpochs'

{------------------------------------------------------------------------------
  Reading indices
------------------------------------------------------------------------------}

readPrimaryIndex
  :: (HasCallStack, IOLike m)
  => HasFS m h
  -> ErrorHandling ImmutableDBError m
  -> EpochNo
  -> m (PrimaryIndex, IsEBB)
     -- ^ The primary index and whether it starts with an EBB or not
readPrimaryIndex hasFS err epoch = do
    primaryIndex <- Primary.load hasFS err epoch
    let firstIsEBB
          | Primary.containsSlot primaryIndex 0
          , Primary.isFilledSlot primaryIndex 0
          = IsEBB
          | otherwise
          = IsNotEBB
    return (primaryIndex, firstIsEBB)

readSecondaryIndex
  :: (HasCallStack, IOLike m)
  => HasFS m h
  -> ErrorHandling ImmutableDBError m
  -> HashInfo hash
  -> EpochNo
  -> IsEBB
  -> m [Entry hash]
readSecondaryIndex hasFS@HasFS { hGetSize } err hashInfo epoch firstIsEBB = do
    !epochFileSize <- withFile hasFS epochFile ReadMode hGetSize
    Secondary.readAllEntries hasFS err hashInfo secondaryOffset
      epoch stopCondition epochFileSize firstIsEBB
  where
    epochFile = renderFile "epoch" epoch
    -- Read from the start
    secondaryOffset = 0
    -- Don't stop until the end
    stopCondition = const False

loadCurrentEpochInfo
  :: (HasCallStack, IOLike m)
  => HasFS m h
  -> ErrorHandling ImmutableDBError m
  -> HashInfo hash
  -> EpochNo
  -> m (CurrentEpochInfo hash)
loadCurrentEpochInfo hasFS err hashInfo epoch = do
    -- We're assuming that when the primary index file exists, the secondary
    -- index file will also exist
    epochExists <- doesFileExist hasFS primaryIndexFile
    if epochExists then do
      (primaryIndex, firstIsEBB) <- readPrimaryIndex hasFS err epoch
      entries <- readSecondaryIndex hasFS err hashInfo epoch firstIsEBB
      return CurrentEpochInfo
        { currentEpochOffsets =
          -- TODO optimise this
            Seq.fromList . Primary.toSecondaryOffsets $ primaryIndex
        , currentEpochEntries = Seq.fromList entries
        }
    else
      return emptyCurrentEpochInfo
  where
    primaryIndexFile = renderFile "primary" epoch

loadPastEpochInfo
  :: (HasCallStack, IOLike m)
  => HasFS m h
  -> ErrorHandling ImmutableDBError m
  -> HashInfo hash
  -> EpochNo
  -> m (PastEpochInfo hash)
loadPastEpochInfo hasFS err hashInfo epoch = do
    (primaryIndex, firstIsEBB) <- readPrimaryIndex hasFS err epoch
    entries <- readSecondaryIndex hasFS err hashInfo epoch firstIsEBB
    return PastEpochInfo
      { pastEpochOffsets = primaryIndex
      , pastEpochEntries = Vector.fromList $ forceElemsToWHNF entries
      }

getEpochInfo
  :: (HasCallStack, IOLike m)
  => CacheEnv m hash h
  -> EpochNo
  -> m (Either (CurrentEpochInfo hash) (PastEpochInfo hash))
getEpochInfo cacheEnv epoch = do
    lastUsed <- LastUsed <$> getMonotonicTime
    -- Make sure we don't leave an empty MVar in case of an exception.
    mbCacheHit <- bracketOnError (takeMVar cacheVar) (tryPutMVar cacheVar) $
      \cached@Cached { currentEpoch, currentEpochInfo, nbPastEpochs } -> if
        | epoch == currentEpoch -> do
          -- Cache hit for the current epoch
          putMVar cacheVar cached
          traceWith tracer $ TraceCurrentEpochHit epoch nbPastEpochs
          return $ Just $ Left currentEpochInfo
        | Just (pastEpochInfo, cached') <- lookupPastEpochInfo epoch lastUsed cached -> do
          -- Cache hit for an epoch in the past
          putMVar cacheVar cached'
          traceWith tracer $ TracePastEpochHit epoch nbPastEpochs
          return $ Just $ Right pastEpochInfo
        | otherwise -> do
          -- Cache miss for an epoch in the past. We don't want to hold on to
          -- the 'cacheVar' MVar, blocking all other access to the cace, while
          -- we're reading things from disk, so put it back now and update the
          -- cache afterwards.
          putMVar cacheVar cached
          traceWith tracer $ TracePastEpochMiss epoch nbPastEpochs
          return Nothing
    case mbCacheHit of
      Just hit -> return hit
      Nothing  -> do
        -- Cache miss, load both entire indices for the epoch from disk.
        pastEpochInfo <- loadPastEpochInfo hasFS err hashInfo epoch
        -- Loading the epoch might have taken some time, so obtain the time
        -- again.
        lastUsed' <- LastUsed <$> getMonotonicTime
        mbEvicted <- updateMVar cacheVar $
          evictIfNecessary pastEpochsToCache .
          addPastEpochInfo epoch lastUsed' pastEpochInfo
        whenJust mbEvicted $ \evicted ->
          -- If we had to evict, we are at 'pastEpochsToCache'
          traceWith tracer $ TracePastEpochEvict evicted pastEpochsToCache
        return $ Right pastEpochInfo
  where
    CacheEnv { hasFS, err, hashInfo, cacheVar, cacheConfig, tracer } = cacheEnv
    CacheConfig { pastEpochsToCache } = cacheConfig

{------------------------------------------------------------------------------
  Operations
------------------------------------------------------------------------------}

-- | Empties the cache and restarts the background expiration thread.
--
-- PRECONDITION: the background thread expiring unused past epochs must have
-- been terminated.
reset
  :: IOLike m
  => CacheEnv m hash h
  -> EpochNo  -- ^ The new current epoch
  -> m ()
reset cacheEnv@CacheEnv { hasFS, err, hashInfo, registry, cacheVar } epoch = do
    currentEpochInfo <- loadCurrentEpochInfo hasFS err hashInfo epoch
    void $ swapMVar cacheVar $ emptyCached epoch currentEpochInfo
    void $ forkLinkedThread registry $ expireUnusedEpochs cacheEnv

{------------------------------------------------------------------------------
  On the primary index
------------------------------------------------------------------------------}

readOffsets
  :: (HasCallStack, Traversable t, IOLike m)
  => CacheEnv m hash h
  -> EpochNo
  -> t RelativeSlot
  -> m (t (Maybe SecondaryOffset))
readOffsets cacheEnv epoch relSlots =
    getEpochInfo cacheEnv epoch <&> \case
      Left CurrentEpochInfo { currentEpochOffsets } ->
        getOffsetFromSecondaryOffsets currentEpochOffsets <$> relSlots
      Right PastEpochInfo { pastEpochOffsets } ->
        getOffsetFromPrimaryIndex pastEpochOffsets <$> relSlots
  where
    getOffsetFromSecondaryOffsets
      :: StrictSeq SecondaryOffset
      -> RelativeSlot
      -> Maybe SecondaryOffset
    getOffsetFromSecondaryOffsets offsets (RelativeSlot s) =
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
  -> EpochNo
  -> m (Maybe RelativeSlot)
readFirstFilledSlot cacheEnv epoch =
    getEpochInfo cacheEnv epoch <&> \case
      Left CurrentEpochInfo { currentEpochOffsets } ->
        firstFilledSlotInSeq currentEpochOffsets
      Right PastEpochInfo { pastEpochOffsets } ->
        Primary.firstFilledSlot pastEpochOffsets
  where
    firstFilledSlotInSeq :: StrictSeq SecondaryOffset -> Maybe RelativeSlot
    firstFilledSlotInSeq = fmap indexToRelativeSlot . Seq.findIndexL (/= 0)
      where
        indexToRelativeSlot :: Int -> RelativeSlot
        indexToRelativeSlot = RelativeSlot . fromIntegral . pred

-- | This is called when a new epoch is started, which means we need to update
-- 'Cached' to reflect this.
openPrimaryIndex
  :: (HasCallStack, IOLike m)
  => CacheEnv m hash h
  -> EpochNo
  -> AllowExisting
  -> m (Handle h)
openPrimaryIndex cacheEnv epoch allowExisting = do
    lastUsed <- LastUsed <$> getMonotonicTime
    pHnd <- Primary.open hasFS epoch allowExisting
    -- Don't leak the handle in case of an exception
    onException hasFsErr err (hClose pHnd) $ do
      newCurrentEpochInfo <- case allowExisting of
        MustBeNew     -> return emptyCurrentEpochInfo
        AllowExisting -> loadCurrentEpochInfo hasFS err hashInfo epoch
      mbEvicted <- updateMVar cacheVar $
        evictIfNecessary pastEpochsToCache .
        openEpoch epoch lastUsed newCurrentEpochInfo
      whenJust mbEvicted $ \evicted ->
        -- If we had to evict, we are at 'pastEpochsToCache'
        traceWith tracer $ TracePastEpochEvict evicted pastEpochsToCache
      return pHnd
  where
    CacheEnv { hasFS, err, hashInfo, cacheVar, cacheConfig, tracer } = cacheEnv
    HasFS { hClose, hasFsErr } = hasFS
    CacheConfig { pastEpochsToCache } = cacheConfig

appendOffsets
  :: (HasCallStack, Foldable f, IOLike m)
  => CacheEnv m hash h
  -> Handle h
  -> f SecondaryOffset
  -> m ()
appendOffsets CacheEnv { hasFS, cacheVar } pHnd offsets = do
    Primary.appendOffsets hasFS pHnd offsets
    updateMVar_ cacheVar addCurrentEpochOffsets
  where
    -- Lenses would be nice here
    addCurrentEpochOffsets :: Cached hash -> Cached hash
    addCurrentEpochOffsets cached@Cached { currentEpochInfo } = cached
      { currentEpochInfo = currentEpochInfo
        { currentEpochOffsets = currentEpochOffsets currentEpochInfo <>
                                Seq.fromList (toList offsets)
        }
      }

{------------------------------------------------------------------------------
  On the secondary index
------------------------------------------------------------------------------}

readEntries
  :: forall m hash h t. (HasCallStack, Traversable t, IOLike m)
  => CacheEnv m hash h
  -> EpochNo
  -> t (IsEBB, SecondaryOffset)
  -> m (t (Secondary.Entry hash, BlockSize))
readEntries cacheEnv@CacheEnv { err, hashInfo } epoch toRead =
    getEpochInfo cacheEnv epoch >>= \case
      Left CurrentEpochInfo { currentEpochEntries } ->
        forM toRead $ \(_isEBB, secondaryOffset) ->
          case currentEpochEntries Seq.!? indexForOffset secondaryOffset of
            Just (WithBlockSize size entry) -> return (entry, BlockSize size)
            Nothing                         -> noEntry secondaryOffset
      Right PastEpochInfo { pastEpochEntries } ->
        forM toRead $ \(_isEBB, secondaryOffset) ->
          case pastEpochEntries Vector.!? indexForOffset secondaryOffset of
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
    noEntry secondaryOffset = throwUnexpectedError err $ InvalidFileError
      (renderFile "secondary" epoch)
      ("no entry missing for " <> show secondaryOffset)
      callStack

readAllEntries
  :: (HasCallStack, IOLike m)
  => CacheEnv m hash h
  -> SecondaryOffset
  -> EpochNo
  -> (Secondary.Entry hash -> Bool)
  -> Word64
  -> IsEBB
  -> m [WithBlockSize (Secondary.Entry hash)]
readAllEntries cacheEnv@CacheEnv { hashInfo } secondaryOffset epoch stopCondition
               _epochFileSize _firstIsEBB =
    getEpochInfo cacheEnv epoch <&> \case
      Left CurrentEpochInfo { currentEpochEntries } ->
        takeUntil (stopCondition . withoutBlockSize) $
        toList $ Seq.drop toDrop currentEpochEntries
      Right PastEpochInfo { pastEpochEntries } ->
        takeUntil (stopCondition . withoutBlockSize) $
        toList $ Vector.drop toDrop pastEpochEntries
  where
    toDrop :: Int
    toDrop = fromIntegral $
      secondaryOffset `div` Secondary.entrySize (hashSize hashInfo)

appendEntry
  :: forall m hash h. (HasCallStack, IOLike m)
  => CacheEnv m hash h
  -> EpochNo
  -> Handle h
  -> Entry hash
  -> m Word64
appendEntry CacheEnv { hasFS, hashInfo, cacheVar } epoch sHnd entry = do
    nbBytes <- Secondary.appendEntry hasFS hashInfo sHnd (withoutBlockSize entry)
    updateMVar_ cacheVar addCurrentEpochEntry
    return nbBytes
  where
    -- Lenses would be nice here
    addCurrentEpochEntry :: Cached hash -> Cached hash
    addCurrentEpochEntry cached@Cached { currentEpoch, currentEpochInfo }
      | currentEpoch /= epoch
      = error $
          "Appending to epoch " <> show epoch <>
          " while the index is still in " <> show currentEpoch
      | otherwise
      = cached
          { currentEpochInfo = currentEpochInfo
            { currentEpochEntries =
                currentEpochEntries currentEpochInfo Seq.|> entry
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
