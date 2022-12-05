{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE Rank2Types                 #-}
{-# LANGUAGE StandaloneDeriving         #-}

module Ouroboros.Consensus.Storage.LedgerDB.HD.BackingStore (
    -- * Backing store interface
    BackingStore (..)
  , BackingStorePath (..)
  , BackingStoreValueHandle (..)
  , RangeQuery (..)
  , bsRead
  , withBsValueHandle
    -- * An in-memory backing store
  , TVarBackingStoreExn (..)
  , newTVarBackingStore
    -- * A trivial backing store
  , trivialBackingStore
  ) where

import qualified Codec.CBOR.Read as CBOR
import qualified Codec.CBOR.Write as CBOR
import           Control.Exception (Exception)
import           Control.Monad (join, unless, void, when)
import qualified Data.ByteString.Lazy as BSL
import           Data.String (fromString)
import           GHC.Generics (Generic)
import           NoThunks.Class (NoThunks, OnlyCheckWhnfNamed (..))

import           Cardano.Binary as CBOR
import           Cardano.Slotting.Slot (SlotNo, WithOrigin (..))

import qualified Ouroboros.Consensus.Storage.FS.API as FS
import qualified Ouroboros.Consensus.Storage.FS.API.Types as FS
import           Ouroboros.Consensus.Util.IOLike (IOLike)
import qualified Ouroboros.Consensus.Util.IOLike as IOLike

{-------------------------------------------------------------------------------
  Backing store interface
-------------------------------------------------------------------------------}

-- | A backing store for a map
data BackingStore m keys values diff = BackingStore {
    -- | Close the backing store
    --
    -- Other methods throw exceptions if called on a closed store.
    bsClose       :: !(m ())
    -- | Create a persistent copy
    --
    -- Each backing store implementation will offer a way to initialize itself
    -- from such a path.
    --
    -- The destination path must not already exist. After this operation, it
    -- will be a directory.
  , bsCopy        :: !(FS.SomeHasFS m -> BackingStorePath -> m ())
    -- | Open a 'BackingStoreValueHandle' capturing the current value of the
    -- entire database
  , bsValueHandle :: !(m (WithOrigin SlotNo, BackingStoreValueHandle m keys values))
    -- | Apply a valid diff to the contents of the backing store
  , bsWrite       :: !(SlotNo -> diff -> m ())
  }

-- | TODO Is there a good way to not assume that any function that creates a
-- 'BackingStore' doesn't hold space leaks in its closure?
deriving via OnlyCheckWhnfNamed "BackingStore" (BackingStore m keys values diff)
  instance NoThunks (BackingStore m keys values diff)

newtype BackingStorePath = BackingStorePath FS.FsPath
  deriving stock (Show, Eq, Ord)
  deriving newtype NoThunks

-- | An ephemeral handle to an immutable value of the entire database
--
-- The performance cost is usually minimal unless this handle is held open too
-- long.
data BackingStoreValueHandle m keys values = BackingStoreValueHandle {
    -- | Close the handle
    --
    -- Other methods throw exceptions if called on a closed handle.
    bsvhClose     :: !(m ())
    -- | See 'RangeQuery'
  , bsvhRangeRead :: !(RangeQuery keys -> m values)
    -- | Read the given keys from the handle
    --
    -- Absent keys will merely not be present in the result instead of causing a
    -- failure or an exception.
  , bsvhRead      :: !(keys -> m values)
  }

data RangeQuery keys = RangeQuery {
      -- | The result of this range query begin at first key that is strictly
      -- greater than the greatest key in 'rqPrev'.
      --
      -- If the given set of keys is 'Just' but contains no keys, then the query
      -- will return no results. (This is the steady-state once a looping range
      -- query reaches the end of the table.)
      rqPrev  :: Maybe keys
      -- | Roughly how many values to read.
      --
      -- The query may return a different number of values than this even if it
      -- has not reached the last key. The only crucial invariant is that the
      -- query only returns an empty map if there are no more keys to read on
      -- disk.
      --
      -- FIXME: can we satisfy this invariant if we read keys from disk but all
      -- of them were deleted in the changelog?
    , rqCount :: !Int
    }
    deriving stock (Show, Eq)

-- | TODO Is there a good way to not assume that any function that creates a
-- 'BackingStoreValueHandle' doesn't hold space leaks in its closure?
deriving via OnlyCheckWhnfNamed "BackingStoreValueHandle" (BackingStoreValueHandle m keys values)
  instance NoThunks (BackingStoreValueHandle m keys values)

-- | A combination of 'bsValueHandle' and 'bsvhRead'
bsRead ::
     IOLike m
  => BackingStore m keys values diff
  -> keys
  -> m (WithOrigin SlotNo, values)
bsRead store keys = withBsValueHandle store $ \slot vh -> do
    values <- bsvhRead vh keys
    pure (slot, values)

-- | A 'IOLike.bracket'ed 'bsValueHandle'
withBsValueHandle ::
     IOLike m
  => BackingStore m keys values diff
  -> (WithOrigin SlotNo -> BackingStoreValueHandle m keys values -> m a)
  -> m a
withBsValueHandle store kont =
    IOLike.bracket
      (bsValueHandle store)
      (bsvhClose . snd)
      (uncurry kont)

{-------------------------------------------------------------------------------
  An in-memory backing store
-------------------------------------------------------------------------------}

data TVarBackingStoreContents m values =
    TVarBackingStoreContentsClosed
  | TVarBackingStoreContents
      !(WithOrigin SlotNo)
      !values
  deriving (Generic, NoThunks)

data TVarBackingStoreExn =
    TVarBackingStoreClosedExn
  | TVarBackingStoreValueHandleClosedExn
  | TVarBackingStoreDirectoryExists
  | TVarBackingStoreNonMonotonicSeq !(WithOrigin SlotNo) !(WithOrigin SlotNo)
  | TVarBackingStoreDeserialiseExn CBOR.DeserialiseFailure
  | TVarIncompleteDeserialiseExn
  deriving anyclass (Exception)
  deriving stock    (Show)

newtype StoreDirIsIncompatible = StoreDirIsIncompatible FilePath
  deriving anyclass (Exception)

instance Show StoreDirIsIncompatible where
  show (StoreDirIsIncompatible p) =
       "In-Memory database not found in the database directory: "
    <> show p
    <> ".\nPre-UTxO-HD and LMDB implementations are incompatible with the In-Memory \
       \ implementation. Please delete your ledger database directory."

-- | Use a 'TVar' as a trivial backing store
newTVarBackingStore ::
     (IOLike m, NoThunks values)
  => (keys -> values -> values)
  -> (RangeQuery keys -> values -> values)
  -> (values -> diff -> values)
  -> (values -> CBOR.Encoding)
  -> (forall s. CBOR.Decoder s values)
  -> Either
       (FS.SomeHasFS m, BackingStorePath)
       (WithOrigin SlotNo, values)   -- ^ initial seqno and contents
  -> m (BackingStore m
          keys
          values
          diff
       )
newTVarBackingStore lookup_ rangeRead_ forwardValues_ enc dec initialization = do
    ref <- do
      (slot, values) <- case initialization of
        Left (FS.SomeHasFS fs, BackingStorePath path) -> do
          tvarFileExists <- FS.doesFileExist fs (extendPath path)
          -- simHasFS would error on unsafeToFilePath unless we take advantage
          -- of lazyness
          unless tvarFileExists $ IOLike.throwIO . StoreDirIsIncompatible =<< FS.unsafeToFilePath fs path
          FS.withFile fs (extendPath path) FS.ReadMode $ \h -> do
            bs <- FS.hGetAll fs h
            case CBOR.deserialiseFromBytes ((,) <$> CBOR.fromCBOR <*> dec) bs of
              Left  err        -> IOLike.throwIO $ TVarBackingStoreDeserialiseExn err
              Right (extra, x) -> do
                unless (BSL.null extra) $ IOLike.throwIO TVarIncompleteDeserialiseExn
                pure x
        Right x -> pure x
      IOLike.newTVarIO $ TVarBackingStoreContents slot values
    pure BackingStore {
        bsClose    = IOLike.atomically $ do
          guardClosed ref
          IOLike.writeTVar ref TVarBackingStoreContentsClosed
      , bsCopy = \(FS.SomeHasFS fs) (BackingStorePath path) ->
          join $ IOLike.atomically $ do
            IOLike.readTVar ref >>= \case
              TVarBackingStoreContentsClosed                ->
                IOLike.throwSTM TVarBackingStoreClosedExn
              TVarBackingStoreContents slot values -> pure $ do
                exists <- FS.doesDirectoryExist fs path
                when exists $ IOLike.throwIO TVarBackingStoreDirectoryExists
                FS.createDirectory fs path
                FS.withFile fs (extendPath path) (FS.WriteMode FS.MustBeNew) $ \h -> do
                  void $ FS.hPutAll fs h $ CBOR.toLazyByteString $ CBOR.toCBOR slot <> enc values
      , bsValueHandle = join $ IOLike.atomically $ do
          IOLike.readTVar ref >>= \case
            TVarBackingStoreContentsClosed                ->
              IOLike.throwSTM TVarBackingStoreClosedExn
            TVarBackingStoreContents slot values -> pure $ do
              refHandleClosed <- IOLike.newTVarIO False
              pure $ (,) slot $ BackingStoreValueHandle {
                  bsvhClose     = IOLike.atomically $ do
                    guardClosed ref
                    guardHandleClosed refHandleClosed
                    IOLike.writeTVar refHandleClosed True
                , bsvhRangeRead = \rq -> IOLike.atomically $ do
                    guardClosed ref
                    guardHandleClosed refHandleClosed
                    pure $ rangeRead_ rq values
                , bsvhRead      = \keys -> IOLike.atomically $ do
                    guardClosed ref
                    guardHandleClosed refHandleClosed
                    pure $ lookup_ keys values
                }
      , bsWrite    = \slot2 diff -> IOLike.atomically $ do
          IOLike.readTVar ref >>= \case
            TVarBackingStoreContentsClosed        ->
              IOLike.throwSTM TVarBackingStoreClosedExn
            TVarBackingStoreContents slot1 values -> do
              unless (slot1 <= At slot2) $
                IOLike.throwSTM $ TVarBackingStoreNonMonotonicSeq (At slot2) slot1
              IOLike.writeTVar ref $
                TVarBackingStoreContents
                  (At slot2)
                  (forwardValues_ values diff)
      }
  where
    extendPath path =
      FS.fsPathFromList $ FS.fsPathToList path <> [fromString "tvar"]

guardClosed ::
     IOLike m
  => IOLike.StrictTVar m (TVarBackingStoreContents ks vs)
  -> IOLike.STM m ()
guardClosed ref = IOLike.readTVar ref >>= \case
  TVarBackingStoreContentsClosed -> IOLike.throwSTM TVarBackingStoreClosedExn
  TVarBackingStoreContents _ _   -> pure ()

guardHandleClosed ::
     IOLike m
  => IOLike.StrictTVar m Bool
  -> IOLike.STM m ()
guardHandleClosed refHandleClosed = do
  isClosed <- IOLike.readTVar refHandleClosed
  when isClosed $ IOLike.throwSTM TVarBackingStoreValueHandleClosedExn

trivialBackingStore :: IOLike m => values -> m (BackingStore m keys values diff)
trivialBackingStore emptyValues = do
  seqNo <- IOLike.newMVar Origin
  pure $ BackingStore
              (pure ())
              (\_ _ -> pure ())
              (do
                  s <- IOLike.readMVar seqNo
                  pure $ ( s
                         , BackingStoreValueHandle
                             (pure ())
                             (\_ -> pure emptyValues)
                             (\_ -> pure emptyValues)
                         )
              )
              (\s _ -> void $ IOLike.swapMVar seqNo (At s))
