{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE RankNTypes         #-}

-- | An implementation of a 'BackingStore' using a TVar. This is the
-- implementation known as "InMemory".
module Ouroboros.Consensus.Storage.LedgerDB.BackingStore.InMemory (
    -- * Constructor
    newTVarBackingStoreInitialiser
    -- * Traces
  , TVarTraceEvent (..)
    -- * Errors
  , StoreDirIsIncompatible (..)
  , TVarBackingStoreExn (..)
  ) where

import           Cardano.Binary as CBOR
import           Cardano.Slotting.Slot (SlotNo, WithOrigin (..))
import qualified Codec.CBOR.Read as CBOR
import qualified Codec.CBOR.Write as CBOR
import           Control.Monad (join, unless, void, when)
import           Control.Tracer (Tracer, traceWith)
import qualified Data.ByteString.Lazy as BSL
import           Data.String (fromString)
import           GHC.Generics (Generic)
import           Ouroboros.Consensus.Storage.LedgerDB.BackingStore
import           Ouroboros.Consensus.Util.IOLike (Exception, IOLike,
                     MonadSTM (STM, atomically), MonadThrow (throwIO), NoThunks,
                     StrictTVar, newTVarIO, readTVar, throwSTM, writeTVar)
import           System.FS.API
                     (HasFS (createDirectory, doesDirectoryExist, doesFileExist, mkFsErrorPath),
                     SomeHasFS (SomeHasFS), hGetAll, hPutAll, withFile)
import qualified System.FS.API.Types as FS
import           System.FS.API.Types (AllowExisting (MustBeNew), FsErrorPath,
                     FsPath (fsPathToList), OpenMode (ReadMode, WriteMode),
                     fsPathFromList)

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

data TVarTraceEvent = TVarTraceOpening
                    | TVarTraceOpened
                    | TVarTraceClosing
                    | TVarTraceClosed
                    | TVarTraceCopying     !FS.FsPath -- ^ To
                    | TVarTraceCopied      !FS.FsPath -- ^ To
                    | TVarTraceWrite       !(WithOrigin SlotNo) !SlotNo
                    | TVarTraceInitialisingFromSnapshot !FS.FsPath
                    | TVarTraceInitialisedFromSnapshot !FS.FsPath
                    | TVarTraceInitialisingFromValues !(WithOrigin SlotNo)
  deriving (Show, Eq)

-- | Use a 'TVar' as a trivial backing store
newTVarBackingStoreInitialiser ::
     (IOLike m, NoThunks values)
  => Tracer m TVarTraceEvent
  -> (keys -> values -> values)
  -> (RangeQuery keys -> values -> values)
  -> (values -> diff -> values)
  -> (values -> CBOR.Encoding)
  -> (forall s. CBOR.Decoder s values)
  -> SomeHasFS m
  -> InitFrom values
  -> m (BackingStore m keys values diff)
newTVarBackingStoreInitialiser tracer lookup_ rangeRead_ forwardValues_ enc dec (SomeHasFS fs0) initialization = do
    traceWith tracer TVarTraceOpening
    ref <- do
      (slot, values) <- case initialization of
        InitFromCopy (BackingStorePath path) -> do
          traceWith tracer $ TVarTraceInitialisingFromSnapshot path
          tvarFileExists <- doesFileExist fs0 (extendPath path)
          unless tvarFileExists $
            throwIO . StoreDirIsIncompatible $ mkFsErrorPath fs0 path
          withFile fs0 (extendPath path) ReadMode $ \h -> do
            bs <- hGetAll fs0 h
            case CBOR.deserialiseFromBytes ((,) <$> CBOR.fromCBOR <*> dec) bs of
              Left  err        -> throwIO $ TVarBackingStoreDeserialiseExn err
              Right (extra, x) -> do
                unless (BSL.null extra) $ throwIO TVarIncompleteDeserialiseExn
                traceWith tracer $ TVarTraceInitialisedFromSnapshot path
                pure x
        InitFromValues slot values -> do
          traceWith tracer $ TVarTraceInitialisingFromValues slot
          pure (slot, values)
      newTVarIO $ TVarBackingStoreContents slot values
    traceWith tracer TVarTraceOpened
    pure BackingStore {
        bsClose    = do
            traceWith tracer TVarTraceClosing
            atomically $ do
              guardClosed ref
              writeTVar ref TVarBackingStoreContentsClosed
            traceWith tracer TVarTraceClosed
      , bsCopy = \(SomeHasFS fs) (BackingStorePath path) -> do
          traceWith tracer $ TVarTraceCopying path
          join $ atomically $ do
            readTVar ref >>= \case
              TVarBackingStoreContentsClosed                ->
                throwSTM TVarBackingStoreClosedExn
              TVarBackingStoreContents slot values -> pure $ do
                exists <- doesDirectoryExist fs path
                when exists $ throwIO TVarBackingStoreDirectoryExists
                createDirectory fs path
                withFile fs (extendPath path) (WriteMode MustBeNew) $ \h -> do
                  void $ hPutAll fs h $ CBOR.toLazyByteString $ CBOR.toCBOR slot <> enc values
          traceWith tracer $ TVarTraceCopied path
      , bsValueHandle = join $ atomically $ do
          readTVar ref >>= \case
            TVarBackingStoreContentsClosed                ->
              throwSTM TVarBackingStoreClosedExn
            TVarBackingStoreContents slot values -> pure $ do
              refHandleClosed <- newTVarIO False
              pure $ (,) slot $ BackingStoreValueHandle {
                  bsvhClose     = atomically $ do
                    guardClosed ref
                    guardHandleClosed refHandleClosed
                    writeTVar refHandleClosed True
                , bsvhRangeRead = \rq -> atomically $ do
                    guardClosed ref
                    guardHandleClosed refHandleClosed
                    pure $ rangeRead_ rq values
                , bsvhRead      = \keys -> atomically $ do
                    guardClosed ref
                    guardHandleClosed refHandleClosed
                    pure $ lookup_ keys values
                }
      , bsWrite    = \slot2 diff -> do
         slot1 <- atomically $ do
          readTVar ref >>= \case
            TVarBackingStoreContentsClosed        ->
              throwSTM TVarBackingStoreClosedExn
            TVarBackingStoreContents slot1 values -> do
              unless (slot1 <= At slot2) $
                throwSTM $ TVarBackingStoreNonMonotonicSeq (At slot2) slot1
              writeTVar ref $
                TVarBackingStoreContents
                  (At slot2)
                  (forwardValues_ values diff)
              pure slot1
         traceWith tracer $ TVarTraceWrite slot1 slot2
      }
  where
    extendPath path =
      fsPathFromList $ fsPathToList path <> [fromString "tvar"]

guardClosed ::
     IOLike m
  => StrictTVar m (TVarBackingStoreContents ks vs)
  -> STM m ()
guardClosed ref = readTVar ref >>= \case
  TVarBackingStoreContentsClosed -> throwSTM TVarBackingStoreClosedExn
  TVarBackingStoreContents _ _   -> pure ()

guardHandleClosed ::
     IOLike m
  => StrictTVar m Bool
  -> STM m ()
guardHandleClosed refHandleClosed = do
  isClosed <- readTVar refHandleClosed
  when isClosed $ throwSTM TVarBackingStoreValueHandleClosedExn

{-------------------------------------------------------------------------------
  Errors
-------------------------------------------------------------------------------}

newtype StoreDirIsIncompatible = StoreDirIsIncompatible FsErrorPath
  deriving anyclass (Exception)

instance Show StoreDirIsIncompatible where
  show (StoreDirIsIncompatible p) =
       "In-Memory database not found in the database directory: "
    <> show p
    <> ".\nPre-UTxO-HD and LMDB implementations are incompatible with the In-Memory \
       \ implementation. Please delete your ledger database directory."
