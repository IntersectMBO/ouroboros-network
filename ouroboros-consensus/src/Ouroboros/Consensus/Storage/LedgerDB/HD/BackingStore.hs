{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE Rank2Types                 #-}
{-# LANGUAGE StandaloneDeriving         #-}

module Ouroboros.Consensus.Storage.LedgerDB.HD.BackingStore (
    -- * Backing store interface
    BackingStore (..)
  , BackingStorePath (..)
  , BackingStoreValueHandle (..)
  , bsRead
  , withBsValueHandle
    -- * An in-memory backing store
  , newTVarBackingStore
  , TVarBackingStoreClosedExn (..)
  , TVarBackingStoreDeserialiseExn (..)
  , TVarBackingStoreValueHandleClosedExn (..)
  ) where

import qualified Codec.CBOR.Read as CBOR
import qualified Codec.CBOR.Write as CBOR
import qualified Control.Exception as Exn
import           Control.Monad (join, unless, void)
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

-- | An ephemeral handle to an immutable value of the entire database
--
-- The performance cost is usually minimal unless this handle is held open too
-- long.
data BackingStoreValueHandle m keys values = BackingStoreValueHandle {
    -- | Close the handle
    --
    -- Other methods throw exceptions if called on a closed handle.
    bsvhClose :: !(m ())
    -- | Read the given keys from the handle
    --
    -- Absent keys will merely not be present in the result instead of causing a
    -- failure or an exception.
  , bsvhRead  :: !(keys -> m values)
  }

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

data TVarBackingStoreClosedExn = TVarBackingStoreClosedExn
  deriving anyclass (Exn.Exception)
  deriving stock    (Show)

data TVarBackingStoreValueHandleClosedExn = TVarBackingStoreValueHandleClosedExn
  deriving anyclass (Exn.Exception)
  deriving stock    (Show)

data TVarBackingStoreDeserialiseExn =
    TVarBackingStoreDeserialiseExn CBOR.DeserialiseFailure
  | TVarIncompleteDeserialiseExn
  deriving anyclass (Exn.Exception)
  deriving stock    (Show)

-- | Use a 'TVar' as a trivial backing store
newTVarBackingStore ::
     (IOLike m, NoThunks values)
  => (keys -> values -> values)
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
newTVarBackingStore lookup_ forwardValues_ enc dec initialization = do
    ref <- do
      (slot, values) <- case initialization of
        Left (FS.SomeHasFS fs, BackingStorePath path) -> do
          FS.withFile fs (extendPath path) FS.ReadMode $ \h -> do
            bs <- FS.hGetAll fs h
            case CBOR.deserialiseFromBytes ((,) <$> CBOR.fromCBOR <*> dec) bs of
              Left  err        -> Exn.throw $ TVarBackingStoreDeserialiseExn err
              Right (extra, x) -> do
                unless (BSL.null extra) $ Exn.throw TVarIncompleteDeserialiseExn
                pure x
        Right x -> pure x
      IOLike.newTVarIO $ TVarBackingStoreContents slot values
    pure BackingStore {
        bsClose    = IOLike.atomically $ do
          IOLike.writeTVar ref TVarBackingStoreContentsClosed
      , bsCopy = \(FS.SomeHasFS fs) (BackingStorePath path) ->
          join $ IOLike.atomically $ do
            IOLike.readTVar ref >>= \case
              TVarBackingStoreContentsClosed                ->
                pure $ Exn.throw TVarBackingStoreClosedExn
              TVarBackingStoreContents slot values -> pure $ do
                FS.createDirectory fs path
                FS.withFile fs (extendPath path) (FS.WriteMode FS.MustBeNew) $ \h -> do
                  void $ FS.hPutAll fs h $ CBOR.toLazyByteString $ CBOR.toCBOR slot <> enc values
      , bsValueHandle = join $ IOLike.atomically $ do
          IOLike.readTVar ref >>= \case
            TVarBackingStoreContentsClosed                ->
              pure $ Exn.throw TVarBackingStoreClosedExn
            TVarBackingStoreContents slot values -> pure $ do
              refHandleClosed <- IOLike.newTVarIO False
              pure $ (,) slot $ BackingStoreValueHandle {
                  bsvhClose = IOLike.atomically $ do
                    IOLike.writeTVar refHandleClosed True
                , bsvhRead  = \keys -> join $ IOLike.atomically $ do
                    isClosed <- IOLike.readTVar refHandleClosed
                    pure $
                      if isClosed
                      then Exn.throw TVarBackingStoreValueHandleClosedExn
                      else pure $ lookup_ keys values
                }
      , bsWrite    = \slot2 diff -> join $ IOLike.atomically $ do
          IOLike.readTVar ref >>= \case
            TVarBackingStoreContentsClosed        ->
              pure $ Exn.throw TVarBackingStoreClosedExn
            TVarBackingStoreContents slot1 values ->
              Exn.assert (slot1 <= At slot2) $ do
                IOLike.writeTVar ref $
                  TVarBackingStoreContents
                    (At slot2)
                    (forwardValues_ values diff)
                pure $ pure ()
      }
  where
    extendPath path =
      FS.fsPathFromList $ FS.fsPathToList path <> [fromString "tvar"]
