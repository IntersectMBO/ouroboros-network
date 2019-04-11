{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiWayIf                 #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TupleSections              #-}

module Ouroboros.Storage.LedgerDB.OnDisk (
    -- * Opening the database
    initLedgerDB
    -- ** Abstraction over the stream API
  , NextBlock(..)
  , StreamAPI(..)
    -- * Write to disk
  , takeSnapshot
    -- * Low-level API (primarily exposed for testing)
  , DiskSnapshot
  , deleteSnapshot
  , snapshotToPath
  ) where

import qualified Codec.CBOR.Write as CBOR
import           Codec.Serialise.Decoding (Decoder)
import           Codec.Serialise.Encoding (Encoding)
import           Control.Monad.Except
import qualified Data.List as List
import           Data.Maybe (mapMaybe)
import           Data.Set (Set)
import qualified Data.Set as Set
import           GHC.Generics (Generic)
import           GHC.Stack
import           System.IO (IOMode (..))
import           Text.Read (readMaybe)

import           Control.Monad.Class.MonadST
import           Control.Monad.Class.MonadThrow

import           Ouroboros.Consensus.Util.CBOR (ReadIncrementalErr,
                     readIncremental)

import           Ouroboros.Storage.FS.API
import           Ouroboros.Storage.FS.API.Types

import           Ouroboros.Storage.LedgerDB.Conf
import           Ouroboros.Storage.LedgerDB.InMemory
import           Ouroboros.Storage.LedgerDB.MemPolicy

{-------------------------------------------------------------------------------
  Abstraction over the streaming API provided by the Chain DB
-------------------------------------------------------------------------------}

-- | Next block returned during streaming
data NextBlock r b = NoMoreBlocks | NextBlock (r, b)

-- | Abstraction over block streaming
--
-- In CPS form to enable the use of 'withXYZ' style iterator init functions.
data StreamAPI m r b = StreamAPI {
      -- | Start streaming after the specified block
      streamAfter :: forall a.
           Tip r
        -- Current tip (exclusive lower bound point for streaming)

        -> (Maybe (m (NextBlock r b)) -> m a)
        -- Get the next block (by value)
        --
        -- 'Nothing' if the lower bound could not be found
        -- (this can happen if the chain was truncated due to disk corruption
        -- and this particular snapshot is now too recent)

        -> m a
    }

-- | Stream all blocks
streamAll :: forall m r b e a. Monad m
          => StreamAPI m r b
          -> Tip r                -- ^ Starting point for streaming
          -> (Tip r -> e)         -- ^ Error when tip not found
          -> a                    -- ^ Starting point when tip /is/ found
          -> ((r, b) -> a -> m a) -- ^ Update function for each block
          -> ExceptT e m a
streamAll StreamAPI{..} tip notFound e f = ExceptT $
    streamAfter tip $ \case
      Nothing      -> return $ Left (notFound tip)
      Just getNext -> do
        let go :: a -> m a
            go a = do mNext <- getNext
                      case mNext of
                        NoMoreBlocks -> return a
                        NextBlock b  -> go =<< f b a
        Right <$> go e

{-------------------------------------------------------------------------------
  Initialize the DB
-------------------------------------------------------------------------------}

-- | Initialize the ledger DB from the most recent snapshot on disk
--
-- If no such snapshot can be found, use the genesis ledger DB.
--
-- We do /not/ catch any exceptions thrown during streaming; should any be
-- thrown, it is the responsibility of the 'ChainStateDB' to catch these
-- and trigger (further) validation. We only discard snapshots if
--
-- * We cannot deserialise them, or
-- * They are too close to the tip of the chain to give all snapshots required
--   by the memory policy (the snapshot being /ahead/ of the chain is a
--   special case of this).
--
-- We do /not/ attempt to use multiple ledger states from disk to construct the
-- ledger DB. Instead we load only a /single/ ledger state from disk, and
-- /compute/ all subsequent ones. This is important, because the ledger states
-- obtained in this way will (hopefully) share much of their memory footprint
-- with their predecessors.
initLedgerDB :: forall m h l r b e. (MonadST m, MonadThrow m)
             => HasFS m h
             -> (forall s. Decoder s l)
             -> (forall s. Decoder s r)
             -> MemPolicy
             -> LedgerDbConf m l r b e
             -> StreamAPI m r b
             -> m (LedgerDB l r)
initLedgerDB hasFS decLedger decRef policy conf streamAPI = do
    snapshots <- listSnapshots hasFS
    tryNewestFirst snapshots
  where
    tryNewestFirst :: [DiskSnapshot] -> m (LedgerDB l r)
    tryNewestFirst [] = do
        -- We're out of snapshots. Start at genesis
        initDb <- ledgerDbFromGenesis policy <$> ledgerDbGenesis conf
        ml     <- runExceptT $ initStartingWith conf streamAPI initDb
        case ml of
          Left _  -> error "invariant violation: invalid current chain"
          Right l -> return l
    tryNewestFirst (s:ss) = do
        -- If we fail to use this snapshot, delete it and try an older one
        ml <- runExceptT $ initFromSnapshot
                             hasFS
                             decLedger
                             decRef
                             policy
                             conf
                             streamAPI
                             s
        case ml of
          Left _  -> deleteSnapshot hasFS s >> tryNewestFirst ss
          Right l -> return l

{-------------------------------------------------------------------------------
  Internal: initialize using the given snapshot
-------------------------------------------------------------------------------}

data InitFailure r =
    -- | We failed to deserialise the snapshot
    --
    -- This can happen due to data corruption in the ledger DB.
    InitFailureRead ReadIncrementalErr

    -- | This snapshot is too recent (ahead of the tip of the chain)
  | InitFailureTooRecent (Tip r)

    -- | Insufficient blocks applied to on-disk snapshot
    --
    -- We apply blocks to the on-disk snapshot to get back to a state where we
    -- can support @k@ rollback. If a particular snapshot is too recent (due
    -- to data loss in either the immutable DB or the volatile DB), we must
    -- try an older snapshot instead.
  | InitFailureIncomplete

-- | Attempt to initialize the ledger DB from the given snapshot
--
-- If the chain DB or ledger layer reports an error, the whole thing is aborted
-- and an error is returned. This should not throw any errors itself (ignoring
-- unexpected exceptions such as asynchronous exceptions, of course).
initFromSnapshot :: forall m h l r b e.
                    (MonadST m, MonadThrow m)
                 => HasFS m h
                 -> (forall s. Decoder s l)
                 -> (forall s. Decoder s r)
                 -> MemPolicy
                 -> LedgerDbConf m l r b e
                 -> StreamAPI m r b
                 -> DiskSnapshot
                 -> ExceptT (InitFailure r) m (LedgerDB l r)
initFromSnapshot hasFS decLedger decRef policy conf streamAPI ss = do
    initDb <- withExceptT InitFailureRead $
                ledgerDbFromChain policy <$>
                  readSnapshot hasFS decLedger decRef ss
    initStartingWith conf streamAPI initDb

-- | Attempt to initialize the ledger DB starting from the given ledger DB
initStartingWith :: forall m l r b e. Monad m
                 => LedgerDbConf m l r b e
                 -> StreamAPI m r b
                 -> LedgerDB l r
                 -> ExceptT (InitFailure r) m (LedgerDB l r)
initStartingWith conf@LedgerDbConf{..} streamAPI initDb = do
    applied <- streamAll streamAPI (ledgerDbTip initDb)
                 InitFailureTooRecent
                 initDb
                 push
    ExceptT $ return $ if ledgerDbIsComplete applied
                         then Right applied
                         else Left InitFailureIncomplete
  where
    push :: (r, b) -> LedgerDB l r -> m (LedgerDB l r)
    push b db = mustBeValid <$> ledgerDbPush conf (BlockVal NotPrevApplied b) db

    mustBeValid :: Either e (LedgerDB l r) -> LedgerDB l r
    mustBeValid (Left _)   = error invalidChain
    mustBeValid (Right db) = db

    invalidChain :: String
    invalidChain = concat [
          "initStartingWith: "
        , "invariant vaiolation: "
        , "ChainDB selected invalid chain"
        ]

{-------------------------------------------------------------------------------
  Write to disk
-------------------------------------------------------------------------------}

-- | Take a snapshot of the ledger DB
--
-- NOTE: This is a lower-level API that unconditionally takes a snapshot
-- (i.e., independent from whether this snapshot corresponds to a state that
-- is more than @k@ back).
--
-- TODO: Should we delete the file if an error occurs during writing?
takeSnapshot :: (MonadThrow m)
             => HasFS m h
             -> (l -> Encoding)
             -> (r -> Encoding)
             -> LedgerDB l r -> m DiskSnapshot
takeSnapshot hasFS encLedger encRef db = do
    ss <- nextAvailable <$> listSnapshots hasFS
    writeSnapshot hasFS encLedger encRef ss (ledgerDbTail db)
    return ss

{-------------------------------------------------------------------------------
  Internal: reading from disk
-------------------------------------------------------------------------------}

-- | On disk snapshots are numbered monotonically
newtype DiskSnapshot = DiskSnapshot Int
  deriving (Show, Eq, Ord, Generic)

-- | Number of the next snapshot, given snapshots currently on disk
nextAvailable :: [DiskSnapshot] -> DiskSnapshot
nextAvailable [] = DiskSnapshot 1
nextAvailable ss = let DiskSnapshot n = maximum ss in DiskSnapshot (n + 1)

-- | Read snapshot from disk
readSnapshot :: forall m l r h. (MonadST m, MonadThrow m)
             => HasFS m h
             -> (forall s. Decoder s l)
             -> (forall s. Decoder s r)
             -> DiskSnapshot
             -> ExceptT ReadIncrementalErr m (ChainSummary l r)
readSnapshot hasFS decLedger decRef =
      ExceptT
    . readIncremental hasFS decoder
    . snapshotToPath
  where
    decoder :: Decoder s (ChainSummary l r)
    decoder = decodeChainSummary decLedger decRef

-- | Write snapshot to disk
writeSnapshot :: forall m l r h. MonadThrow m
              => HasFS m h
              -> (l -> Encoding)
              -> (r -> Encoding)
              -> DiskSnapshot -> ChainSummary l r -> m ()
writeSnapshot hasFS@HasFS{..} encLedger encRef ss cs = do
    withFile hasFS (snapshotToPath ss) WriteMode $ \h ->
      void $ hPut h $ CBOR.toBuilder (encode cs)
  where
    encode :: ChainSummary l r -> Encoding
    encode = encodeChainSummary encLedger encRef

-- | Delete snapshot from disk
deleteSnapshot :: HasCallStack => HasFS m h -> DiskSnapshot -> m ()
deleteSnapshot HasFS{..} = removeFile . snapshotToPath

-- | List on-disk snapshots, most recent first
listSnapshots :: Monad m => HasFS m h -> m [DiskSnapshot]
listSnapshots HasFS{..} =
    aux <$> listDirectory []
  where
    aux :: Set String -> [DiskSnapshot]
    aux = List.sortBy (flip compare) . mapMaybe snapshotFromPath . Set.toList

snapshotToPath :: DiskSnapshot -> FsPath
snapshotToPath (DiskSnapshot ss) = [show ss]

snapshotFromPath :: String -> Maybe DiskSnapshot
snapshotFromPath = fmap DiskSnapshot . readMaybe
