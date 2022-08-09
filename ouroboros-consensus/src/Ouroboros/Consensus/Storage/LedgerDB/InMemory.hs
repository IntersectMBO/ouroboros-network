{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE EmptyDataDeriving          #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuantifiedConstraints      #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}

-- TODO stylish haskell insists on deleting this pragma if placed in the group above.
{-# LANGUAGE DerivingStrategies         #-}

-- TODO: we agreed on removing the ledgerDb prefix from the operations in this
-- module. We can import the module qualified if we need to dissambiguate.
module Ouroboros.Consensus.Storage.LedgerDB.InMemory (
    -- * LedgerDB proper
    LedgerDbCfg (..)
  , ledgerDbWithAnchor
    -- ** opaque
  , LedgerDB
    -- * Ledger DB types (TODO: we might want to place this somewhere else)
  , DbChangelog
  , DbReader (..)
  , KeySetsReader
  , ReadsKeySets (..)
  , RewoundTableKeySets (..)
  , UnforwardedReadSets (..)
  , defaultReadKeySets
  , forwardTableKeySets
  , rewindTableKeySets
    -- ** Serialisation
  , decodeSnapshotBackwardsCompatible
  , encodeSnapshot
    -- ** Queries
  , ledgerDbAnchor
  , ledgerDbChangelog
  , ledgerDbCurrent
  , ledgerDbFlush
  , ledgerDbLastFlushedState
  , ledgerDbPast
  , ledgerDbPrefix
  , ledgerDbPrune
  , ledgerDbSnapshots
  , ledgerDbTip
  , volatileStatesBimap
    -- ** Running updates
  , AnnLedgerError (..)
  , Ap (..)
  , ResolveBlock
  , ResolvesBlocks (..)
  , ThrowsLedgerError (..)
  , defaultResolveBlocks
  , defaultResolveWithErrors
  , defaultThrowLedgerErrors
    -- ** Updates
  , ExceededRollback (..)
  , ledgerDbPush
  , ledgerDbSwitch
    -- * Exports for the benefit of tests
    -- ** Additional queries
  , ledgerDbIsSaturated
  , ledgerDbMaxRollback
    -- ** Pure API
  , ledgerDbPush'
  , ledgerDbPushMany'
  , ledgerDbSwitch'
  ) where

import           Codec.Serialise.Decoding (Decoder)
import qualified Codec.Serialise.Decoding as Dec
import           Codec.Serialise.Encoding (Encoding)
import           Control.Monad.Except hiding (ap)
import           Control.Monad.Reader hiding (ap)
import           Data.Functor.Identity
import           Data.Kind (Constraint, Type)
import           Data.Word
import           GHC.Generics (Generic)
import           GHC.Stack (HasCallStack)
import           NoThunks.Class (NoThunks)

import           Ouroboros.Network.AnchoredSeq (Anchorable (..),
                     AnchoredSeq (..))
import qualified Ouroboros.Network.AnchoredSeq as AS

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Config
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Ledger.SupportsMempool
import           Ouroboros.Consensus.Ledger.SupportsUTxOHD
import qualified Ouroboros.Consensus.Ledger.Extended as Extended
import           Ouroboros.Consensus.Storage.LedgerDB.HD
import           Ouroboros.Consensus.Storage.LedgerDB.Types (PushGoal (..),
                     PushStart (..), Pushing (..),
                     UpdateLedgerDbTraceEvent (..))
import           Ouroboros.Consensus.Util
import           Ouroboros.Consensus.Util.CBOR (decodeWithOrigin)
import           Ouroboros.Consensus.Util.Versioned

{-------------------------------------------------------------------------------
  Ledger DB types
-------------------------------------------------------------------------------}

-- | Type alias for the new-style database
--
-- This type is a 'DbChangelog'. We illustrate its contents below, where @k = 3@
-- (for a state @Li@, the corresponding set of differences is @Di@):
--
-- > stateAnchor | diskAnchor | states                     | tableDiffs
-- > --------------------------------------------------------------------------
-- >      0      |      0     | [ L0 ]                     | [ ]
-- >      0      |      0     | [ L0, L1 ]                 | [ D1 ]
-- >      0      |      0     | [ L0, L1, L2 ]             | [ D1, D2 ]
-- >      0      |      0     | [ L0, L1, L2, L3 ]         | [ D1, D2, D3 ]
-- >      1      |      0     | [ L0, L1, L2, L3, L4 ]     | [ D1, D2, D3, D4 ]
-- >      2      |      0     | [ L0, L1, L2, L3, L4, L5 ] | [ D1, D2, D3, D4, D5 ]    (*)
-- >      2      |      2     | [ L2, L3, L4, L5 ]         | [ D3, D4, D5 ]   -- flush (**)
-- >      3      |      2     | [ L2, L3, L4, L5, L6 ]     | [ D3, D4, D5, D6 ]
--
-- The disk anchor moves when we flush data to disk, and the state anchor points
-- always to the state that represents the tip of the logical immutable
-- database. Notice that @seqNo (last states) - stateAnchor@ is usually @k@
-- except when rollbacks or data corruption take place and will be less than @k@
-- when we just loaded a snapshot. We cannot roll back more than @k@ blocks.
-- This means that after a rollback of @k@ blocks at (*), the database will look
-- something like this:
--
-- >      2      |      0     | [ L0, L1, L2 ]             | [ D1, D2 ]
--
-- And a rollback of @k@ blocks at (**) will look something like this:
--
-- >      2      |      0     | [ L2 ]                     | [ ]
--
-- Notice how the states list always contains the in-memory state of the anchor,
-- but the table differences might not contain the differences for that anchor
-- if they have been flushed to the backend.
--
-- The new-style database has to be coupled with the @BackingStore@ which
-- provides the pointers to the on-disk data.
newtype LedgerDB (l :: LedgerStateKindWithTables) (wt :: SwitchLedgerTables) = LedgerDB { ledgerDbChangelog :: DbChangelog l wt }
  deriving (Generic)

deriving newtype instance Eq       (DbChangelog l wt) => Eq       (LedgerDB l wt)
deriving newtype instance NoThunks (DbChangelog l wt) => NoThunks (LedgerDB l wt)

deriving instance (ShowLedgerState l, ShowLedgerState (LedgerTables l), IsSwitchLedgerTables wt) => Show (LedgerDB l wt)

{-------------------------------------------------------------------------------
  LedgerDB proper
-------------------------------------------------------------------------------}

-- | Ledger DB starting at the specified ledger state
ledgerDbWithAnchor ::
     ( TableStuff l wt
     , GetTip (l wt EmptyMK)
     )
  => l wt EmptyMK -> LedgerDB l wt
ledgerDbWithAnchor = LedgerDB . emptyDbChangeLog

{-------------------------------------------------------------------------------
  Resolve a block
-------------------------------------------------------------------------------}

-- | Resolve a block
--
-- Resolving a block reference to the actual block lives in @m@ because
-- it might need to read the block from disk (and can therefore not be
-- done inside an STM transaction).
--
-- NOTE: The ledger DB will only ask the 'ChainDB' for blocks it knows
-- must exist. If the 'ChainDB' is unable to fulfill the request, data
-- corruption must have happened and the 'ChainDB' should trigger
-- validation mode.
type ResolveBlock m blk = RealPoint blk -> m blk

-- | Monads in which we can resolve blocks
--
-- To guide type inference, we insist that we must be able to infer the type
-- of the block we are resolving from the type of the monad.
class Monad m => ResolvesBlocks m blk | m -> blk where
  resolveBlock :: ResolveBlock m blk

instance Monad m => ResolvesBlocks (ReaderT (ResolveBlock m blk) m) blk where
  resolveBlock r = ReaderT $ \f -> f r

instance (Monad m, ResolvesBlocks m blk) => ResolvesBlocks (ExceptT e m) blk where
  resolveBlock = lift . resolveBlock

defaultResolveBlocks :: ResolveBlock m blk
                     -> ReaderT (ResolveBlock m blk) m a
                     -> m a
defaultResolveBlocks = flip runReaderT

{-------------------------------------------------------------------------------
  Throw a ledger error
-------------------------------------------------------------------------------}

-- | Annotated ledger errors
data AnnLedgerError (l :: LedgerStateKindWithTables) blk wt = AnnLedgerError {
      -- | The ledger DB just /before/ this block was applied
      annLedgerState  :: LedgerDB l wt

      -- | Reference to the block that had the error
    , annLedgerErrRef :: RealPoint blk

      -- | The ledger error itself
    , annLedgerErr    :: LedgerErr l
    }

-- | Monads in which we can throw ledger errors
class Monad m => ThrowsLedgerError m l blk wt where
  throwLedgerError :: LedgerDB l wt -> RealPoint blk -> LedgerErr l -> m a

instance Monad m => ThrowsLedgerError (ExceptT (AnnLedgerError l blk wt) m) l blk wt where
  throwLedgerError l r e = throwError $ AnnLedgerError l r e

defaultThrowLedgerErrors :: ExceptT (AnnLedgerError l blk wt) m a
                         -> m (Either (AnnLedgerError l blk wt) a)
defaultThrowLedgerErrors = runExceptT

defaultResolveWithErrors :: ResolveBlock m blk
                         -> ExceptT (AnnLedgerError l blk wt)
                                    (ReaderT (ResolveBlock m blk) m)
                                    a
                         -> m (Either (AnnLedgerError l blk wt) a)
defaultResolveWithErrors resolve =
      defaultResolveBlocks resolve
    . defaultThrowLedgerErrors

{-------------------------------------------------------------------------------
  Apply blocks on top of a LedgerDB
-------------------------------------------------------------------------------}

-- | 'Ap' is used to pass information about blocks to ledger DB updates
--
-- The constructors serve two purposes:
--
-- * Specify the various parameters
--   a. Are we passing the block by value or by reference?
--   b. Are we applying or reapplying the block?
--
-- * Compute the constraint @c@ on the monad @m@ in order to run the query:
--   a. If we are passing a block by reference, we must be able to resolve it.
--   b. If we are applying rather than reapplying, we might have ledger errors.
data Ap :: (Type -> Type) -> LedgerStateKindWithTables -> SwitchLedgerTables -> Type -> Constraint -> Type where
  ReapplyVal ::           blk -> Ap m l wt blk ( ReadsKeySets m l wt )
  ApplyVal   ::           blk -> Ap m l wt blk ( ReadsKeySets m l wt
                                               , ThrowsLedgerError m l blk wt )
  ReapplyRef :: RealPoint blk -> Ap m l wt blk ( ResolvesBlocks m blk
                                               , ReadsKeySets m l wt
                                               )
  ApplyRef   :: RealPoint blk -> Ap m l wt blk ( ResolvesBlocks m blk
                                               , ThrowsLedgerError m l blk wt
                                               , ReadsKeySets m l wt
                                               )

  -- | 'Weaken' increases the constraint on the monad @m@.
  --
  -- This is primarily useful when combining multiple 'Ap's in a single
  -- homogeneous structure.
  Weaken :: (c' => c) => Ap m l wt blk c -> Ap m l wt blk c'

toRealPoint :: HasHeader blk => Ap m l wt blk c -> RealPoint blk
toRealPoint (ReapplyVal blk) = blockRealPoint blk
toRealPoint (ApplyVal blk)   = blockRealPoint blk
toRealPoint (ReapplyRef rp)  = rp
toRealPoint (ApplyRef rp)    = rp
toRealPoint (Weaken ap)      = toRealPoint ap

-- | Apply block to the current ledger state
--
-- We take in the entire 'LedgerDB' because we record that as part of errors.
applyBlock :: forall m c (l :: Type -> LedgerStateKindWithTables) blk wt
            . ( ApplyBlock (l blk) blk
              , Monad m
              , c
              , HasCallStack
              , LedgerMustSupportUTxOHD l blk wt
              , IsSwitchLedgerTables wt
              )
           => LedgerCfg (l blk)
           -> Ap m (l blk) wt blk c
           -> LedgerDB (l blk) wt
           -> m (l blk wt DiffMK)
applyBlock cfg ap db = case ap of
  ReapplyVal b -> withBlockReadSets b $ \lh ->
    return $ tickThenReapply cfg b lh

  ApplyVal b -> withBlockReadSets b $ \lh ->
      either (throwLedgerError db (blockRealPoint b)) return
      $ runExcept
      $ tickThenApply cfg b lh

  ReapplyRef r  -> do
    b <- resolveBlock r -- TODO: ask: would it make sense to recursively call applyBlock using ReapplyVal?

    withBlockReadSets b $ \lh ->
      return $ tickThenReapply cfg b lh

  ApplyRef r -> do
    b <- resolveBlock r

    withBlockReadSets b $ \lh ->
      either (throwLedgerError db (blockRealPoint b)) return $ runExcept $
      tickThenApply cfg b lh
  Weaken ap' ->
    applyBlock cfg ap' db

  where
    withBlockReadSets
      :: ReadsKeySets m (l blk) wt
      => blk
      -> (l blk wt ValuesMK -> m (l blk wt DiffMK))
      -> m (l blk wt DiffMK)
    withBlockReadSets b f = do
      let ks = getBlockKeySets b :: LedgerTables (l blk) wt KeysMK
      let aks = rewindTableKeySets (ledgerDbChangelog db) ks :: RewoundTableKeySets (l blk) wt
      urs <- readDb aks
      case withHydratedLedgerState urs f of
        Left err ->
          -- We performed the rewind;read;forward sequence in this function. So
          -- the forward operation should not fail. If this is the case we're in
          -- the presence of a problem that we cannot deal with at this level,
          -- so we throw an error.
          --
          -- When we introduce pipelining, if the forward operation fails it
          -- could be because the DB handle was modified by a DB flush that took
          -- place when __after__ we read the unforwarded keys-set from disk.
          -- However, performing rewind;read;forward with the same __locked__
          -- changelog should always succeed.
          error $ "Changelog rewind;read;forward sequence failed, " <> show err
        Right res -> res

    withHydratedLedgerState
      :: UnforwardedReadSets (l blk) wt
      -> (l blk wt ValuesMK -> a)
      -> Either (WithOrigin SlotNo, WithOrigin SlotNo) a
    withHydratedLedgerState urs f = do
      fwd <- forwardTableKeySets (ledgerDbChangelog db) urs
      pure . f . withLedgerTables (ledgerDbCurrent db) $ fwd

{-------------------------------------------------------------------------------
  HD Interface that I need (Could be moved to  Ouroboros.Consensus.Ledger.Basics )
-------------------------------------------------------------------------------}

data RewoundTableKeySets l wt =
    RewoundTableKeySets
      !(WithOrigin SlotNo)   -- ^ the slot to which the keys were rewounded
      !(LedgerTables l wt KeysMK)

rewindTableKeySets ::
     DbChangelog l wt -> LedgerTables l wt KeysMK -> RewoundTableKeySets l wt
rewindTableKeySets dblog =
      RewoundTableKeySets
        (changelogDiffAnchor dblog)

data UnforwardedReadSets l wt = UnforwardedReadSets {
    -- | The Slot number of the anchor of the 'DbChangelog' that was used when
    -- rewinding and reading.
    ursSeqNo  :: !(WithOrigin SlotNo)
    -- | The values that were found in the 'BackingStore'.
  , ursValues :: !(LedgerTables l wt ValuesMK)
    -- | The values that were not present in the 'BackingStore'.
  , ursKeys   :: !(LedgerTables l wt KeysMK)
  }

forwardTableKeySets ::
     (TableStuff l wt, HasCallStack)
  => DbChangelog l wt
  -> UnforwardedReadSets l wt
  -> Either (WithOrigin SlotNo, WithOrigin SlotNo)
            (LedgerTables l wt ValuesMK)
forwardTableKeySets dblog = \(UnforwardedReadSets seqNo' values keys) ->
    if seqNo /= seqNo' then Left (seqNo, seqNo') else
    Right
      $ zipLedgerTables2 forward values keys
      $ changelogDiffs dblog
  where
    seqNo = changelogDiffAnchor dblog

    forward ::
         Ord k
      => ApplyMapKind ValuesMK  k v
      -> ApplyMapKind KeysMK    k v
      -> ApplyMapKind SeqDiffMK k v
      -> ApplyMapKind ValuesMK  k v
    forward (ApplyValuesMK values) (ApplyKeysMK keys) (ApplySeqDiffMK diffs) =
      ApplyValuesMK $ forwardValuesAndKeys values keys (cumulativeDiffSeqUtxoDiff diffs)

dbChangelogPrefix ::
     ( HasHeader blk, HeaderHash blk ~ HeaderHash (l wt EmptyMK)
     , GetTip (l wt EmptyMK)
     , TableStuff l wt
     )
  => Point blk -> DbChangelog l wt -> Maybe (DbChangelog l wt)
dbChangelogPrefix = prefixDbChangelog

-- | Isolates the prefix of the changelog that should be flushed
--
-- TODO take some argument to bound the size of the resulting prefix?
ledgerDbFlush ::
     (GetTip (l wt EmptyMK), TableStuff l wt)
  => DbChangelogFlushPolicy -> LedgerDB l wt -> (DbChangelog l wt, LedgerDB l wt)
ledgerDbFlush policy db = do
    (l, db { ledgerDbChangelog = r })
  where
    (l, r) = flushDbChangelog policy (ledgerDbChangelog db)

class ReadsKeySets m l wt where
  readDb :: KeySetsReader m l wt

type KeySetsReader m l wt = RewoundTableKeySets l wt -> m (UnforwardedReadSets l wt)

newtype DbReader m l wt a = DbReader { runDbReader :: ReaderT (KeySetsReader m l wt) m a}
  deriving newtype (Functor, Applicative, Monad)

instance ReadsKeySets (DbReader m l wt) l wt where
  readDb rks = DbReader $ ReaderT $ \f -> f rks

-- TODO: this is leaking details on how we want to compose monads at the higher levels.
instance (Monad m, ReadsKeySets m l wt) => ReadsKeySets (ReaderT r m) l wt where
  readDb = lift . readDb

instance (Monad m, ReadsKeySets m l wt) => ReadsKeySets (ExceptT e m) l wt where
  readDb = lift . readDb

instance
     ( ReadsKeySets Identity (LedgerState blk) wt
     , Extended.Promote (LedgerState blk) (Extended.ExtLedgerState blk) wt
     )
  => ReadsKeySets Identity (Extended.ExtLedgerState blk) wt where
  readDb (RewoundTableKeySets seqNo rew) = do
      UnforwardedReadSets seqNo' values keys <- readDb (RewoundTableKeySets seqNo rew)
      pure $ UnforwardedReadSets seqNo' values keys

defaultReadKeySets :: KeySetsReader m l wt -> DbReader m l wt a -> m a
defaultReadKeySets f dbReader = runReaderT (runDbReader dbReader) f

{-------------------------------------------------------------------------------
  Queries
-------------------------------------------------------------------------------}

-- | The ledger state at the tip of the chain
ledgerDbCurrent :: GetTip (l wt EmptyMK) => LedgerDB l wt -> l wt EmptyMK
ledgerDbCurrent =
    either unDbChangelogState unDbChangelogState
  . AS.head
  . changelogVolatileStates
  . ledgerDbChangelog

-- | Information about the state of the ledger at the anchor
ledgerDbAnchor :: LedgerDB l wt -> l wt EmptyMK
ledgerDbAnchor =
    unDbChangelogState
  . AS.anchor
  . changelogVolatileStates
  . ledgerDbChangelog

-- | Get the most recently flushed ledger state
--
-- TODO: the comment below might belong somewhere else.
--
-- This is what will be serialized when snapshotting.
--
-- When we take a snapshot, we do it for both the modern and the legacy ledger.
-- Conversely, when we read a snapshot, we assume the snapshot has what we need
-- for both the legacy and modern ledger. So the read/written modern and legacy
-- ledgers must always be coherent pairs, i.e. they're the ledger state as of
-- the same block.
--
-- The legacy snapshot is always written from the immutable tip. If we flush
-- first, then the modern snapshot also is. If we don't flush first, then the
-- modern snapshot might be written from something older than the immutable tip.
-- Therefore we guard this condition with an 'assert' and this imposes the
-- following PRECONDITION:
--
-- PRECONDITION: if you are running the legacy ledger, then you must flush
-- before calling this function
ledgerDbLastFlushedState :: LedgerDB l wt -> l wt EmptyMK
ledgerDbLastFlushedState =
    unDbChangelogState
  . AS.anchor
  . changelogImmutableStates
  . ledgerDbChangelog

-- | All snapshots currently stored by the ledger DB (new to old)
--
-- This also includes the snapshot at the anchor. For each snapshot we also
-- return the distance from the tip.
--
-- TODO: this name is misleading. Probably deserves a renaming.
ledgerDbSnapshots :: LedgerDB l wt -> [(Word64, l wt EmptyMK)]
ledgerDbSnapshots =
      zip [0..]
    . map unDbChangelogState
    . AS.toNewestFirst
    . changelogVolatileStates
    . ledgerDbChangelog

-- | How many blocks can we currently roll back?
ledgerDbMaxRollback :: (GetTip (l wt EmptyMK)) => LedgerDB l wt -> Word64
ledgerDbMaxRollback =
    fromIntegral
  . AS.length
  . changelogVolatileStates
  . ledgerDbChangelog

-- | Reference to the block at the tip of the chain
ledgerDbTip :: GetTip (l wt EmptyMK) => LedgerDB l wt -> Point (l wt EmptyMK)
ledgerDbTip = getTip . ledgerDbCurrent

-- | Have we seen at least @k@ blocks?
ledgerDbIsSaturated :: (forall mk. GetTip (l wt mk)) => SecurityParam -> LedgerDB l wt -> Bool
ledgerDbIsSaturated (SecurityParam k) db =
    ledgerDbMaxRollback db >= k

-- | Get a past ledger state
--
--  \( O(\log(\min(i,n-i)) \)
--
-- When no ledger state (or anchor) has the given 'Point', 'Nothing' is
-- returned.
ledgerDbPast ::
     ( HasHeader blk
     , TableStuff l wt
     , HeaderHash (l wt EmptyMK) ~ HeaderHash l
     , HeaderHash l ~ HeaderHash blk
     , GetTip (l wt EmptyMK)
     )
  => Point blk
  -> LedgerDB l wt
  -> Maybe (l wt EmptyMK)
ledgerDbPast pt db = ledgerDbCurrent <$> ledgerDbPrefix pt db

-- | Get a prefix of the LedgerDB that ends at the given point
--
--  \( O(\log(\min(i,n-i)) \)
--
-- When no ledger state (or anchor) has the given 'Point', 'Nothing' is
-- returned.
ledgerDbPrefix ::
     ( HasHeader blk
     , HeaderHash l ~ HeaderHash blk
     , HeaderHash (l wt EmptyMK) ~ HeaderHash l
     , TableStuff l wt
     , GetTip (l wt EmptyMK)
     )
  => Point blk
  -> LedgerDB l wt
  -> Maybe (LedgerDB l wt)
ledgerDbPrefix pt db
    | pt == castPoint (getTip (ledgerDbAnchor db))
    = Just . LedgerDB . prefixBackToAnchorDbChangelog $ ledgerDbChangelog db
    | otherwise
    = LedgerDB <$> dbChangelogPrefix pt (ledgerDbChangelog db)

-- | Transform the underlying 'AnchoredSeq' using the given functions.
volatileStatesBimap ::
     Anchorable (WithOrigin SlotNo) a b
  => (l wt EmptyMK -> a)
  -> (l wt EmptyMK -> b)
  -> LedgerDB l wt
  -> AnchoredSeq (WithOrigin SlotNo) a b
volatileStatesBimap f g =
      AS.bimap (f . unDbChangelogState) (g . unDbChangelogState)
    . changelogVolatileStates
    . ledgerDbChangelog

-- | Prune snapshots until at we have at most @k@ snapshots in the LedgerDB,
-- excluding the snapshots stored at the anchor.
ledgerDbPrune ::
     GetTip (l wt EmptyMK)
  => SecurityParam -> LedgerDB l wt -> LedgerDB l wt
ledgerDbPrune k db = db {
      ledgerDbChangelog   = pruneVolatilePartDbChangelog k (ledgerDbChangelog db)
    }

 -- NOTE: we must inline 'ledgerDbPrune' otherwise we get unexplained thunks in
 -- 'LedgerDB' and thus a space leak. Alternatively, we could disable the
 -- @-fstrictness@ optimisation (enabled by default for -O1). See #2532.
{-# INLINE ledgerDbPrune #-}

{-------------------------------------------------------------------------------
  Internal updates
-------------------------------------------------------------------------------}

-- | Push an updated ledger state
pushLedgerState ::
     (GetTip (l wt EmptyMK), TableStuff l wt)
  => SecurityParam
  -> l wt DiffMK -- ^ Updated ledger state
  -> LedgerDB l wt -> LedgerDB l wt
pushLedgerState secParam currentNew' db@LedgerDB{..}  =
    ledgerDbPrune secParam $ db {
        ledgerDbChangelog   =
          extendDbChangelog
            ledgerDbChangelog
            currentNew'
      }

{-------------------------------------------------------------------------------
  Internal: rolling back
-------------------------------------------------------------------------------}

-- | Rollback
--
-- Returns 'Nothing' if maximum rollback is exceeded.
rollback ::
     (GetTip (l wt EmptyMK), TableStuff l wt)
  => Word64
  -> LedgerDB l wt
  -> Maybe (LedgerDB l wt)
rollback n db@LedgerDB{..}
    | n <= ledgerDbMaxRollback db
    = Just db {
          ledgerDbChangelog   = rollbackDbChangelog (fromIntegral n)     ledgerDbChangelog
        }
    | otherwise
    = Nothing


{-------------------------------------------------------------------------------
  Updates
-------------------------------------------------------------------------------}

-- | Exceeded maximum rollback supported by the current ledger DB state
--
-- Under normal circumstances this will not arise. It can really only happen
-- in the presence of data corruption (or when switching to a shorter fork,
-- but that is disallowed by all currently known Ouroboros protocols).
--
-- Records both the supported and the requested rollback.
data ExceededRollback = ExceededRollback {
      rollbackMaximum   :: Word64
    , rollbackRequested :: Word64
    }

ledgerDbPush :: forall m c (l :: Type -> LedgerStateKindWithTables) blk wt
              . ( ApplyBlock       (l blk) blk
                , Monad m
                , c
                , HasCallStack
                , IsSwitchLedgerTables wt
                , LedgerMustSupportUTxOHD l blk wt
                )
             => LedgerDbCfg (l blk)
             -> Ap m (l blk) wt blk c -> LedgerDB (l blk) wt -> m (LedgerDB (l blk) wt)
ledgerDbPush cfg ap db =
    (\current' -> pushLedgerState (ledgerDbCfgSecParam cfg) current' db) <$>
      applyBlock (ledgerDbCfg cfg) ap db

-- | Push a bunch of blocks (oldest first)
ledgerDbPushMany ::
     forall m c (l :: Type -> LedgerStateKindWithTables) blk wt.
     ( ApplyBlock       (l blk) blk
     , Monad m
     , c
     , HasCallStack
     , IsSwitchLedgerTables wt
     , LedgerMustSupportUTxOHD l blk wt
     )
  => (Pushing blk -> m ())
  -> LedgerDbCfg (l blk)
  -> [Ap m (l blk) wt blk c] -> LedgerDB (l blk) wt -> m (LedgerDB (l blk) wt)
ledgerDbPushMany trace cfg = repeatedlyM pushAndTrace
  where
    pushAndTrace ap db = do
      let pushing = Pushing . toRealPoint $ ap
      trace pushing
      ledgerDbPush cfg ap db

-- | Switch to a fork
ledgerDbSwitch :: ( ApplyBlock       (l blk) blk
                  , Monad m
                  , c
                  , HasCallStack
                  , IsSwitchLedgerTables wt
                  , LedgerMustSupportUTxOHD l blk wt
                  )
               => LedgerDbCfg (l blk)
               -> Word64          -- ^ How many blocks to roll back
               -> (UpdateLedgerDbTraceEvent blk -> m ())
               -> [Ap m (l blk) wt blk c]  -- ^ New blocks to apply
               -> LedgerDB (l blk) wt
               -> m (Either ExceededRollback (LedgerDB (l blk) wt))
ledgerDbSwitch cfg numRollbacks trace newBlocks db =
    case rollback numRollbacks db of
      Nothing ->
        return $ Left $ ExceededRollback {
            rollbackMaximum   = ledgerDbMaxRollback db
          , rollbackRequested = numRollbacks
          }
      Just db' -> case newBlocks of
        [] -> pure $ Right db'
        -- no blocks to apply to ledger state, return current LedgerDB
        (firstBlock:_) -> do
          let start   = PushStart . toRealPoint $ firstBlock
              goal    = PushGoal  . toRealPoint . last $ newBlocks
          Right <$> ledgerDbPushMany (trace . StartedPushingBlockToTheLedgerDb start goal)
                                     cfg
                                     newBlocks
                                     db'

{-------------------------------------------------------------------------------
  LedgerDB Config
-------------------------------------------------------------------------------}

data LedgerDbCfg l = LedgerDbCfg {
      ledgerDbCfgSecParam :: !SecurityParam
    , ledgerDbCfg         :: !(LedgerCfg l)

      -- TODO: we might need to add support for specifying the flushing rate and
      -- policy in this datatype
    }
  deriving (Generic)

deriving instance NoThunks (LedgerCfg l) => NoThunks (LedgerDbCfg l)

type instance HeaderHash (LedgerDB l wt) = HeaderHash l

instance ( HeaderHash (l wt EmptyMK) ~ HeaderHash l
         , GetTip (l wt EmptyMK)
         ) => GetTip (LedgerDB l wt) where
  getTip = castPoint . getTip . ledgerDbCurrent

{-------------------------------------------------------------------------------
  Support for testing
-------------------------------------------------------------------------------}

pureBlock :: blk -> Ap m l wt blk (ReadsKeySets m l wt)
pureBlock = ReapplyVal

ledgerDbPush' :: ( ApplyBlock            (l blk) blk
                 , ReadsKeySets Identity (l blk) wt
                 , HasCallStack
                 , IsSwitchLedgerTables        wt
                 , LedgerMustSupportUTxOHD l blk wt
                 )
              => LedgerDbCfg (l blk) -> blk -> LedgerDB (l blk) wt -> LedgerDB (l blk) wt
ledgerDbPush' cfg b = runIdentity . ledgerDbPush cfg (pureBlock b)

ledgerDbPushMany' :: ( ApplyBlock            (l blk) blk
                     , ReadsKeySets Identity (l blk) wt
                     , HasCallStack
                     , IsSwitchLedgerTables wt
                     , LedgerMustSupportUTxOHD l blk wt
                     )
                  => LedgerDbCfg (l blk) -> [blk] -> LedgerDB (l blk) wt -> LedgerDB (l blk) wt
ledgerDbPushMany' cfg bs =
  runIdentity . ledgerDbPushMany (const $ pure ()) cfg (map pureBlock bs)

ledgerDbSwitch' :: forall l blk wt.
                   ( ApplyBlock            (l blk) blk
                   , ReadsKeySets Identity (l blk) wt
                   , HasCallStack
                   , IsSwitchLedgerTables wt
                   , LedgerMustSupportUTxOHD l blk wt
                   )
                => LedgerDbCfg (l blk)
                -> Word64 -> [blk] -> LedgerDB (l blk) wt -> Maybe (LedgerDB (l blk) wt)
ledgerDbSwitch' cfg n bs db =
    case runIdentity $ ledgerDbSwitch cfg n (const $ pure ()) (map pureBlock bs) db of
      Left  ExceededRollback{} -> Nothing
      Right db'                -> Just db'

{-------------------------------------------------------------------------------
  Serialisation
-------------------------------------------------------------------------------}

-- | Version 1: uses versioning ('Ouroboros.Consensus.Util.Versioned') and only
-- encodes the ledger state @l@.
snapshotEncodingVersion1 :: VersionNumber
snapshotEncodingVersion1 = 1

-- | Encoder to be used in combination with 'decodeSnapshotBackwardsCompatible'.
encodeSnapshot :: (l -> Encoding) -> l -> Encoding
encodeSnapshot encodeLedger l =
    encodeVersion snapshotEncodingVersion1 (encodeLedger l)

-- | To remain backwards compatible with existing snapshots stored on disk, we
-- must accept the old format as well as the new format.
--
-- The old format:
-- * The tip: @WithOrigin (RealPoint blk)@
-- * The chain length: @Word64@
-- * The ledger state: @l@
--
-- The new format is described by 'snapshotEncodingVersion1'.
--
-- This decoder will accept and ignore them. The encoder ('encodeSnapshot') will
-- no longer encode them.
decodeSnapshotBackwardsCompatible ::
     forall l blk.
     Proxy blk
  -> (forall s. Decoder s l)
  -> (forall s. Decoder s (HeaderHash blk))
  -> forall s. Decoder s l
decodeSnapshotBackwardsCompatible _ decodeLedger decodeHash =
    decodeVersionWithHook
      decodeOldFormat
      [(snapshotEncodingVersion1, Decode decodeVersion1)]
  where
    decodeVersion1 :: forall s. Decoder s l
    decodeVersion1 = decodeLedger

    decodeOldFormat :: Maybe Int -> forall s. Decoder s l
    decodeOldFormat (Just 3) = do
        _ <- withOriginRealPointToPoint <$>
               decodeWithOrigin (decodeRealPoint @blk decodeHash)
        _ <- Dec.decodeWord64
        decodeLedger
    decodeOldFormat mbListLen =
        fail $
          "decodeSnapshotBackwardsCompatible: invalid start " <>
          show mbListLen
