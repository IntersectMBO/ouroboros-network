{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE EmptyDataDeriving          #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE QuantifiedConstraints      #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}

module Ouroboros.Consensus.Storage.LedgerDB.InMemory (
    -- * LedgerDB proper
    LedgerDbCfg (..)
  , ledgerDbWithAnchor
  , oldLedgerDbWithAnchor
    -- ** opaque
  , LedgerDB
  , NewLedgerDB
  , OldLedgerDB
    -- ** Queries
  , ledgerDbAnchor
  , ledgerDbBimap
  , ledgerDbCurrent
  , ledgerDbFlush
  , ledgerDbPast
  , ledgerDbPrefix
  , ledgerDbPrune
  , ledgerDbSnapshots
  , ledgerDbTip
  , oldLedgerDbAnchor
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
    -- * Sync both databases
  , bringUpNewLedgerDB
  , bringUpOldLedgerDB
  , combineLedgerDBs
    -- * Exports for the benefit of tests
    -- ** Additional queries
  , ledgerDbIsSaturated
  , ledgerDbMaxRollback
    -- ** Pure API
  , ledgerDbPush'
  , ledgerDbPushMany'
  , ledgerDbSwitch'
  ) where

import           Control.Monad.Except hiding (ap)
import           Control.Monad.Reader hiding (ap)
import           Data.Functor.Identity
import           Data.Kind (Constraint, Type)
import           Data.Word
import           GHC.Generics (Generic)
import           NoThunks.Class (NoThunks)

import           Ouroboros.Network.AnchoredSeq (Anchorable (..),
                     AnchoredSeq (..))
import qualified Ouroboros.Network.AnchoredSeq as AS

import           Control.Exception
import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Config
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Ledger.Inspect
import           Ouroboros.Consensus.Storage.LedgerDB.Types (PushGoal (..),
                     PushStart (..), Pushing (..),
                     UpdateLedgerDbTraceEvent (..))
import           Ouroboros.Consensus.Storage.LedgerDB.UTxOHD
import           Ouroboros.Consensus.Util

{-------------------------------------------------------------------------------
  Ledger DB types
-------------------------------------------------------------------------------}

-- | Internal state of the ledger DB
--
-- The ledger DB looks like
--
-- > anchor |> snapshots <| current
--
-- where @anchor@ records the oldest known snapshot and @current@ the most
-- recent. The anchor is the oldest point we can roll back to.
--
-- We take a snapshot after each block is applied and keep in memory a window
-- of the last @k@ snapshots. We have verified empirically (#1936) that the
-- overhead of keeping @k snapshots in memory is small, i.e., about 5%
-- compared to keeping a snapshot every 100 blocks. This is thanks to sharing
-- between consecutive snapshots.
--
-- As an example, suppose we have @k = 6@. The ledger DB grows as illustrated
-- below, where we indicate the anchor number of blocks, the stored snapshots,
-- and the current ledger.
--
-- > anchor |> #   [ snapshots ]                   <| tip
-- > ---------------------------------------------------------------------------
-- > G      |> (0) [ ]                             <| G
-- > G      |> (1) [ L1]                           <| L1
-- > G      |> (2) [ L1,  L2]                      <| L2
-- > G      |> (3) [ L1,  L2,  L3]                 <| L3
-- > G      |> (4) [ L1,  L2,  L3,  L4]            <| L4
-- > G      |> (5) [ L1,  L2,  L3,  L4,  L5]       <| L5
-- > G      |> (6) [ L1,  L2,  L3,  L4,  L5,  L6]  <| L6
-- > L1     |> (6) [ L2,  L3,  L4,  L5,  L6,  L7]  <| L7
-- > L2     |> (6) [ L3,  L4,  L5,  L6,  L7,  L8]  <| L8
-- > L3     |> (6) [ L4,  L5,  L6,  L7,  L8,  L9]  <| L9   (*)
-- > L4     |> (6) [ L5,  L6,  L7,  L8,  L9,  L10] <| L10
-- > L5     |> (6) [*L6,  L7,  L8,  L9,  L10, L11] <| L11
-- > L6     |> (6) [ L7,  L8,  L9,  L10, L11, L12] <| L12
-- > L7     |> (6) [ L8,  L9,  L10, L12, L12, L13] <| L13
-- > L8     |> (6) [ L9,  L10, L12, L12, L13, L14] <| L14
--
-- The ledger DB must guarantee that at all times we are able to roll back @k@
-- blocks. For example, if we are on line (*), and roll back 6 blocks, we get
--
-- > L3 |> []
data LedgerDB (l :: LedgerStateKind) = LedgerDB {
      -- | Ledger states
      ledgerDbCheckpoints :: AnchoredSeq
                               (WithOrigin SlotNo)
                               (Checkpoint (l ValuesMK))
                               (Checkpoint (l ValuesMK))
    , ledgerDbChangelog   :: DbChangelog l
    , runDual             :: Bool
    }
  deriving (Generic)

type OldLedgerDB l = AnchoredSeq (WithOrigin SlotNo) (Checkpoint (l ValuesMK)) (Checkpoint (l ValuesMK))
type NewLedgerDB l = DbChangelog l

deriving instance (Eq       (l EmptyMK), Eq       (l ValuesMK)) => Eq       (LedgerDB l)
deriving instance (NoThunks (l EmptyMK), NoThunks (l ValuesMK)) => NoThunks (LedgerDB l)

instance ShowLedgerState l => Show (LedgerDB l) where
  showsPrec = error "showsPrec @LedgerDB"

type instance HeaderHash (LedgerDB l) = HeaderHash l

instance IsLedger l => GetTip (LedgerDB l) where
  getTip = castPoint . getTip . ledgerDbCurrent

-- | Internal newtype wrapper around a ledger state @l@ so that we can define a
-- non-blanket 'Anchorable' instance.
newtype Checkpoint l = Checkpoint {
      unCheckpoint :: l
    }
  deriving (Generic)

deriving instance          Eq       (l ValuesMK) => Eq       (Checkpoint (l ValuesMK))
deriving anyclass instance NoThunks (l ValuesMK) => NoThunks (Checkpoint (l ValuesMK))

instance ShowLedgerState l => Show (Checkpoint (l ValuesMK)) where
  showsPrec = error "showsPrec @CheckPoint"

instance GetTip (l ValuesMK) => Anchorable (WithOrigin SlotNo) (Checkpoint (l ValuesMK)) (Checkpoint (l ValuesMK)) where
  asAnchor = id
  getAnchorMeasure _ = getTipSlot . unCheckpoint

{-------------------------------------------------------------------------------
  Construct LedgerDB
-------------------------------------------------------------------------------}

-- | Empty LedgerDB starting at the specified ledger state
ledgerDbWithAnchor :: GetTip (l ValuesMK) => l ValuesMK -> LedgerDB l
ledgerDbWithAnchor anchor = LedgerDB {
      ledgerDbCheckpoints = Empty (Checkpoint anchor)
    , ledgerDbChangelog   = initialDbChangelog (getTipSlot anchor) anchor
    , runDual = True -- TODO: This is not yet implemented.
    }

oldLedgerDbWithAnchor :: GetTip (l ValuesMK) => l ValuesMK -> OldLedgerDB l
oldLedgerDbWithAnchor anchor = Empty (Checkpoint anchor)

-- | Both databases are already in sync, just combine them in the @LedgerDB@ datatype.
combineLedgerDBs :: Bool          -- ^ Whether to run both databases TODO: Still not implemented
                 -> OldLedgerDB l -- ^ The old-style database
                 -> NewLedgerDB l -- ^ The new-style database
                 -> (LedgerDB l, (Word64, Word64))
combineLedgerDBs runDual ledgerDbCheckpoints ledgerDbChangelog = (LedgerDB {..}, (0,0))

{-------------------------------------------------------------------------------
  Compute signature

  Depending on the parameters (apply by value or by reference, previously
  applied or not) we get different signatures.
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

-- | Annotated ledger errors
data AnnLedgerError (l :: LedgerStateKind) blk = AnnLedgerError {
      -- | The ledger DB just /before/ this block was applied
      annLedgerState  :: LedgerDB l

      -- | Reference to the block that had the error
    , annLedgerErrRef :: RealPoint blk

      -- | The ledger error itself
    , annLedgerErr    :: LedgerErr l
    }

-- | Monads in which we can resolve blocks
--
-- To guide type inference, we insist that we must be able to infer the type
-- of the block we are resolving from the type of the monad.
class Monad m => ResolvesBlocks m blk | m -> blk where
  resolveBlock :: ResolveBlock m blk

instance Monad m => ResolvesBlocks (ReaderT (ResolveBlock m blk) m) blk where
  resolveBlock r = ReaderT $ \f -> f r

defaultResolveBlocks :: ResolveBlock m blk
                     -> ReaderT (ResolveBlock m blk) m a
                     -> m a
defaultResolveBlocks = flip runReaderT

-- Quite a specific instance so we can satisfy the fundep
instance Monad m
      => ResolvesBlocks (ExceptT e (ReaderT (ResolveBlock m blk) m)) blk where
  resolveBlock = lift . resolveBlock

class Monad m => ThrowsLedgerError m l blk where
  throwLedgerError :: LedgerDB l -> RealPoint blk -> LedgerErr l -> m a

defaultThrowLedgerErrors :: ExceptT (AnnLedgerError l blk) m a
                         -> m (Either (AnnLedgerError l blk) a)
defaultThrowLedgerErrors = runExceptT

defaultResolveWithErrors :: ResolveBlock m blk
                         -> ExceptT (AnnLedgerError l blk)
                                    (ReaderT (ResolveBlock m blk) m)
                                    a
                         -> m (Either (AnnLedgerError l blk) a)
defaultResolveWithErrors resolve =
      defaultResolveBlocks resolve
    . defaultThrowLedgerErrors

instance Monad m => ThrowsLedgerError (ExceptT (AnnLedgerError l blk) m) l blk where
  throwLedgerError l r e = throwError $ AnnLedgerError l r e

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
data Ap :: (Type -> Type) -> LedgerStateKind -> Type -> Constraint -> Type where
  ReapplyVal ::           blk -> Ap m l blk ( ReadsKeySets m l )
  ApplyVal   ::           blk -> Ap m l blk ( ReadsKeySets m l
                                            , ThrowsLedgerError m l blk )
  ReapplyRef :: RealPoint blk -> Ap m l blk ( ResolvesBlocks m blk
                                            , ReadsKeySets m l
                                            )
  ApplyRef   :: RealPoint blk -> Ap m l blk ( ResolvesBlocks m blk
                                            , ThrowsLedgerError m l blk
                                            , ReadsKeySets m l
                                            )

  -- | 'Weaken' increases the constraint on the monad @m@.
  --
  -- This is primarily useful when combining multiple 'Ap's in a single
  -- homogeneous structure.
  Weaken :: (c' => c) => Ap m l blk c -> Ap m l blk c'

{-------------------------------------------------------------------------------
  Internal utilities for 'Ap'
-------------------------------------------------------------------------------}

toRealPoint :: HasHeader blk => Ap m l blk c -> RealPoint blk
toRealPoint (ReapplyVal blk) = blockRealPoint blk
toRealPoint (ApplyVal blk)   = blockRealPoint blk
toRealPoint (ReapplyRef rp)  = rp
toRealPoint (ApplyRef rp)    = rp
toRealPoint (Weaken ap)      = toRealPoint ap

-- | Apply block to the current ledger state
--
-- We take in the entire 'LedgerDB' because we record that as part of errors.
applyBlock :: forall m c l blk
            . (ApplyBlock l blk, TickedTableStuff l, Monad m, c)
           => LedgerCfg l
           -> Ap m l blk c
           -> LedgerDB l
           -> m (l ValuesMK, l TrackingMK)
applyBlock cfg ap db = case ap of
    Weaken ap' ->
        applyBlock cfg ap' db
    _ -> do
      old <- oldApplyBlock ap
      new <- newApplyBlock ap

      assert (old == applyTracking oldLedgerDbCurrent new) $ return (old, new)

  where

    oldLedgerDbCurrent :: l ValuesMK
    oldLedgerDbCurrent = either unCheckpoint unCheckpoint . AS.head . ledgerDbCheckpoints $ db

    oldApplyBlock :: Ap m l blk c -> m (l ValuesMK)
    oldApplyBlock = \case
      ReapplyVal b -> return $ forgetLedgerStateTracking $ tickThenReapply cfg b oldLedgerDbCurrent

      ApplyVal b -> forgetLedgerStateTracking <$>
               (  either (throwLedgerError db (blockRealPoint b)) return
                $ runExcept
                $ tickThenApply cfg b oldLedgerDbCurrent)

      ReapplyRef r  -> do
        b <- resolveBlock r -- TODO: ask: would it make sense to recursively call applyBlock using ReapplyVal?

        return $ forgetLedgerStateTracking $ tickThenReapply cfg b oldLedgerDbCurrent

      ApplyRef r -> do
        b <- resolveBlock r

        forgetLedgerStateTracking <$>
             ( either (throwLedgerError db (blockRealPoint b)) return
             $ runExcept
             $ tickThenApply cfg b oldLedgerDbCurrent)

      -- A value of @Weaken@ will not make it to this point, as @applyBlock@ will recurse until it fully unwraps.
      Weaken _ -> error "unreachable"

    newApplyBlock :: Ap m l blk c -> m (l TrackingMK)
    newApplyBlock = \case
      ReapplyVal b -> withBlockReadSets b (ledgerDbChangelog db) $ \lh ->
          return $ tickThenReapply cfg b lh

      ApplyVal b -> withBlockReadSets b (ledgerDbChangelog db) $ \lh ->
               ( either (throwLedgerError db (blockRealPoint b)) return
               $ runExcept
               $ tickThenApply cfg b lh)

      ReapplyRef r  -> do
        b <- resolveBlock r -- TODO: ask: would it make sense to recursively call applyBlock using ReapplyVal?

        withBlockReadSets b (ledgerDbChangelog db) $ \lh ->
          return $ tickThenReapply cfg b lh

      ApplyRef r -> do
        b <- resolveBlock r

        withBlockReadSets b (ledgerDbChangelog db) $ \lh ->
          either (throwLedgerError db (blockRealPoint b)) return $ runExcept $
             tickThenApply cfg b lh

      -- A value of @Weaken@ will not make it to this point, as @applyBlock@ will recurse until it fully unwraps.
      Weaken _ -> error "unreachable"

withBlockReadSets
  :: forall m blk l. (ApplyBlock l blk, Monad m, TableStuff l, ReadsKeySets m l)
  => blk
  -> DbChangelog l
  -> (l ValuesMK -> m (l TrackingMK))
  -> m (l TrackingMK)
withBlockReadSets b db f  = do
  let ks = getBlockKeySets b :: TableKeySets l
  let aks = rewindTableKeySets' db ks :: RewoundTableKeySets l
  urs <- readDb aks
  case withHydratedLedgerState urs db f   of
    Nothing ->
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
      error "Changelog rewind;read;forward sequence failed."
    Just res -> res

withHydratedLedgerState
  :: forall l a. (TableStuff l)
  => UnforwardedReadSets l
  -> DbChangelog l
  -> (l ValuesMK -> a)
  -> Maybe a
withHydratedLedgerState urs db f = do
  rs <- forwardTableKeySets' db urs
  return $ f $ withLedgerTables (seqLast . dbChangelogStates $ db)  rs

{-------------------------------------------------------------------------------
  Queries

  Note that queries will return always the new-style results. We insist on the
  old-style database to be just a backup that ensures results are consistent, but
  it should be invisible to outer modules.
-------------------------------------------------------------------------------}

-- | The ledger state at the tip of the chain
ledgerDbCurrent :: LedgerDB l -> l EmptyMK
ledgerDbCurrent = seqLast . dbChangelogStates . ledgerDbChangelog

-- | Information about the state of the ledger at the anchor
ledgerDbAnchor :: LedgerDB l -> l EmptyMK
ledgerDbAnchor LedgerDB{..} = seqAt (dbChangelogStateAnchor ledgerDbChangelog) (dbChangelogStates ledgerDbChangelog)

oldLedgerDbAnchor :: LedgerDB l -> l ValuesMK
oldLedgerDbAnchor LedgerDB{..} = unCheckpoint . AS.anchor $ ledgerDbCheckpoints

-- | All snapshots currently stored by the ledger DB (new to old)
--
-- This also includes the snapshot at the anchor. For each snapshot we also
-- return the distance from the tip.
ledgerDbSnapshots :: LedgerDB l -> [(Word64, l EmptyMK)]
ledgerDbSnapshots LedgerDB{} = undefined

-- | How many blocks can we currently roll back?
ledgerDbMaxRollback :: GetTip (l ValuesMK) => LedgerDB l -> Word64
ledgerDbMaxRollback LedgerDB{..} =
  let
    old = fromIntegral (AS.length ledgerDbCheckpoints)
    new = fromIntegral
        $ seqLength
        $ seqAnchorAt (dbChangelogStateAnchor ledgerDbChangelog) (dbChangelogStates ledgerDbChangelog)
  in
    assert (old == new) new

-- | Reference to the block at the tip of the chain
ledgerDbTip :: IsLedger l => LedgerDB l -> Point (l EmptyMK)
ledgerDbTip = getTip . ledgerDbCurrent

-- | Have we seen at least @k@ blocks?
ledgerDbIsSaturated :: (forall mk. GetTip (l mk)) => SecurityParam -> LedgerDB l -> Bool
ledgerDbIsSaturated (SecurityParam k) db =
    ledgerDbMaxRollback db >= k

-- | Get a past ledger state
--
--  \( O(\log(\min(i,n-i)) \)
--
-- When no ledger state (or anchor) has the given 'Point', 'Nothing' is
-- returned.
ledgerDbPast :: (HasHeader blk, IsLedger l, HeaderHash l ~ HeaderHash blk)
  => Point blk
  -> LedgerDB l
  -> Maybe (l EmptyMK)
ledgerDbPast pt db = ledgerDbCurrent <$> ledgerDbPrefix pt db

-- | Get a prefix of the LedgerDB
--
--  \( O(\log(\min(i,n-i)) \)
--
-- When no ledger state (or anchor) has the given 'Point', 'Nothing' is
-- returned.
ledgerDbPrefix ::
     (HasHeader blk, IsLedger l, HeaderHash l ~ HeaderHash blk)
  => Point blk
  -> LedgerDB l
  -> Maybe (LedgerDB l)
ledgerDbPrefix pt db
    | pt == (castPoint $ getTip (ledgerDbAnchor db))
    = let old = Empty . AS.anchor . ledgerDbCheckpoints $ db
          new = initialDbChangelogWithEmptyState undefined undefined
      in
        Just . fst $ combineLedgerDBs (runDual db) old new
    | otherwise
    =  do
        checkpoints' <- AS.rollback
                          (pointSlot pt)
                          ((== pt) . castPoint . getTip . unCheckpoint . either id id)
                          (ledgerDbCheckpoints db)

        return $ LedgerDB
                  { ledgerDbCheckpoints = checkpoints'
                  , ledgerDbChangelog   = dbChangelogRollBack (pointSlot pt) $ ledgerDbChangelog db
                  , runDual = runDual db
                  }


-- | Transform the underlying 'AnchoredSeq' using the given functions.
ledgerDbBimap ::
     Anchorable (WithOrigin SlotNo) a b
  => (l ValuesMK -> a)
  -> (l ValuesMK -> b)
  -> AnchoredSeq (WithOrigin SlotNo)
                 (Checkpoint (l ValuesMK))
                 (Checkpoint (l ValuesMK))
  -> AnchoredSeq (WithOrigin SlotNo) a b
ledgerDbBimap f g =
    -- Instead of exposing 'ledgerDbCheckpoints' directly, this function hides
    -- the internal 'Checkpoint' type.
    AS.bimap (f . unCheckpoint) (g . unCheckpoint)


-- | Prune snapshots until at we have at most @k@ snapshots in the LedgerDB,
-- excluding the snapshots stored at the anchor.
ledgerDbPrune :: GetTip (l ValuesMK) => SecurityParam -> LedgerDB l -> LedgerDB l
ledgerDbPrune (SecurityParam k) db = db {
      ledgerDbCheckpoints = AS.anchorNewest k (ledgerDbCheckpoints db)
    }

 -- NOTE: we must inline 'ledgerDbPrune' otherwise we get unexplained thunks in
 -- 'LedgerDB' and thus a space leak. Alternatively, we could disable the
 -- @-fstrictness@ optimisation (enabled by default for -O1). See #2532.
{-# INLINE ledgerDbPrune #-}

-- | Flush the @DbChangelog@ using the functions provided by the
-- @OnDiskLedgerStDb@ interface.
ledgerDbFlush :: Monad m => OnDiskLedgerStDb m l blk -> RealPoint blk -> LedgerDB l -> m (LedgerDB l)
ledgerDbFlush OnDiskLedgerStDb{..} pointToFlush db@LedgerDB{..} = do
  ledgerDbChangelog' <- flushDb pointToFlush ledgerDbChangelog
  return db { ledgerDbChangelog = ledgerDbChangelog' }

{-------------------------------------------------------------------------------
  Internal updates
-------------------------------------------------------------------------------}

-- | Push an updated ledger state to both databases
pushLedgerState ::
     (IsLedger l, TickedTableStuff l)
  => SecurityParam
  -> (l ValuesMK, l TrackingMK) -- ^ Updated ledger state
  -> LedgerDB l -> LedgerDB l
pushLedgerState secParam (currentOld', currentNew') db@LedgerDB{..}  =
    ledgerDbPrune secParam $ db {
        ledgerDbCheckpoints = ledgerDbCheckpoints AS.:> Checkpoint currentOld'
      , ledgerDbChangelog   = extendDbChangelog (stateSeqNo currentNew')
                                                (trackingTablesToDiffs currentNew')
                                                ledgerDbChangelog
      }

-- | The old ledger DB is behind the new ledger DB.
--
-- Pushes blocks in the ImmutableDB up to the tip of the new ledger DB so that
-- they end up being in sync.
bringUpOldLedgerDB :: forall m blk l e.
  ( Monad m
  , ApplyBlock l blk
  , Show e
  , TickedTableStuff l
  )
  => Bool          -- ^ Whether to run both databases TODO: Still not implemented
  -> LedgerDbCfg l -- ^ The LedgerDB configuration
  -> NewLedgerDB l -- ^ The new-style database
  -> ((blk -> (OldLedgerDB l, Word64) -> m (OldLedgerDB l, Word64))
     -> ExceptT e m (OldLedgerDB l, Word64)) -- ^ The continuation. In particular this should be used
                                             -- with @streamUpTo@ from
                                             -- @Ouroboros.Consensus.Storage.LedgerDB.OnDisk.StreamAPI@
  -> m (LedgerDB l, (Word64, Word64))
bringUpOldLedgerDB runDual cfg ledgerDbChangelog cont = do
    either
      (error . ("invariant violation: invalid current chain:" <>) . show)
      (\(ledgerDbCheckpoints, w) -> return (LedgerDB{..}, (w, 0)))
    =<<
      runExceptT (cont push)
  where
    push :: blk
         ->   (OldLedgerDB l, Word64)
         -> m (OldLedgerDB l, Word64)
    push blk !(!db, !replayed) = do
      let ls =   forgetLedgerStateTracking
               $ tickThenReapply (ledgerDbCfg cfg) blk
               $ either unCheckpoint unCheckpoint . AS.head
               $ db

          !db' = AS.anchorNewest (maxRollbacks $ ledgerDbCfgSecParam cfg) (db AS.:> Checkpoint ls)

          !replayed' = replayed + 1

      return (db', replayed')

-- | The new ledger DB is behind the old ledger DB.
--
-- Pushes blocks in the ImmutableDB up to the tip of the old ledger DB so that
-- they end up being in sync.
bringUpNewLedgerDB :: forall m blk l e.
  ( Show e
  , Monad m
  , InspectLedger blk
  , TickedTableStuff l
  , ApplyBlock l blk
  )
  => Bool                                         -- ^ Whether to run both databases TODO: Still not implemented
  -> (RealPoint blk -> [LedgerEvent blk] -> m ()) -- ^ An action that logs replay events. Should be used with a @replayTracer@.
  -> LedgerDbCfg l                                -- ^ The LedgerDB configuration
  -> (LedgerCfg l -> TopLevelConfig blk)          -- ^ Get the top level configuration from the ledger configuration.
  -> (l EmptyMK -> LedgerState blk EmptyMK)       -- ^ Get the underlying @LedgerState@ from @l@
  -> OnDiskLedgerStDb m l blk                     -- ^ The backend handle
  -> OldLedgerDB l                                -- ^ The old-style database
  -> ((blk -> (NewLedgerDB l, Word64) -> m (NewLedgerDB l, Word64))
     -> ExceptT e m (NewLedgerDB l, Word64))      -- ^ The continuation. In particular this should be used with
                                                  -- @streamUpTo@ from
                                                  -- @Ouroboros.Consensus.Storage.LedgerDB.OnDisk.StreamAPI@
  -> m (LedgerDB l, (Word64, Word64))
bringUpNewLedgerDB runDual tracer cfg getTopLevelConfig getLedgerState onDiskLedgerDbSt ledgerDbCheckpoints cont =
    either
      (error . ("invariant violation: invalid current chain:" <>) . show)
      (\(ledgerDbChangelog, w) -> return (LedgerDB{..}, (0, w)))
    =<<
      runExceptT (cont push)
  where
    push :: blk
         ->   (NewLedgerDB l, Word64)
         -> m (NewLedgerDB l, Word64)
    push blk !(db, replayed) = do
      ls <- defaultReadKeySets (readKeySets onDiskLedgerDbSt) (withBlockReadSets blk db $ \lh -> return $ tickThenReapply (ledgerDbCfg cfg) blk lh)
      db'' <- ( if replayed `mod` 2160 == 0
                then flushDb onDiskLedgerDbSt undefined
                else return)
              $ extendDbChangelog (stateSeqNo ls) (trackingTablesToDiffs ls) db
        -- TODO: here initStartingWith could and should probably flush!
        --
        -- If we're replaying from genesis (or simply replaying a very
        -- large number of blocks) we will need to flush from time to
        -- time inside this function.
        --
        -- Note that at the moment no LedgerDB snapshots are taken by
        -- this function.
        --
        -- Note that whatever restore point we take here, it will
        -- belong to the immutable part of the chain. So the restore
        -- point will not lie ahead of the immutable DB tip.
      let replayed' :: Word64
          !replayed' = replayed + 1

          events :: [LedgerEvent blk]
          events = inspectLedger
                       (getTopLevelConfig (ledgerDbCfg cfg))
                       (getLedgerState (seqLast . dbChangelogStates $ db))
                       (getLedgerState (seqLast . dbChangelogStates $ db''))

      tracer (blockRealPoint blk) events
      return (db'', replayed')

{-------------------------------------------------------------------------------
  Internal: rolling back
-------------------------------------------------------------------------------}

-- | Rollback
--
-- Returns 'Nothing' if maximum rollback is exceeded.
rollback :: forall l. GetTip (l ValuesMK) => Word64 -> LedgerDB l -> Maybe (LedgerDB l)
rollback n db@LedgerDB{..}
    | n <= ledgerDbMaxRollback db
    = Just db {
          ledgerDbCheckpoints = AS.dropNewest (fromIntegral n) ledgerDbCheckpoints,
          ledgerDbChangelog = dbChangelogRollBack (undefined n :: WithOrigin SlotNo) ledgerDbChangelog
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

ledgerDbPush :: forall m c l blk
              . (ApplyBlock l blk, TickedTableStuff l, Monad m, c)
             => LedgerDbCfg l
             -> Ap m l blk c -> LedgerDB l -> m (LedgerDB l)
ledgerDbPush cfg ap db =
    (\current' -> pushLedgerState (ledgerDbCfgSecParam cfg) current' db) <$>
      applyBlock (ledgerDbCfg cfg) ap db

-- | Push a bunch of blocks (oldest first)
ledgerDbPushMany ::
     forall m c l blk . (ApplyBlock l blk, TickedTableStuff l, Monad m, c)
  => (Pushing blk -> m ())
  -> LedgerDbCfg l
  -> [Ap m l blk c] -> LedgerDB l -> m (LedgerDB l)
ledgerDbPushMany trace cfg aps initDb = (repeatedlyM pushAndTrace) aps initDb
  where
    pushAndTrace ap db = do
      let pushing = Pushing . toRealPoint $ ap
      trace pushing
      ledgerDbPush cfg ap db

-- | Switch to a fork
ledgerDbSwitch :: (ApplyBlock l blk, TickedTableStuff l, Monad m, c)
               => LedgerDbCfg l
               -> Word64          -- ^ How many blocks to roll back
               -> (UpdateLedgerDbTraceEvent blk -> m ())
               -> [Ap m l blk c]  -- ^ New blocks to apply
               -> LedgerDB l
               -> m (Either ExceededRollback (LedgerDB l))
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
          Right <$> ledgerDbPushMany (trace . (StartedPushingBlockToTheLedgerDb start goal))
                                     cfg
                                     newBlocks
                                     db'

{-------------------------------------------------------------------------------
  LedgerDB Config
-------------------------------------------------------------------------------}

data LedgerDbCfg l = LedgerDbCfg {
      ledgerDbCfgSecParam :: !SecurityParam
    , ledgerDbCfg         :: !(LedgerCfg l)
    }
  deriving (Generic)

deriving instance NoThunks (LedgerCfg l) => NoThunks (LedgerDbCfg l)

{-------------------------------------------------------------------------------
  Support for testing
-------------------------------------------------------------------------------}

pureBlock :: blk -> Ap m l blk (ReadsKeySets m l)
pureBlock = ReapplyVal

ledgerDbPush' :: (ApplyBlock l blk, TickedTableStuff l, ReadsKeySets Identity l)
              => LedgerDbCfg l -> blk -> LedgerDB l -> LedgerDB l
ledgerDbPush' cfg b = runIdentity . ledgerDbPush cfg (pureBlock b)

ledgerDbPushMany' :: (ApplyBlock l blk, TickedTableStuff l, ReadsKeySets Identity l)
                  => LedgerDbCfg l -> [blk] -> LedgerDB l -> LedgerDB l
ledgerDbPushMany' cfg bs =
  runIdentity . ledgerDbPushMany (const $ pure ()) cfg (map pureBlock bs)

ledgerDbSwitch' :: forall l blk
                 . (ApplyBlock l blk, TickedTableStuff l, ReadsKeySets Identity l)
                => LedgerDbCfg l
                -> Word64 -> [blk] -> LedgerDB l -> Maybe (LedgerDB l)
ledgerDbSwitch' cfg n bs db =
    case runIdentity $ ledgerDbSwitch cfg n (const $ pure ()) (map pureBlock bs) db of
      Left  ExceededRollback{} -> Nothing
      Right db'                -> Just db'
