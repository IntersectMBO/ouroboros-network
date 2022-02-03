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
    -- ** opaque
  , LedgerDB
    -- * Ledger DB types (TODO: we might want to place this somewhere else)
  , DbChangelog
  , DbReader (..)
  , ReadsKeySets (..)
  , RewoundTableKeySets (..)
  , TypeOf_readDB
  , UnforwardedReadSets (..)
  , defaultReadKeySets
  , forwardTableKeySets
  , rewindTableKeySets
    -- ** Serialisation
  , decodeSnapshotBackwardsCompatible
  , encodeSnapshot
    -- ** Queries
  , ledgerDbAnchor
  , ledgerDbBimap
  , ledgerDbChangelog
  , ledgerDbCurrent
  , ledgerDbFlush
  , ledgerDbOldest
  , ledgerDbPast
  , ledgerDbPrefix
  , ledgerDbPrune
  , ledgerDbSnapshots
  , ledgerDbTip
  , ledgerDbVolatileCheckpoints
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
  , ledgerDbCurrentValues
    -- ** Pure API
  , ledgerDbPush'
  , ledgerDbPushMany'
  , ledgerDbSwitch'
  ) where

import           Codec.Serialise.Decoding (Decoder)
import qualified Codec.Serialise.Decoding as Dec
import           Codec.Serialise.Encoding (Encoding)
import qualified Control.Exception as Exn
import           Control.Monad.Except hiding (ap)
import           Control.Monad.Reader hiding (ap)
import           Data.Functor.Identity
import           Data.Kind (Constraint, Type)
import           Data.Word
--import qualified Debug.Trace
import           GHC.Generics (Generic)
import           GHC.Stack (HasCallStack)
import           NoThunks.Class (NoThunks)

import           Ouroboros.Network.AnchoredSeq (Anchorable (..),
                     AnchoredSeq (..))
import qualified Ouroboros.Network.AnchoredSeq as AS

import           Control.Exception
import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Config
import           Ouroboros.Consensus.Ledger.Abstract
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

deriving instance (Eq       (l EmptyMK), Eq       (l ValuesMK), Eq       (LedgerTables l SeqDiffMK)) => Eq       (LedgerDB l)
deriving instance (NoThunks (l EmptyMK), NoThunks (l ValuesMK), NoThunks (LedgerTables l SeqDiffMK)) => NoThunks (LedgerDB l)

deriving instance (ShowLedgerState l, ShowLedgerState (LedgerTables l)) => Show (LedgerDB l)

-- | Internal newtype wrapper around a ledger state @l@ so that we can define a
-- non-blanket 'Anchorable' instance.
newtype Checkpoint l = Checkpoint {
      unCheckpoint :: l
    }
  deriving (Generic)

deriving instance          Eq       (l ValuesMK) => Eq       (Checkpoint (l ValuesMK))
deriving anyclass instance NoThunks (l ValuesMK) => NoThunks (Checkpoint (l ValuesMK))

instance ShowLedgerState l => Show (Checkpoint (l ValuesMK)) where
  showsPrec p (Checkpoint l) =
        showParen (p > 10)
      $ showString "Checkpoint " . showsLedgerState sMapKind l

instance GetTip (l ValuesMK) => Anchorable (WithOrigin SlotNo) (Checkpoint (l ValuesMK)) (Checkpoint (l ValuesMK)) where
  asAnchor = id
  getAnchorMeasure _ = getTipSlot . unCheckpoint

instance GetTip (l EmptyMK) => Anchorable (WithOrigin SlotNo) (Checkpoint (l EmptyMK)) (Checkpoint (l EmptyMK)) where
  asAnchor = id
  getAnchorMeasure _ = getTipSlot . unCheckpoint

{-------------------------------------------------------------------------------
  LedgerDB proper
-------------------------------------------------------------------------------}

-- | Ledger DB starting at the specified ledger state
ledgerDbWithAnchor ::
     ( TableStuff l
     , GetTip (l EmptyMK)
     , GetTip (l ValuesMK)
     , StowableLedgerTables l
     )
  => l EmptyMK -> LedgerDB l
ledgerDbWithAnchor anchor = LedgerDB {
      ledgerDbCheckpoints = Empty (Checkpoint (unstowLedgerTables anchor))
    , ledgerDbChangelog   = emptyDbChangeLog anchor
    , runDual = True -- TODO: This is not yet implemented.
    }

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
            . (ApplyBlock l blk, TickedTableStuff l, Monad m, c, HasCallStack)
           => LedgerCfg l
           -> Ap m l blk c
           -> LedgerDB l
           -> m (l ValuesMK, l TrackingMK)
applyBlock cfg ap db = case ap of
    Weaken ap' ->
        applyBlock cfg ap' db
    _ -> do
      legacyLs' <- oldApplyBlock ap
      ls'       <- newApplyBlock ap

      let _ = legacyLs' :: l TrackingMK
          _ = ls'       :: l TrackingMK

          legacyValues          :: l ValuesMK
          legacyValues          =
            forgetLedgerStateTracking legacyLs'
          commutingSquareValues :: l ValuesMK
          commutingSquareValues =
              applyDiffsLedgerTables
                oldLedgerDbCurrent
            $ projectLedgerTables
            $ trackingTablesToDiffs ls'

          bare :: l any -> l EmptyMK
          bare = flip withLedgerTables emptyLedgerTables

{-
          showsTrackingDiff =
              showsLedgerState SKeysMK
            . mapLedgerTables (diffKeysMK . diffTrackingMK)
            . projectLedgerTables
-}

      id
{-        $ Debug.Trace.trace
            (($ "") $ showString "XXX\n" . showsTrackingDiff old . showString "\n" . showsTrackingDiff new)
        $ Debug.Trace.trace
            (($ "") $ showString "YYY\n" . showsLedgerState SEmptyMK (bare old) . showString "\n" . showsLedgerState SEmptyMK (bare new))
-}
        $ assert (bare legacyValues == bare ls')
        $ assert (projectLedgerTables legacyValues == projectLedgerTables commutingSquareValues)
        $ return (legacyValues, ls')
  where

    oldLedgerDbCurrent :: l ValuesMK
    oldLedgerDbCurrent = either unCheckpoint unCheckpoint . AS.head . ledgerDbCheckpoints $ db

    oldApplyBlock :: Ap m l blk c -> m (l TrackingMK)
    oldApplyBlock = \case
      ReapplyVal b -> return $ tickThenReapply cfg b oldLedgerDbCurrent

      ApplyVal b ->
               (  either (throwLedgerError db (blockRealPoint b)) return
                $ runExcept
                $ tickThenApply cfg b oldLedgerDbCurrent)

      ReapplyRef r  -> do
        b <- resolveBlock r -- TODO: ask: would it make sense to recursively call applyBlock using ReapplyVal?

        return $ tickThenReapply cfg b oldLedgerDbCurrent

      ApplyRef r -> do
        b <- resolveBlock r

        id $
             ( either (throwLedgerError db (blockRealPoint b)) return
             $ runExcept
             $ tickThenApply cfg b oldLedgerDbCurrent)

      -- A value of @Weaken@ will not make it to this point, as @applyBlock@ will recurse until it fully unwraps.
      Weaken _ -> error "unreachable"

    newApplyBlock :: Ap m l blk c -> m (l TrackingMK)
    newApplyBlock = \case
      ReapplyVal b -> withBlockReadSets b $ \lh ->
          return $ tickThenReapply cfg b lh

      ApplyVal b -> withBlockReadSets b $ \lh ->
               ( either (throwLedgerError db (blockRealPoint b)) return
               $ runExcept
               $ tickThenApply cfg b lh)

      ReapplyRef r  -> do
        b <- resolveBlock r -- TODO: ask: would it make sense to recursively call applyBlock using ReapplyVal?

        withBlockReadSets b $ \lh ->
          return $ tickThenReapply cfg b lh

      ApplyRef r -> do
        b <- resolveBlock r

        withBlockReadSets b $ \lh ->
          either (throwLedgerError db (blockRealPoint b)) return $ runExcept $
             tickThenApply cfg b lh

      -- A value of @Weaken@ will not make it to this point, as @applyBlock@ will recurse until it fully unwraps.
      Weaken _ -> error "unreachable"

    withBlockReadSets
      :: ReadsKeySets m l
      => blk
      -> (l ValuesMK -> m (l TrackingMK))
      -> m (l TrackingMK)
    withBlockReadSets b f = do
      let ks = getBlockKeySets b :: TableKeySets l
      let aks = rewindTableKeySets (ledgerDbChangelog db) ks :: RewoundTableKeySets l
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
      :: UnforwardedReadSets l
      -> (l ValuesMK -> a)
      -> Either (WithOrigin SlotNo, WithOrigin SlotNo) a
    withHydratedLedgerState urs f =
          (\rs -> f $ withLedgerTables (ledgerDbCurrent db) rs)
      <$> forwardTableKeySets (ledgerDbChangelog db) urs

{-------------------------------------------------------------------------------
  HD Interface that I need (Could be moved to  Ouroboros.Consensus.Ledger.Basics )
-------------------------------------------------------------------------------}

data RewoundTableKeySets l =
    RewoundTableKeySets
      !(WithOrigin SlotNo)   -- ^ the slot to which the keys were rewounded
      !(LedgerTables l RewoundMK)

rewindTableKeySets ::
     TableStuff l
  => DbChangelog l -> TableKeySets l -> RewoundTableKeySets l
rewindTableKeySets dblog = \keys ->
      RewoundTableKeySets
        (changelogDiffAnchor dblog)
    $ zipLedgerTables rewind keys
    $ changelogDiffs dblog
  where
    rewind ::
         Ord k
      => ApplyMapKind KeysMK    k v
      -> ApplyMapKind SeqDiffMK k v
      -> ApplyMapKind RewoundMK k v
    rewind (ApplyKeysMK keys) (ApplySeqDiffMK diffs) =
      ApplyRewoundMK $ rewindKeys keys (cumulativeDiffSeqUtxoDiff diffs)

data UnforwardedReadSets l = UnforwardedReadSets {
    ursSeqNo  :: !(WithOrigin SlotNo)
  , ursValues :: !(LedgerTables l ValuesMK)
  }

forwardTableKeySets ::
     TableStuff l
  => DbChangelog l
  -> UnforwardedReadSets l
  -> Either (WithOrigin SlotNo, WithOrigin SlotNo)
            (TableReadSets l)
forwardTableKeySets dblog = \(UnforwardedReadSets seqNo' values) ->
    if seqNo /= seqNo' then Left (seqNo, seqNo') else
    Right
      $ zipLedgerTables forward values
      $ changelogDiffs dblog
  where
    seqNo = changelogDiffAnchor dblog

    forward ::
         Ord k
      => ApplyMapKind ValuesMK  k v
      -> ApplyMapKind SeqDiffMK k v
      -> ApplyMapKind ValuesMK  k v
    forward (ApplyValuesMK values) (ApplySeqDiffMK diffs) =
      ApplyValuesMK $ forwardValues values (cumulativeDiffSeqUtxoDiff diffs)

ledgerDbVolatileCheckpoints ::
     LedgerDB l
  -> AnchoredSeq
       (WithOrigin SlotNo)
       (Checkpoint (l EmptyMK))
       (Checkpoint (l EmptyMK))
ledgerDbVolatileCheckpoints =
    dbChangelogVolatileCheckpoints . ledgerDbChangelog

dbChangelogVolatileCheckpoints ::
     DbChangelog l
  -> AnchoredSeq
       (WithOrigin SlotNo)
       (Checkpoint (l EmptyMK))
       (Checkpoint (l EmptyMK))
dbChangelogVolatileCheckpoints =
    AS.bimapPreservingMeasure
      (\(DbChangelogState l) -> Checkpoint l)
      (\(DbChangelogState l) -> Checkpoint l)
  . changelogVolatileStates

dbChangelogPrefix ::
     ( HasHeader blk, HeaderHash blk ~ HeaderHash (l EmptyMK)
     , GetTip (l EmptyMK)
     , TableStuff l
     )
  => Point blk -> DbChangelog l -> Maybe (DbChangelog l)
dbChangelogPrefix = prefixDbChangelog

-- | Isolates the prefix of the changelog that should be flushed
--
-- TODO take some argument to bound the size of the resulting prefix?
ledgerDbFlush ::
     (GetTip (l EmptyMK), TableStuff l)
  => DbChangelogFlushPolicy -> LedgerDB l -> (DbChangelog l, LedgerDB l)
ledgerDbFlush policy db = do
    (l, db { ledgerDbChangelog = r })
  where
    (l, r) = flushDbChangelog policy (ledgerDbChangelog db)

class ReadsKeySets m l where

  readDb :: TypeOf_readDB m l

type TypeOf_readDB m l = RewoundTableKeySets l -> m (UnforwardedReadSets l)

newtype DbReader m l a = DbReader { runDbReader :: ReaderT (TypeOf_readDB m l) m a}
  deriving newtype (Functor, Applicative, Monad)

instance ReadsKeySets (DbReader m l) l where
  readDb rks = DbReader $ ReaderT $ \f -> f rks

-- TODO: this is leaking details on how we want to compose monads at the higher levels.
instance (Monad m, ReadsKeySets m l) => ReadsKeySets (ReaderT r m) l where
  readDb = lift . readDb

instance (Monad m, ReadsKeySets m l) => ReadsKeySets (ExceptT e m) l where
  readDb = lift . readDb

instance
     ReadsKeySets Identity (LedgerState blk)
  => ReadsKeySets Identity (Extended.ExtLedgerState blk) where
  readDb (RewoundTableKeySets seqNo (Extended.ExtLedgerStateTables rew)) = do
      UnforwardedReadSets seqNo' values <- readDb (RewoundTableKeySets seqNo rew)
      pure $ UnforwardedReadSets seqNo' (Extended.ExtLedgerStateTables values)

defaultReadKeySets :: TypeOf_readDB m l -> DbReader m l a -> m a
defaultReadKeySets f dbReader = runReaderT (runDbReader dbReader) f

{-------------------------------------------------------------------------------
  Queries
-------------------------------------------------------------------------------}

-- | The ledger state at the tip of the chain
--
-- TODO This won't work if @not 'runDual'@.
ledgerDbCurrentValues :: GetTip (l ValuesMK) => LedgerDB l -> l ValuesMK
ledgerDbCurrentValues =
    either unCheckpoint unCheckpoint
  . AS.head
  . ledgerDbCheckpoints

-- | The ledger state at the tip of the chain
ledgerDbCurrent :: GetTip (l EmptyMK) => LedgerDB l -> l EmptyMK
ledgerDbCurrent =
    either unCheckpoint unCheckpoint
  . AS.head
  . dbChangelogVolatileCheckpoints
  . ledgerDbChangelog

-- | Information about the state of the ledger at the anchor
ledgerDbAnchor :: LedgerDB l -> l EmptyMK
ledgerDbAnchor =
    unCheckpoint
  . AS.anchor
  . dbChangelogVolatileCheckpoints
  . ledgerDbChangelog

-- | Information about the state of the most recently flushed ledger
--
-- This is what will be serialized when snapshotting.
ledgerDbOldest :: forall l.
     ( StandardHash (l EmptyMK)
     , GetTip (l EmptyMK)
     , StowableLedgerTables l
     )
  => LedgerDB l -> l EmptyMK
ledgerDbOldest db =
    if runDual db
    then Exn.assert isFlushed stuffedLegacyAnchor
    else immAnchor
  where
    immAnchor :: l EmptyMK
    immAnchor =
        unDbChangelogState
      $ AS.anchor
      $ changelogImmutableStates
      $ ledgerDbChangelog db

    isFlushed = castPoint (getTip stuffedLegacyAnchor) == getTip immAnchor

    stuffedLegacyAnchor :: l EmptyMK
    stuffedLegacyAnchor = stowLedgerTables legacyAnchor

    legacyAnchor :: l ValuesMK
    legacyAnchor =
        unCheckpoint
      $ AS.anchor
      $ ledgerDbCheckpoints db

-- | All snapshots currently stored by the ledger DB (new to old)
--
-- This also includes the snapshot at the anchor. For each snapshot we also
-- return the distance from the tip.
ledgerDbSnapshots :: TableStuff l => LedgerDB l -> [(Word64, l EmptyMK)]
ledgerDbSnapshots LedgerDB{..} =
    zip [0..]
  $ map (forgetLedgerStateTables . unCheckpoint)
  $ AS.toNewestFirst ledgerDbCheckpoints <> [AS.anchor ledgerDbCheckpoints]

-- | How many blocks can we currently roll back?
ledgerDbMaxRollback :: (GetTip (l ValuesMK), GetTip (l EmptyMK)) => LedgerDB l -> Word64
ledgerDbMaxRollback LedgerDB{..} =
  let
    old = fromIntegral (AS.length ledgerDbCheckpoints)
    new = fromIntegral $ AS.length $ dbChangelogVolatileCheckpoints ledgerDbChangelog
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
ledgerDbPast ::
     ( HasHeader blk, IsLedger l, HeaderHash l ~ HeaderHash blk
     , TableStuff l
     )
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
     ( HasHeader blk, IsLedger l, HeaderHash l ~ HeaderHash blk
     , TableStuff l
     )
  => Point blk
  -> LedgerDB l
  -> Maybe (LedgerDB l)
ledgerDbPrefix pt db
    | pt == (castPoint $ getTip (ledgerDbAnchor db))
    = Just $ LedgerDB {
          ledgerDbCheckpoints = (AS.Empty . AS.anchor)        (ledgerDbCheckpoints db)
        , ledgerDbChangelog   = prefixBackToAnchorDbChangelog (ledgerDbChangelog db)
        , runDual             = runDual db
        }
    | otherwise
    =  do
        checkpoints' <- AS.rollback
                          (pointSlot pt)
                          ((== pt) . castPoint . getTip . unCheckpoint . either id id)
                          (ledgerDbCheckpoints db)
        dblog' <- dbChangelogPrefix pt $ ledgerDbChangelog db
        return $ LedgerDB
                  { ledgerDbCheckpoints = checkpoints'
                  , ledgerDbChangelog   = dblog'
                  , runDual = runDual db
                  }


-- | Transform the underlying 'AnchoredSeq' using the given functions.
ledgerDbBimap ::
     Anchorable (WithOrigin SlotNo) a b
  => (l (mk :: MapKind) -> a)
  -> (l mk -> b)
  -> AnchoredSeq (WithOrigin SlotNo)
                 (Checkpoint (l mk))
                 (Checkpoint (l mk))
  -> AnchoredSeq (WithOrigin SlotNo) a b
ledgerDbBimap f g =
    -- Instead of exposing 'ledgerDbCheckpoints' directly, this function hides
    -- the internal 'Checkpoint' type.
    AS.bimap (f . unCheckpoint) (g . unCheckpoint)


-- | Prune snapshots until at we have at most @k@ snapshots in the LedgerDB,
-- excluding the snapshots stored at the anchor.
ledgerDbPrune ::
     (GetTip (l EmptyMK), GetTip (l ValuesMK))
  => SecurityParam -> LedgerDB l -> LedgerDB l
ledgerDbPrune k db = db {
      ledgerDbCheckpoints =
        AS.anchorNewest (maxRollbacks k) (ledgerDbCheckpoints db)
    , ledgerDbChangelog   = pruneDbChangelog k (ledgerDbChangelog db)
    }

 -- NOTE: we must inline 'ledgerDbPrune' otherwise we get unexplained thunks in
 -- 'LedgerDB' and thus a space leak. Alternatively, we could disable the
 -- @-fstrictness@ optimisation (enabled by default for -O1). See #2532.
{-# INLINE ledgerDbPrune #-}
{-# LANGUAGE DerivingStrategies         #-}

{-------------------------------------------------------------------------------
  Internal updates
-------------------------------------------------------------------------------}

-- | Push an updated ledger state
pushLedgerState ::
     (IsLedger l, TickedTableStuff l)
  => SecurityParam
  -> (l ValuesMK, l TrackingMK) -- ^ Updated ledger state
  -> LedgerDB l -> LedgerDB l
pushLedgerState secParam (currentOld', currentNew') db@LedgerDB{..}  =
    ledgerDbPrune secParam $ db {
        ledgerDbCheckpoints = ledgerDbCheckpoints AS.:> Checkpoint currentOld'
      , ledgerDbChangelog   =
          extendDbChangelog
            ledgerDbChangelog
            (trackingTablesToDiffs currentNew')
      }

{-------------------------------------------------------------------------------
  Internal: rolling back
-------------------------------------------------------------------------------}

-- | Rollback
--
-- Returns 'Nothing' if maximum rollback is exceeded.
rollback :: forall l.
     (GetTip (l ValuesMK), GetTip (l EmptyMK), TableStuff l)
  => Word64 -> LedgerDB l -> Maybe (LedgerDB l)
rollback n db@LedgerDB{..}
    | n <= ledgerDbMaxRollback db
    = Just db {
          ledgerDbCheckpoints = AS.dropNewest       (fromIntegral n) ledgerDbCheckpoints
        , ledgerDbChangelog   = rollbackDbChangelog (fromIntegral n) ledgerDbChangelog
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
              . (ApplyBlock l blk, TickedTableStuff l, Monad m, c, HasCallStack)
             => LedgerDbCfg l
             -> Ap m l blk c -> LedgerDB l -> m (LedgerDB l)
ledgerDbPush cfg ap db =
    (\current' -> pushLedgerState (ledgerDbCfgSecParam cfg) current' db) <$>
      applyBlock (ledgerDbCfg cfg) ap db

-- | Push a bunch of blocks (oldest first)
ledgerDbPushMany ::
     forall m c l blk . (ApplyBlock l blk, TickedTableStuff l, Monad m, c, HasCallStack)
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
ledgerDbSwitch :: (ApplyBlock l blk, TickedTableStuff l, Monad m, c, HasCallStack)
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
    -- ledgerDbFlushingPolicy :: FP
    --
    -- or
    --
    --
    -- ledgerDbTryFlush  :: dbhandle -> DbChangelog l -> m ()
    }
  deriving (Generic)

deriving instance NoThunks (LedgerCfg l) => NoThunks (LedgerDbCfg l)

type instance HeaderHash (LedgerDB l) = HeaderHash l

instance IsLedger l => GetTip (LedgerDB l) where
  getTip = castPoint . getTip . ledgerDbCurrent

{-------------------------------------------------------------------------------
  Support for testing
-------------------------------------------------------------------------------}

pureBlock :: blk -> Ap m l blk (ReadsKeySets m l)
pureBlock = ReapplyVal

ledgerDbPush' :: (ApplyBlock l blk, TickedTableStuff l, ReadsKeySets Identity l, HasCallStack)
              => LedgerDbCfg l -> blk -> LedgerDB l -> LedgerDB l
ledgerDbPush' cfg b = runIdentity . ledgerDbPush cfg (pureBlock b)

ledgerDbPushMany' :: (ApplyBlock l blk, TickedTableStuff l, ReadsKeySets Identity l, HasCallStack)
                  => LedgerDbCfg l -> [blk] -> LedgerDB l -> LedgerDB l
ledgerDbPushMany' cfg bs =
  runIdentity . ledgerDbPushMany (const $ pure ()) cfg (map pureBlock bs)

ledgerDbSwitch' :: forall l blk
                 . (ApplyBlock l blk, TickedTableStuff l, ReadsKeySets Identity l, HasCallStack)
                => LedgerDbCfg l
                -> Word64 -> [blk] -> LedgerDB l -> Maybe (LedgerDB l)
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
