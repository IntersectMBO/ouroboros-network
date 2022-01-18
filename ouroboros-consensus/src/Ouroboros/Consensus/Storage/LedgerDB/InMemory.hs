{-# LANGUAGE AllowAmbiguousTypes        #-}
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
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}

module Ouroboros.Consensus.Storage.LedgerDB.InMemory (
    -- * LedgerDB proper
    LedgerDbCfg (..)
  , mkLedgerDB
  , newLedgerDbWithAnchor
  , newLedgerDbWithEmptyAnchor
  , oldLedgerDbWithAnchor
    -- ** opaque
  , LedgerDB
  , NewLedgerDB
  , OldLedgerDB
    -- * Ledger DB UTxO-HD types (TODO: we might want to place this somewhere else)
  , DbChangelog
  , DbReader (..)
  , ReadKeySets
  , ReadsKeySets (..)
  , RewoundTableKeySets (..)
  , UnforwardedReadSets (..)
  , defaultReadKeySets
  , ledgerDbFlush
    -- ** Serialisation
  , decodeSnapshotBackwardsCompatible
  , encodeSnapshot
    -- ** Queries
  , ledgerDbAnchor
  , ledgerDbBimap
  , ledgerDbCurrent
  , ledgerDbCurrentOld
  , ledgerDbPast
  , ledgerDbPrefix
  , ledgerDbPrune
  , ledgerDbSnapshots
  , ledgerDbTip
  , newLedgerDbCurrent
  , oldLedgerDbCurrent
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
    -- * Initialization
  , newApplyBlock
  , oldApplyBlock
  , pushLedgerStateNew
  , pushLedgerStateOld
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
import           Control.Exception (assert)
import           Control.Monad.Reader hiding (ap)
import           Control.Monad.Trans.Except
import           Data.Functor.Identity
import           Data.Kind (Constraint, Type)
import           Data.Word
import           GHC.Generics (Generic)
import           NoThunks.Class (NoThunks)

import           Cardano.Slotting.Slot (WithOrigin (At))

import           Ouroboros.Network.AnchoredSeq (Anchorable (..),
                     AnchoredSeq (..))
import qualified Ouroboros.Network.AnchoredSeq as AS

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Config
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Storage.LedgerDB.Types (PushGoal (..),
                     PushStart (..), Pushing (..),
                     UpdateLedgerDbTraceEvent (..))
import           Ouroboros.Consensus.Util
import           Ouroboros.Consensus.Util.CBOR (decodeWithOrigin)
import           Ouroboros.Consensus.Util.Versioned

{-------------------------------------------------------------------------------
  Ledger DB types
-------------------------------------------------------------------------------}

-- | Type alias for the old-style database
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
type OldLedgerDB l = AnchoredSeq (WithOrigin SlotNo) (Checkpoint (l ValuesMK)) (Checkpoint (l ValuesMK))

-- | Type alias for the new-style database
--
-- This type is internally a DbChangelog which as an example, with @k = 3@ could look like:
--
-- > stateAnchor   | diskAnchor | states                     | tableDiffs
-- > ----------------------------------------------------------------------------
-- >      0        |      0     | [ L0]                     | [ ]
-- >      0        |      0     | [ L0, L1]                 | [ D1]
-- >      0        |      0     | [ L0, L1, L2]             | [ D1, D2]
-- >      0        |      0     | [ L0, L1, L2, L3]         | [ D1, D2, D3]
-- >      1        |      0     | [ L0, L1, L2, L3, L4]     | [ D1, D2, D3, D4]
-- >      2        |      0     | [ L0, L1, L2, L3, L4, L5] | [ D1, D2, D3, D4, D5]    (*)
-- >      2        |      2     | [ L2, L3, L4, L5]         | [ D3, D4, D5]   -- flush (**)
-- >      3        |      2     | [ L2, L3, L4, L5, L6]     | [ D3, D4, D5, D6]
--
-- The disk anchor moves when we flush data to the disk, and the state anchor
-- points always to the state that represents the tip of the logical immutable
-- database. Notice that @seqNo (last states) - stateAnchor@ is usually @k@
-- (unless rollbacks or corruption take place). We cannot roll back more than
-- @k@ blocks, this means that under a rollback of @k@ blocks at (*), the
-- database will look something like this:
--
-- >      2        |      0     | [ L0, L1, L2]             | [ D1, D2]
--
-- And a rollback of @k@ blocks at (**) will look something like this:
--
-- >      2        |      0     | [ L2]                     | [ ]
--
-- Notice how the states list always contains the in-memory state of the anchor,
-- but the table differences might not contain the differences for that anchor
-- if they have not been flushed to the backend.
--
-- The new-style database has to be coupled with the @OnDiskLedgerStDb@ which
-- provides the pointers to the on-disk data.
type NewLedgerDB l = DbChangelog l

-- | Internal state of the ledger DB
data LedgerDB (l :: LedgerStateKind) = LedgerDB {
      -- | Old-style LedgerDB. If specified, this will be @Nothing@ and we will
      -- just run the new-style database
      ledgerDbCheckpoints :: Maybe (OldLedgerDB l)
      -- | New-style LedgerDB
    , ledgerDbChangelog   :: NewLedgerDB l
    }
  deriving (Generic)

deriving instance (Eq       (l EmptyMK), Eq       (l ValuesMK)) => Eq       (LedgerDB l)
deriving instance (NoThunks (l EmptyMK), NoThunks (l ValuesMK)) => NoThunks (LedgerDB l)

instance ShowLedgerState l => Show (LedgerDB l) where
  showsPrec = error "showsPrec @LedgerDB"

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
  LedgerDB proper
-------------------------------------------------------------------------------}

-- | Old LedgerDB starting at the given ledger state
oldLedgerDbWithAnchor :: GetTip (l ValuesMK) => l ValuesMK -> OldLedgerDB l
oldLedgerDbWithAnchor = Empty . Checkpoint

-- | New LedgerDB starting at the given ledger state
newLedgerDbWithAnchor :: l ValuesMK -> DbChangelog l
newLedgerDbWithAnchor = initialDbChangelog undefined

-- | New LedgerDB starting at the given ledger state. The point provided must
-- match the point from the ledger. The provided point is expected to come from
-- the disk backend, and the assertion checks that it is in sync with the
-- NewLedgerDB
newLedgerDbWithEmptyAnchor ::
     (GetTip (l EmptyMK), StandardHash blk, HeaderHash (l EmptyMK) ~ HeaderHash blk)
  => Point blk
  -> l EmptyMK
  -> DbChangelog l
newLedgerDbWithEmptyAnchor pt l =
    assert (pt == castPoint (getTip l))
  $ initialDbChangelogWithEmptyState (undefined) l

-- | Combine both databases. If an old-style database is provided, assert that
-- they are both in sync, i.e. they have the same tip.
mkLedgerDB
  :: forall l blk.
  ( HeaderHash (l ValuesMK) ~ HeaderHash blk
  , HeaderHash (l EmptyMK)  ~ HeaderHash blk
  , StandardHash blk
  , GetTip (l EmptyMK)
  , GetTip (l ValuesMK)
  )
  => Maybe (OldLedgerDB l)
  -> NewLedgerDB l
  -> LedgerDB l
mkLedgerDB (Just ledgerDbCheckpoints) ledgerDbChangelog =
    assert ((castPoint @(l ValuesMK) @blk . getTip . oldLedgerDbCurrent $ ledgerDbCheckpoints) ==
            (castPoint @(l EmptyMK)  @blk . getTip . newLedgerDbCurrent $ ledgerDbChangelog))
      LedgerDB (Just ledgerDbCheckpoints) ledgerDbChangelog
mkLedgerDB Nothing ledgerDbChangelog =
    LedgerDB Nothing ledgerDbChangelog

{-------------------------------------------------------------------------------
  Resolve a block
-------------------------------------------------------------------------------}

-- | Functions to get an actual block from a real point
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

instance ResolvesBlocks m blk => ResolvesBlocks (ExceptT e m) blk where
  resolveBlock = lift . resolveBlock

defaultResolveBlocks :: ResolveBlock m blk
                     -> ReaderT (ResolveBlock m blk) m a
                     -> m a
defaultResolveBlocks = flip runReaderT

{-------------------------------------------------------------------------------
  Throw a ledger error
-------------------------------------------------------------------------------}

-- | Annotated ledger errors
data AnnLedgerError (l :: LedgerStateKind) blk = AnnLedgerError {
      -- | The ledger DB just /before/ this block was applied
      --
      -- Note that we will always return here a full LedgerDB. This means
      -- precisely that if we fail to apply blocks during initialization (where
      -- we might apply only to the old/new database instead to both or just the
      -- new), this error cannot be constructed and therefore we will crash with
      -- an @error@ call (see
      -- @Ouroboros.Consensus.Storage.LedgerDB.OnDisk.initLedgerDB@). If this is
      -- the case, we have an immutable database that is not a valid chain and
      -- therefore we are already in trouble.
      annLedgerState  :: LedgerDB l

      -- | Reference to the block that had the error
    , annLedgerErrRef :: RealPoint blk

      -- | The ledger error itself
    , annLedgerErr    :: LedgerErr l
    } deriving (Generic)

-- | Monads in which we can throw ledger errors
class Monad m => ThrowsLedgerError m l blk where
  throwLedgerError :: LedgerDB l -> RealPoint blk -> LedgerErr l -> m a

defaultThrowLedgerErrors :: ExceptT (AnnLedgerError l blk) m a
                         -> m (Either (AnnLedgerError l blk) a)
defaultThrowLedgerErrors = runExceptT

instance Monad m => ThrowsLedgerError (ExceptT (AnnLedgerError l blk) m) l blk where
   throwLedgerError l r e = throwE $ AnnLedgerError l r e

defaultResolveWithErrors :: ResolveBlock m blk
                         -> ExceptT (AnnLedgerError l blk)
                                    (ReaderT (ResolveBlock m blk) m)
                                    a
                         -> m (Either (AnnLedgerError l blk) a)
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
           -> m (Either (AnnLedgerError l blk) (Maybe (l ValuesMK), l TrackingMK))
applyBlock cfg ap db = do
  -- Only apply on the old-style database if it is present
  meOld <- maybe (return Nothing) (fmap Just . oldApplyBlock cfg ap) $ ledgerDbCheckpoints db
  eNew  <- newApplyBlock cfg ap $ ledgerDbChangelog db
  case meOld of
    Nothing         ->
      -- We only applied the block on the new-style database
        return
      $ either
          (Left . uncurry (AnnLedgerError db))
          (Right . (Nothing,))
          eNew
    Just res ->
      case (res, eNew) of
        (Left (b, err), Left (_, err'))
          | err == err' ->
              -- Applying a block failed in the same way for both implementations
              return $ Left $ AnnLedgerError db b err
          | otherwise ->
              -- Applying a block failed in different ways for both
              -- implementations, this should never happen.
              error $ "Block application error was different in the implementations\n\told: " <> show err <> "\n\tnew:" <> show err'

        (Right (pre,post), Right new) ->
          -- Block application succeeded in both implementations
          assert (post == applyDiffs pre (trackingTablesToDiffs new))
                       . return
                       $ Right (Just post, new)

        -- The remaining cases mean that block application failed in one
        -- implementation and succeeded in the other one, which is also
        -- unacceptable.
        (Left (b, err), _) ->
             error
          $  "Block application succeeded in the new implementation and failed in the old implementation for block "
          <> show b
          <> " with error "
          <> show err

        (_, Left (b, err)) ->
             error
          $  "Block application succeeded in the old implementation and failed in the new implementation for block "
          <> show b
          <> " with error "
          <> show err

-- | Apply a block to the old-style database. The returned values mean the following:
--
--  * @(RealPoint blk, LedgerErr l)@: as we can't construct a LedgerDB at this
--    point only, we cannot construct an @AnnLedgerError@, so we return the
--    errors to construct the complete error further up in the call chain.
--
--  * @(l ValuesMK, l ValuesMK)@: the first ledger state is the tip of the
--    LedgerDB before applying the block (to later check for agreement with the
--    new-style application of the block) and the second one is the resulting
--    ledger state after applying the block.
oldApplyBlock :: forall m c l blk
            . (ApplyBlock l blk, TickedTableStuff l, Monad m, c)
           => LedgerCfg l
           -> Ap m l blk c
           -> OldLedgerDB l
           -> m (Either (RealPoint blk, LedgerErr l) (l ValuesMK, l ValuesMK))
oldApplyBlock cfg ap db = fmap (oldTip,) <$> runExceptT (oldApplyBlock' cfg ap oldTip)
  where
    oldTip = either unCheckpoint unCheckpoint . AS.head $ db

-- | Internal function. Decoupled from @oldApplyBlock@ in order to be able to
-- refine the @c@ in @Weaken@.
oldApplyBlock' :: forall m c l blk
            . (ApplyBlock l blk, TickedTableStuff l, Monad m, c)
           => LedgerCfg l
           -> Ap m l blk c
           -> l ValuesMK
           -> ExceptT (RealPoint blk, LedgerErr l) m (l ValuesMK)
oldApplyBlock' cfg ap oldTip = case ap of
        ReapplyVal b -> return $ forgetLedgerStateTracking $ tickThenReapply cfg b oldTip

        ApplyVal b -> forgetLedgerStateTracking <$>
                        ( either (throwE . (blockRealPoint b,)) return
                        $ runExcept
                        $ tickThenApply cfg b
                        $ oldTip)

        ReapplyRef r  -> do
          b <- resolveBlock r

          return $ forgetLedgerStateTracking $ tickThenReapply cfg b oldTip

        ApplyRef r -> do
          b <- resolveBlock r

          forgetLedgerStateTracking <$>
            ( either (throwE . (blockRealPoint b,)) return
            $ runExcept
            $ tickThenApply cfg b
            $ oldTip)

        Weaken ap' -> oldApplyBlock' cfg ap' oldTip

-- | Apply a block to the new-style database. The returned values mean the following:
--
--  * @(RealPoint blk, LedgerErr l)@: as we can't construct a LedgerDB at this
--    point only, we cannot construct an @AnnLedgerError@, so we return the
--    errors to construct the complete error further up in the call chain.
--
--  * @(l TrackingMK)@: the resulting ledger state after applying the block.
newApplyBlock :: forall m c l blk
            . (ApplyBlock l blk, TickedTableStuff l, Monad m, c)
           => LedgerCfg l
           -> Ap m l blk c
           -> NewLedgerDB l
           -> m (Either (RealPoint blk, LedgerErr l) (l TrackingMK))
newApplyBlock cfg ap db = case ap of
    ReapplyVal b -> withBlockReadSets b $ \lh ->
                                            return $ return $ tickThenReapply cfg b lh

    ApplyVal b   -> withBlockReadSets b $ \lh ->
                                            runExceptT
                                            $ either (throwE . (blockRealPoint b,)) return
                                            $ runExcept
                                            $ tickThenApply cfg b lh

    ReapplyRef r -> do
      b <- resolveBlock r -- TODO: ask: would it make sense to recursively call applyBlock using ReapplyVal?

      withBlockReadSets b $ \lh ->
                              return $ return $ tickThenReapply cfg b lh

    ApplyRef r   -> do
      b <- resolveBlock r

      withBlockReadSets b $ \lh ->
                              runExceptT
                              $ either (throwE . (blockRealPoint b,)) return
                              $ runExcept
                              $ tickThenApply cfg b lh

    Weaken ap' -> newApplyBlock cfg ap' db

  where
    withBlockReadSets
      :: ReadsKeySets m l
      => blk
      -> (l ValuesMK -> m (Either (RealPoint blk, LedgerErr l) (l TrackingMK)))
      -> m (Either (RealPoint blk, LedgerErr l) (l TrackingMK))
    withBlockReadSets b f = do
      let ks = getBlockKeySets b :: TableKeySets l
      let aks = rewindTableKeySets db ks :: RewoundTableKeySets l
      urs <- readDb aks
      case withHydratedLedgerState urs f of
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
      :: UnforwardedReadSets l
      -> (l ValuesMK -> a)
      -> Maybe a
    withHydratedLedgerState urs f = do
      rs <- forwardTableKeySets db urs
      return $ f $ withLedgerTables (seqLast . dbChangelogStates $ db) rs


{-------------------------------------------------------------------------------
  HD Interface that I need (Could be moved to  Ouroboros.Consensus.Ledger.Basics )
-------------------------------------------------------------------------------}

data Seq (a :: LedgerStateKind) s

seqLast :: Seq state (state mk) -> state mk
seqLast = undefined

seqAt :: SeqNo state -> Seq state (state EmptyMK) -> state EmptyMK
seqAt = undefined

seqAnchorAt :: SeqNo state -> Seq state (state EmptyMK) -> Seq state (state EmptyMK)
seqAnchorAt = undefined

seqLength :: Seq state (state EmptyMK) -> Int
seqLength = undefined

instance IsLedger l => HasSeqNo l where
  stateSeqNo l =
    case getTipSlot l of
      Origin        -> SeqNo 0
      At (SlotNo n) -> SeqNo (n + 1)

data DbChangelog (l :: LedgerStateKind)
  deriving (Eq, Generic, NoThunks)

newtype RewoundTableKeySets l = RewoundTableKeySets (AnnTableKeySets l ()) -- KeySetSanityInfo l

initialDbChangelog
  :: WithOrigin SlotNo -> l ValuesMK -> DbChangelog l
initialDbChangelog = undefined

initialDbChangelogWithEmptyState :: WithOrigin SlotNo -> l EmptyMK -> NewLedgerDB l
initialDbChangelogWithEmptyState = undefined

rewindTableKeySets
  :: DbChangelog l -> TableKeySets l -> RewoundTableKeySets l
rewindTableKeySets = undefined

newtype UnforwardedReadSets l = UnforwardedReadSets (AnnTableReadSets l ())

forwardTableKeySets
  :: DbChangelog l -> UnforwardedReadSets l -> Maybe (TableReadSets l)
forwardTableKeySets = undefined

extendDbChangelog
  :: SeqNo l
  -> l DiffMK
  -- -> Maybe (l SnapshotsMK) TOOD: We won't use this parameter in the first iteration.
  -> DbChangelog l
  -> DbChangelog l
extendDbChangelog = undefined

dbChangelogStateAnchor :: DbChangelog state -> SeqNo state
dbChangelogStateAnchor = undefined

dbChangelogStates :: DbChangelog state -> Seq state (state EmptyMK)
dbChangelogStates = undefined

dbChangelogRollBack :: Point blk -> DbChangelog state -> DbChangelog state
dbChangelogRollBack = undefined

newtype SeqNo (state :: LedgerStateKind) = SeqNo { unSeqNo :: Word64 }
  deriving (Eq, Ord, Show)

class HasSeqNo (state :: LedgerStateKind) where
  stateSeqNo :: state table -> SeqNo state

-- TODO: flushing the changelog will invalidate other copies of 'LedgerDB'. At
-- the moment the flush-locking concern is outside the scope of this module.
-- Clients need to ensure they flush in a safe manner.
--
ledgerDbFlush
  :: Monad m => (DbChangelog l -> m (DbChangelog l)) -> LedgerDB l -> m (LedgerDB l)
ledgerDbFlush changelogFlush db = do
  ledgerDbChangelog' <- changelogFlush (ledgerDbChangelog db)
  return $! db { ledgerDbChangelog = ledgerDbChangelog' }

class ReadsKeySets m l where

  readDb :: ReadKeySets m l

type ReadKeySets m l = RewoundTableKeySets l -> m (UnforwardedReadSets l)

newtype DbReader m l a = DbReader { runDbReader :: ReaderT (ReadKeySets m l) m a}
  deriving newtype (Functor, Applicative, Monad)

instance ReadsKeySets (DbReader m l) l where
  readDb rks = DbReader $ ReaderT $ \f -> f rks

-- TODO: this is leaking details on how we want to compose monads at the higher levels.
instance (Monad m, ReadsKeySets m l) => ReadsKeySets (ReaderT r m) l where
  readDb = lift . readDb

instance (Monad m, ReadsKeySets m l) => ReadsKeySets (ExceptT e m) l where
  readDb = lift . readDb

defaultReadKeySets :: ReadKeySets m l -> DbReader m l a -> m a
defaultReadKeySets f dbReader = runReaderT (runDbReader dbReader) f

{-------------------------------------------------------------------------------
  Queries
-------------------------------------------------------------------------------}

-- | The ledger state at the tip of the chain
ledgerDbCurrent :: LedgerDB l -> l EmptyMK
ledgerDbCurrent =  newLedgerDbCurrent . ledgerDbChangelog

newLedgerDbCurrent :: NewLedgerDB l -> l EmptyMK
newLedgerDbCurrent = seqLast . dbChangelogStates

-- | The complete ledger state at the tip of the old-style database
ledgerDbCurrentOld :: GetTip (l ValuesMK) => LedgerDB l -> Maybe (l ValuesMK)
ledgerDbCurrentOld = fmap oldLedgerDbCurrent . ledgerDbCheckpoints

oldLedgerDbCurrent :: GetTip (l ValuesMK) => OldLedgerDB l -> l ValuesMK
oldLedgerDbCurrent = either unCheckpoint unCheckpoint . AS.head

-- | Information about the state of the ledger at the anchor
ledgerDbAnchor :: LedgerDB l -> l EmptyMK
ledgerDbAnchor LedgerDB{..} = seqAt (dbChangelogStateAnchor ledgerDbChangelog) (dbChangelogStates ledgerDbChangelog)

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
    old = fromIntegral . AS.length <$> ledgerDbCheckpoints
    new = fromIntegral
        $ seqLength
        $ seqAnchorAt (dbChangelogStateAnchor ledgerDbChangelog) (dbChangelogStates ledgerDbChangelog)
  in
    assert (old == Nothing || old == Just new) new

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
     forall l blk. (HasHeader blk, IsLedger l, HeaderHash l ~ HeaderHash blk)
  => Point blk
  -> LedgerDB l
  -> Maybe (LedgerDB l)
ledgerDbPrefix pt db
    | pt == (castPoint . getTip . ledgerDbAnchor $ db)
    = let old = Empty . AS.anchor <$> ledgerDbCheckpoints db
          new = dbChangelogRollBack pt (ledgerDbChangelog db)
      in
        Just $ mkLedgerDB @l @blk old new
    | otherwise
    =  do
        checkpoints' <- AS.rollback
                          (pointSlot pt)
                          ((== pt) . castPoint . getTip . unCheckpoint . either id id) <$>
                          (ledgerDbCheckpoints db)

        return $ LedgerDB
                  { ledgerDbCheckpoints = checkpoints'
                  , ledgerDbChangelog   = dbChangelogRollBack pt $ ledgerDbChangelog db
                  }


-- | Transform the underlying 'AnchoredSeq' using the given functions.
ledgerDbBimap ::
     Anchorable (WithOrigin SlotNo) a b
  => (l ValuesMK -> a)
  -> (l ValuesMK -> b)
  -> OldLedgerDB l
  -> AnchoredSeq (WithOrigin SlotNo) a b
ledgerDbBimap f g =
    -- Instead of exposing 'ledgerDbCheckpoints' directly, this function hides
    -- the internal 'Checkpoint' type.
    AS.bimap (f . unCheckpoint) (g . unCheckpoint)


-- | Prune snapshots until at we have at most @k@ snapshots in the LedgerDB,
-- excluding the snapshots stored at the anchor.
ledgerDbPrune :: GetTip (l ValuesMK) => SecurityParam -> LedgerDB l -> LedgerDB l
ledgerDbPrune (SecurityParam k) db = db {
      ledgerDbCheckpoints = AS.anchorNewest k <$> ledgerDbCheckpoints db
    }

 -- NOTE: we must inline 'ledgerDbPrune' otherwise we get unexplained thunks in
 -- 'LedgerDB' and thus a space leak. Alternatively, we could disable the
 -- @-fstrictness@ optimisation (enabled by default for -O1). See #2532.
{-# INLINE ledgerDbPrune #-}

{-------------------------------------------------------------------------------
  Internal updates
-------------------------------------------------------------------------------}

-- | Push an updated ledger state to the old database
pushLedgerStateOld ::
     IsLedger l
  => SecurityParam
  -> l ValuesMK -- ^ Updated ledger state
  -> OldLedgerDB l -> OldLedgerDB l
pushLedgerStateOld (SecurityParam k) currentOld' db  =
    AS.anchorNewest k $ db AS.:> Checkpoint currentOld'

-- | Push an updated ledger state to the new database
pushLedgerStateNew ::
     (IsLedger l, TickedTableStuff l)
  => l TrackingMK -- ^ Updated ledger state
  -> NewLedgerDB l -> NewLedgerDB l
pushLedgerStateNew currentNew' =
     extendDbChangelog (stateSeqNo currentNew')
                       (trackingTablesToDiffs currentNew')

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
          ledgerDbCheckpoints = AS.dropNewest (fromIntegral n) <$> ledgerDbCheckpoints,
          ledgerDbChangelog = dbChangelogRollBack undefined ledgerDbChangelog -- TODO: fill when the API is clearer
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

-- | Apply a block to the tip of the LedgerDB and push the resulting ledger
-- state into the LedgerDB. This will call @error@ if reapplying a block fails.
ledgerDbPush :: forall m c l blk
              . (ApplyBlock l blk, TickedTableStuff l, Monad m, c)
             => LedgerDbCfg l
             -> Ap m l blk c
             -> LedgerDB l
             -> m (LedgerDB l)
ledgerDbPush cfg ap db@(LedgerDB {..}) = do
  applyResult <- applyBlock (ledgerDbCfg cfg) ap db
  case applyResult of
    Left (AnnLedgerError d b e) -> case ap of
      ApplyVal _ -> throwLedgerError d b e
      ApplyRef _ -> throwLedgerError d b e
      _          -> error $ "Reapplying a block failed: " <> show e
    Right (resOld, resNew) -> return $ db {
        ledgerDbCheckpoints = pushLedgerStateOld (ledgerDbCfgSecParam cfg) <$> resOld  <*> ledgerDbCheckpoints
      , ledgerDbChangelog = pushLedgerStateNew resNew ledgerDbChangelog
      }

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
