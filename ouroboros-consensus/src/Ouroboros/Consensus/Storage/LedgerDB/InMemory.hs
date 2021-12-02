{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE DeriveAnyClass         #-}
{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE NamedFieldPuns         #-}
{-# LANGUAGE QuantifiedConstraints  #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE RecordWildCards        #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE StandaloneDeriving     #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE UndecidableInstances   #-}

module Ouroboros.Consensus.Storage.LedgerDB.InMemory (
    -- * LedgerDB proper
    LedgerDbCfg (..)
  , ledgerDbWithAnchor
    -- ** opaque
  , LedgerDB
    -- ** Serialisation
  , decodeSnapshotBackwardsCompatible
  , encodeSnapshot
    -- ** Queries
  , ledgerDbAnchor
  , ledgerDbBimap
  , ledgerDbCurrent
  , ledgerDbPast
  , ledgerDbPrune
  , ledgerDbSnapshots
  , ledgerDbTip
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
import           Data.Foldable (find)
import           Data.Functor ((<&>))
import           Data.Functor.Identity
import           Data.Kind (Constraint, Type)
import           Data.Word
import           GHC.Generics (Generic)
import           NoThunks.Class (NoThunks)

import           Ouroboros.Network.AnchoredSeq (Anchorable (..),
                     AnchoredSeq (..))
import qualified Ouroboros.Network.AnchoredSeq as AS

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Config
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Storage.LedgerDB.Types
                     (UpdateLedgerDbTraceEvent (..))
import           Ouroboros.Consensus.Ticked
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
newtype LedgerDB l = LedgerDB {
      -- | Ledger states
      ledgerDbCheckpoints :: AnchoredSeq
                               (WithOrigin SlotNo)
                               (Checkpoint l)
                               (Checkpoint l)
    }
  deriving (Show, Eq, Generic, NoThunks)

-- | Internal newtype wrapper around a ledger state @l@ so that we can define a
-- non-blanket 'Anchorable' instance.
newtype Checkpoint l = Checkpoint {
      unCheckpoint :: l
    }
  deriving (Show, Eq, Generic, NoThunks)

instance GetTip l => Anchorable (WithOrigin SlotNo) (Checkpoint l) (Checkpoint l) where
  asAnchor = id
  getAnchorMeasure _ = getTipSlot . unCheckpoint

{-------------------------------------------------------------------------------
  Ticking
-------------------------------------------------------------------------------}

-- | Ticking the ledger DB just ticks the current state
--
-- We don't push the new state into the DB until we apply a block.
data instance Ticked (LedgerDB l) = TickedLedgerDB {
      tickedLedgerDbTicked :: Ticked l
    , tickedLedgerDbOrig   :: LedgerDB l
    }

{-------------------------------------------------------------------------------
  LedgerDB proper
-------------------------------------------------------------------------------}

-- | Ledger DB starting at the specified ledger state
ledgerDbWithAnchor :: GetTip l => l -> LedgerDB l
ledgerDbWithAnchor anchor = LedgerDB {
      ledgerDbCheckpoints = Empty (Checkpoint anchor)
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
data AnnLedgerError l blk = AnnLedgerError {
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
data Ap :: (Type -> Type) -> Type -> Type -> Constraint -> Type where
  ReapplyVal ::           blk -> Ap m l blk ()
  ApplyVal   ::           blk -> Ap m l blk (                      ThrowsLedgerError m l blk)
  ReapplyRef :: RealPoint blk -> Ap m l blk (ResolvesBlocks m blk)
  ApplyRef   :: RealPoint blk -> Ap m l blk (ResolvesBlocks m blk, ThrowsLedgerError m l blk)

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
applyBlock :: forall m c l blk. (ApplyBlock l blk, Monad m, c)
           => LedgerCfg l
           -> Ap m l blk c
           -> LedgerDB l -> m l
applyBlock cfg ap db = case ap of
    ReapplyVal b ->
      return $
        tickThenReapply cfg b l
    ApplyVal b ->
      either (throwLedgerError db (blockRealPoint b)) return $ runExcept $
        tickThenApply cfg b l
    ReapplyRef r  -> do
      b <- resolveBlock r
      return $
        tickThenReapply cfg b l
    ApplyRef r -> do
      b <- resolveBlock r
      either (throwLedgerError db r) return $ runExcept $
        tickThenApply cfg b l
    Weaken ap' ->
      applyBlock cfg ap' db
  where
    l :: l
    l = ledgerDbCurrent db

{-------------------------------------------------------------------------------
  Queries
-------------------------------------------------------------------------------}

-- | The ledger state at the tip of the chain
ledgerDbCurrent :: GetTip l => LedgerDB l -> l
ledgerDbCurrent = either unCheckpoint unCheckpoint . AS.head . ledgerDbCheckpoints

-- | Information about the state of the ledger at the anchor
ledgerDbAnchor :: LedgerDB l -> l
ledgerDbAnchor = unCheckpoint . AS.anchor . ledgerDbCheckpoints

-- | All snapshots currently stored by the ledger DB (new to old)
--
-- This also includes the snapshot at the anchor. For each snapshot we also
-- return the distance from the tip.
ledgerDbSnapshots :: LedgerDB l -> [(Word64, l)]
ledgerDbSnapshots LedgerDB{..} =
    zip
      [0..]
      (map unCheckpoint (AS.toNewestFirst ledgerDbCheckpoints)
        <> [unCheckpoint (AS.anchor ledgerDbCheckpoints)])

-- | How many blocks can we currently roll back?
ledgerDbMaxRollback :: GetTip l => LedgerDB l -> Word64
ledgerDbMaxRollback LedgerDB{..} = fromIntegral (AS.length ledgerDbCheckpoints)

-- | Reference to the block at the tip of the chain
ledgerDbTip :: GetTip l => LedgerDB l -> Point l
ledgerDbTip = getTip . ledgerDbCurrent

-- | Have we seen at least @k@ blocks?
ledgerDbIsSaturated :: GetTip l => SecurityParam -> LedgerDB l -> Bool
ledgerDbIsSaturated (SecurityParam k) db =
    ledgerDbMaxRollback db >= k

-- | Get a past ledger state
--
--  \( O(\log(\min(i,n-i)) \)
--
-- When no ledger state (or anchor) has the given 'Point', 'Nothing' is
-- returned.
ledgerDbPast ::
     (HasHeader blk, IsLedger l, HeaderHash l ~ HeaderHash blk)
  => Point blk
  -> LedgerDB l
  -> Maybe l
ledgerDbPast pt db
    | pt == castPoint (getTip (ledgerDbAnchor db))
    = Just $ ledgerDbAnchor db
    | otherwise
    = fmap unCheckpoint $
        find ((== pt) . castPoint . getTip . unCheckpoint) $
          AS.lookupByMeasure (pointSlot pt) (ledgerDbCheckpoints db)

-- | Transform the underlying 'AnchoredSeq' using the given functions.
ledgerDbBimap ::
     Anchorable (WithOrigin SlotNo) a b
  => (l -> a)
  -> (l -> b)
  -> LedgerDB l
  -> AnchoredSeq (WithOrigin SlotNo) a b
ledgerDbBimap f g =
    -- Instead of exposing 'ledgerDbCheckpoints' directly, this function hides
    -- the internal 'Checkpoint' type.
    AS.bimap (f . unCheckpoint) (g . unCheckpoint) . ledgerDbCheckpoints


-- | Prune snapshots until at we have at most @k@ snapshots in the LedgerDB,
-- excluding the snapshots stored at the anchor.
ledgerDbPrune :: GetTip l => SecurityParam -> LedgerDB l -> LedgerDB l
ledgerDbPrune (SecurityParam k) db = db {
      ledgerDbCheckpoints = AS.anchorNewest k (ledgerDbCheckpoints db)
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
     GetTip l
  => SecurityParam
  -> l -- ^ Updated ledger state
  -> LedgerDB l -> LedgerDB l
pushLedgerState secParam current' db@LedgerDB{..}  =
    ledgerDbPrune secParam $ db {
        ledgerDbCheckpoints = ledgerDbCheckpoints AS.:> Checkpoint current'
      }

{-------------------------------------------------------------------------------
  Internal: rolling back
-------------------------------------------------------------------------------}

-- | Rollback
--
-- Returns 'Nothing' if maximum rollback is exceeded.
rollback :: GetTip l => Word64 -> LedgerDB l -> Maybe (LedgerDB l)
rollback n db@LedgerDB{..}
    | n <= ledgerDbMaxRollback db
    = Just db {
          ledgerDbCheckpoints = AS.dropNewest (fromIntegral n) ledgerDbCheckpoints
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

ledgerDbPush :: forall m c l blk. (ApplyBlock l blk, Monad m, c)
             => LedgerDbCfg l
             -> Ap m l blk c -> LedgerDB l -> m (LedgerDB l)
ledgerDbPush cfg ap db =
    (\current' -> pushLedgerState (ledgerDbCfgSecParam cfg) current' db) <$>
      applyBlock (ledgerDbCfg cfg) ap db

-- | Push a bunch of blocks (oldest first)
ledgerDbPushMany :: (ApplyBlock l blk, Monad m, c)
                 => (UpdateLedgerDbTraceEvent blk -> m ())
                 -> LedgerDbCfg l
                 -> [Ap m l blk c] -> LedgerDB l -> m (LedgerDB l)
ledgerDbPushMany trace = repeatedlyM . pushAndTrace
  where
    pushAndTrace cfg ap db = do
      trace $ StartedPushingBlockToTheLedgerDb $ toRealPoint ap
      res <- ledgerDbPush cfg ap db
      return res

-- | Switch to a fork
ledgerDbSwitch :: (ApplyBlock l blk, Monad m, c)
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
      Just db' ->
        Right <$> ledgerDbPushMany trace cfg newBlocks db'

{-------------------------------------------------------------------------------
  The LedgerDB itself behaves like a ledger
-------------------------------------------------------------------------------}

data LedgerDbCfg l = LedgerDbCfg {
      ledgerDbCfgSecParam :: !SecurityParam
    , ledgerDbCfg         :: !(LedgerCfg l)
    }
  deriving (Generic)

deriving instance NoThunks (LedgerCfg l) => NoThunks (LedgerDbCfg l)

type instance LedgerCfg (LedgerDB l) = LedgerDbCfg l

type instance HeaderHash (LedgerDB l) = HeaderHash l

instance IsLedger l => GetTip (LedgerDB l) where
  getTip = castPoint . getTip . ledgerDbCurrent

instance IsLedger l => GetTip (Ticked (LedgerDB l)) where
  getTip = castPoint . getTip . tickedLedgerDbOrig

instance IsLedger l => IsLedger (LedgerDB l) where
  type LedgerErr (LedgerDB l) = LedgerErr l

  type AuxLedgerEvent (LedgerDB l) = AuxLedgerEvent l

  applyChainTickLedgerResult cfg slot db =
      castLedgerResult ledgerResult <&> \l -> TickedLedgerDB {
          tickedLedgerDbTicked = l
        , tickedLedgerDbOrig   = db
        }
    where
      ledgerResult = applyChainTickLedgerResult
                       (ledgerDbCfg cfg)
                       slot
                       (ledgerDbCurrent db)

instance ApplyBlock l blk => ApplyBlock (LedgerDB l) blk where
  applyBlockLedgerResult cfg blk TickedLedgerDB{..} = do
      ledgerResult <- applyBlockLedgerResult
                        (ledgerDbCfg cfg)
                        blk
                        tickedLedgerDbTicked

      return $ push <$> castLedgerResult ledgerResult
    where
      push :: l -> LedgerDB l
      push l = pushLedgerState (ledgerDbCfgSecParam cfg) l tickedLedgerDbOrig

  reapplyBlockLedgerResult cfg blk TickedLedgerDB{..} =
      push <$> castLedgerResult ledgerResult
    where
      push :: l -> LedgerDB l
      push l = pushLedgerState (ledgerDbCfgSecParam cfg) l tickedLedgerDbOrig

      ledgerResult = reapplyBlockLedgerResult
                       (ledgerDbCfg cfg)
                       blk
                       tickedLedgerDbTicked

{-------------------------------------------------------------------------------
  Support for testing
-------------------------------------------------------------------------------}

pureBlock :: blk -> Ap m l blk ()
pureBlock = ReapplyVal

ledgerDbPush' :: ApplyBlock l blk
              => LedgerDbCfg l -> blk -> LedgerDB l -> LedgerDB l
ledgerDbPush' cfg b = runIdentity . ledgerDbPush cfg (pureBlock b)

ledgerDbPushMany' :: ApplyBlock l blk
                  => LedgerDbCfg l -> [blk] -> LedgerDB l -> LedgerDB l
ledgerDbPushMany' cfg bs =
  runIdentity . ledgerDbPushMany (const $ pure ()) cfg (map pureBlock bs)

ledgerDbSwitch' :: forall l blk. ApplyBlock l blk
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
