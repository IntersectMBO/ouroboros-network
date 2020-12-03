{-# LANGUAGE BangPatterns           #-}
{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE DeriveAnyClass         #-}
{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE KindSignatures         #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE QuantifiedConstraints  #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE RecordWildCards        #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE UndecidableInstances   #-}

module Ouroboros.Consensus.Storage.LedgerDB.InMemory (
     -- * LedgerDB proper
     LedgerDB
   , LedgerDbParams(..)
   , ledgerDbDefaultParams
   , ledgerDbWithAnchor
   , ledgerDbFromGenesis
     -- ** ChainSummary
   , ChainSummary(..)
   , encodeChainSummary
   , decodeChainSummary
     -- ** Queries
   , ledgerDbCurrent
   , ledgerDbTip
   , ledgerDbAnchor
     -- ** Past ledger states
   , ledgerDbPast
   , ledgerDbPastLedgers
     -- ** Running updates
   , Ap(..)
   , AnnLedgerError(..)
   , ResolveBlock
   , ResolvesBlocks(..)
   , ThrowsLedgerError(..)
   , defaultThrowLedgerErrors
   , defaultResolveBlocks
   , defaultResolveWithErrors
     -- ** Updates
   , ExceededRollback(..)
   , ledgerDbPush
   , ledgerDbSwitch
     -- * Exports for the benefit of tests
     -- ** Additional queries
   , ledgerDbChainLength
   , ledgerDbToList
   , ledgerDbMaxRollback
   , ledgerDbSnapshots
   , ledgerDbIsSaturated
   , ledgerDbCountToPrune
   , ledgerDbPastSpec
     -- ** Pure API
   , ledgerDbPush'
   , ledgerDbPushMany'
   , ledgerDbSwitch'
   ) where

import           Codec.Serialise (Serialise (..))
import           Codec.Serialise.Decoding (Decoder)
import qualified Codec.Serialise.Decoding as Dec
import           Codec.Serialise.Encoding (Encoding)
import qualified Codec.Serialise.Encoding as Enc
import           Control.Monad.Except hiding (ap)
import           Control.Monad.Reader hiding (ap)
import           Data.Foldable (find, toList)
import           Data.Function (on)
import           Data.Functor.Identity
import           Data.Kind (Constraint, Type)
import qualified Data.Sequence as LazySeq
import           Data.Sequence.Strict (StrictSeq ((:|>), Empty), (|>))
import qualified Data.Sequence.Strict as Seq
import           Data.Word
import           GHC.Generics (Generic)
import           GHC.Stack (HasCallStack)
import           NoThunks.Class (NoThunks)

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Config
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Ticked
import           Ouroboros.Consensus.Util
import           Ouroboros.Consensus.Util.CBOR (decodeWithOrigin,
                     encodeWithOrigin)

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
data LedgerDB l blk = LedgerDB {
      -- | Older ledger states
      ledgerDbCheckpoints :: !(StrictSeq (Checkpoint l blk))

      -- | Information about the state of the ledger /before/
    , ledgerDbAnchor      :: !(ChainSummary l blk)

      -- | Ledger DB parameters
    , ledgerDbParams      :: !LedgerDbParams
    }
  deriving (Show, Eq, Generic, NoThunks)

newtype LedgerDbParams = LedgerDbParams {
      -- | Security parameter (maximum rollback)
      ledgerDbSecurityParam :: SecurityParam
    }
  deriving (Show, Eq, Generic, NoThunks)

-- | Default parameters
ledgerDbDefaultParams :: SecurityParam -> LedgerDbParams
ledgerDbDefaultParams securityParam = LedgerDbParams {
      ledgerDbSecurityParam = securityParam
    }

{-------------------------------------------------------------------------------
  Ticking
-------------------------------------------------------------------------------}

-- | Ticking the ledger DB just ticks the current state
--
-- We don't push the new state into the DB until we apply a block.
data instance Ticked (LedgerDB l blk) = TickedLedgerDB {
      tickedLedgerDbTicked :: Ticked l
    , tickedLedgerDbOrig   :: LedgerDB l blk
    }

{-------------------------------------------------------------------------------
  Internal: checkpoints
-------------------------------------------------------------------------------}

-- | Checkpoint with a ledger state
data Checkpoint l blk = Checkpoint {
      cpBlock :: !(RealPoint blk)
    , cpState :: !l
    }
  deriving (Show, Eq, Generic, NoThunks)

cpToPair :: Checkpoint l blk -> (RealPoint blk, l)
cpToPair (Checkpoint b s) = (b, s)

{-------------------------------------------------------------------------------
  Chain summary
-------------------------------------------------------------------------------}

-- | Summary of the chain at a particular point in time
data ChainSummary l blk = ChainSummary {
      -- | The tip of the chain
      csTip    :: !(Point blk)

      -- | Length of the chain
    , csLength :: !Word64

      -- | Ledger state
    , csLedger :: !l
    }
  deriving (Show, Eq, Generic, NoThunks)

genesisChainSummary :: l -> ChainSummary l blk
genesisChainSummary = ChainSummary GenesisPoint 0

{-------------------------------------------------------------------------------
  LedgerDB proper
-------------------------------------------------------------------------------}

-- | Ledger DB starting at the specified ledger state
ledgerDbWithAnchor :: LedgerDbParams -> ChainSummary l blk -> LedgerDB l blk
ledgerDbWithAnchor params anchor = LedgerDB {
      ledgerDbCheckpoints = Seq.empty
    , ledgerDbAnchor      = anchor
    , ledgerDbParams      = params
    }

ledgerDbFromGenesis :: LedgerDbParams -> l -> LedgerDB l blk
ledgerDbFromGenesis params = ledgerDbWithAnchor params . genesisChainSummary

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
      annLedgerState  :: LedgerDB l blk

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
  throwLedgerError :: LedgerDB l blk -> RealPoint blk -> LedgerErr l -> m a

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

apRef :: HasHeader blk => Ap m l blk c -> RealPoint blk
apRef (ReapplyVal b) = blockRealPoint b
apRef (ApplyVal   b) = blockRealPoint b
apRef (ReapplyRef r) = r
apRef (ApplyRef   r) = r
apRef (Weaken     a) = apRef a

-- | Apply block to the current ledger state
--
-- We take in the entire 'LedgerDB' because we record that as part of errors.
applyBlock :: forall m c l blk. (ApplyBlock l blk, Monad m, c)
           => LedgerCfg l
           -> Ap m l blk c
           -> LedgerDB l blk -> m l
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
ledgerDbCurrent :: LedgerDB l blk -> l
ledgerDbCurrent LedgerDB{..} = case ledgerDbCheckpoints of
  Empty                  -> csLedger ledgerDbAnchor
  (_ :|> Checkpoint _ l) -> l

-- | Total length of the chain (in terms of number of blocks)
ledgerDbChainLength :: LedgerDB l blk -> Word64
ledgerDbChainLength LedgerDB{..} =
    csLength ledgerDbAnchor + fromIntegral (Seq.length ledgerDbCheckpoints)

-- | References to blocks and corresponding ledger state (from old to new)
ledgerDbToList :: LedgerDB l blk -> [(RealPoint blk, l)]
ledgerDbToList LedgerDB{..} = map cpToPair $ toList ledgerDbCheckpoints

-- | All snapshots currently stored by the ledger DB (new to old)
--
-- This also includes the snapshot at the anchor. For each snapshot we also
-- return the distance from the tip.
ledgerDbSnapshots :: forall l blk. LedgerDB l blk -> [(Word64, l)]
ledgerDbSnapshots LedgerDB{..} = go 0 ledgerDbCheckpoints
  where
    go :: Word64 -> StrictSeq (Checkpoint l blk) -> [(Word64, l)]
    go !offset Empty                   = [(offset, csLedger ledgerDbAnchor)]
    go !offset (ss :|> Checkpoint _ l) = (offset, l) : go (offset + 1) ss

-- | How many blocks can we currently roll back?
ledgerDbMaxRollback :: LedgerDB l blk -> Word64
ledgerDbMaxRollback LedgerDB{..} = fromIntegral (Seq.length ledgerDbCheckpoints)

-- | Reference to the block at the tip of the chain
ledgerDbTip :: LedgerDB l blk -> Point blk
ledgerDbTip LedgerDB{..} =
    case ledgerDbCheckpoints of
      Empty    -> csTip ledgerDbAnchor
      _ :|> cp -> realPointToPoint (cpBlock cp)

-- | Have we seen at least @k@ blocks?
ledgerDbIsSaturated :: LedgerDB l blk -> Bool
ledgerDbIsSaturated LedgerDB{..} =
    fromIntegral (Seq.length ledgerDbCheckpoints) >= k
  where
    LedgerDbParams{..} = ledgerDbParams
    SecurityParam k    = ledgerDbSecurityParam

{-------------------------------------------------------------------------------
  Internal updates
-------------------------------------------------------------------------------}

-- | Internal: shift the anchor given a bunch of checkpoints.
shiftAnchor :: forall blk l. HasCallStack
            => StrictSeq (Checkpoint l blk) -> ChainSummary l blk -> ChainSummary l blk
shiftAnchor toRemove ChainSummary{..} = ChainSummary {
      csTip    = realPointToPoint csTip'
    , csLength = csLength + fromIntegral (Seq.length toRemove)
    , csLedger = csLedger'
    }
  where
    csTip'    :: RealPoint blk
    csLedger' :: l
    (csTip', csLedger') =
        case toRemove of
          Empty                -> error "shiftAnchor: empty list"
          _ :|> Checkpoint r l -> (r, l)

-- | Internal: count number of checkpoints to prune, given total number of
-- checkpoints
--
-- This is exposed for the benefit of tests only.
ledgerDbCountToPrune :: LedgerDbParams -> Int -> Int
ledgerDbCountToPrune LedgerDbParams{..} curSize'
  | curSize <= k = 0
  | otherwise    = fromIntegral $ curSize - k
  where
    SecurityParam k = ledgerDbSecurityParam
    curSize = fromIntegral curSize'

-- | Internal: drop unneeded snapshots from the head of the list
prune :: HasCallStack => LedgerDB l blk -> LedgerDB l blk
prune db@LedgerDB{..} =
    if toPrune == 0
      then db
      else let (removed, kept) = Seq.splitAt toPrune ledgerDbCheckpoints
               anchor'         = shiftAnchor removed ledgerDbAnchor
           in db { ledgerDbAnchor      = anchor'
                 , ledgerDbCheckpoints = kept
                 }
  where
    -- Number of snapshots to remove (assuming curSize > maxSize)
    toPrune :: Int
    toPrune =
      ledgerDbCountToPrune ledgerDbParams (Seq.length ledgerDbCheckpoints)

 -- NOTE: we must inline 'prune' otherwise we get unexplained thunks in
 -- 'LedgerDB' and thus a space leak. Alternatively, we could disable the
 -- @-fstrictness@ optimisation (enabled by default for -O1). See #2532.
{-# INLINE prune #-}

-- | Push an updated ledger state
pushLedgerState :: l              -- ^ Updated ledger state
                -> RealPoint blk  -- ^ Reference to the applied block
                -> LedgerDB l blk -> LedgerDB l blk
pushLedgerState current' ref db@LedgerDB{..}  = prune $ db {
      ledgerDbCheckpoints = snapshots
    }
  where
    snapshots = ledgerDbCheckpoints |> Checkpoint ref current'

{-------------------------------------------------------------------------------
  Internal: rolling back
-------------------------------------------------------------------------------}

-- | Reconstruct ledger DB from a list of checkpoints
reconstructFrom :: forall l blk.
                   LedgerDbParams
                -> ChainSummary l blk
                -> StrictSeq (Checkpoint l blk)
                -> LedgerDB l blk
reconstructFrom params anchor snapshots =
    LedgerDB {
        ledgerDbCheckpoints = snapshots
      , ledgerDbParams      = params
      , ledgerDbAnchor      = anchor
      }

-- | Generalization of rollback using a function on the checkpoints
rollbackTo :: (   ChainSummary l blk
               -> StrictSeq (Checkpoint l blk)
               -> Maybe (StrictSeq (Checkpoint l blk))
              )
           -> LedgerDB l blk
           -> Maybe (LedgerDB l blk)
rollbackTo f (LedgerDB checkpoints anchor params) =
    reconstructFrom params anchor <$> f anchor checkpoints

-- | Rollback
--
-- Returns 'Nothing' if maximum rollback is exceeded.
rollback :: forall l blk.
            Word64
         -> LedgerDB l blk
         -> Maybe (LedgerDB l blk)
rollback 0 db = Just db
rollback n db = rollbackTo (\_anchor -> go) db
  where
    go :: StrictSeq (Checkpoint l blk) -> Maybe (StrictSeq (Checkpoint l blk))
    go checkpoints =
        if Seq.length checkpoints >= fromIntegral n
          then Just $
            Seq.take (Seq.length checkpoints - fromIntegral n) checkpoints
          else Nothing

{-------------------------------------------------------------------------------
  Get past ledger states
-------------------------------------------------------------------------------}

-- | Get a past ledger state
--
--  \( O(\log n * \log n) \).
--
-- When no 'Checkpoint' (or anchor) has the given 'Point', 'Nothing' is
-- returned.
--
-- To avoid a linear search on the checkpoints (typically 2160 of them), we do a
-- binary search benefitting from the cheap splits of the underlying
-- 'StrictSeq' \( O(\log n) \).
ledgerDbPast ::
     forall l blk. HasHeader blk
  => Point blk
  -> LedgerDB l blk
  -> Maybe l
ledgerDbPast tip db
    | tip == ledgerDbTip db
    = Just (ledgerDbCurrent db)
    | tip == csTip (ledgerDbAnchor db)
    = Just (csLedger (ledgerDbAnchor db))
    | NotOrigin tip' <- pointToWithOriginRealPoint tip
    = cpState <$> binarySearch tip' (Seq.fromStrict (ledgerDbCheckpoints db))
    | otherwise
    = Nothing
  where
    binarySearch :: RealPoint blk -> LazySeq.Seq (Checkpoint l blk) -> Maybe (Checkpoint l blk)
    binarySearch _   LazySeq.Empty = Nothing
    binarySearch ref checkpoints   = case LazySeq.splitAt middle checkpoints of
        (before, LazySeq.Empty)        -> binarySearch ref before
        (before, cp LazySeq.:<| after) ->
          case (compare `on` realPointSlot) ref (cpBlock cp) of
            LT -> binarySearch ref before
            GT -> binarySearch ref after
            EQ
              | isMatch cp -> Just cp
              | otherwise  ->
                -- Oh EBBs, why do you make everything so much harder? An EBB
                -- has the same slot as the regular block after it. We look left
                -- and right from @cp@ for checkpoints with the same 'SlotNo'
                -- and do a linear search among those. When it's indeed a slot
                -- populated by both a regular block and EBB, we'll look at
                -- /one/ other checkpoint. In all other cases, we'll look at
                -- /none/. Note that we have to look in both directions because
                -- we don't know whether @cp@ is the EBB or the regular block in
                -- the same slot.
                find isMatch (LazySeq.takeWhileR sameOrder before) `mplus`
                find isMatch (LazySeq.takeWhileL sameOrder after)
      where
        middle :: Int
        middle = LazySeq.length checkpoints `div` 2

        isMatch :: Checkpoint l blk -> Bool
        isMatch cp = cpBlock cp == ref

        sameOrder :: Checkpoint l blk -> Bool
        sameOrder cp = realPointSlot (cpBlock cp) == realPointSlot ref

-- | Get a past ledger state
--
-- \( O(n) \)
--
-- Straightforward implementation of 'ledgerDbPast' using a linear search.
--
-- Can be used in tests to compare against 'ledgerDbPast'.
ledgerDbPastSpec ::
     forall l blk. HasHeader blk
  => Point blk
  -> LedgerDB l blk
  -> Maybe l
ledgerDbPastSpec tip db
    | tip == ledgerDbTip db
    = Just (ledgerDbCurrent db)
    | tip == csTip (ledgerDbAnchor db)
    = Just (csLedger (ledgerDbAnchor db))
    | NotOrigin tip' <- pointToWithOriginRealPoint tip
    = cpState <$> find ((== tip') . cpBlock) (ledgerDbCheckpoints db)
    | otherwise
    = Nothing

-- | Apply the given function to all past ledgers in the 'LedgerDB', including
-- the one stored at the anchor.
--
-- \( O(n) \)
ledgerDbPastLedgers :: (l -> a) -> LedgerDB l r -> (a, StrictSeq a)
ledgerDbPastLedgers f db =
    ( f . csLedger . ledgerDbAnchor $ db
    , fmap (f . cpState) . ledgerDbCheckpoints $ db
    )

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
             => LedgerCfg l
             -> Ap m l blk c -> LedgerDB l blk -> m (LedgerDB l blk)
ledgerDbPush cfg ap db =
    (\current' -> pushLedgerState current' (apRef ap) db) <$>
      applyBlock cfg ap db

-- | Push a bunch of blocks (oldest first)
ledgerDbPushMany :: (ApplyBlock l blk, Monad m, c)
                 => LedgerCfg l
                 -> [Ap m l blk c] -> LedgerDB l blk -> m (LedgerDB l blk)
ledgerDbPushMany = repeatedlyM . ledgerDbPush

-- | Switch to a fork
ledgerDbSwitch :: (ApplyBlock l blk, Monad m, c)
               => LedgerCfg l
               -> Word64          -- ^ How many blocks to roll back
               -> [Ap m l blk c]  -- ^ New blocks to apply
               -> LedgerDB l blk
               -> m (Either ExceededRollback (LedgerDB l blk))
ledgerDbSwitch cfg numRollbacks newBlocks db =
    case rollback numRollbacks db of
      Nothing ->
        return $ Left $ ExceededRollback {
            rollbackMaximum   = ledgerDbMaxRollback db
          , rollbackRequested = numRollbacks
          }
      Just db' ->
        Right <$> ledgerDbPushMany cfg newBlocks db'

{-------------------------------------------------------------------------------
  The LedgerDB itself behaves like a ledger
-------------------------------------------------------------------------------}

type instance LedgerCfg (LedgerDB l blk) = LedgerCfg l

type instance HeaderHash (LedgerDB l blk) = HeaderHash l

instance IsLedger l => GetTip (LedgerDB l blk) where
  getTip = castPoint . getTip . ledgerDbCurrent

instance IsLedger l => GetTip (Ticked (LedgerDB l blk)) where
  getTip = castPoint . getTip . tickedLedgerDbOrig

instance (IsLedger l, HasHeader blk) => IsLedger (LedgerDB l blk) where
  type LedgerErr (LedgerDB l blk) = LedgerErr l

  applyChainTick cfg slot db = TickedLedgerDB {
        tickedLedgerDbTicked = applyChainTick cfg slot (ledgerDbCurrent db)
      , tickedLedgerDbOrig   = db
      }

instance ApplyBlock l blk => ApplyBlock (LedgerDB l blk) blk where
  applyLedgerBlock cfg blk TickedLedgerDB{..} =
      push <$> applyLedgerBlock
                 cfg
                 blk
                 tickedLedgerDbTicked
   where
     push :: l -> LedgerDB l blk
     push l = pushLedgerState l (blockRealPoint blk) tickedLedgerDbOrig

  reapplyLedgerBlock cfg blk TickedLedgerDB{..} =
      push $ reapplyLedgerBlock
               cfg
               blk
               tickedLedgerDbTicked
   where
     push :: l -> LedgerDB l blk
     push l = pushLedgerState l (blockRealPoint blk) tickedLedgerDbOrig

{-------------------------------------------------------------------------------
  Support for testing
-------------------------------------------------------------------------------}

pureBlock :: blk -> Ap m l blk ()
pureBlock = ReapplyVal

ledgerDbPush' :: ApplyBlock l blk
              => LedgerCfg l -> blk -> LedgerDB l blk -> LedgerDB l blk
ledgerDbPush' cfg b = runIdentity . ledgerDbPush cfg (pureBlock b)

ledgerDbPushMany' :: ApplyBlock l blk
                  => LedgerCfg l -> [blk] -> LedgerDB l blk -> LedgerDB l blk
ledgerDbPushMany' cfg bs = runIdentity . ledgerDbPushMany cfg (map pureBlock bs)

ledgerDbSwitch' :: forall l blk. ApplyBlock l blk
                => LedgerCfg l
                -> Word64 -> [blk] -> LedgerDB l blk -> Maybe (LedgerDB l blk)
ledgerDbSwitch' cfg n bs db =
    case runIdentity $ ledgerDbSwitch cfg n (map pureBlock bs) db of
      Left  ExceededRollback{} -> Nothing
      Right db'                -> Just db'

{-------------------------------------------------------------------------------
  Serialisation
-------------------------------------------------------------------------------}

instance (Serialise l, Serialise (HeaderHash blk))
      => Serialise (ChainSummary l blk) where
  encode = encodeChainSummary encode encode
  decode = decodeChainSummary decode decode

encodeChainSummary :: (l -> Encoding)
                   -> (HeaderHash blk -> Encoding)
                   -> ChainSummary l blk -> Encoding
encodeChainSummary encodeLedger encodeHash ChainSummary{..} = mconcat [
      Enc.encodeListLen 3
      -- Note: for backwards-compatibility, we encode it as a @WithOrigin
      -- (RealPoint blk) instead of a @Point blk@, which have different
      -- encodings.
    , encodeWithOrigin
        (encodeRealPoint encodeHash)
        (pointToWithOriginRealPoint csTip)
    , Enc.encodeWord64 csLength
    , encodeLedger csLedger
    ]

decodeChainSummary :: (forall s. Decoder s l)
                   -> (forall s. Decoder s (HeaderHash blk))
                   -> forall s. Decoder s (ChainSummary l blk)
decodeChainSummary decodeLedger decodeHash = do
    Dec.decodeListLenOf 3
    -- Note: for backwards-compatibility, we encode it as a @WithOrigin
    -- (RealPoint blk) instead of a @Point blk@, which have different
    -- encodings.
    csTip    <- withOriginRealPointToPoint <$>
                  decodeWithOrigin (decodeRealPoint decodeHash)
    csLength <- Dec.decodeWord64
    csLedger <- decodeLedger
    return ChainSummary{..}
