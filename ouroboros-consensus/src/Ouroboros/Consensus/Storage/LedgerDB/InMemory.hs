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
import           Data.Proxy
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
data LedgerDB l r = LedgerDB {
      -- | Older ledger states
      ledgerDbCheckpoints :: !(StrictSeq (Checkpoint l r))

      -- | Information about the state of the ledger /before/
    , ledgerDbAnchor      :: !(ChainSummary l r)

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
data instance Ticked (LedgerDB l r) = TickedLedgerDB {
      tickedLedgerDbTicked :: Ticked l
    , tickedLedgerDbOrig   :: LedgerDB l r
    }

{-------------------------------------------------------------------------------
  Internal: checkpoints
-------------------------------------------------------------------------------}

-- | Checkpoint with a ledger state
data Checkpoint l r = Checkpoint {
      cpBlock :: !r
    , cpState :: !l
    }
  deriving (Show, Eq, Generic, NoThunks)

cpToPair :: Checkpoint l r -> (r, l)
cpToPair (Checkpoint r l) = (r, l)

{-------------------------------------------------------------------------------
  Chain summary
-------------------------------------------------------------------------------}

-- | Summary of the chain at a particular point in time
data ChainSummary l r = ChainSummary {
      -- | The tip of the chain
      csTip    :: !(WithOrigin r)

      -- | Length of the chain
    , csLength :: !Word64

      -- | Ledger state
    , csLedger :: !l
    }
  deriving (Show, Eq, Generic, NoThunks)

genesisChainSummary :: l -> ChainSummary l r
genesisChainSummary l = ChainSummary Origin 0 l

{-------------------------------------------------------------------------------
  LedgerDB proper
-------------------------------------------------------------------------------}

-- | Ledger DB starting at the specified ledger state
ledgerDbWithAnchor :: LedgerDbParams -> ChainSummary l r -> LedgerDB l r
ledgerDbWithAnchor params anchor = LedgerDB {
      ledgerDbCheckpoints = Seq.empty
    , ledgerDbAnchor      = anchor
    , ledgerDbParams      = params
    }

ledgerDbFromGenesis :: LedgerDbParams -> l -> LedgerDB l r
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
type ResolveBlock m r b = r -> m b

-- | Annotated ledger errors
data AnnLedgerError l r = AnnLedgerError {
      -- | The ledger DB just /before/ this block was applied
      annLedgerState  :: LedgerDB l r

      -- | Reference to the block that had the error
    , annLedgerErrRef :: r

      -- | The ledger error itself
    , annLedgerErr    :: LedgerErr l
    }

-- | Monads in which we can resolve blocks
--
-- To guide type inference, we insist that we must be able to infer the type
-- of the block we are resolving from the type of the monad.
class Monad m => ResolvesBlocks r b m | m -> b where
  resolveBlock :: r -> m b

instance Monad m => ResolvesBlocks r b (ReaderT (ResolveBlock m r b) m) where
  resolveBlock r = ReaderT $ \f -> f r

defaultResolveBlocks :: ResolveBlock m r b
                     -> ReaderT (ResolveBlock m r b) m a
                     -> m a
defaultResolveBlocks = flip runReaderT

-- Quite a specific instance so we can satisfy the fundep
instance Monad m
      => ResolvesBlocks r b (ExceptT e (ReaderT (ResolveBlock m r b) m)) where
  resolveBlock = lift . resolveBlock

class Monad m => ThrowsLedgerError l r m where
  throwLedgerError :: LedgerDB l r -> r -> LedgerErr l -> m a

defaultThrowLedgerErrors :: ExceptT (AnnLedgerError l r) m a
                         -> m (Either (AnnLedgerError l r) a)
defaultThrowLedgerErrors = runExceptT

defaultResolveWithErrors :: ResolveBlock m r b
                         -> ExceptT (AnnLedgerError l r)
                                    (ReaderT (ResolveBlock m r b) m)
                                    a
                         -> m (Either (AnnLedgerError l r) a)
defaultResolveWithErrors resolve =
      defaultResolveBlocks resolve
    . defaultThrowLedgerErrors

instance Monad m => ThrowsLedgerError l r (ExceptT (AnnLedgerError l r) m) where
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
data Ap :: (Type -> Type) -> Type -> Type -> Type -> Constraint -> Type where
  ReapplyVal :: r -> b -> Ap m l r b ()
  ApplyVal   :: r -> b -> Ap m l r b (                       ThrowsLedgerError l r m)
  ReapplyRef :: r      -> Ap m l r b (ResolvesBlocks  r b m)
  ApplyRef   :: r      -> Ap m l r b (ResolvesBlocks  r b m, ThrowsLedgerError l r m)

  -- | 'Weaken' increases the constraint on the monad @m@.
  --
  -- This is primarily useful when combining multiple 'Ap's in a single
  -- homogeneous structure.
  Weaken :: (c' => c) => Ap m l r b c -> Ap m l r b c'

{-------------------------------------------------------------------------------
  Internal utilities for 'Ap'
-------------------------------------------------------------------------------}

apRef :: Ap m l r b c -> r
apRef (ReapplyVal r _) = r
apRef (ApplyVal   r _) = r
apRef (ReapplyRef r  ) = r
apRef (ApplyRef   r  ) = r
apRef (Weaken     ap)  = apRef ap

-- | Apply block to the current ledger state
--
-- We take in the entire 'LedgerDB' because we record that as part of errors.
applyBlock :: forall m c l r b. (ApplyBlock l b, Monad m, c)
           => LedgerCfg l
           -> Ap m l r b c
           -> LedgerDB l r -> m l
applyBlock cfg ap db = case ap of
    ReapplyVal _r b ->
      return $
        tickThenReapply cfg b l
    ApplyVal r b ->
      either (throwLedgerError db r) return $ runExcept $
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
ledgerDbCurrent :: LedgerDB l r -> l
ledgerDbCurrent LedgerDB{..} = case ledgerDbCheckpoints of
  Empty                  -> csLedger ledgerDbAnchor
  (_ :|> Checkpoint _ l) -> l

-- | Total length of the chain (in terms of number of blocks)
ledgerDbChainLength :: LedgerDB l r -> Word64
ledgerDbChainLength LedgerDB{..} =
    csLength ledgerDbAnchor + fromIntegral (Seq.length ledgerDbCheckpoints)

-- | References to blocks and corresponding ledger state (from old to new)
ledgerDbToList :: LedgerDB l r -> [(r, l)]
ledgerDbToList LedgerDB{..} = map cpToPair $ toList ledgerDbCheckpoints

-- | All snapshots currently stored by the ledger DB (new to old)
--
-- This also includes the snapshot at the anchor. For each snapshot we also
-- return the distance from the tip.
ledgerDbSnapshots :: forall l r. LedgerDB l r -> [(Word64, l)]
ledgerDbSnapshots LedgerDB{..} = go 0 ledgerDbCheckpoints
  where
    go :: Word64 -> StrictSeq (Checkpoint l r) -> [(Word64, l)]
    go !offset Empty                   = [(offset, csLedger ledgerDbAnchor)]
    go !offset (ss :|> Checkpoint _ l) = (offset, l) : go (offset + 1) ss

-- | How many blocks can we currently roll back?
ledgerDbMaxRollback :: LedgerDB l r -> Word64
ledgerDbMaxRollback LedgerDB{..} = fromIntegral (Seq.length ledgerDbCheckpoints)

-- | Reference to the block at the tip of the chain
ledgerDbTip :: LedgerDB l r -> WithOrigin r
ledgerDbTip LedgerDB{..} =
    case ledgerDbCheckpoints of
      Empty    -> csTip ledgerDbAnchor
      _ :|> cp -> NotOrigin (cpBlock cp)

-- | Have we seen at least @k@ blocks?
ledgerDbIsSaturated :: LedgerDB l r -> Bool
ledgerDbIsSaturated LedgerDB{..} =
    fromIntegral (Seq.length ledgerDbCheckpoints) >= k
  where
    LedgerDbParams{..} = ledgerDbParams
    SecurityParam k    = ledgerDbSecurityParam

{-------------------------------------------------------------------------------
  Internal updates
-------------------------------------------------------------------------------}

-- | Internal: shift the anchor given a bunch of checkpoints.
shiftAnchor :: forall r l. HasCallStack
            => StrictSeq (Checkpoint l r) -> ChainSummary l r -> ChainSummary l r
shiftAnchor toRemove ChainSummary{..} = ChainSummary {
      csTip    = NotOrigin csTip'
    , csLength = csLength + fromIntegral (Seq.length toRemove)
    , csLedger = csLedger'
    }
  where
    csTip'    :: r
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
prune :: HasCallStack => LedgerDB l r -> LedgerDB l r
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
pushLedgerState :: l  -- ^ Updated ledger state
                -> r  -- ^ Reference to the applied block
                -> LedgerDB l r -> LedgerDB l r
pushLedgerState current' ref db@LedgerDB{..}  = prune $ db {
      ledgerDbCheckpoints = snapshots
    }
  where
    snapshots = ledgerDbCheckpoints |> Checkpoint ref current'

{-------------------------------------------------------------------------------
  Internal: rolling back
-------------------------------------------------------------------------------}

-- | Reconstruct ledger DB from a list of checkpoints
reconstructFrom :: forall l r.
                   LedgerDbParams
                -> ChainSummary l r
                -> StrictSeq (Checkpoint l r)
                -> LedgerDB l r
reconstructFrom params anchor snapshots =
    LedgerDB {
        ledgerDbCheckpoints = snapshots
      , ledgerDbParams      = params
      , ledgerDbAnchor      = anchor
      }

-- | Generalization of rollback using a function on the checkpoints
rollbackTo :: (   ChainSummary l r
               -> StrictSeq (Checkpoint l r)
               -> Maybe (StrictSeq (Checkpoint l r))
              )
           -> LedgerDB l r
           -> Maybe (LedgerDB l r)
rollbackTo f (LedgerDB checkpoints anchor params) =
    reconstructFrom params anchor <$> f anchor checkpoints

-- | Rollback
--
-- Returns 'Nothing' if maximum rollback is exceeded.
rollback :: forall l r.
            Word64
         -> LedgerDB l r
         -> Maybe (LedgerDB l r)
rollback 0 db = Just db
rollback n db = rollbackTo (\_anchor -> go) db
  where
    go :: StrictSeq (Checkpoint l r) -> Maybe (StrictSeq (Checkpoint l r))
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
-- When no 'Checkpoint' (or anchor) has the given @'WithOrigin' r@, 'Nothing' is
-- returned.
--
-- To avoid a linear search on the checkpoints (typically 2160 of them), we do a
-- binary search benefitting from the cheap splits of the underlying
-- 'StrictSeq' \( O(\log n) \).
--
-- For a binary search, the checkpoints have to be ordered by @r@. In practice,
-- we'll use 'RealPoint' for @r@, which, because of the existence of EBBs,
-- doesn't have a reliable ordering: it orders first on 'SlotNo', which is
-- correct. But in case of a tie, it will look at the hash, which is not what we
-- need: an EBB has the same slot as the block after it, so we'd want the
-- 'RealPoint' of an EBB to be /less/ than the 'RealPoint' of the regular block
-- in the same slot. But the decision is made based on the hash, so we won't get
-- a reliable answer.
--
-- Therefore, we take a projection @refOrder :: r -> ro@ as an argument. The
-- @ro@ type should have a correct ordering, so that the list below is correctly
-- ordered:
--
-- > map (refOrder . cpBlock) $ Seq.toList (ledgerDbCheckpoints db)
--
-- When instantiating @r@ to 'RealPoint', one should use 'realPointSlot' as
-- @refOrder@.
--
-- Note: we don't use @fingertree@ for the checkpoints (with its @search@
-- operation we could use here) because we'd have to impose more constraints on
-- the @r@ type. We could do an interpolation search if we assume more about the
-- @ro@ parameter ('SlotNo'), but that would be more complicated.
ledgerDbPast ::
     forall l r ro. (Ord ro, Eq r)
  => (r -> ro)
  -> WithOrigin r
  -> LedgerDB l r
  -> Maybe l
ledgerDbPast refOrder tip db
    | tip == ledgerDbTip db
    = Just (ledgerDbCurrent db)
    | tip == csTip (ledgerDbAnchor db)
    = Just (csLedger (ledgerDbAnchor db))
    | NotOrigin tip' <- tip
    = cpState <$> binarySearch tip' (Seq.getSeq (ledgerDbCheckpoints db))
    | otherwise
    = Nothing
  where
    binarySearch :: r -> LazySeq.Seq (Checkpoint l r) -> Maybe (Checkpoint l r)
    binarySearch _   LazySeq.Empty = Nothing
    binarySearch ref checkpoints   = case LazySeq.splitAt middle checkpoints of
        (before, LazySeq.Empty)        -> binarySearch ref before
        (before, cp LazySeq.:<| after) ->
          case (compare `on` refOrder) ref (cpBlock cp) of
            LT -> binarySearch ref before
            GT -> binarySearch ref after
            EQ
              | isMatch cp -> Just cp
              | otherwise  ->
                -- Oh EBBs, why do you make everything so much harder? An EBB
                -- has the same slot as the regular block after it. We look left
                -- and right from @cp@ for checkpoints with the same @ro@
                -- ('SlotNo' in practice) and do a linear search among those.
                -- When it's indeed a slot populated by both a regular block and
                -- EBB, we'll look at /one/ other checkpoint. In all other
                -- cases, we'll look at /none/. Note that we have to look in
                -- both directions because we don't know whether @cp@ is the EBB
                -- or the regular block in the same slot.
                find isMatch (LazySeq.takeWhileR sameOrder before) `mplus`
                find isMatch (LazySeq.takeWhileL sameOrder after)
      where
        middle :: Int
        middle = LazySeq.length checkpoints `div` 2

        isMatch :: Checkpoint l r -> Bool
        isMatch cp = cpBlock cp == ref

        sameOrder :: Checkpoint l r -> Bool
        sameOrder cp = refOrder (cpBlock cp) == refOrder ref

-- | Get a past ledger state
--
-- \( O(n) \)
--
-- Straightforward implementation of 'ledgerDbPast' using a linear search.
--
-- Can be used in tests to compare against 'ledgerDbPast'.
ledgerDbPastSpec ::
     forall l r. Eq r
  => WithOrigin r
  -> LedgerDB l r
  -> Maybe l
ledgerDbPastSpec tip db
    | tip == ledgerDbTip db
    = Just (ledgerDbCurrent db)
    | tip == csTip (ledgerDbAnchor db)
    = Just (csLedger (ledgerDbAnchor db))
    | NotOrigin tip' <- tip
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

ledgerDbPush :: forall m c l r b. (ApplyBlock l b, Monad m, c)
             => LedgerCfg l
             -> Ap m l r b c -> LedgerDB l r -> m (LedgerDB l r)
ledgerDbPush cfg ap db =
    (\current' -> pushLedgerState current' (apRef ap) db) <$>
      applyBlock cfg ap db

-- | Push a bunch of blocks (oldest first)
ledgerDbPushMany :: (ApplyBlock l b, Monad m, c)
                 => LedgerCfg l
                 -> [Ap m l r b c] -> LedgerDB l r -> m (LedgerDB l r)
ledgerDbPushMany = repeatedlyM . ledgerDbPush

-- | Switch to a fork
ledgerDbSwitch :: (ApplyBlock l b, Monad m, c)
               => LedgerCfg l
               -> Word64          -- ^ How many blocks to roll back
               -> [Ap m l r b c]  -- ^ New blocks to apply
               -> LedgerDB l r
               -> m (Either ExceededRollback (LedgerDB l r))
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

type instance LedgerCfg (LedgerDB l r) = LedgerCfg l

type instance HeaderHash (LedgerDB l r) = HeaderHash l

instance IsLedger l => GetTip (LedgerDB l r) where
  getTip = castPoint . getTip . ledgerDbCurrent

instance IsLedger l => GetTip (Ticked (LedgerDB l r)) where
  getTip = castPoint . getTip . tickedLedgerDbOrig

instance ( IsLedger l
           -- Required superclass constraints of 'IsLedger'
         , Show     r
         , Eq       r
         , NoThunks r
         ) => IsLedger (LedgerDB l r) where
  type LedgerErr (LedgerDB l r) = LedgerErr l

  applyChainTick cfg slot db = TickedLedgerDB {
        tickedLedgerDbTicked = applyChainTick cfg slot (ledgerDbCurrent db)
      , tickedLedgerDbOrig   = db
      }

instance ApplyBlock l blk => ApplyBlock (LedgerDB l (RealPoint blk)) blk where
  applyLedgerBlock cfg blk TickedLedgerDB{..} =
      push <$> applyLedgerBlock
                 cfg
                 blk
                 tickedLedgerDbTicked
   where
     push :: l -> LedgerDB l (RealPoint blk)
     push l = pushLedgerState l (blockRealPoint blk) tickedLedgerDbOrig

  reapplyLedgerBlock cfg blk TickedLedgerDB{..} =
      push $ reapplyLedgerBlock
               cfg
               blk
               tickedLedgerDbTicked
   where
     push :: l -> LedgerDB l (RealPoint blk)
     push l = pushLedgerState l (blockRealPoint blk) tickedLedgerDbOrig

{-------------------------------------------------------------------------------
  Suppor for testing
-------------------------------------------------------------------------------}

pureBlock :: b -> Ap m l b b ()
pureBlock b = ReapplyVal b b

triviallyResolve :: forall b a. Proxy b
                 -> Reader (ResolveBlock Identity b b) a -> a
triviallyResolve _ = runIdentity . defaultResolveBlocks return

ledgerDbPush' :: ApplyBlock l b
              => LedgerCfg l -> b -> LedgerDB l b -> LedgerDB l b
ledgerDbPush' cfg b = runIdentity . ledgerDbPush cfg (pureBlock b)

ledgerDbPushMany' :: ApplyBlock l b
                  => LedgerCfg l -> [b] -> LedgerDB l b -> LedgerDB l b
ledgerDbPushMany' cfg bs = runIdentity . ledgerDbPushMany cfg (map pureBlock bs)

ledgerDbSwitch' :: forall l b. ApplyBlock l b
                => LedgerCfg l
                -> Word64 -> [b] -> LedgerDB l b -> Maybe (LedgerDB l b)
ledgerDbSwitch' cfg n bs db =
    case triviallyResolve (Proxy @b) $
           ledgerDbSwitch cfg n (map pureBlock bs) db of
      Left  ExceededRollback{} -> Nothing
      Right db'                -> Just db'

{-------------------------------------------------------------------------------
  Serialisation
-------------------------------------------------------------------------------}

instance (Serialise l, Serialise r) => Serialise (ChainSummary l r) where
  encode = encodeChainSummary encode encode
  decode = decodeChainSummary decode decode

encodeChainSummary :: (l -> Encoding)
                   -> (r -> Encoding)
                   -> ChainSummary l r -> Encoding
encodeChainSummary encodeLedger encodeRef ChainSummary{..} = mconcat [
      Enc.encodeListLen 3
    , encodeWithOrigin encodeRef csTip
    , Enc.encodeWord64 csLength
    , encodeLedger csLedger
    ]

decodeChainSummary :: (forall s. Decoder s l)
                   -> (forall s. Decoder s r)
                   -> forall s. Decoder s (ChainSummary l r)
decodeChainSummary decodeLedger decodeRef = do
    Dec.decodeListLenOf 3
    csTip    <- decodeWithOrigin decodeRef
    csLength <- Dec.decodeWord64
    csLedger <- decodeLedger
    return ChainSummary{..}
