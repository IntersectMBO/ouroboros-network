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
     -- ** Pure API
   , ledgerDbPush'
   , ledgerDbPushMany'
   , ledgerDbSwitch'
   , ledgerDbPast'
   ) where

import           Prelude hiding (mod, (/))
import qualified Prelude

import           Codec.Serialise (Serialise (..))
import           Codec.Serialise.Decoding (Decoder)
import qualified Codec.Serialise.Decoding as Dec
import           Codec.Serialise.Encoding (Encoding)
import qualified Codec.Serialise.Encoding as Enc
import           Control.Monad ((>=>))
import           Control.Monad.Except hiding (ap)
import           Control.Monad.Reader hiding (ap)
import           Data.Foldable (toList)
import           Data.Functor.Identity
import           Data.Kind (Constraint)
import           Data.Proxy
import           Data.Sequence.Strict (StrictSeq ((:|>), Empty), (|>))
import qualified Data.Sequence.Strict as Seq
import           Data.Word
import           GHC.Generics (Generic)
import           GHC.Stack (HasCallStack)

import           Cardano.Prelude (NoUnexpectedThunks)
import           Cardano.Slotting.Slot

import           Ouroboros.Consensus.Block.RealPoint
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Protocol.Abstract (SecurityParam (..))
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
-- > anchor |> blocks and snapshots <| current
--
-- where @anchor@ records the oldest known snapshot and @current@ the most
-- recent.
--
-- As an example, suppose we have @snapEvery = 4@ (i.e., we will take a
-- snapshot every 4 blocks) and @k = 6@. The ledger DB grows as illustrated
-- below, where we indicate the anchor, number of blocks, the stored
-- blocks/snapshots, and the current ledger; the oldest block we can roll back
-- to is marked.
--
-- > anchor |> #   [ blocks ]                                      <| tip
-- > ---------------------------------------------------------------------------
-- > *G     |> (0) [ ]                                             <| G
-- > *G     |> (1) [ B1]                                           <| L1
-- > *G     |> (2) [ B1,  B2]                                      <| L2
-- > *G     |> (3) [ B1,  B2,   B3]                                <| L3
-- > *G     |> (4) [ B1,  B2,   B3,  L4]                           <| L4
-- > *G     |> (5) [ B1,  B2,   B3,  L4,  B5]                      <| L5
-- > *G     |> (6) [ B1,  B2,   B3,  L4,  B5,  B6]                 <| L6
-- >  G     |> (7) [*B1,  B2,   B3,  L4,  B5,  B6,  B7]            <| L7
-- >  G     |> (8) [ B1, *B2,   B3,  L4,  B5,  B6,  B7,  L8]       <| L8
-- >  G     |> (9) [ B1,  B2,  *B3,  L4,  B5,  B6,  B7,  L8,  B9]  <| L9   (*)
-- > *L4    |> (6) [ B5,  B6,   B7,  L8,  B9,  B10]                <| L10  (**)
-- >  L4    |> (7) [*B5,  B6,   B7,  L8,  B9,  B10, B11]           <| L11
-- >  L4    |> (8) [ B5, *B6,   B7,  L8,  B9,  B10, B11, L12]      <| L12
-- >  L4    |> (9) [ B5,  B6,  *B7,  L8,  B9,  B10, B12, L12, B13] <| L13
-- > *L8    |> (6) [ B9,  B10,  B12, L12, B13, B14]                <| L14
--
-- The ledger DB must guarantee we must at all times be able to roll back @k@
-- blocks. For example, if we are on line (*), and roll back 6 blocks, we get
--
-- > G |> [B1, B2, B3]
--
-- from which we can /compute/ @G |> [B1, B2, B3] <| L3@ by re-applying
-- @snapEvery - 1@ blocks. However, as soon as we pushed one more block @(**)@,
-- and then roll back 6 blocks, we end up with
--
-- > L4 |> [] <| L4
--
-- This representation has the following properties:
--
-- * The distance between snapshots is at most @snapEvery@. This implies that
--   rolling back involves re-applying at most @snapEvery - 1@ blocks
--   (note that @snapEvery >= 1@).
--
-- * This implies that the number of (references to) blocks we store will vary
--   between @k@ and @k + snapEvery - 1@ (unless we are near genesis).
--
-- * When the number of blocks reaches @k + snapEvery@, we can drop the first
--   @snapEvery@ blocks, shifting up the anchor.
--
-- * The average number of blocks we have to reapply on a rollback is given by
--
--   >    average [0 .. snapEvery - 1]
--   > == ((snapEvery - 1) * snapEvery / 2) / snapEvery
--   > == (snapEvery - 1) / 2
--
--   (Obvious) boundary case: if @snapEvery == 1@, the average number is zero.
--
-- * The number of snapshots we store is in the range
--
--   > [1 + floor(k / snapEvery), 1 + ceiling(k / snapEvery)]
--
--   If @snapEvery@ divides @k@, the number is precisely @1 + k / snapEvery@.
--
-- * Picking a suitable value of @snapEvery@ for a given @k@ is a tradeoff
--   between cost of reapplying blocks and the cost of storing more snapshots.
--   The latter is primarily the cost of less opportunity for garbage
--   collection, which is hard to quantify abstractly. This should probably
--   be determined empirically.
--
-- Some example boundary cases:
--
-- * Suppose @k = 4@ and @snapEvery = 1@:
--
--   > *G  |> []               <| G
--   > *G  |> [L1]             <| L1
--   > *G  |> [L1, L2]         <| L2
--   > *G  |> [L1, L2, L3]     <| L3
--   > *G  |> [L1, L2, L3, L4] <| L4
--   > *L1 |> [L2, L3, L4, L5] <| L5
--   > *L2 |> [L3, L4, L5, L6] <| L6
--
--   Note that in this case the number of blocks will always be @k@ (unless we
--   are close to genesis), and the anchor is always the oldest point we can
--   roll back to.
--
-- * If @k = 1@ and @snapEvery = 4@, we get
--
--   > *G  |> [ ]                  <| G
--   > *G  |> [ B1]                <| L1
--   >  G  |> [*B1,  B2]           <| L2
--   >  G  |> [ B1, *B2,  B3]      <| L3
--   >  G  |> [ B1,  B2, *B3, L4]  <| L4
--   > *L4 |> [ B5]                <| L5
--   >  L4 |> [*B5,  B6]           <| L6
--   >  L4 |> [ B5, *B6,  B7]      <| L7
--   >  L4 |> [ B5,  B6, *B7, L8]  <| L8
--   > *L8 |> [*B9]                <| L9
--
--   (The minimum distance between the current ledger and the maximum rollback
--   is @k@.)
--
-- * If @k = 0@ and @snapEvery = 4@, we get
--
--   > *G  |> [ ]                  <| G
--   >  G  |> [*B1]                <| L1
--   >  G  |> [ B1, *B2]           <| L2
--   >  G  |> [ B1,  B2, *B3]      <| L3
--   > *L4 |> [ ]                  <| L4
--   >  L4 |> [*B5]                <| L5
--   >  L4 |> [ B5, *B6]           <| L6
--   >  L4 |> [ B5,  B6, *B7]      <| L7
--   > *L8 |> [ ]                  <| L8
--
--   @k = 0@ is the only case where the list might be empty. Of course, this is
--   not a particularly useful configuration.
--
-- * If @k = 1@ and @snapEvery = 1@, we get
--
--   > *G  |> [ ]   <| G
--   > *G  |> [ L1] <| L1
--   > *L1 |> [ L2] <| L2
--   > *L2 |> [ L3] <| L3
--
-- * If @k = 0@ and @snapEvery = 1@, we get
--
--   > *G  |> [ ] <| G
--   > *L1 |> [ ] <| L1
--   > *L2 |> [ ] <| L2
--
-- * Finally, if @k = snapEvery = k4@, we get
--
--   > *G  |> [ ]                             <| G
--   > *G  |> [ B1]                           <| L1
--   > *G  |> [ B1,  B2]                      <| L2
--   > *G  |> [ B1,  B2,  B3]                 <| L3
--   > *G  |> [ B1,  B2,  B3, L4]             <| L4
--   >  G  |> [*B1,  B2,  B3, L4, B5]         <| L5
--   >  G  |> [ B1, *B2,  B3, L4, B5, B6]     <| L6
--   >  G  |> [ B1,  B2, *B3, L4, B5, B6, B7] <| L7
--   > *L4 |> [ B5,  B6,  B7, L8]             <| L8
--   >  L4 |> [*B5,  B6,  B7, L8, B9]         <| L9
data LedgerDB l r = LedgerDB {
      -- | The ledger state at the tip of the chain
      ledgerDbCurrent :: !l

      -- | Older ledger states
    , ledgerDbBlocks  :: !(StrictSeq (Checkpoint l r))

      -- | Information about the state of the ledger /before/
    , ledgerDbAnchor  :: !(ChainSummary l r)

      -- | Ledger DB parameters
    , ledgerDbParams  :: !LedgerDbParams
    }
  deriving (Show, Eq, Generic, NoUnexpectedThunks)

data LedgerDbParams = LedgerDbParams {
      -- | Take a snapshot every @n@ blocks
      --
      -- Must be @>= 1@.
      ledgerDbSnapEvery     :: !Word64

      -- | Security parameter (maximum rollback)
    , ledgerDbSecurityParam :: !SecurityParam
    }
  deriving (Show, Eq, Generic, NoUnexpectedThunks)

-- | Default parameters
--
-- TODO: We should decide empirically what a good @snapEvery@ value is.
-- <https://github.com/input-output-hk/ouroboros-network/issues/1026>
ledgerDbDefaultParams :: SecurityParam -> LedgerDbParams
ledgerDbDefaultParams (SecurityParam k) = LedgerDbParams {
      ledgerDbSnapEvery     = 100
    , ledgerDbSecurityParam = SecurityParam k
    }

{-------------------------------------------------------------------------------
  Internal: checkpoints
-------------------------------------------------------------------------------}

data Checkpoint l r =
    -- | Checkpoint with a block reference only
    CpBlock !r

    -- | Checkpoint with a ledger state
  | CpSShot !r !l
  deriving (Show, Eq, Generic, NoUnexpectedThunks)

cpToPair :: Checkpoint l r -> (r, Maybe l)
cpToPair (CpBlock r)   = (r, Nothing)
cpToPair (CpSShot r l) = (r, Just l)

cpBlock :: Checkpoint l r -> r
cpBlock (CpBlock r)   = r
cpBlock (CpSShot r _) = r

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
  deriving (Show, Eq, Generic, NoUnexpectedThunks)

genesisChainSummary :: l -> ChainSummary l r
genesisChainSummary l = ChainSummary Origin 0 l

{-------------------------------------------------------------------------------
  LedgerDB proper
-------------------------------------------------------------------------------}

-- | Ledger DB starting at the specified ledger state
ledgerDbWithAnchor :: LedgerDbParams -> ChainSummary l r -> LedgerDB l r
ledgerDbWithAnchor params anchor = LedgerDB {
      ledgerDbCurrent = csLedger anchor
    , ledgerDbBlocks  = Seq.empty
    , ledgerDbAnchor  = anchor
    , ledgerDbParams  = params
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
data Ap :: (* -> *) -> * -> * -> * -> Constraint -> * where
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

-- | Short-hand for re-applying a block that we have by reference
--
-- This is not defined in terms of 'applyBlock' because we don't need the
-- full ledger DB here (because we never throw any errors).
reapplyRef :: forall m l b r. (ResolvesBlocks r b m, ApplyBlock l b)
           => LedgerCfg l -> r -> l -> m l
reapplyRef cfg r l = (flip (tickThenReapply cfg) l) <$> resolveBlock r

{-------------------------------------------------------------------------------
  Queries
-------------------------------------------------------------------------------}

-- | Total length of the chain (in terms of number of blocks)
ledgerDbChainLength :: LedgerDB l r -> Word64
ledgerDbChainLength LedgerDB{..} =
    csLength ledgerDbAnchor + fromIntegral (Seq.length ledgerDbBlocks)

-- | References to blocks and corresponding ledger state (from old to new)
ledgerDbToList :: LedgerDB l r -> [(r, Maybe l)]
ledgerDbToList LedgerDB{..} = map cpToPair $ toList ledgerDbBlocks

-- | All snapshots currently stored by the ledger DB (new to old)
--
-- For each snapshot we also return the distance from the tip
ledgerDbSnapshots :: forall l r. LedgerDB l r -> [(Word64, l)]
ledgerDbSnapshots LedgerDB{..} = go 0 ledgerDbBlocks
  where
    go :: Word64 -> StrictSeq (Checkpoint l r) -> [(Word64, l)]
    go !offset Empty                = [(offset, csLedger ledgerDbAnchor)]
    go !offset (ss :|> CpBlock _)   =               go (offset + 1) ss
    go !offset (ss :|> CpSShot _ l) = (offset, l) : go (offset + 1) ss

-- | How many blocks can we currently roll back?
ledgerDbMaxRollback :: LedgerDB l r -> Word64
ledgerDbMaxRollback LedgerDB{..} = fromIntegral (Seq.length ledgerDbBlocks)

-- | Reference to the block at the tip of the chain
ledgerDbTip :: LedgerDB l r -> WithOrigin r
ledgerDbTip LedgerDB{..} =
    case ledgerDbBlocks of
      Empty    -> csTip ledgerDbAnchor
      _ :|> cp -> At (cpBlock cp)

-- | Have we seen at least @k@ blocks?
ledgerDbIsSaturated :: LedgerDB l r -> Bool
ledgerDbIsSaturated LedgerDB{..} =
    fromIntegral (Seq.length ledgerDbBlocks) >= k
  where
    LedgerDbParams{..} = ledgerDbParams
    SecurityParam k    = ledgerDbSecurityParam

{-------------------------------------------------------------------------------
  Internal updates
-------------------------------------------------------------------------------}

-- | Internal: shift the anchor given a bunch of blocks
--
-- PRE: The last block in the sequence /must/ contain a ledger snapshot.
shiftAnchor :: forall r l. HasCallStack
            => StrictSeq (Checkpoint l r) -> ChainSummary l r -> ChainSummary l r
shiftAnchor toRemove ChainSummary{..} = ChainSummary {
      csTip    = At csTip'
    , csLength = csLength + fromIntegral (Seq.length toRemove)
    , csLedger = csLedger'
    }
  where
    csTip'    :: r
    csLedger' :: l
    (csTip', csLedger') =
        case toRemove of
          Empty             -> error "shiftAnchor: empty list"
          _ :|> CpBlock _   -> error "shiftAnchor: missing ledger"
          _ :|> CpSShot r l -> (r, l)

-- | Internal: count number of blocks to prune, given total number of blocks
--
-- This is exposed for the benefit of tests only.
ledgerDbCountToPrune :: HasCallStack => LedgerDbParams -> Int -> Int
ledgerDbCountToPrune LedgerDbParams{..} curSize'
  | curSize <= maxSize = 0
  | otherwise          = fromIntegral $ numToRemove
  where
    SecurityParam k = ledgerDbSecurityParam

    -- Current, minimum and maximum number of blocks we need
    curSize, minSize, maxSize :: Word64
    curSize = fromIntegral curSize'
    minSize = k
    maxSize = k + ledgerDbSnapEvery - 1

    -- Number of blocks to remove (assuming curSize > maxSize)
    --
    -- Notes:
    --
    -- * If @curSize > maxSize@, then @curSize >= ledgerDbSnapEvery@
    -- * This means we will at least remove 'ledgerDbSnapEvery' blocks
    -- * We will remove an even multiple of 'ledgerDbSnapEvery' blocks
    -- * This means that the last block we remove must contain a snapshot
    numToRemove :: Word64
    numToRemove = nearestMultiple ledgerDbSnapEvery (curSize - minSize)

    -- Nearest multiple of n, not exceeding x
    --
    -- > nearestMultiple 4 3 == 0
    -- > nearestMultiple 4 4 == 4
    -- > nearestMultiple 4 5 == 4
    -- > ..
    -- > nearestMultiple 4 7 == 4
    -- > nearestMultiple 4 8 == 8
    -- > nearestMultiple 4 9 == 8
    nearestMultiple :: Integral a => a -> a -> a
    nearestMultiple n x = floor (x' `safeDiv` n') * n
      where
        n', x' :: Double
        n' = fromIntegral n
        x' = fromIntegral x

-- | Internal: drop unneeded blocks from the head of the list
--
-- Postcondition: number blocks is between k and k + snapEvery - 1
prune :: HasCallStack => LedgerDB l r -> LedgerDB l r
prune db@LedgerDB{..} =
    if toPrune == 0
      then db
      else let (removed, kept) = Seq.splitAt toPrune ledgerDbBlocks
               anchor'         = shiftAnchor removed ledgerDbAnchor
           in db { ledgerDbAnchor = anchor'
                 , ledgerDbBlocks = kept
                 }
  where
    -- Number of blocks to remove (assuming curSize > maxSize)
    toPrune :: Int
    toPrune = ledgerDbCountToPrune ledgerDbParams (Seq.length ledgerDbBlocks)

-- | Push an updated ledger state
pushLedgerState :: l  -- ^ Updated ledger state
                -> r  -- ^ Reference to the applied block
                -> LedgerDB l r -> LedgerDB l r
pushLedgerState current' ref db@LedgerDB{..}  = prune $ db {
      ledgerDbCurrent = current'
    , ledgerDbBlocks  = blocks'
    }
  where
    LedgerDbParams{..} = ledgerDbParams

    newPos  = fromIntegral (Seq.length ledgerDbBlocks) + 1
    blocks' = ledgerDbBlocks
           |> if newPos `safeMod` ledgerDbSnapEvery == 0
                then CpSShot ref current'
                else CpBlock ref

{-------------------------------------------------------------------------------
  Internal: rolling back
-------------------------------------------------------------------------------}

-- | Compute ledger state after list of checkpoints
--
-- Given a list of checkpoints, find the most recent checkpoint that has a
-- associated ledger state, compute the list of blocks that should be applied
-- on top of that ledger state, then reapply those blocks from old to new.
ledgerAfter :: forall m l r b. (ApplyBlock l b, ResolvesBlocks r b m)
            => LedgerCfg l
            -> ChainSummary l r
            -> StrictSeq (Checkpoint l r)
            -> m l
ledgerAfter cfg anchor blocks' =
    uncurry computeCurrent $ reapply blocks'
  where
    reapply :: StrictSeq (Checkpoint l r) -> ([r], l)
    reapply = go []
      where
        go :: [r] -> StrictSeq (Checkpoint l r) -> ([r], l)
        go acc Empty                = (acc, csLedger anchor)
        go acc (_  :|> CpSShot _ l) = (acc, l)
        go acc (ss :|> CpBlock r)   = go (r:acc) ss

    computeCurrent :: [r] -> l -> m l
    computeCurrent []     = return
    computeCurrent (r:rs) = reapplyRef cfg r >=> computeCurrent rs

-- | Reconstruct ledger DB from a list of checkpoints
reconstructFrom :: forall m l r b. (ApplyBlock l b, ResolvesBlocks r b m)
                => LedgerCfg l
                -> LedgerDbParams
                -> ChainSummary l r
                -> StrictSeq (Checkpoint l r)
                -> m (LedgerDB l r)
reconstructFrom cfg params anchor blocks =
    reconstruct <$> ledgerAfter cfg anchor blocks
  where
    reconstruct :: l -> LedgerDB l r
    reconstruct current = LedgerDB {
          ledgerDbCurrent = current
        , ledgerDbBlocks  = blocks
        , ledgerDbParams  = params
        , ledgerDbAnchor  = anchor
        }

-- | Generalization of rollback using a function on the checkpoints
rollbackTo :: (ApplyBlock l b, ResolvesBlocks r b m)
           => LedgerCfg l
           -> (   ChainSummary l r
               -> StrictSeq (Checkpoint l r)
               -> Maybe (StrictSeq (Checkpoint l r))
              )
           -> LedgerDB l r
           -> m (Maybe (LedgerDB l r))
rollbackTo cfg f (LedgerDB _current blocks anchor params) =
    case f anchor blocks of
      Nothing      -> return Nothing
      Just blocks' -> Just <$> reconstructFrom cfg params anchor blocks'

-- | Rollback
--
-- Returns 'Nothing' if maximum rollback is exceeded.
rollback :: forall m l r b. (ApplyBlock l b, ResolvesBlocks r b m)
         => LedgerCfg l
         -> Word64
         -> LedgerDB l r
         -> m (Maybe (LedgerDB l r))
rollback _   0 db = return $ Just db
rollback cfg n db = rollbackTo cfg (\_anchor -> go) db
  where
    go :: StrictSeq (Checkpoint l r) -> Maybe (StrictSeq (Checkpoint l r))
    go blocks =
        if Seq.length blocks >= fromIntegral n
          then Just $ Seq.take (Seq.length blocks - fromIntegral n) blocks
          else Nothing

{-------------------------------------------------------------------------------
  Get past ledger states
-------------------------------------------------------------------------------}

-- | Get past ledger state
--
-- This may have to re-apply blocks, and hence read from disk.
ledgerDbPast :: forall m l r b. (ApplyBlock l b, ResolvesBlocks r b m, Eq r)
             => LedgerCfg l
             -> WithOrigin r
             -> LedgerDB l r
             -> m (Maybe l)
ledgerDbPast cfg tip db
  | ledgerDbTip db == tip = return $ Just (ledgerDbCurrent db)
  | otherwise             = fmap ledgerDbCurrent <$> rollbackTo cfg go db
  where
    go :: ChainSummary l r
       -> StrictSeq (Checkpoint l r)
       -> Maybe (StrictSeq (Checkpoint l r))
    go anchor blocks =
        case blocks' of
          Empty | csTip anchor /= tip -> Nothing
          _otherwise                  -> Just blocks'
      where
        blocks' :: StrictSeq (Checkpoint l r)
        blocks' = Seq.dropWhileR (\cp -> At (cpBlock cp) /= tip) blocks

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
ledgerDbSwitch :: (ApplyBlock l b, ResolvesBlocks r b m, c)
               => LedgerCfg l
               -> Word64          -- ^ How many blocks to roll back
               -> [Ap m l r b c]  -- ^ New blocks to apply
               -> LedgerDB l r
               -> m (Either ExceededRollback (LedgerDB l r))
ledgerDbSwitch cfg numRollbacks newBlocks db = do
    mRolledBack <- rollback cfg numRollbacks db
    case mRolledBack of
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

instance ( IsLedger l
           -- Required superclass constraints of 'IsLedger'
         , Show               r
         , Eq                 r
         , NoUnexpectedThunks r
         ) => IsLedger (LedgerDB l r) where
  type LedgerCfg (LedgerDB l r) = LedgerCfg l
  type LedgerErr (LedgerDB l r) = LedgerErr l

  applyChainTick cfg slot db =
      TickedLedgerState slot $ db { ledgerDbCurrent = l' }
    where
      TickedLedgerState _slot l' = applyChainTick cfg slot (ledgerDbCurrent db)

instance ApplyBlock l blk => ApplyBlock (LedgerDB l (RealPoint blk)) blk where
  applyLedgerBlock cfg blk (TickedLedgerState slot db) = do
      fmap (\current' -> pushLedgerState current' (blockRealPoint blk) db) $
        applyLedgerBlock cfg blk $
          TickedLedgerState slot (ledgerDbCurrent db)
  reapplyLedgerBlock cfg blk (TickedLedgerState slot db) =
      (\current' -> pushLedgerState current' (blockRealPoint blk) db) $
        reapplyLedgerBlock cfg blk $
          TickedLedgerState slot (ledgerDbCurrent db)
  ledgerTipPoint =
      ledgerTipPoint . ledgerDbCurrent

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

ledgerDbPast' :: forall l b. (ApplyBlock l b, Eq b)
              => LedgerCfg l -> WithOrigin b -> LedgerDB l b -> Maybe l
ledgerDbPast' cfg tip = triviallyResolve (Proxy @b) . ledgerDbPast cfg tip

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

{-------------------------------------------------------------------------------
  Auxiliary
-------------------------------------------------------------------------------}

safeMod :: (Integral a, HasCallStack) => a -> a -> a
safeMod _ 0 = error "safeMod: division by zero"
safeMod x y = x `Prelude.mod` y

safeDiv :: (Eq a, Fractional a, HasCallStack) => a -> a -> a
safeDiv _ 0 = error "safeDiv: division by zero"
safeDiv x y = x Prelude./ y
