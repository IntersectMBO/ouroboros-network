{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}

module Ouroboros.Storage.LedgerDB.InMemory (
     -- * Chain summary
     Tip(..)
   , tipIsGenesis
   , ChainSummary(..)
   , genesisChainSummary
     -- * LedgerDB proper
   , LedgerDB
   , ledgerDbFromChain
   , ledgerDbFromGenesis
     -- ** Queries
   , ledgerDbChainLength
   , ledgerDbToList
   , ledgerDbMaxRollback
   , ledgerDbCurrent
   , ledgerDbTip
   , ledgerDbIsComplete
   , ledgerDbIsSaturated
   , ledgerDbTail
     -- ** Updates
   , BlockInfo(..)
   , ledgerDbPush
   , ledgerDbPushMany
   , ledgerDbSwitch
     -- * Rollback
   , Suffix -- opaque
   , toSuffix
   , fromSuffix
   , rollback
   , rollbackMany
     -- * Pure wrappers (primarily for property testing)
   , ledgerDbComputeCurrent'
   , ledgerDbPush'
   , ledgerDbPushMany'
   , ledgerDbSwitch'
   , fromSuffix'
     -- * Shape of the snapshots
   , Shape(..)
   , memPolicyShape
   , snapshotsShape
     -- * Demo
   , demo
   ) where

import           Codec.Serialise (Serialise)
import           Control.Monad.Except
import           Data.Functor.Identity
import           Data.Void
import           Data.Word
import           GHC.Generics (Generic)

import           Ouroboros.Consensus.Util

import           Ouroboros.Storage.LedgerDB.Conf
import           Ouroboros.Storage.LedgerDB.MemPolicy
import           Ouroboros.Storage.LedgerDB.Offsets

{-------------------------------------------------------------------------------
  Block info
-------------------------------------------------------------------------------}

data BlockInfo r b =
    -- | Block passed by reference
    BlockRef PrevApplied r

    -- | Block passed by value
    --
    -- For convenience we also require the reference to this block
  | BlockVal PrevApplied (r, b)

{-------------------------------------------------------------------------------
  Chain summary
-------------------------------------------------------------------------------}

-- | Tip of the chain
data Tip r = Tip r | TipGen
  deriving (Show, Eq, Generic)

tipIsGenesis :: Tip r -> Bool
tipIsGenesis TipGen  = True
tipIsGenesis (Tip _) = False

-- | Summary of the chain at a particular point in time
data ChainSummary l r = ChainSummary {
      -- | The tip of the chain
      csTip    :: Tip r

      -- | Length of the chain
    , csLength :: Word64

      -- | Ledger state
    , csLedger :: l
    }
  deriving (Show, Eq, Generic)

genesisChainSummary :: l -> ChainSummary l r
genesisChainSummary l = ChainSummary TipGen 0 l

{-------------------------------------------------------------------------------
  LedgerDB proper
-------------------------------------------------------------------------------}

-- | Ledger snapshots
--
-- In addition to the ledger snapshots @l@ themselves we also store references
-- @r@ to the blocks that led to those snapshots. This is important when we
-- need to recompute the snapshots (in particular for rollback).
data LedgerDB l r =
    -- | Apply block @r@ and take a snapshot of the resulting ledger state @l@
    Snap r l (LedgerDB l r)

    -- | Apply block @r@ without taking a snapshot
  | Skip r (LedgerDB l r)

    -- | The tail of the chain that is outside the scope of the snapshots
    --
    -- We record a summary of the chain tail /at this point/ as well as the
    -- missing snapshots, if any.
  | Tail Missing (ChainSummary l r)
  deriving (Show, Eq)

-- | Are we still missing some snapshots?
--
-- This is represented as a list of skips:
--
-- * If the list is empty, all snapshots required by the policy are available.
-- * If the list starts with @n@, the current oldest ledger state will become a
--   snapshot after @n@ blocks have been applied.
type Missing = [Word64]

-- | Construct snapshots when we are in the genesis ledger state (no blocks)
ledgerDbFromChain :: MemPolicy -> ChainSummary l r -> LedgerDB l r
ledgerDbFromChain = Tail . memPolicyToSkips

ledgerDbFromGenesis :: MemPolicy -> l -> LedgerDB l r
ledgerDbFromGenesis policy = ledgerDbFromChain policy . genesisChainSummary

{-------------------------------------------------------------------------------
  Internal: derived functions in terms of 'BlockInfo'
-------------------------------------------------------------------------------}

-- | Block that we've seen before
blockOld :: r -> BlockInfo r b
blockOld = BlockRef PrevApplied

blockRef :: BlockInfo r b -> r
blockRef (BlockRef _ r)      = r
blockRef (BlockVal _ (r, _)) = r

applyBlock :: Monad m
           => LedgerDbConf m l r b e
           -> BlockInfo r b -> l -> m (Either e l)
applyBlock LedgerDbConf{..} (BlockVal pa (_r, b)) l = do
    return $ ledgerDbApply pa b l
applyBlock LedgerDbConf{..} (BlockRef pa r) l = do
    (\b -> ledgerDbApply pa b l) <$> ledgerDbResolve r

snap :: BlockInfo r b -> l -> LedgerDB l r -> LedgerDB l r
snap = Snap . blockRef

skip :: BlockInfo r b -> LedgerDB l r -> LedgerDB l r
skip = Skip . blockRef

{-------------------------------------------------------------------------------
  Queries
-------------------------------------------------------------------------------}

-- | Total length of the chain (in terms of number of blocks)
ledgerDbChainLength :: LedgerDB l r -> Word64
ledgerDbChainLength = go 0
  where
    go :: Word64 -> LedgerDB l r -> Word64
    go !acc (Tail _ cs)   = acc + csLength cs
    go !acc (Snap _ _ ss) = go (acc + 1) ss
    go !acc (Skip _   ss) = go (acc + 1) ss

-- | All snapshots along with their offsets from the tip of the chain
ledgerDbToList :: LedgerDB l r -> Offsets l
ledgerDbToList = offsetsFromPairs . go 0
  where
    go :: Word64 -> LedgerDB l r -> [(Word64, l)]
    go offset (Snap _ l ss) = (offset, l) : go (offset + 1) ss
    go offset (Skip _   ss) =               go (offset + 1) ss
    go offset (Tail _ cs)   = [(offset, csLedger cs)]

-- | How many blocks can be currently roll back?
--
-- If @ss@ is a 'LedgerDB' with @n@ blocks applied, we have
--
-- > ledgerDbMaxRollback ss == min (memPolicyMaxRollback policy) n
ledgerDbMaxRollback :: LedgerDB l r -> Word64
ledgerDbMaxRollback = last . offsetsDropValues . ledgerDbToList

-- | Current snapshot, assuming mem policy specifies it must be recorded
ledgerDbCurrent :: LedgerDB l r -> l
ledgerDbCurrent (Snap _ l _) = l
ledgerDbCurrent (Skip   _ _) = error "ledgerDbCurrent: not stored"
ledgerDbCurrent (Tail _ cs)  = csLedger cs

-- | Reference to the block at the tip of the chain
--
-- Returns 'Nothing' if genesis
ledgerDbTip :: LedgerDB l r -> Tip r
ledgerDbTip (Snap r _ _) = Tip r
ledgerDbTip (Skip r   _) = Tip r
ledgerDbTip (Tail _ cs)  = csTip cs

-- | Is the ledger DB complete?
--
-- This is 'False' if the ledger DB is not saturated, /unless/ this is due to
-- being close to genesis. In other words, the ledger DB is considered complete
-- when it supports the maximum rollback required (if we are close to genesis
-- the maximum required rollback can't go past genesis).
ledgerDbIsComplete :: LedgerDB l r -> Bool
ledgerDbIsComplete (Snap _ _ ss)   = ledgerDbIsComplete ss
ledgerDbIsComplete (Skip _   ss)   = ledgerDbIsComplete ss
ledgerDbIsComplete (Tail []    _)  = True
ledgerDbIsComplete (Tail [0]   _)  = True
ledgerDbIsComplete (Tail (_:_) cs) = tipIsGenesis (csTip cs)

-- | Have all snapshots been filled?
--
-- TODO: Here and elsewhere it would be much nicer to avoid this @Tail [0]@
-- special case. There is redundancy in the representation. We should
-- reconsider this.
ledgerDbIsSaturated :: LedgerDB l r -> Bool
ledgerDbIsSaturated (Snap _ _ ss) = ledgerDbIsSaturated ss
ledgerDbIsSaturated (Skip _   ss) = ledgerDbIsSaturated ss
ledgerDbIsSaturated (Tail []  _)  = True
ledgerDbIsSaturated (Tail [0] _)  = True
ledgerDbIsSaturated (Tail _   _)  = False

-- | The tail of the DB (oldest snapshot)
ledgerDbTail :: LedgerDB l r -> ChainSummary l r
ledgerDbTail (Snap _ _ ss) = ledgerDbTail ss
ledgerDbTail (Skip _   ss) = ledgerDbTail ss
ledgerDbTail (Tail _ cs)   = cs

{-------------------------------------------------------------------------------
  Updates
-------------------------------------------------------------------------------}

-- | Push a block
ledgerDbPush :: forall m l r b e. Monad m
             => LedgerDbConf m l r b e
             -> BlockInfo r b
             -> LedgerDB l r
             -> m (Either e (LedgerDB l r))
ledgerDbPush cfg = runExceptT .: go
  where
    app = ExceptT .: applyBlock cfg

    go :: BlockInfo r b -> LedgerDB l r -> ExceptT e m (LedgerDB l r)
    go b' (Snap r l ss) = snap b' <$> app b' l <*> go (blockOld r) ss
    go b' (Skip r   ss) = skip b' <$>              go (blockOld r) ss
    go b' (Tail os cs)
      | []    <- os     = Tail [] <$> goCS b' cs
      -- Create new snapshots when we need to
      -- For the very last snapshot we use the 'End' constructor itself.
      | [0]   <- os     = Tail [] <$> goCS b' cs
      | 0:os' <- os     = snap b' <$> app b' l <*> pure (Tail      os'  cs)
      | o:os' <- os     = skip b' <$>              pure (Tail (o-1:os') cs)
      where
        l = csLedger cs

    goCS :: BlockInfo r b -> ChainSummary l r -> ExceptT e m (ChainSummary l r)
    goCS b' ChainSummary{..} = do
        csLedger' <- app b' csLedger
        return ChainSummary{
            csTip    = Tip (blockRef b')
          , csLength = csLength + 1
          , csLedger = csLedger'
          }

-- | Push a bunch of blocks (oldest first)
ledgerDbPushMany :: Monad m
                 => LedgerDbConf m l r b e
                 -> [BlockInfo r b]
                 -> LedgerDB l r
                 -> m (Either e (LedgerDB l r))
ledgerDbPushMany conf = runExceptT .: repeatedlyM (ExceptT .: ledgerDbPush conf)

-- | Switch to a fork
--
-- PRE: Must have at least as many new blocks as we are rolling back.
ledgerDbSwitch :: forall m l r b e. Monad m
               => LedgerDbConf m l r b e
               -> Word64              -- ^ How many blocks to roll back
               -> [BlockInfo r b]   -- ^ New blocks to apply
               -> LedgerDB l r
               -> m (Either e (LedgerDB l r))
ledgerDbSwitch cfg numRollbacks newBlocks ss =
      fromSuffix cfg newBlocks
    $ rollbackMany numRollbacks (toSuffix ss)

{-------------------------------------------------------------------------------
  Internal: implementation of rollback
-------------------------------------------------------------------------------}

-- | Suffix of 'LedgerDB' obtained by stripping some recent block
data Suffix l r = Suffix {
    -- | The remaining 'LedgerDB'
    suffixRemaining :: LedgerDB l r

    -- | The stuff we stripped off (old to new)
  , suffixStripped  :: Stripped
  }

data Stripped =
      -- | Nothing missing anymore; the prefix is complete
      SNone

      -- | Stripped off a 'Rec' constructor
    | SSnap Stripped

      -- | Stripped off a 'Skip' constructor
    | SSkip Stripped

-- | Trivial suffix (not actually stripping off any blocks)
toSuffix :: LedgerDB l r -> Suffix l r
toSuffix ss = Suffix { suffixRemaining = ss, suffixStripped = SNone }

-- | Compute current snapshot
--
-- This will be O(1) /provided/ that we store the most recent snapshot
-- (see also 'ledgerDbGetCurrent').
ledgerDbComputeCurrent :: forall m l r b e. Monad m
                       => LedgerDbConf m l r b e
                       -> LedgerDB l r
                       -> m (Either e l)
ledgerDbComputeCurrent cfg = runExceptT . go
  where
    go :: LedgerDB l r -> ExceptT e m l
    go (Snap _ l _)  = return l
    go (Skip r   ss) = go ss >>= ExceptT . applyBlock cfg (blockOld r)
    go (Tail _ cs)   = return (csLedger cs)

fromSuffix :: forall m l r b e. Monad m
           => LedgerDbConf m l r b e
           -> [BlockInfo r b]
           -> Suffix l r
           -> m (Either e (LedgerDB l r))
fromSuffix cfg = \bs suffix -> runExceptT $ do
    -- Here we /must/ call 'ledgerDbComputeCurrent', not 'ledgerDbGetCurrent':
    -- only if we are /very/ lucky would we roll back to a point where we happen
    -- to store a snapshot.
    l <- ExceptT $ ledgerDbComputeCurrent cfg (suffixRemaining suffix)
    go bs suffix l
  where
    go :: [BlockInfo r b] -> Suffix l r -> l -> ExceptT e m (LedgerDB l r)
    go bs     (Suffix ss SNone) _     = ExceptT $ ledgerDbPushMany cfg bs ss
    go (b:bs) (Suffix ss (SSnap m)) l = do l' <- ExceptT $ applyBlock cfg b l
                                           go bs (Suffix (snap b l' ss) m) l'
    go (b:bs) (Suffix ss (SSkip m)) l = do l' <- ExceptT $ applyBlock cfg b l
                                           go bs (Suffix (skip b ss) m) l'
    go []     (Suffix _  (SSnap _)) _ = error "fromSuffix: too few blocks"
    go []     (Suffix _  (SSkip _)) _ = error "fromSuffix: too few blocks"

rollback :: Suffix l r -> Suffix l r
rollback (Suffix (Snap _ _ ss) missing) = Suffix ss (SSnap missing)
rollback (Suffix (Skip _   ss) missing) = Suffix ss (SSkip missing)
rollback (Suffix (Tail _ _)    _) = error "rollback: cannot rollback past end"

rollbackMany :: Word64 -> Suffix l r -> Suffix l r
rollbackMany = nTimes rollback

{-------------------------------------------------------------------------------
  Pure variations (primarily for testing)
-------------------------------------------------------------------------------}

fromIdentity :: Identity (Either Void l) -> l
fromIdentity = mustBeRight . runIdentity

ledgerDbComputeCurrent' :: PureLedgerDbConf l b
                        -> LedgerDB l b -> l
ledgerDbComputeCurrent' f = fromIdentity . ledgerDbComputeCurrent f

ledgerDbPush' :: PureLedgerDbConf l b
              -> BlockInfo b b -> LedgerDB l b -> LedgerDB l b
ledgerDbPush' f = fromIdentity .: ledgerDbPush f

ledgerDbPushMany' :: PureLedgerDbConf l b
                  -> [BlockInfo b b] -> LedgerDB l b -> LedgerDB l b
ledgerDbPushMany' f = fromIdentity .: ledgerDbPushMany f

ledgerDbSwitch' :: PureLedgerDbConf l b
                -> Word64 -> [BlockInfo b b] -> LedgerDB l b -> LedgerDB l b
ledgerDbSwitch' f n = fromIdentity .: ledgerDbSwitch f n

fromSuffix' :: PureLedgerDbConf l b
            -> [BlockInfo b b] -> Suffix l b -> LedgerDB l b
fromSuffix' f = fromIdentity .: fromSuffix f

{-------------------------------------------------------------------------------
  Shape
-------------------------------------------------------------------------------}

-- | Shape of the snapshots
--
-- The shape of the snapshots tells us which snapshots are included and which
-- aren't. This is used only for testing and stating invariants: both
-- 'ledgerDbPush' and 'ledgerDbSwitch' must preserve this shape.
data Shape = Included Shape | Excluded Shape | Nil
  deriving (Show, Eq)

-- | The shape that the policy dictates
memPolicyShape :: MemPolicy -> Shape
memPolicyShape = go 0 . offsetsDropValues . memPolicyToOffsets
  where
    go :: Word64 -> [Word64] -> Shape
    go _   []     = Nil
    go cur (o:os)
      | cur == o  = Included $ go (cur + 1)    os
      | otherwise = Excluded $ go (cur + 1) (o:os)

-- | Compute the shape of the snapshots
snapshotsShape :: LedgerDB l r -> Shape
snapshotsShape (Snap _ _ ss) = Included (snapshotsShape ss)
snapshotsShape (Skip  _  ss) = Excluded (snapshotsShape ss)
snapshotsShape (Tail os _)   = go os
  where
    go :: Missing -> Shape
    go []      = Included Nil
    go [0]     = Included Nil
    go (0:os') = Included $ go os'
    go (o:os') = Excluded $ go (o-1:os')

{-------------------------------------------------------------------------------
  Serialisation

  TODO: We shouldn't use the 'Generic' instances here
-------------------------------------------------------------------------------}

instance (Serialise r)              => Serialise (Tip r)
instance (Serialise r, Serialise l) => Serialise (ChainSummary l r)

{-------------------------------------------------------------------------------
  Demo
-------------------------------------------------------------------------------}

type Branch     = Char
newtype DemoLedger = DL (Branch, Int) deriving (Show)
newtype DemoBlock  = DB (Branch, Int) deriving (Show)
newtype DemoRef    = DR (Branch, Int) deriving (Show)
newtype DemoErr    = DE (Int, Int)    deriving (Show)

demoConf :: LedgerDbConf Identity DemoLedger DemoRef DemoBlock DemoErr
demoConf = LedgerDbConf {
      ledgerDbGenesis = Identity $ DL ('a', 0)
    , ledgerDbApply   = \_prevApplied (DB r@(_, n)) (DL (_, l)) ->
                          if n > l then Right $ DL r
                                   else Left  $ DE (n, l)
    , ledgerDbResolve = \(DR b) -> Identity (DB b)
    }

demoPolicy :: MemPolicy
demoPolicy = memPolicyFromOffsets (offsetsWithoutValues [0, 2, 5])

db0 :: LedgerDB DemoLedger DemoRef
db0 = ledgerDbFromGenesis demoPolicy (DL ('a', 0))

demo :: [LedgerDB DemoLedger DemoRef]
demo = db0 : go 1 8 db0
  where
    go :: Int -> Int -> LedgerDB DemoLedger DemoRef -> [LedgerDB DemoLedger DemoRef]
    go n m db =
      if n > m then
        let blockInfos = [ BlockVal NotPrevApplied (DR ('b', n-1), DB ('b', n-1))
                         , BlockVal NotPrevApplied (DR ('b', n-0), DB ('b', n-0))
                         ]
            Identity (Right db') = ledgerDbSwitch demoConf 1 blockInfos db
        in [db']
      else
        let blockInfo = BlockVal NotPrevApplied (DR ('a', n), DB ('a', n))
            Identity (Right db') = ledgerDbPush demoConf blockInfo db
        in db' : go (n + 1) m db'


{-
ledgerDbSwitch :: forall m l r b e. Monad m
               => LedgerDbConf m l r b e
               -> Word64              -- ^ How many blocks to roll back
               -> [BlockInfo r b]   -- ^ New blocks to apply
               -> LedgerDB l r
               -> m (Either e (LedgerDB l r))
-}
