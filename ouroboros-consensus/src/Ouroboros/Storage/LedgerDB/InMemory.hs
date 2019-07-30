{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}

module Ouroboros.Storage.LedgerDB.InMemory (
     -- * Chain summary
     ChainSummary(..)
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
   , ledgerDbIsSaturated
   , ledgerDbTail
     -- ** Result types
   , PushManyResult (..)
   , SwitchResult (..)
   , pushManyResultToEither
   , switchResultToEither
     -- ** Updates
   , Apply(..)
   , Err
   , RefOrVal(..)
   , ledgerDbPush
   , ledgerDbReapply
   , ledgerDbSwitch
     -- * Low-level rollback support (primarily for property testing)
   , Suffix -- opaque
   , toSuffix
   , fromSuffix'
     -- * Pure wrappers (primarily for property testing)
   , ledgerDbComputeCurrent'
   , ledgerDbPush'
   , ledgerDbPushMany'
   , ledgerDbSwitch'
     -- * Shape of the snapshots
   , Shape(..)
   , memPolicyShape
   , snapshotsShape
     -- * Serialisation
   , encodeChainSummary
   , decodeChainSummary
     -- * Demo
   , demo
   ) where

import           Codec.Serialise (Serialise (..))
import           Codec.Serialise.Decoding (Decoder)
import qualified Codec.Serialise.Decoding as Dec
import           Codec.Serialise.Encoding (Encoding)
import qualified Codec.Serialise.Encoding as Enc
import           Control.Exception (Exception, throw)
import           Control.Monad.Except (Except, ExceptT (..), lift, runExcept,
                     runExceptT, throwError)
import           Data.Functor.Identity
import           Data.Typeable (Typeable)
import           Data.Void
import           Data.Word
import           GHC.Generics (Generic)
import           GHC.Stack (CallStack, HasCallStack, callStack)

import           Ouroboros.Consensus.Util

import           Ouroboros.Storage.Common
import           Ouroboros.Storage.LedgerDB.Conf
import           Ouroboros.Storage.LedgerDB.MemPolicy
import           Ouroboros.Storage.LedgerDB.Offsets

{-------------------------------------------------------------------------------
  Block info
-------------------------------------------------------------------------------}

-- | Has a block previously been applied?
--
-- In addition to the term level value, we also reflect at the type level
-- whether the block has been previously applied. This allows us to be more
-- precise when we apply blocks: re-applying previously applied blocks cannot
-- result in ledger errors.
data Apply :: Bool -> * where
    -- | This block was previously applied
    --
    -- If we reapply a previously applied block, we /must/ reapply it to the
    -- very same state. This means that we cannot have any errors.
    --
    -- This constructor is polymorphic: this allows us to be conservative at
    -- the type level ("this may result in a ledger error") whilst still marking
    -- a block as previously applied ("don't bother checking for errors"). This
    -- is useful for example when we have a bunch of blocks, some of which have
    -- been previously applied and some of which have not. Applying all of them
    -- could definitely result in a ledger error, even if we skip checks for
    -- some of them.
    Reapply :: Apply pa

    -- | Not previously applied
    --
    -- All checks must be performed
    Apply :: Apply 'False

-- | Error we get from applying a block
--
-- If the block was previously applied, we can't get any errors.
type family Err (ap :: Bool) (e :: *) :: * where
  Err 'True  e = Void
  Err 'False e = e

-- | Pass a block by value or by reference
data RefOrVal r b = Ref r | Val r b

ref :: RefOrVal r b -> r
ref (Ref r  ) = r
ref (Val r _) = r

{-------------------------------------------------------------------------------
  Chain summary
-------------------------------------------------------------------------------}

-- | Summary of the chain at a particular point in time
data ChainSummary l r = ChainSummary {
      -- | The tip of the chain
      csTip    :: !(Tip r)

      -- | Length of the chain
    , csLength :: !Word64

      -- | Ledger state
    , csLedger :: !l
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
--
-- We maintain the invariant that the shape of the 'LedgerDB' (see 'Shape')
-- is always equal to the shape required by the memory policy. This implies that
--
-- * we always store the most recent snapshot (provided that the memory policy
--   requires a ledger state at offset 0)
-- * as well as one at offset @k@ (so that we can roll back at most @k@ blocks)
data LedgerDB l r =
    -- | Apply block @r@ and take a snapshot of the resulting ledger state @l@
    Snap !r !l !(LedgerDB l r)

    -- | Apply block @r@ without taking a snapshot
  | Skip !r !(LedgerDB l r)

    -- | The tail of the chain that is outside the scope of the snapshots
    --
    -- We record a summary of the chain tail /at this point/ as well as the
    -- missing snapshots, if any.
  | Tail !Missing !(ChainSummary l r)
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

applyBlock :: forall m l r b e ap. Monad m
           => LedgerDbConf m l r b e
           -> (Apply ap, RefOrVal r b)
           -> l -> ExceptT (Err ap e) m l
applyBlock LedgerDbConf{..} (pa, rb) l = ExceptT $
    case rb of
      Ref  r   -> apply pa <$> ldbConfResolve r
      Val _r b -> return $ apply pa b
  where
    apply :: Apply ap -> b -> Either (Err ap e) l
    apply Apply   b =         ldbConfApply   b l
    apply Reapply b = Right $ ldbConfReapply b l

reapplyBlock :: forall m l r b e. Monad m
             => LedgerDbConf m l r b e -> RefOrVal r b -> l -> m l
reapplyBlock cfg b = fmap mustBeRight
                   . runExceptT
                   . applyBlock cfg (Reapply @'True, b)

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
ledgerDbTip :: LedgerDB l r -> Tip r
ledgerDbTip (Snap r _ _) = Tip r
ledgerDbTip (Skip r   _) = Tip r
ledgerDbTip (Tail _ cs)  = csTip cs

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
  Result types
-------------------------------------------------------------------------------}

-- | The result of 'ledgerDbPushMany', i.e. when validating a number of new
-- blocks.
--
-- About the boolean type index: if all blocks given to 'ledgerDbPushMany' are
-- 'Reapply', and so @ap ~ 'True@, then the result /must/ be 'ValidBlocks'.
data PushManyResult e l r :: Bool -> * where
  -- | A block is invalid. We return the 'LedgerDB' corresponding to the block
  -- before it. We also return the validation error and include a reference to
  -- the invalid block.
  InvalidBlock :: e -> r -> LedgerDB l r -> PushManyResult e l r 'False

  -- | All blocks were valid, a 'LedgerDB' corresponding to the last valid
  -- block is returned.
  ValidBlocks ::            LedgerDB l r -> PushManyResult e l r ap

-- | The result of 'ledgerDbSwitch', i.e. when validating a fork of the
-- current chain.
--
-- First, we roll back @m@ blocks and then apply @n@ new blocks. It is
-- important that @n >= m@, because we cannot return a valid 'LedgerDB' that
-- is \"older\" (corresponds to an older block) than the given one.
--
-- We divide the new blocks in the /prefix/, the first @m@ new blocks, and the
-- /suffix/, the @n-m@ last blocks, which are pushed using 'ledgerDbPushMany'.
data SwitchResult e l r :: Bool -> * where
  -- | When a block in the prefix is invalid, we cannot return a valid
  -- 'LedgerDB'. We return the validation error and include a reference to the
  -- invalid block.
  InvalidBlockInPrefix :: e -> r                  -> SwitchResult e l r 'False

  -- | The result of pushing the blocks in the suffix with 'ledgerDbPushMany'.
  PushSuffix           :: PushManyResult e l r ap -> SwitchResult e l r ap

pushManyResultToEither :: PushManyResult e l r ap
                       -> Either (Err ap e) (LedgerDB l r)
pushManyResultToEither = \case
    InvalidBlock e _ _ -> Left e
    ValidBlocks      l -> Right l

switchResultToEither :: SwitchResult e l r ap
                     -> Either (Err ap e) (LedgerDB l r)
switchResultToEither = \case
    InvalidBlockInPrefix e _ -> Left e
    PushSuffix pm            -> pushManyResultToEither pm

{-------------------------------------------------------------------------------
  Updates
-------------------------------------------------------------------------------}

ledgerDbPush :: forall m l r b e ap. Monad m
              => LedgerDbConf m l r b e
              -> (Apply ap, RefOrVal r b)
              -> LedgerDB l r
              -> m (Either (Err ap e) (LedgerDB l r))
ledgerDbPush cfg (pa, new) ldb = runExceptT $
    case ldb of
      Snap r l ss -> Snap (ref new) <$> appNew l <*> reapp r ss
      Skip r   ss -> Skip (ref new) <$>              reapp r ss
      Tail os cs
        | []    <- os -> Tail [] <$> goCS cs
      -- Create new snapshots when we need to
      -- For the very last snapshot we use the 'End' constructor itself.
        | [0]   <- os -> Tail [] <$> goCS cs
        | 0:os' <- os -> Snap (ref new) <$> appNew l <*> pure (Tail      os'  cs)
        | o:os' <- os -> Skip (ref new) <$>              pure (Tail (o-1:os') cs)
        where
          l = csLedger cs
  where
    appNew  = applyBlock cfg (pa, new)
    reapp r = lift . ledgerDbReapply cfg (Ref r)

    goCS :: ChainSummary l r -> ExceptT (Err ap e) m (ChainSummary l r)
    goCS ChainSummary{..} = do
        csLedger' <- appNew csLedger
        return ChainSummary{
            csTip    = Tip (ref new)
          , csLength = csLength + 1
          , csLedger = csLedger'
          }

ledgerDbReapply :: Monad m
                => LedgerDbConf m l r b e
                -> RefOrVal r b -> LedgerDB l r -> m (LedgerDB l r)
ledgerDbReapply cfg b = fmap mustBeRight . ledgerDbPush cfg (Reapply @'True, b)

-- | Push a bunch of blocks (oldest first)
ledgerDbPushMany :: forall ap m l r b e. Monad m
                 => LedgerDbConf m l r b e
                 -> [(Apply ap, RefOrVal r b)]
                 -> LedgerDB l r
                 -> m (PushManyResult e l r ap)
ledgerDbPushMany cfg = flip go
  where
    go :: LedgerDB l r
       -> [(Apply ap, RefOrVal r b)]
       -> m (PushManyResult e l r ap)
    go l = \case
      [] -> return $ ValidBlocks l
      b@(ap, rov):bs -> case ap of
        Apply   -> ledgerDbPush cfg b l >>= \case
          Left  e  -> return $ InvalidBlock e (ref rov) l
          Right l' -> go l' bs
        -- We have Reapply @'False, so 'ledgerDbPush' returns a non-Void
        -- error, but we know it can't happen, so just use Reapply @'True.
        Reapply -> ledgerDbPush cfg (Reapply @'True, rov) l >>= \case
          Left  e  -> absurd e
          Right l' -> go l' bs

-- | Switch to a fork
--
-- PRE: Must have at least as many new blocks as we are rolling back.
ledgerDbSwitch :: ( Monad m
                  , HasCallStack
                  , Show l
                  , Show r
                  , Typeable l
                  , Typeable r
                  )
               => LedgerDbConf m l r b e
               -> Word64                      -- ^ How many blocks to roll back
               -> [(Apply ap, RefOrVal r b)]  -- ^ New blocks to apply
               -> LedgerDB l r
               -> m (SwitchResult e l r ap)
ledgerDbSwitch cfg numRollbacks newBlocks ss =
    case runExcept $ rollbackMany numRollbacks (toSuffix ss) of
      Right suffix -> fromSuffix cfg newBlocks suffix
      Left  err    -> throw $ InvalidRollback numRollbacks ss err callStack

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
                       => LedgerDbConf m l r b e -> LedgerDB l r -> m l
ledgerDbComputeCurrent cfg = go
  where
    go :: LedgerDB l r -> m l
    go (Snap _ l _)  = return l
    go (Skip r   ss) = go ss >>= reapplyBlock cfg (Ref r)
    go (Tail _ cs)   = return (csLedger cs)

fromSuffix :: forall m l r b e ap. (Monad m, HasCallStack)
           => LedgerDbConf m l r b e
           -> [(Apply ap, RefOrVal r b)]
           -> Suffix l r
           -> m (SwitchResult e l r ap)
fromSuffix cfg = \bs suffix -> do
    -- Here we /must/ call 'ledgerDbComputeCurrent', not 'ledgerDbGetCurrent':
    -- only if we are /very/ lucky would we roll back to a point where we happen
    -- to store a snapshot.
    l <- ledgerDbComputeCurrent cfg (suffixRemaining suffix)
    go bs suffix l
  where
    go :: [(Apply ap, RefOrVal r b)]
       -> Suffix l r -> l -> m (SwitchResult e l r ap)
    go bs (Suffix ss SNone) _ =
        PushSuffix <$> ledgerDbPushMany cfg bs ss
    go ((ap,b):bs) (Suffix ss (SSnap m)) l =
        applyBlock' ap b l
          (\e -> return $ InvalidBlockInPrefix e (ref b))
          (\l' -> go bs (Suffix (Snap (ref b) l' ss) m) l')
    go ((ap,b):bs) (Suffix ss (SSkip m)) l =
        applyBlock' ap b l
          (\e  -> return $ InvalidBlockInPrefix e (ref b))
          (\l' -> go bs (Suffix (Skip (ref b) ss) m) l')
    go [] (Suffix _  (SSnap _)) _ =
        error "fromSuffix: too few blocks"
    go [] (Suffix _  (SSkip _)) _ =
        error "fromSuffix: too few blocks"

    applyBlock' :: forall a. Apply ap -> RefOrVal r b -> l
                -> (ap ~ 'False => e -> m a)
                -> (l -> m a)
                -> m a
    applyBlock' ap b l kErr kOk = case ap of
      Apply   -> runExceptT (applyBlock cfg (ap, b) l) >>= \case
        Left  e  -> kErr e
        Right l' -> kOk l'
      Reapply -> runExceptT (applyBlock cfg (Reapply @'True, b) l) >>= \case
        Left  e  -> absurd e
        Right l' -> kOk l'

rollback :: Suffix l r -> Except String (Suffix l r)
rollback (Suffix (Snap _ _ ss) missing) = return $ Suffix ss (SSnap missing)
rollback (Suffix (Skip _   ss) missing) = return $ Suffix ss (SSkip missing)
rollback (Suffix (Tail _ _)    _) = throwError "rollback: cannot rollback past end"

rollbackMany :: Word64 -> Suffix l r -> Except String (Suffix l r)
rollbackMany = nTimesM rollback

-- | Invalid rollback exception
--
-- If this is ever thrown it indicates a bug in the code
data InvalidRollbackException l r =
    InvalidRollback {
        -- | Number of blocks requested to rollback
        invalidRollbackCount :: Word64

        -- | The ledger DB at the time of the rollback
      , invalidRollbackDb    :: LedgerDB l r

        -- | Error message
      , invalidRollbackErr   :: String

        -- | Callstack
      , invalidRollbackStack :: CallStack
      }
  deriving (Show)

instance (Typeable l, Typeable r, Show r, Show l)
      => Exception (InvalidRollbackException l r)

{-------------------------------------------------------------------------------
  Pure variations (primarily for testing)
-------------------------------------------------------------------------------}

fromEither :: Identity (Either Void l) -> l
fromEither = mustBeRight . runIdentity

fromPushManyResult :: Identity (PushManyResult Void l b 'False) -> LedgerDB l b
fromPushManyResult = mustBeRight . pushManyResultToEither . runIdentity

fromSwitchResult :: Identity (SwitchResult Void l b 'False) -> LedgerDB l b
fromSwitchResult = mustBeRight . switchResultToEither . runIdentity

pureBlock :: b -> (Apply 'False, RefOrVal b b)
pureBlock b = (Apply, Val b b)

ledgerDbComputeCurrent' :: PureLedgerDbConf l b -> LedgerDB l b -> l
ledgerDbComputeCurrent' cfg = runIdentity . ledgerDbComputeCurrent cfg

ledgerDbPush' :: PureLedgerDbConf l b -> b -> LedgerDB l b -> LedgerDB l b
ledgerDbPush' cfg b = fromEither . ledgerDbPush cfg (pureBlock b)

ledgerDbPushMany' :: PureLedgerDbConf l b -> [b] -> LedgerDB l b -> LedgerDB l b
ledgerDbPushMany' cfg bs = fromPushManyResult . ledgerDbPushMany cfg (map pureBlock bs)

ledgerDbSwitch' :: (HasCallStack, Show l, Show b, Typeable l, Typeable b)
                => PureLedgerDbConf l b
                -> Word64 -> [b] -> LedgerDB l b -> LedgerDB l b
ledgerDbSwitch' cfg n bs = fromSwitchResult . ledgerDbSwitch cfg n (map pureBlock bs)

fromSuffix' :: PureLedgerDbConf l b -> [b] -> Suffix l b -> LedgerDB l b
fromSuffix' cfg bs = fromSwitchResult . fromSuffix cfg (map pureBlock bs)

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
-------------------------------------------------------------------------------}

instance (Serialise l, Serialise r) => Serialise (ChainSummary l r) where
  encode = encodeChainSummary encode encode
  decode = decodeChainSummary decode decode

encodeChainSummary :: (l -> Encoding)
                   -> (r -> Encoding)
                   -> ChainSummary l r -> Encoding
encodeChainSummary encodeLedger encodeRef ChainSummary{..} = mconcat [
      Enc.encodeListLen 3
    , encodeTip encodeRef csTip
    , Enc.encodeWord64 csLength
    , encodeLedger csLedger
    ]

decodeChainSummary :: (forall s. Decoder s l)
                   -> (forall s. Decoder s r)
                   -> forall s. Decoder s (ChainSummary l r)
decodeChainSummary decodeLedger decodeRef = do
    Dec.decodeListLenOf 3
    csTip    <- decodeTip decodeRef
    csLength <- Dec.decodeWord64
    csLedger <- decodeLedger
    return ChainSummary{..}

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
      ldbConfGenesis = Identity $ DL ('a', 0)
    , ldbConfResolve = \(DR b) -> Identity (DB b)
    , ldbConfApply   = \(DB r@(_, n)) (DL (_, l)) ->
        if n > l then Right $ DL r
                 else Left  $ DE (n, l)
    , ldbConfReapply = \(DB r@(_, n)) (DL (_, l)) ->
        if n > l then DL r
                 else error "ledgerDbReapply: block applied in wrong state"
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
        let blockInfos = [ (Apply, Val (DR ('b', n-1)) (DB ('b', n-1)))
                         , (Apply, Val (DR ('b', n-0)) (DB ('b', n-0)))
                         ]
            Identity (PushSuffix (ValidBlocks db')) = ledgerDbSwitch demoConf 1 blockInfos db
        in [db']
      else
        let blockInfo = (Apply, Val (DR ('a', n)) (DB ('a', n)))
            Identity (Right db') = ledgerDbPush demoConf blockInfo db
        in db' : go (n + 1) m db'
