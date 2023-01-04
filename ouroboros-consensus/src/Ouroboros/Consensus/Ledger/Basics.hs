{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DefaultSignatures     #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DeriveTraversable     #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}

-- | Definition is 'IsLedger'
--
-- Normally this is imported from "Ouroboros.Consensus.Ledger.Abstract". We
-- pull this out to avoid circular module dependencies.
module Ouroboros.Consensus.Ledger.Basics (
    -- * GetTip
    GetTip (..)
  , getTipHash
  , getTipSlot
    -- * Ledger Events
  , LedgerResult (..)
  , VoidLedgerEvent
  , castLedgerResult
  , embedLedgerResult
  , pureLedgerResult
    -- * Definition of a ledger independent of a choice of block
  , IsLedger (..)
  , LedgerCfg
  , applyChainTick
    -- * Link block to its ledger
  , LedgerConfig
  , LedgerError
  , LedgerState
  , LedgerStateKind
  , TickedLedgerState
    -- ** Queries
  , DiskLedgerView (..)
  , FootprintL (..)
    -- ** Changelog
  , DbChangelog (..)
  , DbChangelogFlushPolicy (..)
  , DbChangelogState (..)
  , emptyDbChangeLog
  , extendDbChangelog
  , flushDbChangelog
  , prefixBackToAnchorDbChangelog
  , prefixDbChangelog
  , pruneVolatilePartDbChangelog
  , rollbackDbChangelog
  , youngestImmutableSlotDbChangelog
  ) where

import qualified Control.Exception as Exn
import           Data.Bifunctor (bimap)
import           Data.Kind (Type)
import           GHC.Generics (Generic)
import           GHC.Show (showCommaSpace, showSpace)
import           NoThunks.Class (NoThunks (..))

import           Cardano.Slotting.Slot (WithOrigin (..))

import           Ouroboros.Network.AnchoredSeq (AnchoredSeq)
import qualified Ouroboros.Network.AnchoredSeq as AS
import           Ouroboros.Network.Protocol.LocalStateQuery.Type
                     (FootprintL (..))

import           Ouroboros.Consensus.Block.Abstract
import           Ouroboros.Consensus.Config.SecurityParam (SecurityParam (..))
import           Ouroboros.Consensus.Ledger.Tables
import           Ouroboros.Consensus.Ticked
import           Ouroboros.Consensus.Util ((..:))

import           Ouroboros.Consensus.Ledger.Tables.Utils (forgetLedgerTables)
import           Ouroboros.Consensus.Storage.LedgerDB.HD.BackingStore
                     (RangeQuery)
import           Ouroboros.Consensus.Storage.LedgerDB.HD.DiffSeq

{-------------------------------------------------------------------------------
  Tip
-------------------------------------------------------------------------------}

class GetTip l where
  -- | Point of the most recently applied block
  --
  -- Should be 'genesisPoint' when no blocks have been applied yet
  getTip :: l -> Point l

type instance HeaderHash (Ticked  l)                   = HeaderHash l
type instance HeaderHash (Ticked1 (l :: k -> Type))    = HeaderHash l
type instance HeaderHash (Ticked1 (l :: k -> Type) mk) = HeaderHash l

getTipHash :: GetTip l => l -> ChainHash l
getTipHash = pointHash . getTip

getTipSlot :: GetTip l => l -> WithOrigin SlotNo
getTipSlot = pointSlot . getTip

{-------------------------------------------------------------------------------
  Events directly from the ledger
-------------------------------------------------------------------------------}

-- | A 'Data.Void.Void' isomorph for explicitly declaring that some ledger has
-- no events
data VoidLedgerEvent l

-- | The result of invoke a ledger function that does validation
--
-- Note: we do not instantiate 'Applicative' or 'Monad' for this type because
-- those interfaces would typically incur space leaks. We encourage you to
-- process the events each time you invoke a ledger function.
data LedgerResult l a = LedgerResult
  { lrEvents :: [AuxLedgerEvent l]
  , lrResult :: !a
  }
  deriving (Foldable, Functor, Traversable)

castLedgerResult ::
     (AuxLedgerEvent l ~ AuxLedgerEvent l')
  => LedgerResult l  a
  -> LedgerResult l' a
castLedgerResult (LedgerResult x0 x1) = LedgerResult x0 x1

embedLedgerResult ::
     (AuxLedgerEvent l -> AuxLedgerEvent l')
  -> LedgerResult l  a
  -> LedgerResult l' a
embedLedgerResult inj lr = lr{lrEvents = inj `map` lrEvents lr}

pureLedgerResult :: a -> LedgerResult l a
pureLedgerResult a = LedgerResult {
    lrEvents = mempty
  , lrResult = a
  }

{-------------------------------------------------------------------------------
  Definition of a ledger independent of a choice of block
-------------------------------------------------------------------------------}

-- | Static environment required for the ledger
type family LedgerCfg (l :: LedgerStateKind) :: Type

class ( -- Requirements on the ledger state itself
        ShowLedgerState                     l
--      , forall mk. IsMapKind mk                => Eq       (l mk)
--      , forall mk. (IsMapKind mk, Typeable mk) => NoThunks (l mk)
      , Eq       (l EmptyMK)
      , NoThunks (l EmptyMK)
      , Eq       (l DiffMK)
      , NoThunks (l DiffMK)
      , Eq       (l ValuesMK)
      , NoThunks (l ValuesMK)
      , NoThunks (l SeqDiffMK)
        -- Requirements on 'LedgerCfg'
      , NoThunks (LedgerCfg l)
        -- Requirements on 'LedgerErr'
      , Show     (LedgerErr l)
      , Eq       (LedgerErr l)
      , NoThunks (LedgerErr l)
        -- Get the tip
        --
        -- See comment for 'applyChainTickLedgerResult' about the tip of the
        -- ticked ledger.
      , forall mk. GetTip         (l mk)
      , forall mk. GetTip (Ticked1 l mk)
      , HeaderHash (l EmptyMK) ~ HeaderHash l
      , HeaderHash (l ValuesMK) ~ HeaderHash l
      , HeaderHash (l DiffMK) ~ HeaderHash l
      , HeaderHash (l TrackingMK) ~ HeaderHash l
      , NoThunks (LedgerTables l SeqDiffMK)
      , NoThunks (LedgerTables l ValuesMK) -- for TVars in in-memory backing store
      , StowableLedgerTables l
      ) => IsLedger (l :: LedgerStateKind) where
  -- | Errors that can arise when updating the ledger
  --
  -- This is defined here rather than in 'ApplyBlock', since the /type/ of
  -- these errors does not depend on the type of the block.
  type family LedgerErr l :: Type

  -- | Event emitted by the ledger
  --
  -- TODO we call this 'AuxLedgerEvent' to differentiate from 'LedgerEvent' in
  -- 'InspectLedger'. When that module is rewritten to make use of ledger
  -- derived events, we may rename this type.
  type family AuxLedgerEvent l :: Type

  -- | Apply "slot based" state transformations
  --
  -- When a block is applied to the ledger state, a number of things happen
  -- purely based on the slot number of that block. For example:
  --
  -- * In Byron, scheduled updates are applied, and the update system state is
  --   updated.
  -- * In Shelley, delegation state is updated (on epoch boundaries).
  --
  -- The consensus layer must be able to apply such a "chain tick" function,
  -- primarily when validating transactions in the mempool (which, conceptually,
  -- live in "some block in the future") or when extracting valid transactions
  -- from the mempool to insert into a new block to be produced.
  --
  -- This is not allowed to throw any errors. After all, if this could fail,
  -- it would mean a /previous/ block set up the ledger state in such a way
  -- that as soon as a certain slot was reached, /any/ block would be invalid.
  --
  -- Currently, ticking transformations don't require data from the ledger
  -- tables, therefore the input map kind is fixed to @EmptyMK@. As translating
  -- a @LedgerState@ between eras happens during the ticking operation on the
  -- @HardForkBlock@ instance, the return type has to allow for new tables to be
  -- populated in the translation thus it is set to @ValuesMK@. Currently this
  -- does happen in the Byron to Shelley translation where the UTxO map is
  -- populated.
  --
  -- PRECONDITION: The slot number must be strictly greater than the slot at
  -- the tip of the ledger (except for EBBs, obviously..).
  --
  -- NOTE: 'applyChainTickLedgerResult' should /not/ change the tip of the
  -- underlying ledger state, which should still refer to the most recent
  -- applied /block/. In other words, we should have
  --
  -- >    ledgerTipPoint (applyChainTick cfg slot st)
  -- > == ledgerTipPoint st
  --
  -- NOTE: The 'IsMapKind' constraint is here for the same reason it's on
  -- 'projectLedgerTables'
  applyChainTickLedgerResult ::
       LedgerCfg l
    -> SlotNo
    -> l EmptyMK
    -> LedgerResult l (Ticked1 l DiffMK)

-- | 'lrResult' after 'applyChainTickLedgerResult'
applyChainTick ::
     IsLedger l
  => LedgerCfg l
  -> SlotNo
  -> l EmptyMK
  -> Ticked1 l DiffMK
applyChainTick = lrResult ..: applyChainTickLedgerResult

{-------------------------------------------------------------------------------
  Link block to its ledger
-------------------------------------------------------------------------------}

-- | Ledger state associated with a block
data family LedgerState blk :: LedgerStateKind

type instance HeaderHash (LedgerState blk)    = HeaderHash blk
type instance HeaderHash (LedgerState blk mk) = HeaderHash blk

instance StandardHash blk => StandardHash (LedgerState blk)
instance StandardHash blk => StandardHash (LedgerState blk mk)

type LedgerConfig      blk    = LedgerCfg (LedgerState blk)
type LedgerError       blk    = LedgerErr (LedgerState blk)
type TickedLedgerState blk mk = Ticked1   (LedgerState blk) mk

{-------------------------------------------------------------------------------
  UTxO HD stubs
-------------------------------------------------------------------------------}

-- | Monadic functions used to query this this block type's 'LargeL' ledger
-- states, which typically involve accessing disk.
data DiskLedgerView m l =
    DiskLedgerView
      !(l EmptyMK)
      (LedgerTables l KeysMK -> m (LedgerTables l ValuesMK))
      (RangeQuery (LedgerTables l KeysMK) -> m (LedgerTables l ValuesMK))   -- TODO will be unacceptably coarse once we have multiple tables
      (m ())

{-------------------------------------------------------------------------------
  Changelog
-------------------------------------------------------------------------------}

-- |
--
-- INVARIANT: the head of 'changelogImmutableStates' is the anchor of
-- 'changelogVolatileStates'.
data DbChangelog l = DbChangelog {
    changelogDiffAnchor      :: !(WithOrigin SlotNo)
  , changelogDiffs           :: !(LedgerTables l SeqDiffMK)
  , changelogImmutableStates ::
      !(AnchoredSeq
          (WithOrigin SlotNo)
          (DbChangelogState l)
          (DbChangelogState l)
       )
  , changelogVolatileStates  ::
      !(AnchoredSeq
          (WithOrigin SlotNo)
          (DbChangelogState l)
          (DbChangelogState l)
       )
  }
  deriving (Generic)

deriving instance (Eq       (LedgerTables l SeqDiffMK), Eq       (l EmptyMK)) => Eq       (DbChangelog l)
deriving instance (NoThunks (LedgerTables l SeqDiffMK), NoThunks (l EmptyMK)) => NoThunks (DbChangelog l)

instance (ShowLedgerState l, ShowLedgerState (LedgerTables l)) => Show (DbChangelog l) where
  showsPrec p dblog =
        showParen (p >= 11)
      $   showString "DbChangelog {"
        . showSpace      . showString "changelogDiffAnchor = "      . shows changelogDiffAnchor
        . showCommaSpace . showString "changelogDiffs = "           . showsLedgerState changelogDiffs
        . showCommaSpace . showString "changelogImmutableStates = " . shows changelogImmutableStates
        . showCommaSpace . showString "changelogVolatileStates = "  . shows changelogVolatileStates
        . showString " }"
    where
      DbChangelog _dummy _ _ _ = dblog
      DbChangelog {
          changelogDiffAnchor
        , changelogDiffs
        , changelogImmutableStates
        , changelogVolatileStates
        } = dblog

newtype DbChangelogState l = DbChangelogState {unDbChangelogState :: l EmptyMK}
  deriving (Generic)

deriving instance Eq       (l EmptyMK) => Eq       (DbChangelogState l)
deriving instance NoThunks (l EmptyMK) => NoThunks (DbChangelogState l)

instance ShowLedgerState l => Show (DbChangelogState l) where
  showsPrec p (DbChangelogState x) =
        showParen (p >= 11)
      $ showString "DbChangelogState " . showsLedgerState x

instance GetTip (l EmptyMK) => AS.Anchorable (WithOrigin SlotNo) (DbChangelogState l) (DbChangelogState l) where
  asAnchor = id
  getAnchorMeasure _ = getTipSlot . unDbChangelogState

emptyDbChangeLog ::
     (TableStuff l, GetTip (l EmptyMK))
  => l EmptyMK -> DbChangelog l
emptyDbChangeLog anchor =
    DbChangelog {
        changelogDiffAnchor      = getTipSlot anchor
      , changelogDiffs           = pureLedgerTables (ApplySeqDiffMK empty)
      , changelogImmutableStates = AS.Empty (DbChangelogState anchor)
      , changelogVolatileStates  = AS.Empty (DbChangelogState anchor)
      }

extendDbChangelog ::
     (TableStuff l, GetTip (l EmptyMK))
  => DbChangelog l -> l DiffMK -> DbChangelog l
extendDbChangelog dblog newState =
    DbChangelog {
        changelogDiffAnchor
      , changelogDiffs           =
          zipLedgerTables ext changelogDiffs tablesDiff
      , changelogImmutableStates
      , changelogVolatileStates  =
          changelogVolatileStates AS.:> DbChangelogState l'
      }
  where
    DbChangelog {
        changelogDiffAnchor
      , changelogDiffs
      , changelogImmutableStates
      , changelogVolatileStates
      } = dblog

    l'         = forgetLedgerTables  newState
    tablesDiff = projectLedgerTables newState

    slot = case getTipSlot l' of
      Origin -> error "impossible! extendDbChangelog"
      At s   -> s

    ext ::
         (Ord k, Eq v)
      => SeqDiffMK k v
      -> DiffMK    k v
      -> SeqDiffMK k v
    ext (ApplySeqDiffMK sq) (ApplyDiffMK d) =
      ApplySeqDiffMK $ extend sq slot d

pruneVolatilePartDbChangelog ::
     GetTip (l EmptyMK)
  => SecurityParam -> DbChangelog l -> DbChangelog l
pruneVolatilePartDbChangelog (SecurityParam k) dblog =
    Exn.assert (AS.length imm' + AS.length vol' == AS.length imm + AS.length vol) $
    DbChangelog {
        changelogDiffAnchor
      , changelogDiffs
      , changelogImmutableStates = imm'
      , changelogVolatileStates  = vol'
      }
  where
    DbChangelog {
        changelogDiffAnchor
      , changelogDiffs
      , changelogImmutableStates
      , changelogVolatileStates
      } = dblog

    imm = changelogImmutableStates
    vol = changelogVolatileStates

    nvol = AS.length vol

    (imm', vol') =
      if toEnum nvol <= k then (imm, vol) else
      let (l, r) = AS.splitAt (nvol - fromEnum k) vol
      in (AS.unsafeJoin imm l, r)   -- TODO check fit?

-- | The flush policy
data DbChangelogFlushPolicy =
    -- | Always flush everything older than the immutable tip
    DbChangelogFlushAllImmutable

flushDbChangelog :: forall l.
     (GetTip (l EmptyMK), TableStuff l)
  => DbChangelogFlushPolicy
  -> DbChangelog l
  -> (DbChangelog l, DbChangelog l)
flushDbChangelog DbChangelogFlushAllImmutable dblog =
      (ldblog, rdblog)
  where
    DbChangelog {
        changelogDiffAnchor
      , changelogDiffs
      , changelogImmutableStates
      , changelogVolatileStates
      } = dblog

    imm = changelogImmutableStates
    vol = changelogVolatileStates

    immTip = AS.anchor vol

    -- TODO #2 by point, not by count, so sequences can be ragged
    split ::
         (Ord k, Eq v)
      => SeqDiffMK k v
      -> (SeqDiffMK k v, SeqDiffMK k v)
    split (ApplySeqDiffMK sq) =
        bimap ApplySeqDiffMK ApplySeqDiffMK
      $ splitlAt (AS.length imm) sq

    -- TODO #1 one pass
    l = mapLedgerTables (fst . split) changelogDiffs
    r = mapLedgerTables (snd . split) changelogDiffs

    ldblog = DbChangelog {
        changelogDiffAnchor
      , changelogDiffs           = l
      , changelogImmutableStates = imm
      , changelogVolatileStates  = AS.Empty immTip
      }

    rdblog = DbChangelog {
        changelogDiffAnchor      = getTipSlot (unDbChangelogState immTip)
      , changelogDiffs           = r
      , changelogImmutableStates = AS.Empty immTip
      , changelogVolatileStates  = vol
      }

-- | Roll back the volatile states up to the specified point.
prefixDbChangelog ::
     ( StandardHash (l EmptyMK)
     , GetTip (l EmptyMK)
     , TableStuff l
     )
  => Point (l EmptyMK) -> DbChangelog l -> Maybe (DbChangelog l)
prefixDbChangelog pt dblog = do
    let vol = changelogVolatileStates
    vol' <-
      AS.rollback
        (pointSlot pt)
        ((== pt) . getTip . unDbChangelogState . either id id)
        vol
    let ndropped                  = AS.length vol - AS.length vol'
        diffs'                    =
          mapLedgerTables (trunc ndropped) changelogDiffs
    Exn.assert (ndropped >= 0) $ pure DbChangelog {
          changelogDiffAnchor
        , changelogDiffs           = diffs'
        , changelogImmutableStates
        , changelogVolatileStates  = vol'
        }
  where
    DbChangelog {
        changelogDiffAnchor
      , changelogDiffs
      , changelogImmutableStates
      , changelogVolatileStates
      } = dblog

prefixBackToAnchorDbChangelog ::
     (GetTip (l EmptyMK), TableStuff l)
  => DbChangelog l -> DbChangelog l
prefixBackToAnchorDbChangelog dblog =
    DbChangelog {
        changelogDiffAnchor
      , changelogDiffs           = diffs'
      , changelogImmutableStates
      , changelogVolatileStates  = AS.Empty (AS.anchor vol)
      }
  where
    DbChangelog {
        changelogDiffAnchor
      , changelogDiffs
      , changelogImmutableStates
      , changelogVolatileStates
      } = dblog

    vol                       = changelogVolatileStates
    ndropped                  = AS.length vol
    diffs'                    =
      mapLedgerTables (trunc ndropped) changelogDiffs

trunc ::
     (Ord k, Eq v)
  => Int -> SeqDiffMK k v -> SeqDiffMK k v
trunc n (ApplySeqDiffMK sq) =
  ApplySeqDiffMK $ fst $ splitrAtFromEnd n sq

rollbackDbChangelog ::
     (GetTip (l EmptyMK), TableStuff l)
  => Int -> DbChangelog l -> DbChangelog l
rollbackDbChangelog n dblog =
    DbChangelog {
        changelogDiffAnchor
      , changelogDiffs           = mapLedgerTables (trunc n) changelogDiffs
      , changelogImmutableStates
      , changelogVolatileStates  = AS.dropNewest n changelogVolatileStates
      }
  where
    DbChangelog {
        changelogDiffAnchor
      , changelogDiffs
      , changelogImmutableStates
      , changelogVolatileStates
      } = dblog

youngestImmutableSlotDbChangelog ::
     GetTip (l EmptyMK)
  => DbChangelog l -> WithOrigin SlotNo
youngestImmutableSlotDbChangelog =
      getTipSlot
    . either unDbChangelogState unDbChangelogState
    . AS.head
    . changelogImmutableStates
