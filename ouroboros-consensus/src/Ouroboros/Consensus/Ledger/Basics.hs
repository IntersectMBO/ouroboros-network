{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveFoldable        #-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DeriveTraversable     #-}
{-# LANGUAGE DerivingVia           #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE Rank2Types            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
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
    -- * UTxO HD
    -- ** Isolating the tables
  , TableStuff (..)
  , TickedTableStuff (..)
  , mapOverLedgerTables
  , overLedgerTables
  , zipOverLedgerTables
  , mapOverLedgerTablesTicked
  , overLedgerTablesTicked
  , zipOverLedgerTablesTicked
    -- ** Tables values
  , MapKind (..)
  , SMapKind
  , Sing (..)
  , ApplyMapKind (..)
  , diffTrackingMK
  , emptyAppliedMK
  , mapValuesAppliedMK
  , sMapKind
  , toSMapKind
  , valuesTrackingMK
    -- ** Queries
  , DiskLedgerView
  , FootprintL (..)
    -- ** Convenience alises
  , TableKeySets
  , TableReadSets
  , applyDiffsLedgerTables
  , emptyLedgerTables
  , forgetLedgerStateTables
  , forgetLedgerStateTracking
  , forgetTickedLedgerStateTracking
  , trackingTablesToDiffs
    -- ** Special classes of ledger states
  , InMemory (..)
  , StowableLedgerTables (..)
    -- ** Changelog
  , DbChangelog (..)
  , DbChangelogFlushPolicy (..)
  , DbChangelogState (..)
  , emptyDbChangeLog
  , extendDbChangelog
  , flushDbChangelog
  , prefixBackToAnchorDbChangelog
  , prefixDbChangelog
  , pruneDbChangelog
  , rollbackDbChangelog
    -- ** Misc
  , ShowLedgerState (..)
  ) where

import qualified Codec.Serialise as InMemHD
import qualified Codec.Serialise.Decoding as InMemHD
import qualified Codec.Serialise.Encoding as InMemHD
import qualified Control.Exception as Exn
import           Data.Bifunctor (bimap)
import           Data.Kind (Type)
import           Data.Typeable (Typeable)
import           GHC.Generics (Generic)
import           NoThunks.Class (NoThunks (..), OnlyCheckWhnfNamed (..))
import qualified NoThunks.Class as NoThunks

import           Cardano.Slotting.Slot (WithOrigin (..))

import           Ouroboros.Network.AnchoredSeq (AnchoredSeq)
import qualified Ouroboros.Network.AnchoredSeq as AS
import           Ouroboros.Network.Protocol.LocalStateQuery.Type
                     (FootprintL (..))

import           Ouroboros.Consensus.Block.Abstract
import           Ouroboros.Consensus.Config.SecurityParam (SecurityParam (..))
import           Ouroboros.Consensus.Ticked
import           Ouroboros.Consensus.Util ((..:))
import           Ouroboros.Consensus.Util.Singletons

import           Ouroboros.Consensus.Storage.LedgerDB.HD

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
  Basic LedgerState classes
-------------------------------------------------------------------------------}

-- TODO where is this class needed?
class ShowLedgerState (l :: LedgerStateKind) where
  showsLedgerState :: SMapKind mk -> l mk -> ShowS   -- TODO someway to show the mk values

{-------------------------------------------------------------------------------
  Definition of a ledger independent of a choice of block
-------------------------------------------------------------------------------}

-- | Static environment required for the ledger
type family LedgerCfg (l :: LedgerStateKind) :: Type

class ( -- Requirements on the ledger state itself
        ShowLedgerState                     l
      , forall mk. Eq                      (l mk)
      , forall mk. Typeable mk => NoThunks (l mk)
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
      , NoThunks          (LedgerTables l ValuesMK)   -- for in-memory store
      , InMemHD.Serialise (LedgerTables l ValuesMK)   -- for in-memory store
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
  -- PRECONDITION: The slot number must be strictly greater than the slot at
  -- the tip of the ledger (except for EBBs, obviously..).
  --
  -- NOTE: 'applyChainTickLedgerResult' should /not/ change the tip of the
  -- underlying ledger state, which should still refer to the most recent
  -- applied /block/. In other words, we should have
  --
  -- >    ledgerTipPoint (applyChainTick cfg slot st)
  -- > == ledgerTipPoint st
  applyChainTickLedgerResult ::
       LedgerCfg l
    -> SlotNo
    -> l mk
    -> LedgerResult l (Ticked1 l mk)

-- | 'lrResult' after 'applyChainTickLedgerResult'
applyChainTick :: IsLedger l => LedgerCfg l -> SlotNo -> l mk -> Ticked1 l mk
applyChainTick = lrResult ..: applyChainTickLedgerResult

-- This can't be in IsLedger because we have a compositional IsLedger instance
-- for LedgerState HardForkBlock but we will not (at least ast first) have a
-- compositional LedgerTables instance for HardForkBlock.
class (ShowLedgerState (LedgerTables l), Eq (l ValuesMK)) => TableStuff (l :: LedgerStateKind) where

  data family LedgerTables l :: LedgerStateKind

  projectLedgerTables :: l mk -> LedgerTables l mk

  -- | Overwrite the tables in some ledger state.
  --
  -- The contents of the tables should not be /younger/ than the content of the
  -- ledger state. In particular, for a
  -- 'Ouroboros.Consensus.HardFork.Combinator.Basics.HardForkBlock' ledger, the
  -- tables argument should not contain any data from eras that succeed the
  -- current era of the ledger state argument.
  --
  -- TODO: reconsider the name: don't we use 'withX' in the context of bracket like functions?
  withLedgerTables :: l any -> LedgerTables l mk -> l mk

  pureLedgerTables ::
       (forall k v.
            Ord k
         => ApplyMapKind mk k v
       )
    -> LedgerTables l mk

  mapLedgerTables ::
       (forall k v.
            Ord k
         => ApplyMapKind mk1 k v
         -> ApplyMapKind mk2 k v
       )
    -> LedgerTables l mk1
    -> LedgerTables l mk2

  zipLedgerTables ::
       (forall k v.
            Ord k
         => ApplyMapKind mk1 k v
         -> ApplyMapKind mk2 k v
         -> ApplyMapKind mk3 k v
       )
    -> LedgerTables l mk1
    -> LedgerTables l mk2
    -> LedgerTables l mk3

overLedgerTables ::
     TableStuff l
  => (LedgerTables l mk1 -> LedgerTables l mk2)
  -> l mk1
  -> l mk2
overLedgerTables f l = withLedgerTables l $ f $ projectLedgerTables l

mapOverLedgerTables ::
     TableStuff l
  => (forall k v.
          Ord k
       => ApplyMapKind mk1 k v
       -> ApplyMapKind mk2 k v
     )
  -> l mk1
  -> l mk2
mapOverLedgerTables f = overLedgerTables $ mapLedgerTables f

zipOverLedgerTables ::
     TableStuff l
  => (forall k v.
          Ord k
       => ApplyMapKind mk1 k v
       -> ApplyMapKind mk2 k v
       -> ApplyMapKind mk3 k v
     )
  ->              l mk1
  -> LedgerTables l mk2
  ->              l mk3
zipOverLedgerTables f l tables2 =
    overLedgerTables
      (\tables1 -> zipLedgerTables f tables1 tables2)
      l

-- Separate so that we can have a 'TableStuff' instance for 'Ticked1' without
-- involving double-ticked types.
class TableStuff l => TickedTableStuff (l :: LedgerStateKind) where
  projectLedgerTablesTicked :: Ticked1 l mk  -> LedgerTables l mk
  withLedgerTablesTicked    :: Ticked1 l any -> LedgerTables l mk -> Ticked1 l mk

overLedgerTablesTicked ::
     TickedTableStuff l
  => (LedgerTables l mk1 -> LedgerTables l mk2)
  -> Ticked1 l mk1
  -> Ticked1 l mk2
overLedgerTablesTicked f l =
    withLedgerTablesTicked l $ f $ projectLedgerTablesTicked l

mapOverLedgerTablesTicked ::
     TickedTableStuff l
  => (forall k v.
         Ord k
      => ApplyMapKind mk1 k v
      -> ApplyMapKind mk2 k v
     )
  -> Ticked1 l mk1
  -> Ticked1 l mk2
mapOverLedgerTablesTicked f = overLedgerTablesTicked $ mapLedgerTables f

zipOverLedgerTablesTicked ::
     TickedTableStuff l
  => (forall k v.
         Ord k
      => ApplyMapKind mk1 k v
      -> ApplyMapKind mk2 k v
      -> ApplyMapKind mk3 k v
     )
  -> Ticked1      l mk1
  -> LedgerTables l mk2
  -> Ticked1      l mk3
zipOverLedgerTablesTicked f l tables2 =
    overLedgerTablesTicked
      (\tables1 -> zipLedgerTables f tables1 tables2)
      l

{-------------------------------------------------------------------------------
  Convenience aliases
-------------------------------------------------------------------------------}

-- | Empty values for every table
emptyLedgerTables :: forall l mk.
  (TableStuff l, SingI mk) => LedgerTables l mk
emptyLedgerTables =
    pureLedgerTables $ emptyAppliedMK (sing :: SMapKind mk)

forgetLedgerStateTracking :: TableStuff l => l TrackingMK -> l ValuesMK
forgetLedgerStateTracking = mapOverLedgerTables valuesTrackingMK

forgetLedgerStateTables :: TableStuff l => l mk -> l EmptyMK
forgetLedgerStateTables =
    mapOverLedgerTables f
  where
    f :: ApplyMapKind mk k v -> ApplyMapKind EmptyMK k v
    f _ = ApplyEmptyMK

-- | Used only for the dual ledger
applyDiffsLedgerTables :: TableStuff l => l ValuesMK -> LedgerTables l DiffMK -> l ValuesMK
applyDiffsLedgerTables =
    zipOverLedgerTables f
  where
    f ::
         Ord k
      => ApplyMapKind ValuesMK k v
      -> ApplyMapKind DiffMK   k v
      -> ApplyMapKind ValuesMK k v
    f (ApplyValuesMK values) (ApplyDiffMK diff) =
      ApplyValuesMK (forwardValues values diff)

trackingTablesToDiffs :: TableStuff l => l TrackingMK -> l DiffMK
trackingTablesToDiffs = mapOverLedgerTables diffTrackingMK

forgetTickedLedgerStateTracking ::
  TickedTableStuff l => Ticked1 l TrackingMK -> Ticked1 l ValuesMK
forgetTickedLedgerStateTracking = mapOverLedgerTablesTicked valuesTrackingMK

{-------------------------------------------------------------------------------
  Concrete ledger tables
-------------------------------------------------------------------------------}

type LedgerStateKind = MapKind -> Type

data MapKind = DiffMK
             | EmptyMK
             | KeysMK
             | RewoundMK
             | SeqDiffMK
             | TrackingMK
             | ValuesMK

data ApplyMapKind :: MapKind -> Type -> Type -> Type where
  ApplyDiffMK     :: !(UtxoDiff    k v)                    -> ApplyMapKind DiffMK       k v
  ApplyEmptyMK    ::                                          ApplyMapKind EmptyMK      k v
  ApplyKeysMK     :: !(UtxoKeys    k v)                    -> ApplyMapKind KeysMK       k v
  ApplySeqDiffMK  :: !(SeqUtxoDiff k v)                    -> ApplyMapKind SeqDiffMK    k v
  ApplyTrackingMK :: !(UtxoValues  k v) -> !(UtxoDiff k v) -> ApplyMapKind TrackingMK   k v
  ApplyValuesMK   :: !(UtxoValues  k v)                    -> ApplyMapKind ValuesMK     k v
  ApplyRewoundMK  :: !(RewoundKeys k v)                    -> ApplyMapKind RewoundMK    k v

emptyAppliedMK :: Ord k => SMapKind mk -> ApplyMapKind mk k v
emptyAppliedMK = \case
    SEmptyMK    -> ApplyEmptyMK
    SKeysMK     -> ApplyKeysMK     emptyUtxoKeys
    SValuesMK   -> ApplyValuesMK   emptyUtxoValues
    STrackingMK -> ApplyTrackingMK emptyUtxoValues emptyUtxoDiff
    SDiffMK     -> ApplyDiffMK     emptyUtxoDiff
    SSeqDiffMK  -> ApplySeqDiffMK  emptySeqUtxoDiff
    SRewoundMK  -> ApplyRewoundMK  (RewoundKeys emptyUtxoKeys emptyUtxoValues emptyUtxoKeys)

mapValuesAppliedMK :: Ord k => (v -> v') -> ApplyMapKind mk k v ->  ApplyMapKind mk k v'
mapValuesAppliedMK f = \case
  ApplyEmptyMK            -> ApplyEmptyMK
  ApplyKeysMK ks          -> ApplyKeysMK     (castUtxoKeys ks)
  ApplyValuesMK vs        -> ApplyValuesMK   (mapUtxoValues f vs)
  ApplyTrackingMK vs diff -> ApplyTrackingMK (mapUtxoValues f vs)     (mapUtxoDiff f diff)
  ApplyDiffMK diff        -> ApplyDiffMK     (mapUtxoDiff f diff)
  ApplySeqDiffMK diffs    -> ApplySeqDiffMK  (mapSeqUtxoDiff f diffs)
  ApplyRewoundMK rew      -> ApplyRewoundMK  (mapRewoundKeys f rew)

diffTrackingMK :: ApplyMapKind TrackingMK k v -> ApplyMapKind DiffMK k v
diffTrackingMK (ApplyTrackingMK _values diff) = ApplyDiffMK diff

valuesTrackingMK :: ApplyMapKind TrackingMK k v -> ApplyMapKind ValuesMK k v
valuesTrackingMK (ApplyTrackingMK values _diff) = ApplyValuesMK values

{-
instance (Ord k, Eq v) => Eq (ApplyMapKind mk k v) where
  ApplyEmptyMK    == _               = True
  ApplyKeysMK   l == ApplyKeysMK   r = l == r
  ApplyValuesMK l == ApplyValuesMK r = l == r
  ApplyTrackingMK == _               = True
  ApplyDiffMK     == _               = True
-}

instance (Ord k, NoThunks k, NoThunks v) => NoThunks (ApplyMapKind mk k v) where
  wNoThunks ctxt   = NoThunks.allNoThunks . \case
    ApplyEmptyMK            -> []
    ApplyKeysMK ks          -> [noThunks ctxt ks]
    ApplyValuesMK vs        -> [noThunks ctxt vs]
    ApplyTrackingMK vs diff -> [noThunks ctxt vs, noThunks ctxt diff]
    ApplyDiffMK diff        -> [noThunks ctxt diff]
    ApplySeqDiffMK diffs    -> [noThunks ctxt diffs]
    ApplyRewoundMK rew      -> [noThunks ctxt rew]

  showTypeOf _ = "ApplyMapKind"

data instance Sing (mk :: MapKind) :: Type where
  SEmptyMK    :: Sing EmptyMK
  SKeysMK     :: Sing KeysMK
  SValuesMK   :: Sing ValuesMK
  STrackingMK :: Sing TrackingMK
  SDiffMK     :: Sing DiffMK
  SSeqDiffMK  :: Sing SeqDiffMK
  SRewoundMK  :: Sing RewoundMK

type SMapKind = Sing :: MapKind -> Type

instance SingI EmptyMK    where sing = SEmptyMK
instance SingI KeysMK     where sing = SKeysMK
instance SingI ValuesMK   where sing = SValuesMK
instance SingI TrackingMK where sing = STrackingMK
instance SingI DiffMK     where sing = SDiffMK
instance SingI SeqDiffMK  where sing = SSeqDiffMK
instance SingI RewoundMK  where sing = SRewoundMK

sMapKind :: SingI mk => SMapKind mk
sMapKind = sing

toSMapKind :: ApplyMapKind mk k v -> SMapKind mk
toSMapKind = \case
    ApplyEmptyMK{}    -> SEmptyMK
    ApplyKeysMK{}     -> SKeysMK
    ApplyValuesMK{}   -> SValuesMK
    ApplyTrackingMK{} -> STrackingMK
    ApplyDiffMK{}     -> SDiffMK
    ApplySeqDiffMK{}  -> SSeqDiffMK
    ApplyRewoundMK{}  -> SRewoundMK

instance Eq (Sing (mk :: MapKind)) where
  _ == _ = True

instance Show (Sing (mk :: MapKind)) where
  show = \case
    SEmptyMK    -> "SEmptyMK"
    SKeysMK     -> "SKeysMK"
    SValuesMK   -> "SValuesMK"
    STrackingMK -> "STrackingMK"
    SDiffMK     -> "SDiffMK"
    SSeqDiffMK  -> "SSeqDiffMK"
    SRewoundMK  -> "SRewoundMK"

deriving via OnlyCheckWhnfNamed "Sing @MapKind" (Sing (mk :: MapKind)) instance NoThunks (Sing mk)

instance InMemHD.Serialise (Sing EmptyMK)    where encode SEmptyMK    = InMemHD.encodeNull; decode = SEmptyMK    <$ InMemHD.decodeNull
instance InMemHD.Serialise (Sing KeysMK)     where encode SKeysMK     = InMemHD.encodeNull; decode = SKeysMK     <$ InMemHD.decodeNull
instance InMemHD.Serialise (Sing ValuesMK)   where encode SValuesMK   = InMemHD.encodeNull; decode = SValuesMK   <$ InMemHD.decodeNull
instance InMemHD.Serialise (Sing TrackingMK) where encode STrackingMK = InMemHD.encodeNull; decode = STrackingMK <$ InMemHD.decodeNull
instance InMemHD.Serialise (Sing DiffMK)     where encode SDiffMK     = InMemHD.encodeNull; decode = SDiffMK     <$ InMemHD.decodeNull
instance InMemHD.Serialise (Sing SeqDiffMK)  where encode SSeqDiffMK  = InMemHD.encodeNull; decode = SSeqDiffMK  <$ InMemHD.decodeNull
instance InMemHD.Serialise (Sing RewoundMK)  where encode SRewoundMK  = InMemHD.encodeNull; decode = SRewoundMK  <$ InMemHD.decodeNull

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
data family DiskLedgerView blk :: (Type -> Type) -> Type

type TableKeySets l = LedgerTables l KeysMK

{-------------------------------------------------------------------------------
  Special classes of ledger states

  TODO if having such class(es) make sense but only in the context of testing
  code we should move this to the appropriate module.
-------------------------------------------------------------------------------}

class InMemory (l :: LedgerStateKind) where
  
  -- | If the ledger state is always in memory, then l mk will be isomorphic to
  -- l mk' for all mk, mk'. As a result, we can convert between ledgers states
  -- indexed by different map kinds.
  --
  -- This function is useful to combine functions that operate on functions that
  -- transform the map kind on a ledger state (eg applyChainTickLedgerResult).
  convertMapKind :: l mk -> l mk'

type TableReadSets l = LedgerTables l ValuesMK

-- | TODO Once we remove the dual ledger, we won't need this anymore
class StowableLedgerTables (l :: LedgerStateKind) where
  stowLedgerTables   :: l ValuesMK -> l EmptyMK
  unstowLedgerTables :: l EmptyMK  -> l ValuesMK

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

newtype DbChangelogState l = DbChangelogState {unDbChangelogState :: l EmptyMK}
  deriving (Generic)

deriving instance Eq       (l EmptyMK) => Eq       (DbChangelogState l)
deriving instance NoThunks (l EmptyMK) => NoThunks (DbChangelogState l)

instance GetTip (l EmptyMK) => AS.Anchorable (WithOrigin SlotNo) (DbChangelogState l) (DbChangelogState l) where
  asAnchor = id
  getAnchorMeasure _ = getTipSlot . unDbChangelogState

emptyDbChangeLog ::
     (TableStuff l, GetTip (l EmptyMK))
  => l EmptyMK -> DbChangelog l
emptyDbChangeLog anchor =
    DbChangelog {
        changelogDiffAnchor      = getTipSlot anchor
      , changelogDiffs           = pureLedgerTables (ApplySeqDiffMK emptySeqUtxoDiff)
      , changelogImmutableStates = AS.Empty (DbChangelogState anchor)
      , changelogVolatileStates  = AS.Empty (DbChangelogState anchor)
      }

extendDbChangelog ::
     (TableStuff l, GetTip (l EmptyMK))
  => DbChangelog l -> l DiffMK -> DbChangelog l
extendDbChangelog dblog newState =
    DbChangelog {
        changelogDiffAnchor      = changelogDiffAnchor dblog
      , changelogDiffs           =
          zipLedgerTables ext (changelogDiffs dblog) tablesDiff
      , changelogImmutableStates = changelogImmutableStates dblog
      , changelogVolatileStates  =
          changelogVolatileStates dblog AS.:> DbChangelogState l'
      }
  where
    l'         = forgetLedgerStateTables newState
    tablesDiff = projectLedgerTables     newState

    slot = case getTipSlot l' of
      Origin -> error "impossible! extendDbChangelog"
      At s   -> s

    ext ::
         Ord k
      => ApplyMapKind SeqDiffMK k v
      -> ApplyMapKind DiffMK    k v
      -> ApplyMapKind SeqDiffMK k v
    ext (ApplySeqDiffMK sq) (ApplyDiffMK diff) =
      ApplySeqDiffMK $ extendSeqUtxoDiff sq slot diff

pruneDbChangelog ::
     GetTip (l EmptyMK)
  => SecurityParam -> DbChangelog l -> DbChangelog l
pruneDbChangelog (SecurityParam k) dblog =
    DbChangelog {
        changelogDiffAnchor      = changelogDiffAnchor dblog
      , changelogDiffs           = changelogDiffs      dblog
      , changelogImmutableStates = imm'
      , changelogVolatileStates  = vol'
      }
  where
    imm = changelogImmutableStates dblog
    vol = changelogVolatileStates  dblog

    nvol = AS.length vol

    (imm', vol') =
      if toEnum nvol <= k then (imm, vol) else
      let (l, r) = AS.splitAt (nvol - fromEnum k) vol
      in (AS.unsafeJoin imm l, r)   -- TODO check fit?

-- | The flush policy
data DbChangelogFlushPolicy =
    -- | Always flush everything older than the immutable tip
    DbChangelogFlushAllImmutable

flushDbChangelog ::
     (GetTip (l EmptyMK), TableStuff l)
  => DbChangelogFlushPolicy
  -> DbChangelog l
  -> (DbChangelog l, DbChangelog l)
flushDbChangelog DbChangelogFlushAllImmutable dblog =
    (ldblog, rdblog)
  where
    imm = changelogImmutableStates dblog
    vol = changelogVolatileStates  dblog

    immTip = AS.anchor vol

    -- TODO #2 by point, not by count, so sequences can be ragged
    split ::
         Ord k
      => ApplyMapKind SeqDiffMK k v
      -> (ApplyMapKind SeqDiffMK k v, ApplyMapKind SeqDiffMK k v)
    split (ApplySeqDiffMK sq) =
        bimap ApplySeqDiffMK ApplySeqDiffMK
      $ splitAtSeqUtxoDiff (AS.length imm) sq

    -- TODO #1 one pass
    l = mapLedgerTables (fst . split) (changelogDiffs dblog)
    r = mapLedgerTables (snd . split) (changelogDiffs dblog)

    ldblog = DbChangelog {
        changelogDiffAnchor      = changelogDiffAnchor dblog
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

prefixDbChangelog ::
     ( HasHeader blk
     , HeaderHash blk ~ HeaderHash (l EmptyMK)
     , GetTip (l EmptyMK)
     , TableStuff l
     )
  => Point blk -> DbChangelog l -> Maybe (DbChangelog l)
prefixDbChangelog pt dblog = do
    let vol = changelogVolatileStates dblog
    vol' <-
      AS.rollback
        (pointSlot pt)
        ((== pt) . castPoint . getTip . unDbChangelogState . either id id)
        vol
    let ndropped                  = AS.length vol - AS.length vol'
        diffs'                    =
          mapLedgerTables (trunc ndropped) (changelogDiffs dblog)
    Exn.assert (ndropped >= 0) $ pure DbChangelog {
          changelogDiffAnchor      = changelogDiffAnchor      dblog
        , changelogDiffs           = diffs'
        , changelogImmutableStates = changelogImmutableStates dblog
        , changelogVolatileStates  = vol'
        }

prefixBackToAnchorDbChangelog ::
     (GetTip (l EmptyMK), TableStuff l)
  => DbChangelog l -> DbChangelog l
prefixBackToAnchorDbChangelog dblog =
    DbChangelog {
        changelogDiffAnchor      = changelogDiffAnchor      dblog
      , changelogDiffs           = diffs'
      , changelogImmutableStates = changelogImmutableStates dblog
      , changelogVolatileStates  = AS.Empty (AS.anchor vol)
      }
  where
    vol                       = changelogVolatileStates dblog
    ndropped                  = AS.length vol
    diffs'                    =
      mapLedgerTables (trunc ndropped) (changelogDiffs dblog)

trunc ::
     Ord k
  => Int -> ApplyMapKind SeqDiffMK k v -> ApplyMapKind SeqDiffMK k v
trunc n (ApplySeqDiffMK sq) =
  ApplySeqDiffMK $ fst $ splitAtFromEndSeqUtxoDiff n sq

rollbackDbChangelog ::
     (GetTip (l EmptyMK), TableStuff l)
  => Int -> DbChangelog l -> DbChangelog l
rollbackDbChangelog n dblog =
    DbChangelog {
        changelogDiffAnchor      = changelogDiffAnchor      dblog
      , changelogDiffs           = mapLedgerTables (trunc n) (changelogDiffs dblog)
      , changelogImmutableStates = changelogImmutableStates dblog
      , changelogVolatileStates  = AS.dropNewest n (changelogImmutableStates dblog)
      }
