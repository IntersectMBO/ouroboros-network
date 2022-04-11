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
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE Rank2Types            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeApplications      #-}
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
  , mapOverLedgerTablesTicked
  , overLedgerTables
  , overLedgerTablesTicked
  , zipOverLedgerTables
  , zipOverLedgerTablesTicked
    -- ** Tables values
  , ApplyMapKind (..)
  , MapKind (..)
  , SMapKind
  , Sing (..)
  , calculateDifference
  , diffKeysMK
  , diffTrackingMK
  , emptyAppliedMK
  , mapValuesAppliedMK
  , sMapKind
  , sMapKind'
  , showsApplyMapKind
  , toSMapKind
  , valuesTrackingMK
    -- ** Queries
  , DiskLedgerView (..)
  , FootprintL (..)
    -- ** Convenience alises
  , TableKeySets
  , TableReadSets
  , applyDiffsLedgerTables
  , emptyLedgerTables
  , forgetLedgerStateTables
  , forgetLedgerStateTracking
  , forgetTickedLedgerStateTracking
  , polyEmptyLedgerTables
  , trackingTablesToDiffs
    -- ** Special classes of ledger states
  , InMemory (..)
  , StowableLedgerTables (..)
  , isCandidateForUnstowDefault
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
    -- ** Misc
  , ShowLedgerState (..)
  ) where

import qualified Codec.CBOR.Decoding as CBOR
import qualified Codec.CBOR.Encoding as CBOR
import qualified Control.Exception as Exn
import           Control.Monad (when)
import           Data.Bifunctor (bimap)
import           Data.Kind (Type)
import qualified Data.Map as Map
import           Data.Monoid (All (..))
import           Data.Typeable (Typeable)
import           Data.Word (Word8)
import           GHC.Generics (Generic)
import           GHC.Show (showCommaSpace, showSpace)
import           GHC.Stack (HasCallStack)
import           NoThunks.Class (NoThunks (..), OnlyCheckWhnfNamed (..))
import qualified NoThunks.Class as NoThunks

import           Cardano.Binary (FromCBOR (..), ToCBOR (..))
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
import           Ouroboros.Consensus.Storage.LedgerDB.HD.BackingStore
                     (RangeQuery)

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

class ShowLedgerState (l :: LedgerStateKind) where
  showsLedgerState :: SMapKind mk -> l mk -> ShowS

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
      , ToCBOR   (LedgerTables l ValuesMK)
      , FromCBOR (LedgerTables l ValuesMK)
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
  --
  -- NOTE: The 'SingI' constraint is here for the same reason its on
  -- 'projectLedgerTables'
  applyChainTickLedgerResult ::
       SingI mk
    => LedgerCfg l
    -> SlotNo
    -> l mk
    -> LedgerResult l (Ticked1 l mk)

-- | 'lrResult' after 'applyChainTickLedgerResult'
applyChainTick ::
     (IsLedger l, SingI mk)
  => LedgerCfg l
  -> SlotNo
  -> l mk
  -> Ticked1 l mk
applyChainTick = lrResult ..: applyChainTickLedgerResult

-- This can't be in IsLedger because we have a compositional IsLedger instance
-- for LedgerState HardForkBlock but we will not (at least ast first) have a
-- compositional LedgerTables instance for HardForkBlock.
class ( ShowLedgerState (LedgerTables l)
      , Eq (l EmptyMK)
      , Eq (LedgerTables l DiffMK)
      , Eq (LedgerTables l ValuesMK)
      ) => TableStuff (l :: LedgerStateKind) where

  data family LedgerTables l :: LedgerStateKind

  -- | Some values of @l mk@ do not determine @mk@, hence the 'SingI' constraint.
  --
  -- If it were always the case that @l mk@ not determing @mk@ implies
  -- @LedgerTables l mk@ also does not determine @mk@, then we would not need
  -- the 'SingI' constraint. Unfortunately, that is not always the case. The
  -- example we have found in our prototype UTxO HD implementat is that a Byron
  -- ledger state does not determine @mk@, but the Cardano ledger tables do.
  projectLedgerTables :: SingI mk => l mk -> LedgerTables l mk

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

  foldLedgerTables ::
       Monoid m
    => (forall k v.
            Ord k
         => ApplyMapKind mk k v
         -> m
       )
    -> LedgerTables l mk
    -> m

overLedgerTables ::
     (TableStuff l, SingI mk1)
  => (LedgerTables l mk1 -> LedgerTables l mk2)
  -> l mk1
  -> l mk2
overLedgerTables f l = withLedgerTables l $ f $ projectLedgerTables l

mapOverLedgerTables ::
     (TableStuff l, SingI mk1)
  => (forall k v.
          Ord k
       => ApplyMapKind mk1 k v
       -> ApplyMapKind mk2 k v
     )
  -> l mk1
  -> l mk2
mapOverLedgerTables f = overLedgerTables $ mapLedgerTables f

zipOverLedgerTables ::
     (TableStuff l, SingI mk1)
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
  -- | NOTE: The 'SingI' constraint is here for the same reason its on
  -- 'projectLedgerTables'
  projectLedgerTablesTicked :: SingI mk => Ticked1 l mk  -> LedgerTables l mk
  withLedgerTablesTicked    ::             Ticked1 l any -> LedgerTables l mk -> Ticked1 l mk

overLedgerTablesTicked ::
     (TickedTableStuff l, SingI mk1)
  => (LedgerTables l mk1 -> LedgerTables l mk2)
  -> Ticked1 l mk1
  -> Ticked1 l mk2
overLedgerTablesTicked f l =
    withLedgerTablesTicked l $ f $ projectLedgerTablesTicked l

mapOverLedgerTablesTicked ::
     (TickedTableStuff l, SingI mk1)
  => (forall k v.
         Ord k
      => ApplyMapKind mk1 k v
      -> ApplyMapKind mk2 k v
     )
  -> Ticked1 l mk1
  -> Ticked1 l mk2
mapOverLedgerTablesTicked f = overLedgerTablesTicked $ mapLedgerTables f

zipOverLedgerTablesTicked ::
     (TickedTableStuff l, SingI mk1)
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

emptyLedgerTables :: TableStuff l => LedgerTables l EmptyMK
emptyLedgerTables = polyEmptyLedgerTables

-- | Empty values for every table
polyEmptyLedgerTables :: forall mk l.
  (TableStuff l, SingI mk) => LedgerTables l mk
polyEmptyLedgerTables =
    pureLedgerTables $ emptyAppliedMK (sing :: SMapKind mk)

forgetLedgerStateTracking :: TableStuff l => l TrackingMK -> l ValuesMK
forgetLedgerStateTracking = mapOverLedgerTables valuesTrackingMK

forgetLedgerStateTables :: TableStuff l => l mk -> l EmptyMK
forgetLedgerStateTables l = withLedgerTables l emptyLedgerTables

-- | Used only for the dual ledger
applyDiffsLedgerTables ::
     (TableStuff l, HasCallStack)
  => l ValuesMK
  -> LedgerTables l DiffMK
  -> l ValuesMK
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
             | QueryMK
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

  ApplyQueryAllMK  ::                    ApplyMapKind QueryMK k v
  ApplyQuerySomeMK :: !(UtxoKeys k v) -> ApplyMapKind QueryMK k v

emptyAppliedMK :: Ord k => SMapKind mk -> ApplyMapKind mk k v
emptyAppliedMK = \case
    SEmptyMK    -> ApplyEmptyMK
    SKeysMK     -> ApplyKeysMK     emptyUtxoKeys
    SValuesMK   -> ApplyValuesMK   emptyUtxoValues
    STrackingMK -> ApplyTrackingMK emptyUtxoValues emptyUtxoDiff
    SDiffMK     -> ApplyDiffMK     emptyUtxoDiff
    SSeqDiffMK  -> ApplySeqDiffMK  emptySeqUtxoDiff
    SRewoundMK  -> ApplyRewoundMK  (RewoundKeys emptyUtxoKeys emptyUtxoValues emptyUtxoKeys)
    SQueryMK    -> ApplyQuerySomeMK emptyUtxoKeys

instance Ord k => Semigroup (ApplyMapKind KeysMK k v) where
  ApplyKeysMK l <> ApplyKeysMK r = ApplyKeysMK (l <> r)

instance Ord k => Monoid (ApplyMapKind KeysMK k v) where
  mempty = ApplyKeysMK mempty

mapValuesAppliedMK :: Ord k => (v -> v') -> ApplyMapKind mk k v ->  ApplyMapKind mk k v'
mapValuesAppliedMK f = \case
  ApplyEmptyMK            -> ApplyEmptyMK
  ApplyKeysMK ks          -> ApplyKeysMK     (castUtxoKeys ks)
  ApplyValuesMK vs        -> ApplyValuesMK   (mapUtxoValues f vs)
  ApplyTrackingMK vs diff -> ApplyTrackingMK (mapUtxoValues f vs)     (mapUtxoDiff f diff)
  ApplyDiffMK diff        -> ApplyDiffMK     (mapUtxoDiff f diff)
  ApplySeqDiffMK diffs    -> ApplySeqDiffMK  (mapSeqUtxoDiff f diffs)
  ApplyRewoundMK rew      -> ApplyRewoundMK  (mapRewoundKeys f rew)

  ApplyQueryAllMK         -> ApplyQueryAllMK
  ApplyQuerySomeMK ks     -> ApplyQuerySomeMK (castUtxoKeys ks)

diffKeysMK :: ApplyMapKind DiffMK k v -> ApplyMapKind KeysMK k v
diffKeysMK (ApplyDiffMK diff) = ApplyKeysMK $ keysUtxoDiff diff

diffTrackingMK :: ApplyMapKind TrackingMK k v -> ApplyMapKind DiffMK k v
diffTrackingMK (ApplyTrackingMK _values diff) = ApplyDiffMK diff

valuesTrackingMK :: ApplyMapKind TrackingMK k v -> ApplyMapKind ValuesMK k v
valuesTrackingMK (ApplyTrackingMK values _diff) = ApplyValuesMK values

instance (Ord k, Eq v) => Eq (ApplyMapKind mk k v) where
  ApplyEmptyMK          == _                     = True
  ApplyKeysMK   l       == ApplyKeysMK   r       = l == r
  ApplyValuesMK l       == ApplyValuesMK r       = l == r
  ApplyTrackingMK l1 l2 == ApplyTrackingMK r1 r2 = l1 == r1 && l2 == r2
  ApplyDiffMK l         == ApplyDiffMK r         = l == r
  ApplySeqDiffMK l      == ApplySeqDiffMK r      = l == r
  ApplyRewoundMK l      == ApplyRewoundMK r      = l == r
  ApplyQueryAllMK       == ApplyQueryAllMK       = True
  ApplyQuerySomeMK l    == ApplyQuerySomeMK r    = l == r
  _                     == _                     = False

instance (Ord k, NoThunks k, NoThunks v) => NoThunks (ApplyMapKind mk k v) where
  wNoThunks ctxt   = NoThunks.allNoThunks . \case
    ApplyEmptyMK            -> []
    ApplyKeysMK ks          -> [noThunks ctxt ks]
    ApplyValuesMK vs        -> [noThunks ctxt vs]
    ApplyTrackingMK vs diff -> [noThunks ctxt vs, noThunks ctxt diff]
    ApplyDiffMK diff        -> [noThunks ctxt diff]
    ApplySeqDiffMK diffs    -> [noThunks ctxt diffs]
    ApplyRewoundMK rew      -> [noThunks ctxt rew]
    ApplyQueryAllMK         -> []
    ApplyQuerySomeMK ks     -> [noThunks ctxt ks]

  showTypeOf _ = "ApplyMapKind"

instance
     (Typeable mk, Ord k, ToCBOR k, ToCBOR v, SingI mk)
  => ToCBOR (ApplyMapKind mk k v) where
  toCBOR = \case
      ApplyEmptyMK            -> encodeArityAndTag 0 []
      ApplyKeysMK ks          -> encodeArityAndTag 1 [toCBOR ks]
      ApplyValuesMK vs        -> encodeArityAndTag 2 [toCBOR vs]
      ApplyTrackingMK vs diff -> encodeArityAndTag 3 [toCBOR vs, toCBOR diff]
      ApplyDiffMK diff        -> encodeArityAndTag 4 [toCBOR diff]
      ApplySeqDiffMK diffs    -> encodeArityAndTag 5 [toCBOR diffs]
      ApplyRewoundMK rew      -> encodeArityAndTag 6 [toCBOR rew]
      ApplyQueryAllMK         -> encodeArityAndTag 7 []
      ApplyQuerySomeMK ks     -> encodeArityAndTag 7 [toCBOR ks]
    where
      encodeArityAndTag :: Word8 -> [CBOR.Encoding] -> CBOR.Encoding
      encodeArityAndTag tag xs =
           CBOR.encodeListLen (1 + toEnum (length xs))
        <> CBOR.encodeWord8 tag

instance
     (Typeable mk, Ord k, FromCBOR k, FromCBOR v, SingI mk)
  => FromCBOR (ApplyMapKind mk k v) where
  fromCBOR = do
    case smk of
      SEmptyMK    -> decodeArityAndTag 0 0 *> (ApplyEmptyMK    <$  pure ())
      SKeysMK     -> decodeArityAndTag 1 1 *> (ApplyKeysMK     <$> fromCBOR)
      SValuesMK   -> decodeArityAndTag 1 2 *> (ApplyValuesMK   <$> fromCBOR)
      STrackingMK -> decodeArityAndTag 2 3 *> (ApplyTrackingMK <$> fromCBOR <*> fromCBOR)
      SDiffMK     -> decodeArityAndTag 1 4 *> (ApplyDiffMK     <$> fromCBOR)
      SSeqDiffMK  -> decodeArityAndTag 1 5 *> (ApplySeqDiffMK  <$> fromCBOR)
      SRewoundMK  -> decodeArityAndTag 1 6 *> (ApplyRewoundMK  <$> fromCBOR)
      SQueryMK    -> do
        len <- CBOR.decodeListLen
        tag <- CBOR.decodeWord8
        case (len, tag) of
          (2, 7) -> pure ApplyQueryAllMK
          (3, 8) -> ApplyQuerySomeMK <$> fromCBOR
          o      -> fail $ "decode @ApplyMapKind SQueryMK, " <> show o
    where
      smk = sMapKind @mk

      decodeArityAndTag :: Int -> Word8 -> CBOR.Decoder s ()
      decodeArityAndTag len tag = do
        len' <- CBOR.decodeListLen
        tag' <- CBOR.decodeWord8
        when
          (len /= 1 + len' || tag /= tag')
          (fail $ "decode @ApplyMapKind " <> show (smk, len', tag'))

showsApplyMapKind :: (Show k, Show v) => ApplyMapKind mk k v -> ShowS
showsApplyMapKind = \case
    ApplyEmptyMK                -> showString "ApplyEmptyMK"
    ApplyKeysMK keys            -> showParen True $ showString "ApplyKeysMK " . shows keys
    ApplyValuesMK values        -> showParen True $ showString "ApplyValuesMK " . shows values
    ApplyTrackingMK values diff -> showParen True $ showString "ApplyTrackingMK " . shows values . showString " " . shows diff
    ApplyDiffMK diff            -> showParen True $ showString "ApplyDiffMK " . shows diff
    ApplySeqDiffMK sq           -> showParen True $ showString "ApplySeqDiffMK " . shows sq
    ApplyRewoundMK rew          -> showParen True $ showString "ApplyRewoundMK " . shows rew

    ApplyQueryAllMK       -> showParen True $ showString "ApplyQueryAllMK"
    ApplyQuerySomeMK keys -> showParen True $ showString "ApplyQuerySomeMK " . shows keys

data instance Sing (mk :: MapKind) :: Type where
  SEmptyMK    :: Sing EmptyMK
  SKeysMK     :: Sing KeysMK
  SValuesMK   :: Sing ValuesMK
  STrackingMK :: Sing TrackingMK
  SDiffMK     :: Sing DiffMK
  SSeqDiffMK  :: Sing SeqDiffMK
  SRewoundMK  :: Sing RewoundMK
  SQueryMK    :: Sing QueryMK

type SMapKind = Sing :: MapKind -> Type

instance SingI EmptyMK    where sing = SEmptyMK
instance SingI KeysMK     where sing = SKeysMK
instance SingI ValuesMK   where sing = SValuesMK
instance SingI TrackingMK where sing = STrackingMK
instance SingI DiffMK     where sing = SDiffMK
instance SingI SeqDiffMK  where sing = SSeqDiffMK
instance SingI RewoundMK  where sing = SRewoundMK
instance SingI QueryMK    where sing = SQueryMK

sMapKind :: SingI mk => SMapKind mk
sMapKind = sing

sMapKind' :: SingI mk => proxy mk -> SMapKind mk
sMapKind' _ = sMapKind

toSMapKind :: ApplyMapKind mk k v -> SMapKind mk
toSMapKind = \case
    ApplyEmptyMK{}     -> SEmptyMK
    ApplyKeysMK{}      -> SKeysMK
    ApplyValuesMK{}    -> SValuesMK
    ApplyTrackingMK{}  -> STrackingMK
    ApplyDiffMK{}      -> SDiffMK
    ApplySeqDiffMK{}   -> SSeqDiffMK
    ApplyRewoundMK{}   -> SRewoundMK

    ApplyQueryAllMK{}  -> SQueryMK
    ApplyQuerySomeMK{} -> SQueryMK

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
    SQueryMK    -> "SQueryMK"

deriving via OnlyCheckWhnfNamed "Sing @MapKind" (Sing (mk :: MapKind)) instance NoThunks (Sing mk)

-- TODO include a tag, for some self-description
instance ToCBOR (Sing EmptyMK)    where toCBOR SEmptyMK    = CBOR.encodeNull
instance ToCBOR (Sing KeysMK)     where toCBOR SKeysMK     = CBOR.encodeNull
instance ToCBOR (Sing ValuesMK)   where toCBOR SValuesMK   = CBOR.encodeNull
instance ToCBOR (Sing TrackingMK) where toCBOR STrackingMK = CBOR.encodeNull
instance ToCBOR (Sing DiffMK)     where toCBOR SDiffMK     = CBOR.encodeNull
instance ToCBOR (Sing SeqDiffMK)  where toCBOR SSeqDiffMK  = CBOR.encodeNull
instance ToCBOR (Sing RewoundMK)  where toCBOR SRewoundMK  = CBOR.encodeNull
instance ToCBOR (Sing QueryMK)    where toCBOR SQueryMK    = CBOR.encodeNull

-- TODO include a tag, for some self-description
instance FromCBOR (Sing EmptyMK)    where fromCBOR = SEmptyMK    <$ CBOR.decodeNull
instance FromCBOR (Sing KeysMK)     where fromCBOR = SKeysMK     <$ CBOR.decodeNull
instance FromCBOR (Sing ValuesMK)   where fromCBOR = SValuesMK   <$ CBOR.decodeNull
instance FromCBOR (Sing TrackingMK) where fromCBOR = STrackingMK <$ CBOR.decodeNull
instance FromCBOR (Sing DiffMK)     where fromCBOR = SDiffMK     <$ CBOR.decodeNull
instance FromCBOR (Sing SeqDiffMK)  where fromCBOR = SSeqDiffMK  <$ CBOR.decodeNull
instance FromCBOR (Sing RewoundMK)  where fromCBOR = SRewoundMK  <$ CBOR.decodeNull
instance FromCBOR (Sing QueryMK)    where fromCBOR = SQueryMK    <$ CBOR.decodeNull

calculateDifference ::
     Ord k
  => ApplyMapKind ValuesMK k v
  -> ApplyMapKind ValuesMK k v
  -> ApplyMapKind TrackingMK k v
calculateDifference (ApplyValuesMK before) (ApplyValuesMK after) =
    ApplyTrackingMK after (differenceUtxoValues before after)

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
  stowLedgerTables     :: l ValuesMK -> l EmptyMK
  unstowLedgerTables   :: l EmptyMK  -> l ValuesMK
  -- | When we deserialize a snapshot from the disk, said snapshot will be
  -- @ExtLedgerState blk EmptyMK@ and we will try to unstow it to get the legacy
  -- style ledger state. However, if we run without the legacy database, it will
  -- be the case that the serialized ledger state is indeed deprived from a
  -- UTxO, and unstowing it will result in an empty UTxO in the legacy database.
  --
  -- This function should check that the UTxO inside the ledger state is not empty.
  isCandidateForUnstow :: l EmptyMK -> Bool

-- | True if and only if all tables are non-empty
--
-- TODO reconsider each occurrences of this as soon as we add a second table
-- beyond the UTxO map
isCandidateForUnstowDefault ::
     (TableStuff l, StowableLedgerTables l)
  => l EmptyMK -> Bool
isCandidateForUnstowDefault =
      getAll
    . foldLedgerTables (All . not . nullValues)
    . projectLedgerTables
    . unstowLedgerTables
  where
    nullValues :: ApplyMapKind ValuesMK k v -> Bool
    nullValues (ApplyValuesMK (UtxoValues vs)) = Map.null vs

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
        . showCommaSpace . showString "changelogDiffs = "           . showsLedgerState sMapKind changelogDiffs
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
      $ showString "DbChangelogState " . showsLedgerState sMapKind x

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
         Ord k
      => ApplyMapKind SeqDiffMK k v
      -> (ApplyMapKind SeqDiffMK k v, ApplyMapKind SeqDiffMK k v)
    split (ApplySeqDiffMK sq) =
        bimap ApplySeqDiffMK ApplySeqDiffMK
      $ splitAtSeqUtxoDiff (AS.length imm) sq

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

prefixDbChangelog ::
     ( HasHeader blk
     , HeaderHash blk ~ HeaderHash (l EmptyMK)
     , GetTip (l EmptyMK)
     , TableStuff l
     )
  => Point blk -> DbChangelog l -> Maybe (DbChangelog l)
prefixDbChangelog pt dblog = do
    let vol = changelogVolatileStates
    vol' <-
      AS.rollback
        (pointSlot pt)
        ((== pt) . castPoint . getTip . unDbChangelogState . either id id)
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
     Ord k
  => Int -> ApplyMapKind SeqDiffMK k v -> ApplyMapKind SeqDiffMK k v
trunc n (ApplySeqDiffMK sq) =
  ApplySeqDiffMK $ fst $ splitAtFromEndSeqUtxoDiff n sq

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
