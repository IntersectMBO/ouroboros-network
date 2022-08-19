{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DeriveTraversable     #-}
{-# LANGUAGE DerivingVia           #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE InstanceSigs          #-}
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
    FlipGetTip
  , GetTip (..)
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
  , noNewTickingDiffs
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
  , ApplyMapKind
  , ApplyMapKind' (..)
  , MapKind
  , SMapKind
  , Sing (..)
  , emptyAppliedMK
  , mapValuesAppliedMK
  , sMapKind
  , sMapKind'
  , showsApplyMapKind
  , toSMapKind
    -- *** Mediators
  , CodecMK (..)
  , DiffMK
  , EmptyMK
  , IsApplyMapKind
  , KeysMK
  , NameMK (..)
  , QueryMK
  , SeqDiffMK
  , TrackingMK
  , UnApplyMapKind
  , ValuesMK
    -- ** Queries
  , DiskLedgerView (..)
  , FootprintL (..)
    -- ** Convenience aliases
  , applyLedgerTablesDiffs
  , applyLedgerTablesDiffsTicked
  , attachAndApplyDiffsTicked
  , calculateAdditions
  , calculateDifference
  , calculateDifferenceTicked
  , emptyLedgerTables
  , forgetLedgerTables
  , forgetLedgerTablesDiffs
  , forgetLedgerTablesDiffsTicked
  , forgetLedgerTablesTicked
  , forgetLedgerTablesValues
  , polyEmptyLedgerTables
  , prependLedgerTablesDiffs
  , prependLedgerTablesDiffsFromTicked
  , prependLedgerTablesDiffsRaw
  , prependLedgerTablesDiffsTicked
  , prependLedgerTablesTrackingDiffs
  , reapplyTrackingTicked
    -- ** Special classes of ledger states
  , ExtractLedgerTables (..)
  , IgnoresMapKind (..)
  , IgnoresMapKindTicked (..)
  , IgnoresTables (..)
  , StowableLedgerTables (..)
    -- ** Serialization
  , SufficientSerializationForAnyBackingStore (..)
  , valuesMKDecoder
  , valuesMKEncoder
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
    -- * Exported only for testing
  , rawApplyDiffs
  , rawCalculateDifference
    -- * WithLedgerTables
  , IsSwitchLedgerTables
  , LedgerStateKindWithTables
  , LedgerTables (..)
  , SwitchLedgerTables (..)
  , findOutWT
  , rf
  ) where

import qualified Codec.CBOR.Decoding as CBOR
import qualified Codec.CBOR.Encoding as CBOR
import qualified Control.Exception as Exn
import           Control.Monad (when)
import           Data.Bifunctor (bimap)
import           Data.Kind (Type)
import qualified Data.Map as Map
import           Data.Monoid (Sum (..))
import           Data.Proxy
import           Data.Typeable (Typeable)
import           Data.Word (Word8)
import           GHC.Generics (Generic)
import           GHC.Show (showCommaSpace, showSpace)
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

type instance HeaderHash (Ticked  l)         = HeaderHash l
type instance HeaderHash (Ticked1 (l wt))    = HeaderHash l
type instance HeaderHash (Ticked1 (l wt) mk) = HeaderHash l

getTipHash :: GetTip l => l -> ChainHash l
getTipHash = pointHash . getTip

getTipSlot :: GetTip l => l -> WithOrigin SlotNo
getTipSlot = pointSlot . getTip

class (GetTip (l wt mk), GetTip (Ticked1 (l wt) mk)) => FlipGetTip l mk wt
instance (GetTip (l wt mk), GetTip (Ticked1 (l wt) mk)) => FlipGetTip l mk wt

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

class ShowLedgerState (l :: LedgerStateKindWithTables) where
  showsLedgerState :: SMapKind mk -> l wt (ApplyMapKind' mk) -> ShowS

{-------------------------------------------------------------------------------
  Definition of a ledger independent of a choice of block
-------------------------------------------------------------------------------}

-- | Static environment required for the ledger
type family LedgerCfg (l :: LedgerStateKindWithTables) :: Type

class ( -- Requirements on the ledger state itself
        -- Requirements on 'LedgerCfg'
        NoThunks (LedgerCfg l)
        -- Requirements on 'LedgerErr'
      , Show     (LedgerErr l)
      , Eq       (LedgerErr l)
      , NoThunks (LedgerErr l)
      ) => IsLedger (l :: LedgerStateKindWithTables) where
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
  -- NOTE: The 'IsApplyMapKind' constraint is here for the same reason it's on
  -- 'projectLedgerTables'
  applyChainTickLedgerResult ::
       IsSwitchLedgerTables wt
    => LedgerCfg l
    -> SlotNo
    -> l wt EmptyMK
    -> LedgerResult l (Ticked1 (l wt) DiffMK)

-- | 'lrResult' after 'applyChainTickLedgerResult'
applyChainTick ::
     (IsLedger l, IsSwitchLedgerTables wt)
  => LedgerCfg l
  -> SlotNo
  -> l wt EmptyMK
  -> Ticked1 (l wt) DiffMK
applyChainTick = lrResult ..: applyChainTickLedgerResult

{-------------------------------------------------------------------------------
 [Ticked]TableStuff
-------------------------------------------------------------------------------}

-- | When applying a block that is not on an era transition, ticking won't
-- generate new values, so this function can be used to wrap the call to the
-- ledger rules that perform the tick.
noNewTickingDiffs ::
     TableStuff l wt
  => l wt any
  -> l wt DiffMK
noNewTickingDiffs l = withLedgerTables l polyEmptyLedgerTables

-- for LedgerState HardrfForkBlock but we will not (at least ast first) have a
-- compositional LedgerTables instance for HardForkBlock.
class TableStuff (l :: LedgerStateKindWithTables) (wt :: SwitchLedgerTables) where

  data family LedgerTables l wt :: LedgerStateKind

  -- | Some values of @l mk@ do not determine @mk@, hence the 'SingI' constraint.
  --
  -- If it were always the case that @l mk@ not determing @mk@ implies
  -- @LedgerTables l mk@ also does not determine @mk@, then we would not need
  -- the 'SingI' constraint. Unfortunately, that is not always the case. The
  -- example we have found in our prototype UTxO HD implementat is that a Byron
  -- ledger state does not determine @mk@, but the Cardano ledger tables do.
  projectLedgerTables :: IsApplyMapKind mk => l wt mk -> LedgerTables l wt mk

  -- | Overwrite the tables in some ledger state.
  --
  -- The contents of the tables should not be /younger/ than the content of the
  -- ledger state. In particular, for a
  -- 'Ouroboros.Consensus.HardFork.Combinator.Basics.HardForkBlock' ledger, the
  -- tables argument should not contain any data from eras that succeed the
  -- current era of the ledger state argument.
  --
  -- TODO: reconsider the name: don't we use 'withX' in the context of bracket like functions?
  --
  -- TODO: This 'IsApplyMapKind' constraint is necessary because the
  -- 'CardanoBlock' instance uses 'projectLedgerTables'.
  withLedgerTables :: IsApplyMapKind mk => l wt any -> LedgerTables l wt mk -> l wt mk

  pureLedgerTables ::
       (forall k v.
            Ord k
         => mk k v
       )
    -> LedgerTables l wt mk

  mapLedgerTables ::
       (forall k v.
            Ord k
         => mk1 k v
         -> mk2 k v
       )
    -> LedgerTables l wt mk1
    -> LedgerTables l wt mk2

  traverseLedgerTables ::
       Applicative f
    => (forall k v . Ord k
        =>    mk1 k v
        -> f (mk2 k v)
       )
    ->    LedgerTables l wt mk1
    -> f (LedgerTables l wt mk2)

  zipLedgerTables ::
       (forall k v.
            Ord k
         => mk1 k v
         -> mk2 k v
         -> mk3 k v
       )
    -> LedgerTables l wt mk1
    -> LedgerTables l wt mk2
    -> LedgerTables l wt mk3

  zipLedgerTables2 ::
       (forall k v.
            Ord k
         => mk1 k v
         -> mk2 k v
         -> mk3 k v
         -> mk4 k v
       )
    -> LedgerTables l wt mk1
    -> LedgerTables l wt mk2
    -> LedgerTables l wt mk3
    -> LedgerTables l wt mk4

  zipLedgerTablesA ::
       Applicative f
    => (forall k v.
            Ord k
         => mk1 k v
         -> mk2 k v
         -> f (mk3 k v)
       )
    -> LedgerTables l wt mk1
    -> LedgerTables l wt mk2
    -> f (LedgerTables l wt mk3)

  zipLedgerTables2A ::
       Applicative f
    => (forall k v.
            Ord k
         => mk1 k v
         -> mk2 k v
         -> mk3 k v
         -> f (mk4 k v)
       )
    -> LedgerTables l wt mk1
    -> LedgerTables l wt mk2
    -> LedgerTables l wt mk3
    -> f (LedgerTables l wt mk4)

  foldLedgerTables ::
       Monoid m
    => (forall k v.
            Ord k
         => mk k v
         -> m
       )
    -> LedgerTables l wt mk
    -> m

  foldLedgerTables2 ::
       Monoid m
    => (forall k v.
           Ord k
        => mk1 k v
        -> mk2 k v
        -> m
       )
    -> LedgerTables l wt mk1
    -> LedgerTables l wt mk2
    -> m

  namesLedgerTables :: LedgerTables l wt NameMK

overLedgerTables ::
     (TableStuff l wt, IsApplyMapKind mk1, IsApplyMapKind mk2)
  => (LedgerTables l wt mk1 -> LedgerTables l wt mk2)
  -> l wt mk1
  -> l wt mk2
overLedgerTables f l = withLedgerTables l $ f $ projectLedgerTables l

mapOverLedgerTables ::
     (TableStuff l wt, IsApplyMapKind mk1, IsApplyMapKind mk2)
  => (forall k v.
          Ord k
       => mk1 k v
       -> mk2 k v
     )
  -> l wt mk1
  -> l wt mk2
mapOverLedgerTables f = overLedgerTables $ mapLedgerTables f

zipOverLedgerTables ::
     (TableStuff l wt, IsApplyMapKind mk1, IsApplyMapKind mk3)
  => (forall k v.
          Ord k
       => mk1 k v
       -> mk2 k v
       -> mk3 k v
     )
  ->              l wt mk1
  -> LedgerTables l wt mk2
  ->              l wt mk3
zipOverLedgerTables f l tables2 =
    overLedgerTables
      (\tables1 -> zipLedgerTables f tables1 tables2)
      l

-- Separate so that we can have a 'TableStuff' instance for 'Ticked1' without
-- involving double-ticked types.
class TableStuff l wt => TickedTableStuff (l :: LedgerStateKindWithTables) (wt :: SwitchLedgerTables) where
  -- | NOTE: The 'IsApplyMapKind mk2' constraint is here for the same reason
  -- it's on 'projectLedgerTables'
  projectLedgerTablesTicked :: IsApplyMapKind mk => Ticked1 (l wt) mk  -> LedgerTables l wt mk
  -- | NOTE: The 'IsApplyMapKind mk2' constraint is here for the same reason
  -- it's on 'withLedgerTables'
  withLedgerTablesTicked    :: IsApplyMapKind mk => Ticked1 (l wt) any -> LedgerTables l wt mk -> Ticked1 (l wt) mk

overLedgerTablesTicked ::
     (TickedTableStuff l wt, IsApplyMapKind mk1, IsApplyMapKind mk2)
  => (LedgerTables l wt mk1 -> LedgerTables l wt mk2)
  -> Ticked1 (l wt) mk1
  -> Ticked1 (l wt) mk2
overLedgerTablesTicked f l =
    withLedgerTablesTicked l $ f $ projectLedgerTablesTicked l

mapOverLedgerTablesTicked ::
     (TickedTableStuff l wt, IsApplyMapKind mk1, IsApplyMapKind mk2)
  => (forall k v.
         Ord k
      => mk1 k v
      -> mk2 k v
     )
  -> Ticked1 (l wt) mk1
  -> Ticked1 (l wt) mk2
mapOverLedgerTablesTicked f = overLedgerTablesTicked $ mapLedgerTables f

zipOverLedgerTablesTicked ::
     (TickedTableStuff l wt, IsApplyMapKind mk1, IsApplyMapKind mk3)
  => (forall k v.
         Ord k
      => mk1 k v
      -> mk2 k v
      -> mk3 k v
     )
  -> Ticked1      (l wt) mk1
  -> LedgerTables l wt mk2
  -> Ticked1      (l wt) mk3
zipOverLedgerTablesTicked f l tables2 =
    overLedgerTablesTicked
      (\tables1 -> zipLedgerTables f tables1 tables2)
      l

{-------------------------------------------------------------------------------
  Serialization Codecs
-------------------------------------------------------------------------------}

-- | This class provides a 'CodecMK' that can be used to encode/decode keys and
-- values on @'LedgerTables l mk@
class SufficientSerializationForAnyBackingStore (l :: LedgerStateKindWithTables) (wt :: SwitchLedgerTables) where
  codecLedgerTables :: LedgerTables l wt CodecMK

-- | Default encoder of @'LedgerTables l ''ValuesMK'@ to be used by the
-- in-memory backing store.
valuesMKEncoder ::
     ( TableStuff l wt
     , SufficientSerializationForAnyBackingStore l wt
     )
  => LedgerTables l wt ValuesMK
  -> CBOR.Encoding
valuesMKEncoder tables =
       CBOR.encodeListLen (getSum (foldLedgerTables (\_ -> Sum 1) tables))
    <> foldLedgerTables2 go codecLedgerTables tables
  where
    go :: CodecMK k v -> ApplyMapKind ValuesMK k v -> CBOR.Encoding
    go (CodecMK encK encV _decK _decV) (ApplyValuesMK (UtxoValues m)) =
         CBOR.encodeMapLen (fromIntegral $ Map.size m)
      <> Map.foldMapWithKey (\k v -> encK k <> encV v) m

-- | Default encoder of @'LedgerTables l wt ValuesMK'@ to be used by the
-- in-memory backing store.
valuesMKDecoder ::
     ( TableStuff l wt
     , SufficientSerializationForAnyBackingStore l wt
     )
  => CBOR.Decoder s (LedgerTables l wt ValuesMK)
valuesMKDecoder = do
    numTables <- CBOR.decodeListLen
    if numTables == 0
      then
        return polyEmptyLedgerTables
      else do
        mapLen <- CBOR.decodeMapLen
        ret    <- traverseLedgerTables (go mapLen) codecLedgerTables
        Exn.assert (getSum (foldLedgerTables (\_ -> Sum 1) ret) == numTables)
          $ return ret
 where
  go :: Ord k
     => Int
     -> CodecMK k v
     -> CBOR.Decoder s (ApplyMapKind ValuesMK k v)
  go len (CodecMK _encK _encV decK decV) =
        ApplyValuesMK . UtxoValues . Map.fromList
    <$> sequence (replicate len ((,) <$> decK <*> decV))

{-------------------------------------------------------------------------------
  Convenience aliases
-------------------------------------------------------------------------------}

emptyLedgerTables ::
     TableStuff l wt
  => LedgerTables l wt EmptyMK
emptyLedgerTables = polyEmptyLedgerTables

-- | Empty values for every table
polyEmptyLedgerTables ::
  ( TableStuff l wt
  , IsApplyMapKind mk
  )
  => LedgerTables l wt mk
polyEmptyLedgerTables = pureLedgerTables $ emptyAppliedMK sMapKind

-- Forget all

forgetLedgerTables ::
     TableStuff l wt
  => l wt mk
  -> l wt EmptyMK
forgetLedgerTables l = withLedgerTables l emptyLedgerTables

forgetLedgerTablesTicked ::
     TickedTableStuff l wt
  => Ticked1 (l wt) mk
  -> Ticked1 (l wt) EmptyMK
forgetLedgerTablesTicked l = withLedgerTablesTicked l emptyLedgerTables

-- Forget values

rawForgetValues :: TrackingMK k v -> DiffMK k v
rawForgetValues (ApplyTrackingMK _values diff) = ApplyDiffMK diff

forgetLedgerTablesValues ::
     TableStuff l wt
  => l wt TrackingMK
  -> l wt DiffMK
forgetLedgerTablesValues = mapOverLedgerTables rawForgetValues

-- Forget diffs

rawForgetDiffs :: TrackingMK k v -> ValuesMK k v
rawForgetDiffs (ApplyTrackingMK values _diff) = ApplyValuesMK values

forgetLedgerTablesDiffs       ::       TableStuff l wt =>         (l wt) TrackingMK ->         (l wt) ValuesMK
forgetLedgerTablesDiffsTicked :: TickedTableStuff l wt => Ticked1 (l wt) TrackingMK -> Ticked1 (l wt) ValuesMK
forgetLedgerTablesDiffs       = mapOverLedgerTables rawForgetDiffs
forgetLedgerTablesDiffsTicked = mapOverLedgerTablesTicked rawForgetDiffs

-- Prepend diffs

rawPrependDiffs ::
     Ord k
  => DiffMK k v -- ^ Earlier differences
  -> DiffMK k v -- ^ Later differences
  -> DiffMK k v
rawPrependDiffs (ApplyDiffMK (UtxoDiff d1)) (ApplyDiffMK (UtxoDiff d2)) = ApplyDiffMK (UtxoDiff (d1 `Map.union` d2))

prependLedgerTablesDiffsRaw        ::       TableStuff l wt => LedgerTables  l wt  DiffMK ->          l wt  DiffMK ->          l wt  DiffMK
prependLedgerTablesDiffs           ::       TableStuff l wt =>               l wt  DiffMK ->          l wt  DiffMK ->          l wt  DiffMK
prependLedgerTablesDiffsFromTicked :: TickedTableStuff l wt => Ticked1      (l wt) DiffMK ->          l wt  DiffMK ->          l wt  DiffMK
prependLedgerTablesDiffsTicked     :: TickedTableStuff l wt =>               l wt  DiffMK -> Ticked1 (l wt) DiffMK -> Ticked1 (l wt) DiffMK
prependLedgerTablesDiffsRaw        = flip (zipOverLedgerTables rawPrependDiffs)
prependLedgerTablesDiffs           = prependLedgerTablesDiffsRaw . projectLedgerTables
prependLedgerTablesDiffsFromTicked = prependLedgerTablesDiffsRaw . projectLedgerTablesTicked
prependLedgerTablesDiffsTicked     = flip (zipOverLedgerTablesTicked rawPrependDiffs) . projectLedgerTables

-- Apply diffs

rawApplyDiffs ::
     Ord k
  => ValuesMK k v -- ^ Values to which differences are applied
  -> DiffMK   k v -- ^ Differences to apply
  -> ValuesMK k v
rawApplyDiffs (ApplyValuesMK vals) (ApplyDiffMK diffs) = ApplyValuesMK (forwardValues vals diffs)

applyLedgerTablesDiffs       ::       TableStuff l wt => l wt ValuesMK ->          l wt  DiffMK ->          l wt  ValuesMK
applyLedgerTablesDiffsTicked :: TickedTableStuff l wt => l wt ValuesMK -> Ticked1 (l wt) DiffMK -> Ticked1 (l wt) ValuesMK
applyLedgerTablesDiffs       = flip (zipOverLedgerTables       $ flip rawApplyDiffs) . projectLedgerTables
applyLedgerTablesDiffsTicked = flip (zipOverLedgerTablesTicked $ flip rawApplyDiffs) . projectLedgerTables

-- Calculate differences

rawCalculateDifference ::
     Ord k
  => ValuesMK   k v
  -> ValuesMK   k v
  -> TrackingMK k v
rawCalculateDifference (ApplyValuesMK before) (ApplyValuesMK after) = ApplyTrackingMK after (differenceUtxoValues before after)

calculateAdditions        ::       TableStuff l wt =>          l wt  ValuesMK ->                                     l wt  TrackingMK
calculateDifference       :: TickedTableStuff l wt => Ticked1 (l wt) ValuesMK ->          l wt  ValuesMK ->          l wt  TrackingMK
calculateDifferenceTicked :: TickedTableStuff l wt => Ticked1 (l wt) ValuesMK -> Ticked1 (l wt) ValuesMK -> Ticked1 (l wt) TrackingMK
calculateAdditions               after = zipOverLedgerTables       (flip rawCalculateDifference) after polyEmptyLedgerTables
calculateDifference       before after = zipOverLedgerTables       (flip rawCalculateDifference) after (projectLedgerTablesTicked before)
calculateDifferenceTicked before after = zipOverLedgerTablesTicked (flip rawCalculateDifference) after (projectLedgerTablesTicked before)

rawAttachAndApplyDiffs ::
     Ord k
  => DiffMK     k v
  -> ValuesMK   k v
  -> TrackingMK k v
rawAttachAndApplyDiffs (ApplyDiffMK d) (ApplyValuesMK v) =
  ApplyTrackingMK (forwardValues v d) d

-- | Replace the tables in the first parameter with the tables of the second
-- parameter after applying the differences in the first parameter to them
attachAndApplyDiffsTicked ::
     TickedTableStuff l wt
  => Ticked1 (l wt) DiffMK
  ->         l wt ValuesMK
  -> Ticked1 (l wt) TrackingMK
attachAndApplyDiffsTicked after before =
    zipOverLedgerTablesTicked rawAttachAndApplyDiffs after
  $ projectLedgerTables before

rawPrependTrackingDiffs ::
   Ord k
   => TrackingMK k v
   -> TrackingMK k v
   -> TrackingMK k v
rawPrependTrackingDiffs (ApplyTrackingMK v d2) (ApplyTrackingMK _v d1) =
  ApplyTrackingMK v (d1 <> d2)

-- | Mappend the differences in the ledger tables. Keep the ledger state of the
-- first one.
prependLedgerTablesTrackingDiffs ::
     TickedTableStuff l wt
  => Ticked1 (l wt) TrackingMK
  -> Ticked1 (l wt) TrackingMK
  -> Ticked1 (l wt) TrackingMK
prependLedgerTablesTrackingDiffs after before =
    zipOverLedgerTablesTicked rawPrependTrackingDiffs after
  $ projectLedgerTablesTicked before

rawReapplyTracking ::
     Ord k
  => TrackingMK k v
  -> ValuesMK   k v
  -> TrackingMK k v
rawReapplyTracking (ApplyTrackingMK _v d) (ApplyValuesMK v) =
  ApplyTrackingMK (forwardValues v d) d

-- | Replace the tables in the first parameter with the tables of the second
-- parameter after applying the differences in the first parameter to them
reapplyTrackingTicked ::
     TickedTableStuff l wt
  => Ticked1 (l wt) TrackingMK
  ->         l wt ValuesMK
  -> Ticked1 (l wt) TrackingMK
reapplyTrackingTicked after before =
    zipOverLedgerTablesTicked rawReapplyTracking after
  $ projectLedgerTables before

{-------------------------------------------------------------------------------
  With or without ledger tables
-------------------------------------------------------------------------------}

data SwitchLedgerTables = WithLedgerTables
                        | WithoutLedgerTables

type IsSwitchLedgerTables (wt :: SwitchLedgerTables) = (SingI wt, Typeable wt)

data instance Sing (sw :: SwitchLedgerTables) :: Type where
  SWithLedgerTables    :: Sing WithLedgerTables
  SWithoutLedgerTables :: Sing WithoutLedgerTables

findOutWT :: IsSwitchLedgerTables wt => proxy wt -> Sing wt
findOutWT _ = sing

instance SingI WithLedgerTables    where sing = SWithLedgerTables
instance SingI WithoutLedgerTables where sing = SWithoutLedgerTables

rf :: forall c wt ans. (c WithLedgerTables, c WithoutLedgerTables, IsSwitchLedgerTables wt)
 => Proxy c
  -> Proxy wt
  -> (c wt => ans)
  -> ans
rf _  _ f = case findOutWT (Proxy @wt) of
  SWithLedgerTables    -> f
  SWithoutLedgerTables -> f

instance IgnoresMapKind l => TableStuff l WithoutLedgerTables where

  data instance LedgerTables l WithoutLedgerTables mk = NoLedgerTables
    deriving (Eq, Show, Generic, NoThunks)

  projectLedgerTables  _                                               =      NoLedgerTables
  withLedgerTables     st _                                            = convertMapKind st
  pureLedgerTables     _                                               =      NoLedgerTables
  mapLedgerTables      _  NoLedgerTables                               =      NoLedgerTables
  traverseLedgerTables _  NoLedgerTables                               = pure NoLedgerTables
  zipLedgerTables      _  NoLedgerTables NoLedgerTables                =      NoLedgerTables
  zipLedgerTables2     _  NoLedgerTables NoLedgerTables NoLedgerTables =      NoLedgerTables
  zipLedgerTablesA     _  NoLedgerTables NoLedgerTables                = pure NoLedgerTables
  zipLedgerTables2A    _  NoLedgerTables NoLedgerTables NoLedgerTables = pure NoLedgerTables
  foldLedgerTables     _  NoLedgerTables                               = mempty
  foldLedgerTables2    _  NoLedgerTables NoLedgerTables                = mempty
  namesLedgerTables                                                    =      NoLedgerTables

instance (TableStuff l WithoutLedgerTables, IgnoresMapKindTicked l) => TickedTableStuff l WithoutLedgerTables where
  projectLedgerTablesTicked _ = NoLedgerTables
  withLedgerTablesTicked st _ = convertMapKindTicked st

instance SufficientSerializationForAnyBackingStore l WithoutLedgerTables where
  codecLedgerTables = NoLedgerTables

instance IgnoresMapKind l => StowableLedgerTables l WithoutLedgerTables where
  stowLedgerTables   = convertMapKind
  unstowLedgerTables = convertMapKind

{-------------------------------------------------------------------------------
  Concrete map kinds
-------------------------------------------------------------------------------}

type MapKind                   = {- key -} Type -> {- value -} Type -> Type
type LedgerStateKind           = MapKind -> Type
type LedgerStateKindWithTables = SwitchLedgerTables -> MapKind -> Type

data MapKind' = DiffMK'
              | EmptyMK'
              | KeysMK'
              | QueryMK'
              | SeqDiffMK'
              | TrackingMK'
              | ValuesMK'

type DiffMK     = ApplyMapKind' DiffMK'
type EmptyMK    = ApplyMapKind' EmptyMK'
type KeysMK     = ApplyMapKind' KeysMK'
type QueryMK    = ApplyMapKind' QueryMK'
type SeqDiffMK  = ApplyMapKind' SeqDiffMK'
type TrackingMK = ApplyMapKind' TrackingMK'
type ValuesMK   = ApplyMapKind' ValuesMK'

-- | A codec 'MapKind' that will be used to refer to @'LedgerTables' l CodecMK@
-- as the codecs that can encode every key and value in the @'LedgerTables' l
-- mk@.
data CodecMK k v = CodecMK
                     (k -> CBOR.Encoding)
                     (v -> CBOR.Encoding)
                     (forall s . CBOR.Decoder s k)
                     (forall s . CBOR.Decoder s v)

newtype NameMK k v = NameMK String

type ApplyMapKind mk = mk

data ApplyMapKind' :: MapKind' -> Type -> Type -> Type where
  ApplyDiffMK     :: !(UtxoDiff    k v)                    -> ApplyMapKind' DiffMK'       k v
  ApplyEmptyMK    ::                                          ApplyMapKind' EmptyMK'      k v
  ApplyKeysMK     :: !(UtxoKeys    k v)                    -> ApplyMapKind' KeysMK'       k v
  ApplySeqDiffMK  :: !(SeqUtxoDiff k v)                    -> ApplyMapKind' SeqDiffMK'    k v
  ApplyTrackingMK :: !(UtxoValues  k v) -> !(UtxoDiff k v) -> ApplyMapKind' TrackingMK'   k v
  ApplyValuesMK   :: !(UtxoValues  k v)                    -> ApplyMapKind' ValuesMK'     k v

  ApplyQueryAllMK  ::                    ApplyMapKind' QueryMK' k v
  ApplyQuerySomeMK :: !(UtxoKeys k v) -> ApplyMapKind' QueryMK' k v

emptyAppliedMK :: Ord k => SMapKind mk -> ApplyMapKind' mk k v
emptyAppliedMK = \case
    SEmptyMK    -> ApplyEmptyMK
    SKeysMK     -> ApplyKeysMK     emptyUtxoKeys
    SValuesMK   -> ApplyValuesMK   emptyUtxoValues
    STrackingMK -> ApplyTrackingMK emptyUtxoValues emptyUtxoDiff
    SDiffMK     -> ApplyDiffMK     emptyUtxoDiff
    SSeqDiffMK  -> ApplySeqDiffMK  emptySeqUtxoDiff
    SQueryMK    -> ApplyQuerySomeMK emptyUtxoKeys

instance Ord k => Semigroup (ApplyMapKind' KeysMK' k v) where
  ApplyKeysMK l <> ApplyKeysMK r = ApplyKeysMK (l <> r)

instance Ord k => Monoid (ApplyMapKind' KeysMK' k v) where
  mempty = ApplyKeysMK mempty

mapValuesAppliedMK :: Ord k => (v -> v') -> ApplyMapKind' mk k v ->  ApplyMapKind' mk k v'
mapValuesAppliedMK f = \case
  ApplyEmptyMK            -> ApplyEmptyMK
  ApplyKeysMK ks          -> ApplyKeysMK     (castUtxoKeys ks)
  ApplyValuesMK vs        -> ApplyValuesMK   (mapUtxoValues f vs)
  ApplyTrackingMK vs diff -> ApplyTrackingMK (mapUtxoValues f vs)     (mapUtxoDiff f diff)
  ApplyDiffMK diff        -> ApplyDiffMK     (mapUtxoDiff f diff)
  ApplySeqDiffMK diffs    -> ApplySeqDiffMK  (mapSeqUtxoDiff f diffs)

  ApplyQueryAllMK         -> ApplyQueryAllMK
  ApplyQuerySomeMK ks     -> ApplyQuerySomeMK (castUtxoKeys ks)

instance (Ord k, Eq v) => Eq (ApplyMapKind' mk k v) where
  ApplyEmptyMK          == _                     = True
  ApplyKeysMK   l       == ApplyKeysMK   r       = l == r
  ApplyValuesMK l       == ApplyValuesMK r       = l == r
  ApplyTrackingMK l1 l2 == ApplyTrackingMK r1 r2 = l1 == r1 && l2 == r2
  ApplyDiffMK l         == ApplyDiffMK r         = l == r
  ApplySeqDiffMK l      == ApplySeqDiffMK r      = l == r
  ApplyQueryAllMK       == ApplyQueryAllMK       = True
  ApplyQuerySomeMK l    == ApplyQuerySomeMK r    = l == r
  _                     == _                     = False

instance (Ord k, NoThunks k, NoThunks v) => NoThunks (ApplyMapKind' mk k v) where
  wNoThunks ctxt   = NoThunks.allNoThunks . \case
    ApplyEmptyMK            -> []
    ApplyKeysMK ks          -> [noThunks ctxt ks]
    ApplyValuesMK vs        -> [noThunks ctxt vs]
    ApplyTrackingMK vs diff -> [noThunks ctxt vs, noThunks ctxt diff]
    ApplyDiffMK diff        -> [noThunks ctxt diff]
    ApplySeqDiffMK diffs    -> [noThunks ctxt diffs]
    ApplyQueryAllMK         -> []
    ApplyQuerySomeMK ks     -> [noThunks ctxt ks]

  showTypeOf _ = "ApplyMapKind"

instance
     (Typeable mk, Ord k, ToCBOR k, ToCBOR v, SingI mk)
  => ToCBOR (ApplyMapKind' mk k v) where
  toCBOR = \case
      ApplyEmptyMK            -> encodeArityAndTag 0 []
      ApplyKeysMK ks          -> encodeArityAndTag 1 [toCBOR ks]
      ApplyValuesMK vs        -> encodeArityAndTag 2 [toCBOR vs]
      ApplyTrackingMK vs diff -> encodeArityAndTag 3 [toCBOR vs, toCBOR diff]
      ApplyDiffMK diff        -> encodeArityAndTag 4 [toCBOR diff]
      ApplySeqDiffMK diffs    -> encodeArityAndTag 5 [toCBOR diffs]
      ApplyQueryAllMK         -> encodeArityAndTag 7 []
      ApplyQuerySomeMK ks     -> encodeArityAndTag 7 [toCBOR ks]
    where
      encodeArityAndTag :: Word8 -> [CBOR.Encoding] -> CBOR.Encoding
      encodeArityAndTag tag xs =
           CBOR.encodeListLen (1 + toEnum (length xs))
        <> CBOR.encodeWord8 tag
        <> mconcat xs

instance
     (Typeable mk, Ord k, FromCBOR k, FromCBOR v, SingI mk)
  => FromCBOR (ApplyMapKind' mk k v) where
  fromCBOR = do
    case smk of
      SEmptyMK    -> decodeArityAndTag 0 0 *> (ApplyEmptyMK    <$  pure ())
      SKeysMK     -> decodeArityAndTag 1 1 *> (ApplyKeysMK     <$> fromCBOR)
      SValuesMK   -> decodeArityAndTag 1 2 *> (ApplyValuesMK   <$> fromCBOR)
      STrackingMK -> decodeArityAndTag 2 3 *> (ApplyTrackingMK <$> fromCBOR <*> fromCBOR)
      SDiffMK     -> decodeArityAndTag 1 4 *> (ApplyDiffMK     <$> fromCBOR)
      SSeqDiffMK  -> decodeArityAndTag 1 5 *> (ApplySeqDiffMK  <$> fromCBOR)
      SQueryMK    -> do
        len <- CBOR.decodeListLen
        tag <- CBOR.decodeWord8
        case (len, tag) of
          (2, 7) -> pure ApplyQueryAllMK
          (3, 8) -> ApplyQuerySomeMK <$> fromCBOR
          o      -> fail $ "decode @ApplyMapKind SQueryMK, " <> show o
    where
      smk = sMapKind @(ApplyMapKind' mk)

      decodeArityAndTag :: Int -> Word8 -> CBOR.Decoder s ()
      decodeArityAndTag len tag = do
        len' <- CBOR.decodeListLen
        tag' <- CBOR.decodeWord8
        -- @len@ here ought to match the @length xs@ in @encodeArityAndTag@ in
        -- the corresponding 'ToCBOR' instance, so we need to add one in order
        -- to match the length recorded in the CBOR stream (ie @len'@)
        --
        -- This use of 'when' corresponds to the use of 'decodeListLenOf'
        -- throughout most of our CBOR decoders: it catches encoder/decoder
        -- mismatches.
        when
          (1 + len /= len' || tag /= tag')
          (fail $ "decode @ApplyMapKind " <> show (smk, len, tag, len', tag'))

showsApplyMapKind :: (Show k, Show v) => ApplyMapKind' mk k v -> ShowS
showsApplyMapKind = \case
    ApplyEmptyMK                -> showString "ApplyEmptyMK"
    ApplyKeysMK keys            -> showParen True $ showString "ApplyKeysMK " . shows keys
    ApplyValuesMK values        -> showParen True $ showString "ApplyValuesMK " . shows values
    ApplyTrackingMK values diff -> showParen True $ showString "ApplyTrackingMK " . shows values . showString " " . shows diff
    ApplyDiffMK diff            -> showParen True $ showString "ApplyDiffMK " . shows diff
    ApplySeqDiffMK sq           -> showParen True $ showString "ApplySeqDiffMK " . shows sq

    ApplyQueryAllMK       -> showParen True $ showString "ApplyQueryAllMK"
    ApplyQuerySomeMK keys -> showParen True $ showString "ApplyQuerySomeMK " . shows keys

data instance Sing (mk :: MapKind') :: Type where
  SEmptyMK    :: Sing EmptyMK'
  SKeysMK     :: Sing KeysMK'
  SValuesMK   :: Sing ValuesMK'
  STrackingMK :: Sing TrackingMK'
  SDiffMK     :: Sing DiffMK'
  SSeqDiffMK  :: Sing SeqDiffMK'
  SQueryMK    :: Sing QueryMK'

type SMapKind = Sing :: MapKind' -> Type

type family UnApplyMapKind (mk :: MapKind) :: MapKind' where
  UnApplyMapKind (ApplyMapKind' mk') = mk'

type IsApplyMapKind mk = (mk ~ ApplyMapKind' (UnApplyMapKind mk), SingI (UnApplyMapKind mk), Typeable mk)

instance SingI EmptyMK'    where sing = SEmptyMK
instance SingI KeysMK'     where sing = SKeysMK
instance SingI ValuesMK'   where sing = SValuesMK
instance SingI TrackingMK' where sing = STrackingMK
instance SingI DiffMK'     where sing = SDiffMK
instance SingI SeqDiffMK'  where sing = SSeqDiffMK
instance SingI QueryMK'    where sing = SQueryMK

sMapKind :: IsApplyMapKind mk => SMapKind (UnApplyMapKind mk)
sMapKind = sing

sMapKind' :: IsApplyMapKind mk => proxy mk -> SMapKind (UnApplyMapKind mk)
sMapKind' _ = sMapKind

toSMapKind :: ApplyMapKind' mk k v -> SMapKind mk
toSMapKind = \case
    ApplyEmptyMK{}     -> SEmptyMK
    ApplyKeysMK{}      -> SKeysMK
    ApplyValuesMK{}    -> SValuesMK
    ApplyTrackingMK{}  -> STrackingMK
    ApplyDiffMK{}      -> SDiffMK
    ApplySeqDiffMK{}   -> SSeqDiffMK

    ApplyQueryAllMK{}  -> SQueryMK
    ApplyQuerySomeMK{} -> SQueryMK

instance Eq (Sing (mk :: MapKind')) where
  _ == _ = True

instance Show (Sing (mk :: MapKind')) where
  show = \case
    SEmptyMK    -> "SEmptyMK"
    SKeysMK     -> "SKeysMK"
    SValuesMK   -> "SValuesMK"
    STrackingMK -> "STrackingMK"
    SDiffMK     -> "SDiffMK"
    SSeqDiffMK  -> "SSeqDiffMK"
    SQueryMK    -> "SQueryMK"

deriving via OnlyCheckWhnfNamed "Sing @MapKind'" (Sing (mk :: MapKind')) instance NoThunks (Sing mk)

-- TODO include a tag, for some self-description
instance ToCBOR (Sing EmptyMK')    where toCBOR SEmptyMK    = CBOR.encodeNull
instance ToCBOR (Sing KeysMK')     where toCBOR SKeysMK     = CBOR.encodeNull
instance ToCBOR (Sing ValuesMK')   where toCBOR SValuesMK   = CBOR.encodeNull
instance ToCBOR (Sing TrackingMK') where toCBOR STrackingMK = CBOR.encodeNull
instance ToCBOR (Sing DiffMK')     where toCBOR SDiffMK     = CBOR.encodeNull
instance ToCBOR (Sing SeqDiffMK')  where toCBOR SSeqDiffMK  = CBOR.encodeNull
instance ToCBOR (Sing QueryMK')    where toCBOR SQueryMK    = CBOR.encodeNull

-- TODO include a tag, for some self-description
instance FromCBOR (Sing EmptyMK')    where fromCBOR = SEmptyMK    <$ CBOR.decodeNull
instance FromCBOR (Sing KeysMK')     where fromCBOR = SKeysMK     <$ CBOR.decodeNull
instance FromCBOR (Sing ValuesMK')   where fromCBOR = SValuesMK   <$ CBOR.decodeNull
instance FromCBOR (Sing TrackingMK') where fromCBOR = STrackingMK <$ CBOR.decodeNull
instance FromCBOR (Sing DiffMK')     where fromCBOR = SDiffMK     <$ CBOR.decodeNull
instance FromCBOR (Sing SeqDiffMK')  where fromCBOR = SSeqDiffMK  <$ CBOR.decodeNull
instance FromCBOR (Sing QueryMK')    where fromCBOR = SQueryMK    <$ CBOR.decodeNull

{-------------------------------------------------------------------------------
  Link block to its ledger
-------------------------------------------------------------------------------}

-- | Ledger state associated with a block
data family LedgerState blk :: LedgerStateKindWithTables

type instance HeaderHash (LedgerState blk)       = HeaderHash blk
type instance HeaderHash (LedgerState blk wt)    = HeaderHash blk
type instance HeaderHash (LedgerState blk wt mk) = HeaderHash blk

instance StandardHash blk => StandardHash (LedgerState blk)
instance StandardHash blk => StandardHash (LedgerState blk wt)
instance StandardHash blk => StandardHash (LedgerState blk wt mk)

type LedgerConfig      blk       = LedgerCfg (LedgerState blk)
type LedgerError       blk       = LedgerErr (LedgerState blk)
type TickedLedgerState blk wt mk = Ticked1   (LedgerState blk wt) mk

{-------------------------------------------------------------------------------
  UTxO HD stubs
-------------------------------------------------------------------------------}

-- | Monadic functions used to query this this block type's 'LargeL' ledger
-- states, which typically involve accessing disk.
data DiskLedgerView m l wt =
    DiskLedgerView
      !(l wt EmptyMK)
      (LedgerTables l wt KeysMK -> m (LedgerTables l wt ValuesMK))
      (RangeQuery (LedgerTables l wt KeysMK) -> m (LedgerTables l wt ValuesMK))   -- TODO will be unacceptably coarse once we have multiple tables
      (m ())

{-------------------------------------------------------------------------------
  Special classes of ledger states

  TODO if having such class(es) make sense but only in the context of testing
  code we should move this to the appropriate module.
-------------------------------------------------------------------------------}

-- | If the ledger state is always in memory, then l mk will be isomorphic to l
-- mk' for all mk, mk'. As a result, we can convert between ledger states
-- indexed by different map kinds.
--
-- This class will only be implemented by blocks that don't use tables at all.
class IgnoresTables l where
  convertTables :: l wt mk -> l wt' mk'

-- | This ledger state has no tables when applied to 'WithoutLedgerTables'.
--
-- This class should be implemented by every block type.
class IgnoresMapKind l where
  convertMapKind :: l WithoutLedgerTables mk
                 -> l WithoutLedgerTables mk'

-- | This ledger state has no tables when applied to 'WithoutLedgerTables'.
--
-- This class should be implemented by every block type.
class IgnoresMapKindTicked l where
  convertMapKindTicked :: Ticked1 (l WithoutLedgerTables) mk
                       -> Ticked1 (l WithoutLedgerTables) mk'

-- | Some specific operations that modify the SwitchLedgerTables type.
class ExtractLedgerTables l where
  -- | The incoming ledger state on the protocol info as a genesis ledger state
  -- will come without ledger tables but with values inside. This function
  -- allows us to modify the type so that it accomodates for the case in which
  -- we want to run with tables.
  --
  -- In particular, if the first era doesn't have tables, this is basically a
  -- coercion.
  extractLedgerTables :: l WithoutLedgerTables ValuesMK
                      -> l WithLedgerTables    ValuesMK

  -- | For code that doesn't use the tables at all, this function can be used to
  -- just nuke away all the tables, avoiding another `wt` parameter on outer
  -- layers.
  destroyTables :: l WithLedgerTables    mk
                -> l WithoutLedgerTables mk

-- | This class allows us to move values from the LedgerTables into the ledger
-- state. As the ledger currently doesn't know about tables, this is the only
-- way to make things invisible to it.
class StowableLedgerTables l wt where
  stowLedgerTables     :: l wt ValuesMK -> l wt EmptyMK
  unstowLedgerTables   :: l wt EmptyMK  -> l wt ValuesMK

{-------------------------------------------------------------------------------
  Changelog
-------------------------------------------------------------------------------}

-- |
--
-- INVARIANT: the head of 'changelogImmutableStates' is the anchor of
-- 'changelogVolatileStates'.
data DbChangelog (l :: LedgerStateKindWithTables) (wt :: SwitchLedgerTables) = DbChangelog {
    changelogDiffAnchor      :: !(WithOrigin SlotNo)
  , changelogDiffs           :: !(LedgerTables l wt SeqDiffMK)
  , changelogImmutableStates ::
      !(AnchoredSeq
          (WithOrigin SlotNo)
          (DbChangelogState (l wt))
          (DbChangelogState (l wt))
       )
  , changelogVolatileStates  ::
      !(AnchoredSeq
          (WithOrigin SlotNo)
          (DbChangelogState (l wt))
          (DbChangelogState (l wt))
       )
  }
  deriving (Generic)

deriving instance (Eq       (LedgerTables l wt SeqDiffMK), Eq       (l wt EmptyMK)) => Eq       (DbChangelog l wt)
deriving instance (NoThunks (LedgerTables l wt SeqDiffMK), NoThunks (l wt EmptyMK)) => NoThunks (DbChangelog l wt)

instance (Show (DbChangelogState (l wt)), ShowLedgerState (LedgerTables l)) => Show (DbChangelog l wt) where
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

instance (ShowLedgerState l, IsSwitchLedgerTables wt) => Show (DbChangelogState (l wt)) where
  showsPrec p (DbChangelogState x) =
        showParen (p >= 11)
      $ showString "DbChangelogState " . showsLedgerState sMapKind x

instance GetTip (l EmptyMK) => AS.Anchorable (WithOrigin SlotNo) (DbChangelogState l) (DbChangelogState l) where
  asAnchor = id
  getAnchorMeasure _ = getTipSlot . unDbChangelogState

emptyDbChangeLog ::
     (TableStuff l wt, GetTip (l wt EmptyMK))
  => l wt EmptyMK -> DbChangelog l wt
emptyDbChangeLog anchor =
    DbChangelog {
        changelogDiffAnchor      = getTipSlot anchor
      , changelogDiffs           = pureLedgerTables (ApplySeqDiffMK emptySeqUtxoDiff)
      , changelogImmutableStates = AS.Empty (DbChangelogState anchor)
      , changelogVolatileStates  = AS.Empty (DbChangelogState anchor)
      }

extendDbChangelog ::
     (TableStuff l wt, GetTip (l wt EmptyMK))
  => DbChangelog l wt -> l wt DiffMK -> DbChangelog l wt
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

    l'         = forgetLedgerTables newState
    tablesDiff = projectLedgerTables newState

    slot = case getTipSlot l' of
      Origin -> error "impossible! extendDbChangelog"
      At s   -> s

    ext ::
         Ord k
      => SeqDiffMK k v
      -> DiffMK    k v
      -> SeqDiffMK k v
    ext (ApplySeqDiffMK sq) (ApplyDiffMK diff) =
      ApplySeqDiffMK $ extendSeqUtxoDiff sq slot diff

pruneVolatilePartDbChangelog ::
     GetTip (l wt EmptyMK)
  => SecurityParam -> DbChangelog l wt -> DbChangelog l wt
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

flushDbChangelog :: forall l wt.
     (GetTip (l wt EmptyMK), TableStuff l wt)
  => DbChangelogFlushPolicy
  -> DbChangelog l wt
  -> (DbChangelog l wt, DbChangelog l wt)
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
      => SeqDiffMK k v
      -> (SeqDiffMK k v, SeqDiffMK k v)
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
     , HeaderHash blk ~ HeaderHash (l wt EmptyMK)
     , GetTip (l wt EmptyMK)
     , TableStuff l wt
     )
  => Point blk -> DbChangelog l wt -> Maybe (DbChangelog l wt)
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
     (GetTip (l wt EmptyMK), TableStuff l wt)
  => DbChangelog l wt -> DbChangelog l wt
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
  => Int -> SeqDiffMK k v -> SeqDiffMK k v
trunc n (ApplySeqDiffMK sq) =
  ApplySeqDiffMK $ fst $ splitAtFromEndSeqUtxoDiff n sq

rollbackDbChangelog ::
     (GetTip (l wt EmptyMK), TableStuff l wt)
  => Int -> DbChangelog l wt -> DbChangelog l wt
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
     GetTip (l wt EmptyMK)
  => DbChangelog l wt -> WithOrigin SlotNo
youngestImmutableSlotDbChangelog =
      getTipSlot
    . either unDbChangelogState unDbChangelogState
    . AS.head
    . changelogImmutableStates
