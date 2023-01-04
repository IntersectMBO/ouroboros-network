{-# LANGUAGE ConstraintKinds    #-}
{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DefaultSignatures  #-}
{-# LANGUAGE DerivingVia        #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE PolyKinds          #-}
{-# LANGUAGE Rank2Types         #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies       #-}

-- | The Ledger Tables represent the portion of the data on disk that has been
-- pulled from disk and attached to the in-memory Ledger State or that will
-- eventually be written to disk.
--
-- With UTxO-HD and the split of the Ledger State into the in-memory part and
-- the on-disk part, this splitting was reflected in the new type parameter
-- added to the Ledger State, to which we refer as "the MapKind" or @mk@.
--
-- Every @'LedgerState'@ is associated with a @'LedgerTables'@ and they both
-- share the @mk@. They both are of kind @'LedgerStateKind'@. @'LedgerTables'@
-- is just a way to refer /only/ to a partial view of the on-disk data without
-- having the rest of the in-memory Ledger State in scope.
--
-- The @mk@ can be instantiated to anything that is map-like, i.e. that expects
-- two type parameters, the key and the value. In particular, we provide here
-- the usual structures that we expect to deal with, namely:
--
-- - @'ValuesMK'@: a map of keys to values
--
-- - @'KeysMK'@: a set of keys
--
-- - @'DiffMK'@: a map of keys to value differences
--
-- - @'TrackingMK'@: the product of both @'ValuesMK'@ and @'DiffMK'@
--
-- - @'SeqDiffMK'@: an efficient sequence of differences
--
-- - @'CodecMK'@: @CBOR@ encoder and decoder for the key and value
--
-- Using the @'TableStuff'@ class, we can manipulate the @mk@ on the Ledger
-- Tables, and we can extract Ledger Tables from a Ledger State which will share
-- the same @mk@, or we can replace the Ledger Tables associated to a particular
-- in-memory Ledger State.
--
-- The general idea is that we keep a @'SeqDiffMK'@ on the @'DbChangelog'@ and
-- by reading values from the backing store and then forwarding them through the
-- sequence of diffs, we obtain partial (or total but that would be pointlessly
-- inefficient) views on the Ledger State as if the data was not split (see
-- @'withBlockReadSets'@).
--
-- The @'DbChangelog'@ keeps also a sequence of @'LedgerState' blk 'EmptyMK'@
-- which represent only the in-memory side. Then when applying a Ledger rule on
-- top of a Ledger State, we:
--
-- 1. Take the appropriate in-memory Ledger State from the @'DbChangelog'@, (by
--    means of @'ledgerDbPrefix'@).
--
-- 2. Consult the disk for the values needed by the rule (for example if
--    applying a block, we only need the TxIns of the transactions in the block)
--    (see @'getBlockKeySets'@ and @'readDb'@).
--
-- 3. Forward the values read from the disk through the @'SeqDiffMK'@ up until
--    the desired Ledger State (see @'forwardValues'@).
--
-- 4. Associate the Ledger State from (1) and the forwarded tables of (3) and
--    then call the Ledger rule.
--
-- 5. Get back a @'DiffMK'@ with differences to the provided values that will
--    then have to be stored in the @'DbChangelog'@ (see the type of
--    @'applyBlockLedgerResult'@).
module Ouroboros.Consensus.Ledger.Tables (
    -- * Kinds
    LedgerStateKind
  , MapKind
    -- * Basic LedgerState classes
  , ShowLedgerState (..)
  , StowableLedgerTables (..)
  , TableStuff (..)
  , TickedTableStuff (..)
  , mapOverLedgerTables
  , mapOverLedgerTablesTicked
  , overLedgerTables
  , overLedgerTablesTicked
  , zipOverLedgerTables
  , zipOverLedgerTablesTicked
    -- * Concrete Ledger tables
  , ApplyMapKind
  , ApplyMapKind' (..)
  , showsApplyMapKind
  , CodecMK (..)
  , DiffMK
  , EmptyMK
  , IsMapKind (..)
  , KeysMK
  , NameMK (..)
  , QueryMK
  , SeqDiffMK
  , TrackingMK
  , ValuesMK
    -- * Serialization
  , SufficientSerializationForAnyBackingStore (..)
  , valuesMKDecoder
  , valuesMKEncoder
    -- * Special classes
  , InMemory (..)
  ) where

import qualified Codec.CBOR.Decoding as CBOR
import qualified Codec.CBOR.Encoding as CBOR
import qualified Control.Exception as Exn
import           Data.Kind (Type)
import qualified Data.Map as Map
import           Data.Monoid (Sum (..))
import           NoThunks.Class (NoThunks (..))
import qualified NoThunks.Class as NoThunks

import           Ouroboros.Consensus.Ticked

import           Ouroboros.Consensus.Storage.LedgerDB.HD.DiffSeq

{-------------------------------------------------------------------------------
  Basic LedgerState classes
-------------------------------------------------------------------------------}

class ShowLedgerState (l :: LedgerStateKind) where
  showsLedgerState :: l (ApplyMapKind' mk) -> ShowS

class ( ShowLedgerState (LedgerTables l)
      , Eq (l EmptyMK)
      , Eq (LedgerTables l DiffMK)
      , Eq (LedgerTables l ValuesMK)
      ) => TableStuff (l :: LedgerStateKind) where

  data family LedgerTables l :: LedgerStateKind

  -- | Extract the ledger tables from a ledger state
  --
  -- NOTE: This 'IsMapKind' constraint is necessary because the 'CardanoBlock'
  -- instance uses 'mapMK'.
  projectLedgerTables :: IsMapKind mk => l mk -> LedgerTables l mk

  -- | Overwrite the tables in some ledger state.
  --
  -- The contents of the tables should not be /younger/ than the content of the
  -- ledger state. In particular, for a
  -- 'Ouroboros.Consensus.HardFork.Combinator.Basics.HardForkBlock' ledger, the
  -- tables argument should not contain any data from eras that succeed the
  -- current era of the ledger state argument.
  --
  -- NOTE: This 'IsMapKind' constraint is necessary because the
  -- 'CardanoBlock' instance uses 'mapMK'.
  withLedgerTables :: IsMapKind mk => l any -> LedgerTables l mk -> l mk

  pureLedgerTables ::
       (forall k v.
            (Ord k, Eq v)
         => mk k v
       )
    -> LedgerTables l mk

  mapLedgerTables ::
       (forall k v.
            (Ord k, Eq v)
         => mk1 k v
         -> mk2 k v
       )
    -> LedgerTables l mk1
    -> LedgerTables l mk2

  traverseLedgerTables ::
       Applicative f
    => (forall k v .
           (Ord k, Eq v)
        =>    mk1 k v
        -> f (mk2 k v)
       )
    ->    LedgerTables l mk1
    -> f (LedgerTables l mk2)

  zipLedgerTables ::
       (forall k v.
            (Ord k, Eq v)
         => mk1 k v
         -> mk2 k v
         -> mk3 k v
       )
    -> LedgerTables l mk1
    -> LedgerTables l mk2
    -> LedgerTables l mk3

  zipLedgerTables2 ::
       (forall k v.
            (Ord k, Eq v)
         => mk1 k v
         -> mk2 k v
         -> mk3 k v
         -> mk4 k v
       )
    -> LedgerTables l mk1
    -> LedgerTables l mk2
    -> LedgerTables l mk3
    -> LedgerTables l mk4

  zipLedgerTablesA ::
       Applicative f
    => (forall k v.
            (Ord k, Eq v)
         => mk1 k v
         -> mk2 k v
         -> f (mk3 k v)
       )
    -> LedgerTables l mk1
    -> LedgerTables l mk2
    -> f (LedgerTables l mk3)

  zipLedgerTables2A ::
       Applicative f
    => (forall k v.
            (Ord k, Eq v)
         => mk1 k v
         -> mk2 k v
         -> mk3 k v
         -> f (mk4 k v)
       )
    -> LedgerTables l mk1
    -> LedgerTables l mk2
    -> LedgerTables l mk3
    -> f (LedgerTables l mk4)

  foldLedgerTables ::
       Monoid m
    => (forall k v.
            (Ord k, Eq v)
         => mk k v
         -> m
       )
    -> LedgerTables l mk
    -> m

  foldLedgerTables2 ::
       Monoid m
    => (forall k v.
           (Ord k, Eq v)
        => mk1 k v
        -> mk2 k v
        -> m
       )
    -> LedgerTables l mk1
    -> LedgerTables l mk2
    -> m

  namesLedgerTables :: LedgerTables l NameMK

overLedgerTables ::
     (TableStuff l, IsMapKind mk1, IsMapKind mk2)
  => (LedgerTables l mk1 -> LedgerTables l mk2)
  -> l mk1
  -> l mk2
overLedgerTables f l = withLedgerTables l $ f $ projectLedgerTables l

mapOverLedgerTables ::
     (TableStuff l, IsMapKind mk1, IsMapKind mk2)
  => (forall k v.
          (Ord k, Eq v)
       => mk1 k v
       -> mk2 k v
     )
  -> l mk1
  -> l mk2
mapOverLedgerTables f = overLedgerTables $ mapLedgerTables f

zipOverLedgerTables ::
     (TableStuff l, IsMapKind mk1, IsMapKind mk3)
  => (forall k v.
          (Ord k, Eq v)
       => mk1 k v
       -> mk2 k v
       -> mk3 k v
     )
  ->              l mk1
  -> LedgerTables l mk2
  ->              l mk3
zipOverLedgerTables f l tables2 =
    overLedgerTables
      (\tables1 -> zipLedgerTables f tables1 tables2)
      l

class TableStuff l => TickedTableStuff (l :: LedgerStateKind) where
  -- | NOTE: The 'IsMapKind' constraint is here for the same reason
  -- it's on 'projectLedgerTables'
  projectLedgerTablesTicked :: IsMapKind mk => Ticked1 l mk  -> LedgerTables l mk
  -- | NOTE: The 'IsMapKind' constraint is here for the same reason
  -- it's on 'withLedgerTables'
  withLedgerTablesTicked    :: IsMapKind mk => Ticked1 l any -> LedgerTables l mk -> Ticked1 l mk

overLedgerTablesTicked ::
     (TickedTableStuff l, IsMapKind mk1, IsMapKind mk2)
  => (LedgerTables l mk1 -> LedgerTables l mk2)
  -> Ticked1 l mk1
  -> Ticked1 l mk2
overLedgerTablesTicked f l =
    withLedgerTablesTicked l $ f $ projectLedgerTablesTicked l

mapOverLedgerTablesTicked ::
     (TickedTableStuff l, IsMapKind mk1, IsMapKind mk2)
  => (forall k v.
         (Ord k, Eq v)
      => mk1 k v
      -> mk2 k v
     )
  -> Ticked1 l mk1
  -> Ticked1 l mk2
mapOverLedgerTablesTicked f = overLedgerTablesTicked $ mapLedgerTables f

zipOverLedgerTablesTicked ::
     (TickedTableStuff l, IsMapKind mk1, IsMapKind mk3)
  => (forall k v.
         (Ord k, Eq v)
      => mk1 k v
      -> mk2 k v
      -> mk3 k v
     )
  -> Ticked1      l mk1
  -> LedgerTables l mk2
  -> Ticked1      l mk3
zipOverLedgerTablesTicked f l tables2 =
    overLedgerTablesTicked
      (\tables1 -> zipLedgerTables f tables1 tables2)
      l

class StowableLedgerTables (l :: LedgerStateKind) where
  stowLedgerTables     :: l ValuesMK -> l EmptyMK
  unstowLedgerTables   :: l EmptyMK  -> l ValuesMK

{-------------------------------------------------------------------------------
  Concrete ledger tables
-------------------------------------------------------------------------------}

type MapKind         = {- key -} Type -> {- value -} Type -> Type
type LedgerStateKind = MapKind -> Type

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
  ApplyDiffMK     :: !(Diff    k v)                -> ApplyMapKind' DiffMK'       k v
  ApplyEmptyMK    ::                                  ApplyMapKind' EmptyMK'      k v
  ApplyKeysMK     :: !(Keys    k v)                -> ApplyMapKind' KeysMK'       k v
  ApplySeqDiffMK  :: !(DiffSeq k v)                -> ApplyMapKind' SeqDiffMK'    k v
  ApplyTrackingMK :: !(Values  k v) -> !(Diff k v) -> ApplyMapKind' TrackingMK'   k v
  ApplyValuesMK   :: !(Values  k v)                -> ApplyMapKind' ValuesMK'     k v

  ApplyQueryAllMK  ::                ApplyMapKind' QueryMK' k v
  ApplyQuerySomeMK :: !(Keys k v) -> ApplyMapKind' QueryMK' k v

-- | A general interface to mapkinds.
--
-- In some cases where @mk@ is not specialised to a concrete mapkind like
-- @'ValuesMK'@, there are often still a number of operations that we can
-- perform for this @mk@ that make sense regardless of the concrete mapkind. For
-- example, we should always be able to map over a mapkind to change the type of
-- values that it contains. This class is an interface to mapkinds that provides
-- such common functions.
class IsMapKind (mk :: MapKind) where
  emptyMK :: forall k v. (Ord k, Eq v) => mk k v
  default emptyMK :: forall k v. Monoid (mk k v) => mk k v
  emptyMK = mempty

  mapMK :: forall k v v'. (Ord k, Eq v, Eq v') => (v -> v') -> mk k v -> mk k v'
  default mapMK :: forall k v v'. (Functor (mk k)) => (v -> v') -> mk k v -> mk k v'
  mapMK = fmap

instance IsMapKind EmptyMK where
  emptyMK = ApplyEmptyMK
  mapMK _ ApplyEmptyMK = ApplyEmptyMK

instance IsMapKind KeysMK where
  emptyMK = ApplyKeysMK mempty
  mapMK f (ApplyKeysMK ks) = ApplyKeysMK $ fmap f ks

instance IsMapKind ValuesMK where
  emptyMK = ApplyValuesMK mempty
  mapMK f (ApplyValuesMK vs) = ApplyValuesMK $ fmap f vs

instance IsMapKind TrackingMK where
  emptyMK = ApplyTrackingMK mempty mempty
  mapMK f (ApplyTrackingMK vs d) = ApplyTrackingMK (fmap f vs) (fmap f d)

instance IsMapKind DiffMK where
  emptyMK = ApplyDiffMK mempty

instance IsMapKind SeqDiffMK where
  emptyMK = ApplySeqDiffMK empty
  mapMK f (ApplySeqDiffMK ds) = ApplySeqDiffMK $ mapDiffSeq f ds

instance IsMapKind (ApplyMapKind' QueryMK') where
  emptyMK = ApplyQuerySomeMK mempty
  mapMK f qmk = case qmk of
    ApplyQuerySomeMK ks -> ApplyQuerySomeMK $ fmap f ks
    ApplyQueryAllMK     -> ApplyQueryAllMK

instance Ord k => Semigroup (ApplyMapKind' KeysMK' k v) where
  ApplyKeysMK l <> ApplyKeysMK r = ApplyKeysMK (l <> r)

instance Ord k => Monoid (ApplyMapKind' KeysMK' k v) where
  mempty = ApplyKeysMK mempty

instance Functor (DiffMK k) where
  fmap f (ApplyDiffMK d) = ApplyDiffMK $ fmap f d

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
    ApplyEmptyMK         -> []
    ApplyKeysMK ks       -> [noThunks ctxt ks]
    ApplyValuesMK vs     -> [noThunks ctxt vs]
    ApplyTrackingMK vs d -> [noThunks ctxt vs, noThunks ctxt d]
    ApplyDiffMK d        -> [noThunks ctxt d]
    ApplySeqDiffMK ds    -> [noThunks ctxt ds]
    ApplyQueryAllMK      -> []
    ApplyQuerySomeMK ks  -> [noThunks ctxt ks]

  showTypeOf _ = "ApplyMapKind"

showsApplyMapKind :: (Show k, Show v) => ApplyMapKind' mk k v -> ShowS
showsApplyMapKind = \case
    ApplyEmptyMK             -> showString "ApplyEmptyMK"
    ApplyKeysMK keys         -> showParen True $ showString "ApplyKeysMK " . shows keys
    ApplyValuesMK values     -> showParen True $ showString "ApplyValuesMK " . shows values
    ApplyTrackingMK values d -> showParen True $ showString "ApplyTrackingMK " . shows values . showString " " . shows d
    ApplyDiffMK d            -> showParen True $ showString "ApplyDiffMK " . shows d
    ApplySeqDiffMK sq        -> showParen True $ showString "ApplySeqDiffMK " . shows sq

    ApplyQueryAllMK       -> showParen True $ showString "ApplyQueryAllMK"
    ApplyQuerySomeMK keys -> showParen True $ showString "ApplyQuerySomeMK " . shows keys

instance (Show k, Show v) => Show (ApplyMapKind' mk k v) where
  show = flip showsApplyMapKind ""

{-------------------------------------------------------------------------------
  Serialization Codecs
-------------------------------------------------------------------------------}

-- | This class provides a 'CodecMK' that can be used to encode/decode keys and
-- values on @'LedgerTables' l mk@
class SufficientSerializationForAnyBackingStore (l :: LedgerStateKind) where
  codecLedgerTables :: LedgerTables l CodecMK

-- | Default encoder of @'LedgerTables' l ''ValuesMK'@ to be used by the
-- in-memory backing store.
valuesMKEncoder ::
     ( TableStuff l
     , SufficientSerializationForAnyBackingStore l
     )
  => LedgerTables l ValuesMK
  -> CBOR.Encoding
valuesMKEncoder tables =
       CBOR.encodeListLen (getSum (foldLedgerTables (\_ -> Sum 1) tables))
    <> foldLedgerTables2 go codecLedgerTables tables
  where
    go :: CodecMK k v -> ApplyMapKind ValuesMK k v -> CBOR.Encoding
    go (CodecMK encK encV _decK _decV) (ApplyValuesMK (Values m)) =
         CBOR.encodeMapLen (fromIntegral $ Map.size m)
      <> Map.foldMapWithKey (\k v -> encK k <> encV v) m

-- | Default encoder of @'LedgerTables' l ''ValuesMK'@ to be used by the
-- in-memory backing store.
--
-- TODO: we need to make sure there are tests that exercise this function.
valuesMKDecoder ::
     ( TableStuff l
     , SufficientSerializationForAnyBackingStore l
     )
  => CBOR.Decoder s (LedgerTables l ValuesMK)
valuesMKDecoder = do
    numTables <- CBOR.decodeListLen
    if numTables == 0
      then
        return $ pureLedgerTables emptyMK
      else do
        mapLen <- CBOR.decodeMapLen
        ret    <- traverseLedgerTables (go mapLen) codecLedgerTables
        Exn.assert ((getSum (foldLedgerTables (\_ -> Sum 1) ret)) == numTables)
          $ return ret
 where
  go :: Ord k
     => Int
     -> CodecMK k v
     -> CBOR.Decoder s (ApplyMapKind ValuesMK k v)
  go len (CodecMK _encK _encV decK decV) =
        ApplyValuesMK . Values . Map.fromList
    <$> sequence (replicate len ((,) <$> decK <*> decV))

{-------------------------------------------------------------------------------
  Special classes of ledger states
-------------------------------------------------------------------------------}

class InMemory (l :: LedgerStateKind) where

  -- | If the ledger state is always in memory, then l mk will be isomorphic to
  -- l mk' for all mk, mk'. As a result, we can convert between ledgers states
  -- indexed by different map kinds.
  --
  -- This function is useful to combine functions that operate on functions that
  -- transform the map kind on a ledger state (eg applyChainTickLedgerResult).
  convertMapKind :: l mk -> l mk'

