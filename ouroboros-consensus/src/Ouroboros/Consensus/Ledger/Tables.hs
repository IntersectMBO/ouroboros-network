{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE DefaultSignatures        #-}
{-# LANGUAGE DeriveAnyClass           #-}
{-# LANGUAGE DeriveGeneric            #-}
{-# LANGUAGE DerivingStrategies       #-}
{-# LANGUAGE FlexibleContexts         #-}
{-# LANGUAGE GADTs                    #-}
{-# LANGUAGE QuantifiedConstraints    #-}
{-# LANGUAGE Rank2Types               #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies             #-}
{-# LANGUAGE UndecidableInstances     #-}

-- | See @'LedgerTables'@
module Ouroboros.Consensus.Ledger.Tables (
    -- * Kinds
    LedgerStateKind
  , MapKind
    -- * Basic LedgerState classes
  , CanStowLedgerTables (..)
  , HasLedgerTables (..)
  , HasTickedLedgerTables (..)
    -- * @MapKind@s
    -- ** Interface
  , IsMapKind (..)
    -- ** Concrete definitions
  , Canonical (..)
  , CodecMK (..)
  , DiffMK (..)
  , EmptyMK (..)
  , KeysMK (..)
  , NameMK (..)
  , SeqDiffMK (..)
  , TrackingMK (..)
  , ValuesMK (..)
    -- * Serialization
  , CanSerializeLedgerTables (..)
  , valuesMKDecoder
  , valuesMKEncoder
    -- * Special classes
  , LedgerTablesAreTrivial (..)
  ) where

import qualified Codec.CBOR.Decoding as CBOR
import qualified Codec.CBOR.Encoding as CBOR
import qualified Control.Exception as Exn
import           Control.Monad (replicateM)
import           Data.Kind (Constraint, Type)
import           Data.Map.Diff.Strict (Diff)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Monoid (Sum (..))
import           Data.Set (Set)
import           GHC.Generics
import           NoThunks.Class (NoThunks (..))
import           Ouroboros.Consensus.Storage.LedgerDB.DiffSeq (DiffSeq, empty,
                     mapDiffSeq)
import           Ouroboros.Consensus.Ticked

{-------------------------------------------------------------------------------
  Kinds
-------------------------------------------------------------------------------}

type MapKind         = {- key -} Type -> {- value -} Type -> Type
type LedgerStateKind = MapKind -> Type

{-------------------------------------------------------------------------------
  Basic LedgerState classes
-------------------------------------------------------------------------------}

-- | Manipulating the @mk@ on the @'LedgerTables'@, extracting @'LedgerTables'@
-- from a @'LedgerState'@ (which will share the same @mk@), or replacing the
-- @'LedgerTables'@ associated to a particular in-memory @'LedgerState'@.
type HasLedgerTables :: LedgerStateKind -> Constraint
class ( forall mk. IsMapKind mk => Eq       (LedgerTables l mk)
      , forall mk. IsMapKind mk => NoThunks (LedgerTables l mk)
      , forall mk. IsMapKind mk => Show     (LedgerTables l mk)
      ) => HasLedgerTables l where

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
  -- two type parameters, the key and the value.
  data family LedgerTables l :: LedgerStateKind

  -- | Extract the ledger tables from a ledger state
  --
  -- This 'IsMapKind' constraint is necessary because the 'CardanoBlock'
  -- instance uses 'mapMK'.
  projectLedgerTables :: forall mk. IsMapKind mk => l mk -> LedgerTables l mk
  default projectLedgerTables ::
        LedgerTablesAreTrivial l
      => l mk
      -> LedgerTables l mk
  projectLedgerTables _ = trivialLedgerTables

  -- | Overwrite the tables in the given ledger state.
  --
  -- The contents of the tables should not be /younger/ than the content of the
  -- ledger state. In particular, for a
  -- 'Ouroboros.Consensus.HardFork.Combinator.Basics.HardForkBlock' ledger, the
  -- tables argument should not contain any data from eras that succeed the
  -- current era of the ledger state argument.
  --
  -- This 'IsMapKind' constraint is necessary because the 'CardanoBlock'
  -- instance uses 'mapMK'.
  withLedgerTables :: IsMapKind mk => l any -> LedgerTables l mk -> l mk
  default withLedgerTables ::
       (IsMapKind mk, LedgerTablesAreTrivial l)
    => l any
    -> LedgerTables l mk
    -> l mk
  withLedgerTables st _ = convertMapKind st

  pureLedgerTables ::
       (forall k v.
            (Ord k, Eq v)
         => mk k v
       )
    -> LedgerTables l mk
  default pureLedgerTables ::
       LedgerTablesAreTrivial l
    => (forall k v.
            (Ord k, Eq v)
         => mk k v
       )
    -> LedgerTables l mk
  pureLedgerTables _ = trivialLedgerTables

  mapLedgerTables ::
       (forall k v.
            (Ord k, Eq v)
         => mk1 k v
         -> mk2 k v
       )
    -> LedgerTables l mk1
    -> LedgerTables l mk2
  default mapLedgerTables ::
       LedgerTablesAreTrivial l
    => (forall k v.
            (Ord k, Eq v)
         => mk1 k v
         -> mk2 k v
       )
    -> LedgerTables l mk1
    -> LedgerTables l mk2
  mapLedgerTables _ _ = trivialLedgerTables

  traverseLedgerTables ::
       Applicative f
    => (forall k v .
           (Ord k, Eq v)
        =>    mk1 k v
        -> f (mk2 k v)
       )
    ->    LedgerTables l mk1
    -> f (LedgerTables l mk2)
  default traverseLedgerTables ::
       (Applicative f, LedgerTablesAreTrivial l)
    => (forall k v .
           (Ord k, Eq v)
        =>    mk1 k v
        -> f (mk2 k v)
       )
    ->    LedgerTables l mk1
    -> f (LedgerTables l mk2)
  traverseLedgerTables _ _ = pure trivialLedgerTables

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
  default zipLedgerTables ::
       LedgerTablesAreTrivial l
    => (forall k v.
            (Ord k, Eq v)
         => mk1 k v
         -> mk2 k v
         -> mk3 k v
       )
    -> LedgerTables l mk1
    -> LedgerTables l mk2
    -> LedgerTables l mk3
  zipLedgerTables _ _ _ = trivialLedgerTables

  zipLedgerTables3 ::
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
  default zipLedgerTables3 ::
       LedgerTablesAreTrivial l
    => (forall k v.
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
  zipLedgerTables3 _ _ _ _ = trivialLedgerTables

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
  default zipLedgerTablesA ::
       (Applicative f, LedgerTablesAreTrivial l)
    => (forall k v.
            (Ord k, Eq v)
         => mk1 k v
         -> mk2 k v
         -> f (mk3 k v)
       )
    -> LedgerTables l mk1
    -> LedgerTables l mk2
    -> f (LedgerTables l mk3)
  zipLedgerTablesA _ _ _ = pure trivialLedgerTables

  zipLedgerTables3A ::
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
  default zipLedgerTables3A ::
       (Applicative f, LedgerTablesAreTrivial l)
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
  zipLedgerTables3A _ _ _ _ = pure trivialLedgerTables

  foldLedgerTables ::
       Monoid m
    => (forall k v.
            (Ord k, Eq v)
         => mk k v
         -> m
       )
    -> LedgerTables l mk
    -> m
  foldLedgerTables _ _ = mempty

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
  foldLedgerTables2 _ _ _ = mempty

  -- | The ledger tables will eventually be stored in some BackingStore under a
  -- db-like table named after this value
  namesLedgerTables :: LedgerTables l NameMK
  default namesLedgerTables :: LedgerTablesAreTrivial l => LedgerTables l NameMK
  namesLedgerTables = trivialLedgerTables

type HasTickedLedgerTables :: LedgerStateKind -> Constraint
class HasLedgerTables l => HasTickedLedgerTables l where
  -- The 'IsMapKind' constraint is here for the same reason it's on
  -- 'projectLedgerTables'
  projectLedgerTablesTicked :: IsMapKind mk => Ticked1 l mk  -> LedgerTables l mk
  default projectLedgerTablesTicked ::
       LedgerTablesAreTrivial l
    => Ticked1 l mk
    -> LedgerTables l mk
  projectLedgerTablesTicked _ = trivialLedgerTables

  -- The 'IsMapKind' constraint is here for the same reason it's on
  -- 'withLedgerTables'
  withLedgerTablesTicked :: IsMapKind mk => Ticked1 l any -> LedgerTables l mk -> Ticked1 l mk

-- | LedgerTables are projections of data from a LedgerState and as such they
-- can be injected back into a LedgerState. This is necessary because the Ledger
-- rules are unaware of UTxO-HD changes. Thus, by stowing the ledger tables, we are
-- able to provide a Ledger State with a restricted UTxO set that is enough to
-- execute the Ledger rules.
--
-- In particular, HardForkBlocks are never given diretly to the ledger but
-- rather unwrapped and then it is the inner ledger state the one we give to the
-- ledger. This means that all the single era blocks must be an instance of this
-- class, but HardForkBlocks might avoid doing so.
type CanStowLedgerTables :: LedgerStateKind -> Constraint
class CanStowLedgerTables l where

  stowLedgerTables     :: l ValuesMK -> l EmptyMK
  default stowLedgerTables :: LedgerTablesAreTrivial l => l ValuesMK -> l EmptyMK
  stowLedgerTables = convertMapKind

  unstowLedgerTables   :: l EmptyMK  -> l ValuesMK
  default unstowLedgerTables :: LedgerTablesAreTrivial l => l EmptyMK -> l ValuesMK
  unstowLedgerTables = convertMapKind

{-------------------------------------------------------------------------------
  'LedgerTables' are 'Monoid's
-------------------------------------------------------------------------------}

instance ( forall k v. (Ord k, Eq v) => Semigroup (mk k v)
         , HasLedgerTables l
         ) => Semigroup (LedgerTables l mk) where
  (<>) = zipLedgerTables (<>)

instance ( forall k v. (Ord k, Eq v) => Monoid (mk k v)
         , HasLedgerTables l
         ) => Monoid (LedgerTables l mk) where
  mempty = pureLedgerTables mempty

{-------------------------------------------------------------------------------
  @MapKind@s
-------------------------------------------------------------------------------}

{- $concrete-tables

We provide here  the usual structures that we expect to deal with, namely:

- 'ValuesMK': a map of keys to values

- 'KeysMK': a set of keys

- 'DiffMK': a map of keys to value differences

- 'TrackingMK': the product of both 'ValuesMK' and 'DiffMK'

- 'SeqDiffMK': an efficient sequence of differences

- 'CodecMK': @CBOR@ encoder and decoder for the key and value

-}

-- | A general interface to mapkinds.
--
-- In some cases where @mk@ is not specialised to a concrete mapkind like
-- @'ValuesMK'@, there are often still a number of operations that we can
-- perform for this @mk@ that make sense regardless of the concrete mapkind. For
-- example, we should always be able to map over a mapkind to change the type of
-- values that it contains. This class is an interface to mapkinds that provides
-- such common functions.
type IsMapKind :: MapKind -> Constraint
class ( forall k v. (Eq       k, Eq       v) => Eq       (mk k v)
      , forall k v. (NoThunks k, NoThunks v) => NoThunks (mk k v)
      , forall k v. (Show     k, Show     v) => Show     (mk k v)
      ) => IsMapKind mk where
  emptyMK :: forall k v. (Ord k, Eq v) => mk k v
  default emptyMK :: forall k v. Monoid (mk k v) => mk k v
  emptyMK = mempty

  mapMK :: forall k v v'. (Ord k, Eq v, Eq v') => (v -> v') -> mk k v -> mk k v'
  default mapMK :: forall k v v'. (Functor (mk k)) => (v -> v') -> mk k v -> mk k v'
  mapMK = fmap

-- | The Canonical MapKind, which has no constructor, and therefore a
-- @LedgerState blk Canonical@ will have all the contents in memory. This is
-- just a phantom type used to sketch later development and shall not land on
-- the release version.
data Canonical k v = Canonical
  deriving stock (Generic, Eq, Show)
  deriving anyclass NoThunks

newtype DiffMK     k v = DiffMK      (Diff k v)
  deriving stock (Generic, Eq, Show)
  deriving anyclass NoThunks

data    EmptyMK    k v = EmptyMK
  deriving stock (Generic, Eq, Show)
  deriving anyclass NoThunks

newtype KeysMK     k v = KeysMK      (Set k)
  deriving stock (Generic, Eq, Show)
  deriving anyclass NoThunks

newtype SeqDiffMK  k v = SeqDiffMK   (DiffSeq k v)
  deriving stock (Generic, Eq, Show)
  deriving anyclass NoThunks

data    TrackingMK k v = TrackingMK !(Map k v) !(Diff k v)
  deriving (Generic, Eq, Show, NoThunks)

newtype ValuesMK   k v = ValuesMK    (Map k v)
  deriving stock (Generic, Eq, Show)
  deriving anyclass NoThunks

-- | A codec 'MapKind' that will be used to refer to @'LedgerTables' l CodecMK@
-- as the codecs that can encode every key and value in the @'LedgerTables' l
-- mk@.
--
-- It is important to note that in the context of the HardForkCombinator, the
-- key @k@ has to be accessible from any era we are currently in, regardless of
-- which era it was created in. Because of that, we need that the serialization
-- of the key remains stable accross eras.
--
-- Ledger will provide more efficient encoders than CBOR, which will produce a
-- @'ShortByteString'@ directly.
data CodecMK k v = CodecMK
                     (k -> CBOR.Encoding)
                     (v -> CBOR.Encoding)
                     (forall s . CBOR.Decoder s k)
                     (forall s . CBOR.Decoder s v)

newtype NameMK k v = NameMK String

instance IsMapKind Canonical where
  emptyMK = Canonical
  mapMK _ Canonical = Canonical

instance IsMapKind EmptyMK where
  emptyMK = EmptyMK
  mapMK _ EmptyMK = EmptyMK

instance IsMapKind KeysMK where
  emptyMK = KeysMK mempty
  mapMK _ (KeysMK ks) = KeysMK ks

instance IsMapKind ValuesMK where
  emptyMK = ValuesMK mempty
  mapMK f (ValuesMK vs) = ValuesMK $ fmap f vs

instance IsMapKind TrackingMK where
  emptyMK = TrackingMK mempty mempty
  mapMK f (TrackingMK vs d) = TrackingMK (fmap f vs) (fmap f d)

instance IsMapKind DiffMK where
  emptyMK = DiffMK mempty
  mapMK f (DiffMK d) = DiffMK $ fmap f d

instance IsMapKind SeqDiffMK where
  emptyMK = SeqDiffMK empty
  mapMK f (SeqDiffMK ds) = SeqDiffMK $ mapDiffSeq f ds

instance Ord k => Semigroup (KeysMK k v) where
  KeysMK l <> KeysMK r = KeysMK (l <> r)

instance Ord k => Monoid (KeysMK k v) where
  mempty = KeysMK mempty

instance Functor (DiffMK k) where
  fmap f (DiffMK d) = DiffMK $ fmap f d

{-------------------------------------------------------------------------------
  Serialization Codecs
-------------------------------------------------------------------------------}

-- | This class provides a 'CodecMK' that can be used to encode/decode keys and
-- values on @'LedgerTables' l mk@
type CanSerializeLedgerTables :: LedgerStateKind -> Constraint
class CanSerializeLedgerTables l where
  codecLedgerTables :: LedgerTables l CodecMK
  default codecLedgerTables :: LedgerTablesAreTrivial l => LedgerTables l CodecMK
  codecLedgerTables = trivialLedgerTables

-- | Default encoder of @'LedgerTables' l ''ValuesMK'@ to be used by the
-- in-memory backing store.
valuesMKEncoder ::
     ( HasLedgerTables l
     , CanSerializeLedgerTables l
     )
  => LedgerTables l ValuesMK
  -> CBOR.Encoding
valuesMKEncoder tables =
       CBOR.encodeListLen (getSum (foldLedgerTables (\_ -> Sum 1) tables))
    <> foldLedgerTables2 go codecLedgerTables tables
  where
    go :: CodecMK k v -> ValuesMK k v -> CBOR.Encoding
    go (CodecMK encK encV _decK _decV) (ValuesMK m) =
         CBOR.encodeMapLen (fromIntegral $ Map.size m)
      <> Map.foldMapWithKey (\k v -> encK k <> encV v) m

-- | Default decoder of @'LedgerTables' l ''ValuesMK'@ to be used by the
-- in-memory backing store.
valuesMKDecoder ::
     ( HasLedgerTables l
     , CanSerializeLedgerTables l
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
        Exn.assert (getSum (foldLedgerTables (\_ -> Sum 1) ret) == numTables)
          $ return ret
 where
  go :: Ord k
     => Int
     -> CodecMK k v
     -> CBOR.Decoder s (ValuesMK k v)
  go len (CodecMK _encK _encV decK decV) =
        ValuesMK . Map.fromList
    <$> replicateM len ((,) <$> decK <*> decV)

{-------------------------------------------------------------------------------
  Special classes of ledger states
-------------------------------------------------------------------------------}

type LedgerTablesAreTrivial :: LedgerStateKind -> Constraint
-- | For some ledger states we won't be defining 'LedgerTables' and instead the
-- ledger state will be fully stored in memory, as before UTxO-HD. The ledger
-- states that are defined this way can be made instances of this class which
-- allows for easy manipulation of the types of @mk@ required at any step of the
-- program.
class LedgerTablesAreTrivial l where
  -- | If the ledger state is always in memory, then @l mk@ will be isomorphic
  -- to @l mk'@ for all @mk@, @mk'@. As a result, we can convert between ledgers
  -- states indexed by different map kinds.
  --
  -- This function is useful to combine functions that operate on functions that
  -- transform the map kind on a ledger state (eg @applyChainTickLedgerResult@).
  convertMapKind :: IsMapKind mk' => l mk -> l mk'

  -- | As the ledger tables are trivial, this functions provides the only data
  -- constructor that is defined for them.
  trivialLedgerTables :: LedgerTables l mk
