{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}

{-# LANGUAGE UndecidableInstances       #-}

module Ouroboros.Consensus.Storage.LedgerDB.HD.DiffSeq (
    -- * Sequences of diffs
    DiffSeq (..)
  , Element (..)
  , InternalMeasure (..)
  , Length (..)
  , SlotNo (..)
  , TopMeasure (..)
    -- * Short-hands for type-class constraints
  , IM
  , SM
  , TM
    -- * API: Type classes
  , HasDiff (..)
  , HasLength (..)
  , HasSlot (..)
    -- * API: functions
  , cumulativeDiff
  , extend
  , extend'
  , length
  , mapDiffSeq
  , maxSlot
  , split
  , splitAt
  , splitAtFromEnd
  , splitAtSlot
  ) where

import           Prelude hiding (length, splitAt)

import qualified Control.Exception as Exn
import           Data.Bifunctor (Bifunctor (bimap))
import           Data.Group
import           Data.Monoid (Sum (..))
import           Data.Semigroup (Max (..))
import           GHC.Generics (Generic)
import           NoThunks.Class (NoThunks)

import           Data.FingerTree.TopMeasured.Strict hiding (split)
import qualified Data.FingerTree.TopMeasured.Strict as TMFT (split)

-- import           Cardano.Binary (FromCBOR (..), ToCBOR (..))
import qualified Cardano.Slotting.Slot as Slot

import           Ouroboros.Consensus.Storage.LedgerDB.HD.TableTypes
import           Ouroboros.Consensus.Storage.LedgerDB.HD.ToStore (ToStoreKind)


{-------------------------------------------------------------------------------
  Sequences of diffs
-------------------------------------------------------------------------------}

newtype DiffSeq (ts :: ToStoreKind) k v =
  DiffSeq
    (StrictFingerTree
      (TopMeasure ts k v)
      (InternalMeasure ts k v)
      (Element ts k v)
    )
  deriving stock (Generic, Show, Eq)
  deriving newtype (Semigroup, Monoid)
  deriving anyclass (NoThunks)


-- The @'SlotNo'@ is not included in the top-level measure, since it is
-- not a @'Group'@ instance.
data TopMeasure (ts :: ToStoreKind) k v =
    TopMeasure
      {-# UNPACK #-} !Length             -- ^ Cumulative length
                     !(TableDiff ts k v) -- ^ Cumulative diff
  deriving stock (Generic, Show, Eq)
  deriving anyclass (NoThunks)

data InternalMeasure (ts :: ToStoreKind) k v =
    InternalMeasure
      {-# UNPACK #-} !Length         -- ^ Cumulative length
                     !(Maybe SlotNo) -- ^ Right-most slot number
  deriving stock (Generic, Show, Eq)
  deriving anyclass (NoThunks)

data Element (ts :: ToStoreKind) k v =
    Element
      {-# UNPACK #-} !SlotNo
                     !(TableDiff ts k v)
  deriving stock (Generic, Show, Eq)
  deriving anyclass (NoThunks)


-- | Length as a @'Sum'@.
newtype Length = Length {unLength :: Int}
  deriving stock (Generic, Show, Eq, Ord)
  deriving newtype (Num)
  deriving anyclass (NoThunks)
  deriving Semigroup via Sum Int
  deriving Monoid via Sum Int
  deriving Group via Sum Int

-- | Right-most slot number as a @'Max'@.
newtype SlotNo = SlotNo {unSlotNo :: Slot.SlotNo}
  deriving stock (Generic, Show, Eq, Ord)
  deriving newtype (Num)
  deriving anyclass (NoThunks)
  deriving Semigroup via Max Slot.SlotNo
  deriving Monoid via Max Slot.SlotNo

{-------------------------------------------------------------------------------
  Top-measuring
-------------------------------------------------------------------------------}

instance (Ord k, Eq v) => TopMeasured (TopMeasure ts k v) (Element ts k v) where
  measureTop (Element _ d) = TopMeasure 1 d

instance (Ord k, Eq v) => Semigroup (TopMeasure ts k v) where
  TopMeasure len1 d1 <> TopMeasure len2 d2 =
      TopMeasure (len1 <> len2) (d1 <> d2)

instance (Ord k, Eq v) => Monoid (TopMeasure ts k v) where
  mempty = TopMeasure mempty mempty

instance (Ord k, Eq v) => Group (TopMeasure ts k v) where
  invert (TopMeasure len d) =
    TopMeasure (invert len) (invert d)

{-------------------------------------------------------------------------------
  Internal-measuring
-------------------------------------------------------------------------------}

instance Measured (InternalMeasure ts k v) (Element ts k v) where
  measure (Element slotNo _d) = InternalMeasure 1 (Just slotNo)

instance Semigroup (InternalMeasure ts k v) where
  InternalMeasure len1 sl1 <> InternalMeasure len2 sl2 =
    InternalMeasure (len1 <> len2) (sl1 <> sl2)

instance Monoid (InternalMeasure ts k v) where
  mempty = InternalMeasure mempty mempty

{-------------------------------------------------------------------------------
  Short-hands for type-class constraints
-------------------------------------------------------------------------------}

-- Short-hands for @'TopMeasured'@ and @'InternalMeasured'@ constraints for
-- the @'DiffSeq'@-related datatypes.

-- | Short-hand for @'Topmeasured'@.
type TM (ts :: ToStoreKind) k v =
  TopMeasured (TopMeasure ts k v) (Element ts k v)

-- | Short-hand for @'InternalMeasured'@.
type IM (ts :: ToStoreKind) k v =
  InternalMeasured (InternalMeasure ts  k v) (Element ts k v)

-- | Short-hand for @'SuperMeasured'@.
type SM (ts :: ToStoreKind) k v =
  (TM ts k v, IM ts k v)

{-------------------------------------------------------------------------------
  API: type classes
-------------------------------------------------------------------------------}

class HasDiff (ts :: ToStoreKind) k v where
  getTableDiffE :: Element ts k v -> TableDiff ts k v
  getTableDiffTM :: TopMeasure ts k v -> TableDiff ts k v
  mapElement :: (v -> v') -> Element ts k v -> Element ts k v'

class HasLength (ts :: ToStoreKind) k v where
  getLength :: TopMeasure ts k v -> Length
  getInternalMeasureLength :: InternalMeasure ts k v -> Length

class HasSlot (ts :: ToStoreKind) k v where
  getElementSlot         :: Element ts k v -> SlotNo
  getInternalMeasureSlot :: InternalMeasure ts k v -> Maybe SlotNo

instance HasDiff ts k v where
  getTableDiffE (Element _ d) = d
  getTableDiffTM (TopMeasure _ d) = d
  mapElement f (Element sl d) = Element sl (mapTableDiff f d)

instance HasLength ts k v where
  getLength (TopMeasure s _) = s
  getInternalMeasureLength (InternalMeasure s _) = s

instance HasSlot ts k v where
  getElementSlot (Element sl _) = sl
  getInternalMeasureSlot (InternalMeasure _ sl) = sl

{-------------------------------------------------------------------------------
  API: derived functions
-------------------------------------------------------------------------------}

cumulativeDiff ::
     TM ts k v
  => DiffSeq ts k v
  -> TableDiff ts k v
cumulativeDiff (DiffSeq ft) = getTableDiffTM $ measureTop ft

length ::
     TM ts k v
  => DiffSeq ts k v -> Int
length (DiffSeq ft) = unLength . getLength $ measureTop ft

extend ::
     SM ts k v
  => DiffSeq ts k v
  -> Element ts k v
  -> DiffSeq ts k v
extend (DiffSeq ft) el = DiffSeq $ ft |> el

extend' ::
     SM ts k v
  => DiffSeq ts k v
  -> Element ts k v
  -> DiffSeq ts k v
extend' (DiffSeq ft) el =
    Exn.assert invariant $ DiffSeq $ ft |> el
  where
    invariant = case getInternalMeasureSlot (measure el) of
      Nothing  -> True
      Just sl0 -> sl0 <= getElementSlot el

maxSlot ::
     IM ts k v
  => DiffSeq ts k v
  -> Maybe Slot.SlotNo
maxSlot (DiffSeq ft) =
    unwrapInner $ getInternalMeasureSlot $ measureInternal ft
  where
    -- We care about /real/ slot numbers, so we should return a
    -- @'Slot.SlotNo'@.
    unwrapInner :: Maybe SlotNo -> Maybe Slot.SlotNo
    unwrapInner Nothing            = Nothing
    unwrapInner (Just (SlotNo sl)) = Just sl

split ::
     SM ts k v
  => (InternalMeasure ts k v -> Bool)
  -> DiffSeq ts k v
  -> (DiffSeq ts k v, DiffSeq ts k v)
split p (DiffSeq ft) = bimap DiffSeq DiffSeq $ TMFT.split p ft

splitAt ::
     SM ts k v
  => Int
  -> DiffSeq ts k v
  -> (DiffSeq ts k v, DiffSeq ts k v)
splitAt n = split ((Length n<) . getInternalMeasureLength)

splitAtFromEnd ::
     SM ts k v
  => Int
  -> DiffSeq ts k v
  -> (DiffSeq ts k v, DiffSeq ts k v)
splitAtFromEnd n dseq =
    Exn.assert (n <= len) $ splitAt (len - n) dseq
  where
    len = length dseq

splitAtSlot ::
     SM ts k v
  => Slot.SlotNo
  -> DiffSeq ts k v
  -> (DiffSeq ts k v, DiffSeq ts k v)
splitAtSlot sl = split p
  where
    p = maybe True (SlotNo sl <) . getInternalMeasureSlot

mapDiffSeq ::
       ( SM ts k v
       , SM ts k v'
       )
    => (v -> v')
    -> DiffSeq ts k v
    -> DiffSeq ts k v'
mapDiffSeq f (DiffSeq ft) = DiffSeq $ fmap' (mapElement f) ft
