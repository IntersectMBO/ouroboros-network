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
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeFamilies               #-}

{-# LANGUAGE UndecidableInstances       #-}

module Ouroboros.Consensus.Storage.LedgerDB.HD.DiffSeq (
    -- * Sequences of diffs
    DiffSeq (..)
    -- * @'Diffable'@ API: types
  , DIM
  , DM
  , DTM
  , Diffable (..)
  , HasDiff (..)
  , HasLength (..)
  , HasSlot (..)
  , Length (..)
  , SlotNo (..)
    -- * @'Diffable'@ API: functions
  , cumulativeDiff
  , emptyDiffSeq
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
import           Data.Monoid (Sum (..))
import           Data.Semigroup (Max (..))
import           GHC.Generics (Generic)
import           NoThunks.Class (NoThunks)

import           Data.FingerTree.Strict.Alt (Alt, InternalMeasured, TopMeasured)
import qualified Data.FingerTree.Strict.Alt as Alt

-- import           Cardano.Binary (FromCBOR (..), ToCBOR (..))
import qualified Cardano.Slotting.Slot as Slot

import           Ouroboros.Consensus.Storage.LedgerDB.HD.TableTypes
import           Ouroboros.Consensus.Storage.LedgerDB.HD.ToStore (ToStoreKind)


{-------------------------------------------------------------------------------
  Sequences of diffs
-------------------------------------------------------------------------------}

newtype DiffSeq (ts :: ToStoreKind) k v =
  DiffSeq
    (Alt
      (TopMeasure ts k v)
      (InternalMeasure ts k v)
      (Element ts k v)
    )
  deriving stock Generic

deriving stock instance ( Eq (TopMeasure ts k v)
                        , Eq (InternalMeasure ts k v)
                        , Eq (Element ts k v)
                        ) => Eq (DiffSeq ts k v)

deriving stock instance ( Show (TopMeasure ts k v)
                        , Show (InternalMeasure ts k v)
                        , Show (Element ts k v)
                        ) => Show (DiffSeq ts k v)

deriving anyclass instance ( NoThunks (TopMeasure ts k v)
                           , NoThunks (InternalMeasure ts k v)
                           , NoThunks (Element ts k v)
                           ) => NoThunks (DiffSeq ts k v)

{-------------------------------------------------------------------------------
  @'Diffable'@ API: types
-------------------------------------------------------------------------------}

class (DTM ts k v, DIM ts k v) => Diffable ts k v where
  data family Element (ts :: ToStoreKind) k v
  data family TopMeasure (ts :: ToStoreKind) k v
  data family InternalMeasure (ts :: ToStoreKind) k v

type DTM (ts :: ToStoreKind) k v =
  TopMeasured (TopMeasure ts k v) (Element ts k v)
type DIM (ts :: ToStoreKind) k v =
  InternalMeasured (InternalMeasure ts  k v) (Element ts k v)
type DM (ts :: ToStoreKind) k v =
  (DTM ts k v, DIM ts k v)


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


newtype Length = Length {unLength :: Int}
  deriving stock (Generic, Show, Eq, Ord)
  deriving newtype (Num)
  deriving anyclass (NoThunks)
  deriving Semigroup via Sum Int

newtype SlotNo = SlotNo {unSlotNo :: Slot.SlotNo}
  deriving stock (Generic, Show, Eq, Ord)
  deriving anyclass (NoThunks)
  deriving Semigroup via Max Slot.SlotNo

{-------------------------------------------------------------------------------
  @'Diffable'@ API: derived functions
-------------------------------------------------------------------------------}

emptyDiffSeq ::
     ( TopMeasured (TopMeasure ts k v) (Element ts k v)
     , InternalMeasured (InternalMeasure ts k v) (Element ts k v)
     ) => DiffSeq ts k v
emptyDiffSeq = DiffSeq Alt.empty

cumulativeDiff ::
     ( DTM ts k v
     , HasDiff ts k v
     )
  => DiffSeq ts k v
  -> TableDiff ts k v
cumulativeDiff (DiffSeq alt) = getTableDiffTM $ Alt.measureTop alt

length ::
       (DTM ts k v, HasLength ts k v)
    => DiffSeq ts k v -> Int
length (DiffSeq alt) = unLength . getLength $ Alt.measureTop alt

extend ::
     DM ts k v
  => DiffSeq ts k v
  -> Element ts k v
  -> DiffSeq ts k v
extend (DiffSeq alt) el = DiffSeq $ alt Alt.|> el

extend' ::
     (DM ts k v, HasSlot ts k v)
  => DiffSeq ts k v
  -> Element ts k v
  -> DiffSeq ts k v
extend' (DiffSeq alt) el =
    Exn.assert invariant $ DiffSeq $ alt Alt.|> el
  where
    invariant = case getInternalMeasureSlot (Alt.measure el) of
      Nothing  -> True
      Just sl0 -> sl0 <= getElementSlot el

maxSlot ::
     (DIM ts k v, HasSlot ts k v)
  => DiffSeq ts k v
  -> Maybe Slot.SlotNo
maxSlot (DiffSeq alt) =
    unwrapInner $ getInternalMeasureSlot $ Alt.measureInternal alt
  where
    -- We care about /real/ slot numbers, so we should return a
    -- @'Slot.SlotNo'@.
    unwrapInner :: Maybe SlotNo -> Maybe Slot.SlotNo
    unwrapInner Nothing            = Nothing
    unwrapInner (Just (SlotNo sl)) = Just sl

split ::
     DM ts k v
  => (InternalMeasure ts k v -> Bool)
  -> DiffSeq ts k v
  -> (DiffSeq ts k v, DiffSeq ts k v)
split p (DiffSeq alt) = bimap DiffSeq DiffSeq $ Alt.split p alt

splitAt ::
     (DM ts k v, HasLength ts k v)
  => Int
  -> DiffSeq ts k v
  -> (DiffSeq ts k v, DiffSeq ts k v)
splitAt n = split ((<Length n) . getInternalMeasureLength)

splitAtFromEnd ::
     (DM ts k v, HasLength ts k v)
  => Int
  -> DiffSeq ts k v
  -> (DiffSeq ts k v, DiffSeq ts k v)
splitAtFromEnd n dseq =
    Exn.assert (n <= len) $ splitAt (len - n) dseq
  where
    len = length dseq

splitAtSlot ::
     (DM ts k v, HasSlot ts k v)
  => Slot.SlotNo
  -> DiffSeq ts k v
  -> (DiffSeq ts k v, DiffSeq ts k v)
splitAtSlot sl = split p
  where
    p = maybe True (SlotNo sl <) . getInternalMeasureSlot

mapDiffSeq ::
       ( DM ts k v
       , DM ts k v'
       , HasDiff ts k v
       )
    => (v -> v')
    -> DiffSeq ts k v
    -> DiffSeq ts k v'
mapDiffSeq f (DiffSeq alt) = DiffSeq $ Alt.fmap' (mapElement f) alt
