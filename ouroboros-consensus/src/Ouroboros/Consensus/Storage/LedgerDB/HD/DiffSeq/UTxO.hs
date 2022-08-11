{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}

{-# OPTIONS_GHC -Wno-orphans       #-}

module Ouroboros.Consensus.Storage.LedgerDB.HD.DiffSeq.UTxO (
    Element (..)
  , InternalMeasure (..)
  , TopMeasure (..)
  ) where

import           Data.Group
import           GHC.Generics (Generic)
import           NoThunks.Class (NoThunks)

import           Data.FingerTree.Strict.Alt (Measured (..), TopMeasured (..))

import           Ouroboros.Consensus.Storage.LedgerDB.HD.DiffSeq
import           Ouroboros.Consensus.Storage.LedgerDB.HD.TableTypes
                     (TableDiff (..), mapTableDiff)
import           Ouroboros.Consensus.Storage.LedgerDB.HD.ToStore
                     (ToStoreKind (UTxO))

instance (Ord k, Eq v) => Diffable UTxO k v where

  data Element UTxO k v =
      UtxoElement {-# UNPACK #-} !SlotNo !(TableDiff UTxO k v)
    deriving stock (Generic, Show, Eq)
    deriving anyclass (NoThunks)

  -- The @'SlotNo'@ is not included in the top-level measure, since it is
  -- not a @'Group'@ instance.
  data TopMeasure UTxO k v =
      UtxoTM
        {-# UNPACK #-} !Length               -- ^ cumulative size
                       !(TableDiff UTxO k v) -- ^ cumulative diff
    deriving stock (Generic, Show, Eq)
    deriving anyclass (NoThunks)

  data InternalMeasure UTxO k v =
      UtxoIM
        {-# UNPACK #-} !Length          -- ^ cumulative size
                       !(Maybe SlotNo)  -- ^ rightmost, ie maximum. Does not
                                        --   exist if the corresponding sequeence
                                        --   is empty.
    deriving stock (Generic, Show, Eq)
    deriving anyclass (NoThunks)


instance (Ord k, Eq v) => Semigroup (TopMeasure UTxO k v) where
  UtxoTM size1 ad1 <> UtxoTM size2 ad2 =
      UtxoTM (size1 + size2) (ad1 <> ad2)

instance (Ord k, Eq v) => Monoid (TopMeasure UTxO k v) where
  mempty = UtxoTM 0 mempty

instance (Ord k, Eq v) => Group (TopMeasure UTxO k v) where
  invert (UtxoTM size (TableDiff ad)) =
    UtxoTM (- size) (TableDiff $ invert ad)

instance (Ord k, Eq v)
      => TopMeasured (TopMeasure UTxO k v) (Element UTxO k v) where
  measureTop (UtxoElement _ d) = UtxoTM 1 d


instance Semigroup (InternalMeasure UTxO k v) where
  UtxoIM len1 sl1 <> UtxoIM len2 sl2 = UtxoIM (len1 + len2) (sl1 <> sl2)

instance Monoid (InternalMeasure UTxO k v) where
  mempty = UtxoIM 0 Nothing

instance Measured (InternalMeasure UTxO k v) (Element UTxO k v) where
  measure (UtxoElement slotNo _d) = UtxoIM 1 (Just slotNo)

instance HasDiff UTxO k v where
  getTableDiffE (UtxoElement _ d) = d
  getTableDiffTM (UtxoTM _ d) = d
  mapElement f (UtxoElement sl d) = UtxoElement sl (mapTableDiff f d)

instance HasLength UTxO k v where
  getLength (UtxoTM s _) = s
  getInternalMeasureLength (UtxoIM s _) = s

instance HasSlot UTxO k v where
  getElementSlot (UtxoElement sl _) = sl
  getInternalMeasureSlot (UtxoIM _ sl) = sl


