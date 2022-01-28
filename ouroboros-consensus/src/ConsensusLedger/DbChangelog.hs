{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module ConsensusLedger.DbChangelog where

import           Data.Kind (Type)
import           Data.Map (Map)

class DbChangelog chlog l where

  -- PROBLEM: here we can't use the concrete DiffMK, otherwise we're just
  -- mapping one concrete implementation to another. We don't seem to want to
  -- use type families to have stuff like 'DiffState l' so I don't know if
  -- having a DbChangelog abstraction makes any sense.
  extendDbChangelog :: chlog -> l DiffMK -> chlog

data MapKind = DiffMK

data ApplyMapKind :: MapKind -> Type -> Type -> Type where
  ApplyDiffMK     :: !(UtxoDiff    k v)                    -> ApplyMapKind DiffMK       k v

{-------------------------------------------------------------------------------
-- Nick.HD
-------------------------------------------------------------------------------}

newtype UtxoDiff k v = UtxoDiff (Map k (UtxoEntryDiff v))

data UtxoEntryDiff v = UtxoEntryDiff !v !UtxoEntryDiffState

data UtxoEntryDiffState = UedsDel | UedsIns | UedsInsAndDel
