{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}

module ConsensusLedger.DbChangelog.Impl.DuncanAndDouglas where

import           ConsensusLedger.DbChangelog

import           Data.Kind (Type)
import qualified DuncanAndDouglas.DbChangelog as DD

instance DbChangelog (DD.DbChangelog MediatedL) l where
  extendDbChangelog dbchlog ldiff =
    DD.extendDbChangelog
      undefined -- SeqNo MediatedL
      (translateDiff ldiff) -- MediatedL TableDiff
      Nothing
      dbchlog

translateDiff :: l DiffMK -> MediatedL DD.TableDiff
translateDiff = undefined


f :: l DiffMK -> ApplyMapKind DiffMK k v
f = undefined

data MediatedL (ml :: (DD.TableType -> Type -> Type -> Type))

instance DD.HasOnDiskTables MediatedL where
  data Tables MediatedL tk = XXX

instance DD.HasTables MediatedL where

instance DD.HasTables (DD.Tables MediatedL) where

instance DD.HasOnlyTables (DD.Tables MediatedL) where
