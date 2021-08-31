{-# LANGUAGE RankNTypes #-}
-- |

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
module Example where

import LedgerOnDisk.KVHandle.Class
import Control.Lens
import Prelude hiding (lookup)
import LedgerOnDisk.Mapping.PTMap
import Example.Types
import Example.Rules

prepareLedgerState :: forall dbhandle. (DB LedgerState dbhandle) => dbhandle -> Block -> IO (ReadSet LedgerState dbhandle)
prepareLedgerState handle block = prepare handle (ledgerStateBlockKeys block)

submitLedgerState :: forall a anymap dbhandle. (DB LedgerState dbhandle) => dbhandle -> ReadSet LedgerState dbhandle -> LedgerState anymap -> (LedgerState PTMap  -> (a, LedgerState PTMap)) -> IO a
submitLedgerState handle readset template f = submit handle readset go where
  go odm = let
    (a, LedgerStateMappings
      { sm_utxos = UTxOStateMappings
        { sm_utxo = PTMap _ utxo_diff
        , sm_utxoagg = PTMap _ utxoagg_diff
        }
      }) = f $ template & onDiskMappingsLens .~ odm
    in (a, LedgerStateMappings
         { sm_utxos = UTxOStateMappings
           { sm_utxo = utxo_diff
           , sm_utxoagg = utxoagg_diff
           }
         })
