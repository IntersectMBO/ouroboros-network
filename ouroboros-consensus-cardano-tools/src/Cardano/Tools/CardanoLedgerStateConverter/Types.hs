
module Cardano.Tools.CardanoLedgerStateConverter.Types
  ( SelectDB (..)
  , Config (..)
  ) where

import           Ouroboros.Consensus.Storage.LedgerDB.OnDisk (DiskSnapshot)


data SelectDB = SelectImmutableDB (Maybe DiskSnapshot)

data Config = Config {
    dbDir      :: FilePath
  , selectDB   :: SelectDB
  }
