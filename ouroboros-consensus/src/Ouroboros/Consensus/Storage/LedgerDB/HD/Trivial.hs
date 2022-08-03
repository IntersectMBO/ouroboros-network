-- |

module Ouroboros.Consensus.Storage.LedgerDB.HD.Trivial (trivialBackingStore) where

import Data.Proxy

import Cardano.Slotting.Slot

import Ouroboros.Consensus.Storage.LedgerDB.HD.BackingStore

trivialBackingStore ::
     Applicative m
  => Proxy m
  -> Proxy keys
  -> values
  -> Proxy diffs
  -> BackingStore m keys values diff
trivialBackingStore m keys emptyValues _ = BackingStore {
    bsClose       =         pure ()
  , bsCopy        = \_ _ -> pure ()
  , bsValueHandle =         pure (At 0, trivialBackingStoreValueHandle m keys emptyValues)
  , bsWrite       = \_ _ -> pure ()
  }

trivialBackingStoreValueHandle ::
     Applicative m
  => Proxy m
  -> Proxy keys
  -> values
  -> BackingStoreValueHandle m keys values
trivialBackingStoreValueHandle _ _ emptyValues = BackingStoreValueHandle {
    bsvhClose     =       pure ()
  , bsvhRangeRead = \_ -> pure emptyValues
  , bsvhRead      = \_ -> pure emptyValues
  }
