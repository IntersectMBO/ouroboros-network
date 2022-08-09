{-# LANGUAGE TupleSections #-}
-- |

module Ouroboros.Consensus.Storage.LedgerDB.HD.Trivial (trivialBackingStore) where

import Data.Proxy

import Cardano.Slotting.Slot

import Ouroboros.Consensus.Storage.LedgerDB.HD.BackingStore
import Ouroboros.Consensus.Util.IOLike

trivialBackingStore ::
     IOLike m
  => Proxy m
  -> Proxy keys
  -> values
  -> Proxy diffs
  -> m (BackingStore m keys values diff)
trivialBackingStore m keys emptyValues _ = do
  slot <- newTVarIO Origin
  pure BackingStore {
    bsClose       =         pure ()
  , bsCopy        = \_ _ -> pure ()
  , bsValueHandle =         (, trivialBackingStoreValueHandle m keys emptyValues) <$> atomically (readTVar slot)
  , bsWrite       = \newSlot _ -> do
      atomically $ writeTVar slot (At newSlot)
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
