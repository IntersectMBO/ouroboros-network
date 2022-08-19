{-# LANGUAGE TupleSections #-}
-- | A trivial backing store that has no tables and just updates the slot number.

module Ouroboros.Consensus.Storage.LedgerDB.HD.Trivial (trivialBackingStore) where

import           Data.Maybe (fromMaybe)
import           Data.Proxy

import           Cardano.Slotting.Slot

import           Ouroboros.Consensus.Storage.LedgerDB.HD.BackingStore
import           Ouroboros.Consensus.Util.IOLike

trivialBackingStore ::
     IOLike m
  => Proxy m
  -> Proxy keys
  -> values
  -> Proxy diffs
  -> Maybe (WithOrigin SlotNo)
  -> m (BackingStore m keys values diff)
trivialBackingStore m keys emptyValues _ ms = do
  slot <- newTVarIO $ fromMaybe Origin ms
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
