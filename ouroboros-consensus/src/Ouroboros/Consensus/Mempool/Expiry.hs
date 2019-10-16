{-# LANGUAGE PatternSynonyms #-}

-- | Mempool transaction expiry.
module Ouroboros.Consensus.Mempool.Expiry
  ( ExpiryThreshold (NoExpiryThreshold, ExpiryThreshold)
  , ExpirySlotNo (..)
  , expirySlotNo
  , splitExpiredTxs
  ) where

import           GHC.Stack (HasCallStack)

import           Ouroboros.Consensus.BlockchainTime (NumSlots (..))
import           Ouroboros.Consensus.Mempool.TxSeq (TxSeq (..),
                     splitAfterSlotNo)
import           Ouroboros.Network.Block (SlotNo (..))

{-# COMPLETE NoExpiryThreshold, ExpiryThreshold #-}

-- | The number of slots that must pass before a transaction should be
-- considered expired.
data ExpiryThreshold
  = NoExpiryThreshold
  -- ^ Transactions in the mempool do not expire.
  -- This is useful for tests in which we don't care about transaction expiry.
  | UnsafeExpiryThreshold !NumSlots
  -- ^ Transactions in the mempool expire after a given number of slots pass.
  -- n.b. This constructor is considered unsafe because it can accept values
  -- that are less than or equal to 0. Because of this, it is preferred to use
  -- the smart constructor, 'ExpiryThreshold'.
  deriving Show

-- | A smart constructor for 'ExpiryThreshold' which ensures that the provided
-- 'NumSlots' value is greater than 0.
pattern ExpiryThreshold :: HasCallStack => NumSlots -> ExpiryThreshold
pattern ExpiryThreshold n <- UnsafeExpiryThreshold n where
  ExpiryThreshold numSlots@(NumSlots n)
    | n > 0     = UnsafeExpiryThreshold numSlots
    | otherwise = error "ExpiryThreshold: NumSlots must be greater than 0"

-- | A 'SlotNo' that indicates at which point all prior transactions are
-- to be expired.
data ExpirySlotNo
  = NoExpirySlot
  -- ^ No transactions should be expired. This can happen when we haven't
  -- progressed far enough along the chain to meet the 'ExpiryThreshold'.
  | ExpirySlotNo !SlotNo
  -- ^ The slot number from which all prior transactions are expired.
  deriving (Eq, Show)

-- | 'SlotNo's less than or equal to this are considered to be expired.
expirySlotNo :: SlotNo
             -- ^ The current slot number
             -> ExpiryThreshold
             -- ^ The expiry threshold
             -> ExpirySlotNo
expirySlotNo currentSlotNo expThreshold = case expThreshold of
  NoExpiryThreshold -> NoExpirySlot
  ExpiryThreshold (NumSlots et) ->
    if unSlotNo currentSlotNo >= fromIntegral et
      then ExpirySlotNo $ SlotNo
            (unSlotNo currentSlotNo - fromIntegral et)
      else NoExpirySlot

-- | Split the expired transactions from the unexpired:
--
-- * 'fst' of the result are expired
-- * 'snd' of the result are unexpired
splitExpiredTxs :: TxSeq tx
                -> ExpirySlotNo
                -> (TxSeq tx, TxSeq tx)
splitExpiredTxs txSeq expSlotNo = case expSlotNo of
  NoExpirySlot    -> (Empty, txSeq)
  ExpirySlotNo sn -> splitAfterSlotNo txSeq sn
