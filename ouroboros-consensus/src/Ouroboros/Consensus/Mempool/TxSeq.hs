{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE ViewPatterns               #-}

module Ouroboros.Consensus.Mempool.TxSeq (
    TicketNo(..)
  , TxTicket(..)
  , TxSeq(Empty, (:>), (:<))
  , fromTxSeq
  , lookupByTicketNo
  , splitAfterTicketNo
  , zeroTicketNo
  ) where

import           Data.FingerTree (FingerTree)
import qualified Data.FingerTree as FingerTree
import qualified Data.Foldable as Foldable
import           Data.Word (Word64)


{-------------------------------------------------------------------------------
  Mempool transaction sequence as a finger tree
-------------------------------------------------------------------------------}

-- | We allocate each transaction a (monotonically increasing) ticket number
-- as it enters the mempool.
--
newtype TicketNo = TicketNo Word64
  deriving (Eq, Ord, Enum, Bounded, Show)

-- | The transaction ticket number from which our counter starts.
zeroTicketNo :: TicketNo
zeroTicketNo = TicketNo 0

-- | We pair up transactions in the mempool with their ticket number.
--
data TxTicket tx = TxTicket !tx !TicketNo
  deriving Show

-- | The mempool is a sequence of transactions with their ticket numbers.
-- Transactions are allocated monotonically increasing ticket numbers as they
-- are appended to the mempool sequence. Transactions can be removed from any
-- position, not just the front.
--
-- The sequence is thus ordered by the ticket numbers. We can use the ticket
-- numbers as a compact representation for a \"reader\" location in the
-- sequence. If a reader knows it has seen all txs with a lower ticket number
-- then it is only interested in transactions with higher ticket numbers.
--
-- The mempool sequence is represented by a fingertree. We use a fingertree
-- measure to allow not just normal sequence operations but also efficient
-- splitting and indexing by the ticket number.
--
newtype TxSeq tx = TxSeq (FingerTree TxSeqMeasure (TxTicket tx))
  deriving Show

instance Foldable TxSeq where
  foldMap f (TxSeq txs) = Foldable.foldMap (f . (\(TxTicket tx _) -> tx)) txs
  null      (TxSeq txs) = Foldable.null txs
  length    (TxSeq txs) = mSize $ FingerTree.measure txs

-- | The 'FingerTree' relies on a \"measure\" for subsequences in the tree.
-- A measure of the size of the subsequence allows for efficient sequence
-- operations. Also measuing the min and max ticket number allows for efficient
-- operations based on the ticket number (assuming the sequence is ordered by
-- ticket number).
--
-- To use a 'FingerTree' with a 'TxSeqMeasure' we have to provide a way to
-- measure individual elements of the sequence (i.e. 'TxTicket's), via a
-- 'Measured' instance, and also a way to combine the measures, via a 'Monoid'
-- instance.
--
data TxSeqMeasure = TxSeqMeasure {
       mMinTicket :: !TicketNo,
       mMaxTicket :: !TicketNo,
       mSize      :: !Int
     }
  deriving Show

instance FingerTree.Measured TxSeqMeasure (TxTicket tx) where
  measure (TxTicket _ tno) = TxSeqMeasure tno tno 1

instance Semigroup TxSeqMeasure where
  vl <> vr = TxSeqMeasure
               (mMinTicket vl `min` mMinTicket vr)
               (mMaxTicket vl `max` mMaxTicket vr)
               (mSize      vl   +   mSize      vr)

instance Monoid TxSeqMeasure where
  mempty  = TxSeqMeasure maxBound minBound 0
  mappend = (<>)

-- | A helper function for the ':>' pattern.
--
viewBack :: TxSeq tx -> Maybe (TxSeq tx, TxTicket tx)
viewBack (TxSeq txs) = case FingerTree.viewr txs of
                         FingerTree.EmptyR     -> Nothing
                         txs' FingerTree.:> tx -> Just (TxSeq txs', tx)

-- | A helper function for the ':<' pattern.
--
viewFront :: TxSeq tx -> Maybe (TxTicket tx, TxSeq tx)
viewFront (TxSeq txs) = case FingerTree.viewl txs of
                          FingerTree.EmptyL     -> Nothing
                          tx FingerTree.:< txs' -> Just (tx, TxSeq txs')

-- | An empty mempool sequence.
--
pattern Empty :: TxSeq tx
pattern Empty <- (viewFront -> Nothing) where
  Empty = TxSeq FingerTree.empty

-- | \( O(1) \). Access or add a tx at the back of the mempool sequence.
--
-- New txs are always added at the back.
--
pattern (:>) :: TxSeq tx -> TxTicket tx -> TxSeq tx
pattern txs :> tx <- (viewBack -> Just (txs, tx)) where
  TxSeq txs :> tx = TxSeq (txs FingerTree.|> tx)  --TODO: assert ordered by ticket no

-- | \( O(1) \). Access a tx at the front of the mempool sequence.
--
-- Note that we never add txs at the front. We access txs from front to back
-- when forwarding txs to other peers, or when adding txs to blocks.
--
pattern (:<) :: TxTicket tx -> TxSeq tx -> TxSeq tx
pattern tx :< txs <- (viewFront -> Just (tx, txs))

infixl 5 :>, :<

{-# COMPLETE Empty, (:>) #-}
{-# COMPLETE Empty, (:<) #-}


-- | \( O(\log(n) \). Look up a transaction in the sequence by its 'TicketNo'.
--
lookupByTicketNo :: TxSeq tx -> TicketNo -> Maybe tx
lookupByTicketNo (TxSeq txs) n =
    case FingerTree.search (\ml mr -> mMaxTicket ml >= n
                                   && mMinTicket mr >  n) txs of
      FingerTree.Position _ (TxTicket tx _) _ -> Just tx
      _                                       -> Nothing

-- | \( O(\log(n) \). Split the sequence of transactions into two parts
-- based on the given 'TicketNo'. The first part has transactions with tickets
-- less than or equal to the given ticket, and the second part has transactions
-- with tickets strictly greater than the given ticket.
--
splitAfterTicketNo :: TxSeq tx -> TicketNo -> (TxSeq tx, TxSeq tx)
splitAfterTicketNo (TxSeq txs) n =
    case FingerTree.split (\m -> mMaxTicket m > n) txs of
      (l, r) -> (TxSeq l, TxSeq r)

-- | Convert a 'TxSeq' to a list of pairs of transactions and their
-- associated 'TicketNo's.
fromTxSeq :: TxSeq tx -> [(tx, TicketNo)]
fromTxSeq (TxSeq ftree) = fmap
  (\(TxTicket tx tn) -> (tx, tn))
  (Foldable.toList $ ftree)
