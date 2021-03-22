{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE ViewPatterns               #-}
-- | Intended for qualified import.
--
-- > import           Ouroboros.Consensus.Mempool.TxSeq (TxSeq (..))
-- > import qualified Ouroboros.Consensus.Mempool.TxSeq as TxSeq
module Ouroboros.Consensus.Mempool.TxSeq (
    TicketNo (..)
  , TxSeq (Empty, (:>), (:<))
  , TxTicket (..)
  , fromList
  , lookupByTicketNo
  , splitAfterTicketNo
  , splitAfterTxSize
  , toList
  , toMempoolSize
  , toTuples
  , zeroTicketNo
    -- * Reference implementations for testing
  , splitAfterTxSizeSpec
  ) where

import           Data.FingerTree.Strict (StrictFingerTree)
import qualified Data.FingerTree.Strict as FingerTree
import qualified Data.Foldable as Foldable
import           Data.Word (Word64)
import           GHC.Generics (Generic)
import           NoThunks.Class (NoThunks)

import           Ouroboros.Network.Protocol.TxSubmission.Type (TxSizeInBytes)

import           Ouroboros.Consensus.Mempool.API (MempoolSize (..))

{-------------------------------------------------------------------------------
  Mempool transaction sequence as a finger tree
-------------------------------------------------------------------------------}

-- | We allocate each transaction a (monotonically increasing) ticket number
-- as it enters the mempool.
--
newtype TicketNo = TicketNo Word64
  deriving stock (Eq, Ord, Show)
  deriving newtype (Enum, Bounded, NoThunks)

-- | The transaction ticket number from which our counter starts.
zeroTicketNo :: TicketNo
zeroTicketNo = TicketNo 0

-- | We associate transactions in the mempool with their ticket number and
-- size in bytes.
--
data TxTicket tx = TxTicket
  { txTicketTx            :: !tx
    -- ^ The transaction associated with this ticket.
  , txTicketNo            :: !TicketNo
    -- ^ The ticket number.
  , txTicketTxSizeInBytes :: !TxSizeInBytes
    -- ^ The byte size of the transaction ('txTicketTx') associated with this
    -- ticket.
  } deriving (Eq, Show, Generic, NoThunks)

-- | The mempool is a sequence of transactions with their ticket numbers and
-- size in bytes.
--
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
newtype TxSeq tx = TxSeq (StrictFingerTree TxSeqMeasure (TxTicket tx))
  deriving stock   (Show)
  deriving newtype (NoThunks)

instance Foldable TxSeq where
  foldMap f (TxSeq txs) = Foldable.foldMap (f . txTicketTx) txs
  null      (TxSeq txs) = Foldable.null txs
  length    (TxSeq txs) = mSize $ FingerTree.measure txs

-- | The 'StrictFingerTree' relies on a \"measure\" for subsequences in the
-- tree. A measure of the size of the subsequence allows for efficient
-- sequence operations. Also measuring the min and max ticket number allows
-- for efficient operations based on the ticket number (assuming the sequence
-- is ordered by ticket number).
--
-- To use a 'StrictFingerTree' with a 'TxSeqMeasure' we have to provide a way
-- to measure individual elements of the sequence (i.e. 'TxTicket's), via a
-- 'Measured' instance, and also a way to combine the measures, via a 'Monoid'
-- instance.
--
data TxSeqMeasure = TxSeqMeasure {
       mMinTicket :: !TicketNo,
       mMaxTicket :: !TicketNo,
       mSizeBytes :: !TxSizeInBytes,
       mSize      :: !Int
     }
  deriving Show

instance FingerTree.Measured TxSeqMeasure (TxTicket tx) where
  measure (TxTicket _ tno tsz) = TxSeqMeasure tno tno tsz 1

instance Semigroup TxSeqMeasure where
  vl <> vr = TxSeqMeasure
               (mMinTicket vl `min` mMinTicket vr)
               (mMaxTicket vl `max` mMaxTicket vr)
               (mSizeBytes vl   +   mSizeBytes vr)
               (mSize      vl   +   mSize      vr)

instance Monoid TxSeqMeasure where
  mempty  = TxSeqMeasure maxBound minBound 0 0
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


-- | \( O(\log(n)) \). Look up a transaction in the sequence by its 'TicketNo'.
--
lookupByTicketNo :: TxSeq tx -> TicketNo -> Maybe tx
lookupByTicketNo (TxSeq txs) n =
    case FingerTree.search (\ml mr -> mMaxTicket ml >= n
                                   && mMinTicket mr >  n) txs of
      FingerTree.Position _ (TxTicket tx n' _) _ | n' == n -> Just tx
      _                                                    -> Nothing

-- | \( O(\log(n)) \). Split the sequence of transactions into two parts
-- based on the given 'TicketNo'. The first part has transactions with tickets
-- less than or equal to the given ticket, and the second part has transactions
-- with tickets strictly greater than the given ticket.
--
splitAfterTicketNo :: TxSeq tx -> TicketNo -> (TxSeq tx, TxSeq tx)
splitAfterTicketNo (TxSeq txs) n =
    case FingerTree.split (\m -> mMaxTicket m > n) txs of
      (l, r) -> (TxSeq l, TxSeq r)

-- | \( O(\log(n)) \). Split the sequence of transactions into two parts
-- based on the given 'TxSizeInBytes'. The first part has transactions whose
-- summed 'TxSizeInBytes' is less than or equal to the given 'TxSizeInBytes',
-- and the second part has the remaining transactions in the sequence.
--
splitAfterTxSize :: TxSeq tx -> TxSizeInBytes -> (TxSeq tx, TxSeq tx)
splitAfterTxSize (TxSeq txs) n =
    case FingerTree.split (\m -> mSizeBytes m > n) txs of
      (l, r) -> (TxSeq l, TxSeq r)

-- | \( O(n) \). Specification of 'splitAfterTxSize'.
--
-- Use 'splitAfterTxSize' as it should be faster.
--
-- This function is used to verify whether 'splitAfterTxSize' behaves as
-- expected.
splitAfterTxSizeSpec :: TxSeq tx -> TxSizeInBytes -> (TxSeq tx, TxSeq tx)
splitAfterTxSizeSpec txseq n =
    mapTuple fromList $ go 0 [] (toList txseq)
  where
    mapTuple :: (a -> b) -> (a, a) -> (b, b)
    mapTuple f (x, y) = (f x, f y)

    go :: TxSizeInBytes
       -> [TxTicket tx]
       -> [TxTicket tx]
       -> ([TxTicket tx], [TxTicket tx])
    go accByteSize accTickets = \case
      []
        -> (reverse accTickets, [])
      t:ts
        | let accByteSize' = accByteSize + txTicketTxSizeInBytes t
        , accByteSize' <= n
        -> go accByteSize' (t:accTickets) ts
        | otherwise
        -> (reverse accTickets, t:ts)

-- | Given a list of 'TxTicket's, construct a 'TxSeq'.
fromList :: [TxTicket tx] -> TxSeq tx
fromList = Foldable.foldl' (:>) Empty

-- | Convert a 'TxSeq' to a list of 'TxTicket's.
toList :: TxSeq tx -> [TxTicket tx]
toList (TxSeq ftree) = Foldable.toList ftree

-- | Convert a 'TxSeq' to a list of pairs of transactions and their
-- associated 'TicketNo's.
toTuples :: TxSeq tx -> [(tx, TicketNo)]
toTuples (TxSeq ftree) = fmap
    (\ticket -> (txTicketTx ticket, txTicketNo ticket))
    (Foldable.toList ftree)

-- | \( O(1) \). Return the 'MempoolSize' of the given 'TxSeq'.
toMempoolSize :: TxSeq tx -> MempoolSize
toMempoolSize (TxSeq ftree) = MempoolSize
    { msNumTxs   = fromIntegral mSize
    , msNumBytes = mSizeBytes
    }
  where
    TxSeqMeasure { mSizeBytes, mSize } = FingerTree.measure ftree
