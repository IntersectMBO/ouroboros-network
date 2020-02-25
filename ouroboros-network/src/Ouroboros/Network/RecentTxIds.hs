{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Ouroboros.Network.RecentTxIds
  ( RecentTxIds (..)

    -- * Query
  , null
  , member
  , intersection
  , earliestExpiryTime

    -- * Construction
  , empty

    -- * Insertion
  , insertTxId
  , insertTxIds

    -- * Expiration
  , expireTxIds

    -- * Conversion
  , toList
  ) where

import           Cardano.Prelude (NoUnexpectedThunks, OnlyCheckIsWHNF (..))
import           Prelude hiding (null)

import           Control.Monad.Class.MonadTime (Time)
import           Data.Foldable (foldl')
import qualified Data.List as List (filter)
import qualified Data.OrdPSQ as OrdPSQ (atMostView, empty, findMin, fold',
                     insert, member, null)
import           Data.OrdPSQ (OrdPSQ)

-- | A collection of transaction IDs that we've most recently added to the
-- mempool from instances of the transaction submission server.
--
-- Each transaction ID is associated with an expiry 'Time' at which they
-- should be removed from the collection.
newtype RecentTxIds txid = RecentTxIds (OrdPSQ txid Time ())
  deriving stock (Show)
  deriving newtype (NoUnexpectedThunks)

{-----------------------------------------------------------------------------
  Query
-----------------------------------------------------------------------------}

-- | \( O(1) \). Whether the collection is empty.
null :: RecentTxIds txid -> Bool
null (RecentTxIds psq) = OrdPSQ.null psq

-- | \( O(\log(n)) \). Determine whether the provided transaction ID is
-- present in the 'RecentTxIds'.
member :: Ord txid => txid -> RecentTxIds txid -> Bool
member txId (RecentTxIds psq) = OrdPSQ.member txId psq

-- | \( O(m*\log(n)) \). Determine which of the provided transaction IDs
-- already exist within the provided 'RecentTxIds'.
intersection :: Ord txid => [txid] -> RecentTxIds txid -> [txid]
intersection txIds (RecentTxIds psq) = List.filter (flip OrdPSQ.member psq) txIds

-- | \( O(1) \). Find the earliest expiry time of all the elements in the
-- collection.
--
-- If the collection is empty, this will return 'Nothing'.
earliestExpiryTime :: Ord txid => RecentTxIds txid -> Maybe Time
earliestExpiryTime (RecentTxIds psq) =
  (\(_, time, _) -> time) <$> OrdPSQ.findMin psq

{-----------------------------------------------------------------------------
  Construction
-----------------------------------------------------------------------------}

-- | \( O(1) \). An empty 'RecentTxIds'.
empty :: RecentTxIds txid
empty = RecentTxIds OrdPSQ.empty

{-----------------------------------------------------------------------------
  Insertion
-----------------------------------------------------------------------------}

-- | \( O(\log(n)) \). Insert a new transaction ID and 'Time' (expected to
-- represent the expiry time) into the 'RecentTxIds'.
insertTxId :: Ord txid => txid -> Time -> RecentTxIds txid -> RecentTxIds txid
insertTxId txId time (RecentTxIds psq) = RecentTxIds $
  OrdPSQ.insert txId time () psq

-- | \( O(m*\log(n)) \). Insert new transaction IDs and 'Time' (expected to
-- represent the expiry time) into the 'RecentTxIds'.
insertTxIds :: (Foldable t, Ord txid)
            => t txid
            -> Time
            -> RecentTxIds txid
            -> RecentTxIds txid
insertTxIds txIds time txPsq = foldl'
  (\acc txId -> insertTxId txId time acc)
  txPsq
  txIds

{-----------------------------------------------------------------------------
  Expiration
-----------------------------------------------------------------------------}

-- | Expire/remove transaction IDs that are associated with expiry 'Time's
-- less than or equal to the provided 'Time'.
--
-- The 'fst' of the result contains those transaction IDs that are expired
-- while the 'snd' of the result contains the rest of the 'RecentTxIds' stripped
-- of those transaction IDs.
expireTxIds :: Ord txid
            => Time
            -> RecentTxIds txid
            -> ([(txid, Time)], RecentTxIds txid)
expireTxIds time (RecentTxIds psq) = (expired', RecentTxIds psq')
  where
    (expired, psq') = OrdPSQ.atMostView time psq
    expired' = [(txId, time') | (txId, time', _) <- expired]

{-----------------------------------------------------------------------------
  Conversion
-----------------------------------------------------------------------------}

-- | \( O(n) \). Convert to a list of pairs of transaction IDs and their
-- expiry times. The order of the list is not specified.
toList :: RecentTxIds txid -> [(txid, Time)]
toList (RecentTxIds psq) = OrdPSQ.fold'
  (\k p _ acc -> (k, p):acc)
  []
  psq

{-----------------------------------------------------------------------------
  Orphan instances
-----------------------------------------------------------------------------}

deriving via OnlyCheckIsWHNF "OrdPSQ" (OrdPSQ k p v)
  instance NoUnexpectedThunks (OrdPSQ k p v)
