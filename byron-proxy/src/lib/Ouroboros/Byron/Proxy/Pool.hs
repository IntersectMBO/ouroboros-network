module Ouroboros.Byron.Proxy.Pool
  ( Pool
  , PoolRounds
  , insert
  , lookup
  , withPool
  ) where

import Prelude hiding (lookup)
import Control.Applicative ((<|>))
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (withAsync)
import Control.Concurrent.STM (STM, atomically)
import Control.Concurrent.STM.TVar (TVar, modifyTVar', newTVarIO, readTVar)
import Data.Map (Map)
import qualified Data.Map as Map
import Numeric.Natural (Natural)

-- | Mutable STM interface to a 'PoolRounds k v'.
-- Use `withPool` to get a `Pool` which will be automatically cleared on a
-- given interval. Entries will stay in the `Pool` at least n microseconds and
-- at most 2*n microseconds.
newtype Pool k v = Pool
  { getPool :: TVar (PoolRounds k v)
  }

-- | A current pool and the previous current pool.
data PoolRounds k v = PoolRounds
  { poolCurrent :: !(Map k v)
  , poolRecent  :: !(Map k v)
  }

newRound :: PoolRounds k v -> PoolRounds k v
newRound pr = PoolRounds { poolCurrent = Map.empty, poolRecent = poolCurrent pr }

poolRoundsInsert :: ( Ord k ) => k -> v -> PoolRounds k v -> PoolRounds k v
poolRoundsInsert k v pr = pr { poolCurrent = Map.insert k v (poolCurrent pr) }

poolRoundsLookup :: ( Ord k ) => k -> PoolRounds k v -> Maybe v
poolRoundsLookup k pr =
  Map.lookup k (poolCurrent pr) <|> Map.lookup k (poolRecent pr)

insert :: ( Ord k ) => k -> v -> Pool k v -> STM ()
insert k v pool = modifyTVar' (getPool pool) (poolRoundsInsert k v)

lookup :: ( Ord k ) => k -> Pool k v -> STM (Maybe v)
lookup k pool = do
  rounds <- readTVar (getPool pool)
  pure $ poolRoundsLookup k rounds

-- | Create and use a 'Pool' with rounds of a given length in microseconds.
-- Data will remain in the pool for at least this interval and at most twice
-- this interval.
-- We use 'Pool's to back the inv/req/data relay. The length must be
-- sufficiently long that we can expect all relaying to be done before this
-- interval has passed.
withPool :: Natural -> (Pool k v -> IO t) -> IO t
withPool usNat k = do
  poolVar <- newTVarIO (PoolRounds Map.empty Map.empty)
  withAsync (reaper poolVar) $ \_ -> k (Pool poolVar)
  where
  us :: Int
  us = fromEnum usNat
  reaper poolVar = threadDelay us >> swapRounds poolVar >> reaper poolVar
  swapRounds poolVar = atomically $ modifyTVar' poolVar newRound
