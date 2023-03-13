{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Ouroboros.Consensus.HardFork.History.Caching (
    RunWithCachedSummary (..)
  , runWithCachedSummary
  ) where

import           Data.Kind (Type)
import           Ouroboros.Consensus.HardFork.History.Qry
import           Ouroboros.Consensus.HardFork.History.Summary
import           Ouroboros.Consensus.Util.IOLike

{-------------------------------------------------------------------------------
  Caching the summary
-------------------------------------------------------------------------------}

-- | Stateful abstraction to execute queries
data RunWithCachedSummary (xs :: [Type]) m = RunWithCachedSummary {
      -- | Run the specified query
      --
      -- If the query fails with a 'PastHorizonException', it will update its
      -- internal state (compute a new summary) and try again. If that /still/
      -- fails, the 'PastHorizonException' is returned.
      --
      cachedRunQuery :: forall a. Qry a
                     -> STM m (Either PastHorizonException a)
    }

-- | Construct 'RunWithCachedSummary' given action that computes the summary
--
-- Most use cases will probably construct this action from an action that reads
-- the ledger state and then computes the summary from that.
runWithCachedSummary :: forall m xs. MonadSTM m
                     => STM m (Summary xs)
                     -> m (RunWithCachedSummary xs m)
runWithCachedSummary getLatestSummary = do
    initSummary <- atomically getLatestSummary
    var <- newTVarIO initSummary
    return $ RunWithCachedSummary { cachedRunQuery = go var }
  where
    go :: StrictTVar m (Summary xs)
       -> Qry a -> STM m (Either PastHorizonException a)
    go var q = do
        summary <- readTVar var
        case runQuery q summary of
          Right a             -> return (Right a)
          Left  PastHorizon{} -> do
            summary' <- getLatestSummary
            writeTVar var summary'
            return $ runQuery q summary'
