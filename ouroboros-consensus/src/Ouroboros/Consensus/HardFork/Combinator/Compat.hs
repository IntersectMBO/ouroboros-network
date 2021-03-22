{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE EmptyCase           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

module Ouroboros.Consensus.HardFork.Combinator.Compat (
    HardForkCompatQuery (..)
    -- * Convenience constructors
  , compatGetEraStart
  , compatGetInterpreter
  , compatIfCurrent
    -- * Wrappers
  , forwardCompatQuery
  , singleEraCompatQuery
  ) where

import           Data.Kind (Type)
import           Data.SOP.Strict

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.BlockchainTime
import           Ouroboros.Consensus.HardFork.History.Summary (Bound, Summary,
                     initBound, neverForksSummary)
import           Ouroboros.Consensus.Util.SOP

import           Ouroboros.Consensus.HardFork.Abstract
import           Ouroboros.Consensus.HardFork.Combinator.Abstract.SingleEraBlock
import           Ouroboros.Consensus.HardFork.Combinator.Basics
import           Ouroboros.Consensus.HardFork.Combinator.Ledger.Query
import qualified Ouroboros.Consensus.HardFork.History.Qry as Qry

{-------------------------------------------------------------------------------
  Query language
-------------------------------------------------------------------------------}

-- | Version of @Query (HardForkBlock xs)@ without the restriction to have
-- at least two eras
data HardForkCompatQuery blk :: Type -> Type where
  CompatIfCurrent ::
       Query blk result
    -> HardForkCompatQuery blk result

  CompatAnytime ::
       QueryAnytime result
    -> EraIndex (HardForkIndices blk)
    -> HardForkCompatQuery blk result

  CompatHardFork ::
       QueryHardFork (HardForkIndices blk) result
    -> HardForkCompatQuery blk result

{-------------------------------------------------------------------------------
  Convenience constructors for 'HardForkCompatQuery'
-------------------------------------------------------------------------------}

-- | Submit query to underlying ledger
compatIfCurrent ::
     Query blk result
  -> HardForkCompatQuery blk result
compatIfCurrent = CompatIfCurrent

-- | Get the start of the specified era, if known
compatGetEraStart ::
     EraIndex (HardForkIndices blk)
  -> HardForkCompatQuery blk (Maybe Bound)
compatGetEraStart = CompatAnytime GetEraStart

-- | Get an interpreter for history queries
--
-- I.e., this can be used for slot/epoch/time conversions.
compatGetInterpreter ::
    HardForkCompatQuery blk (Qry.Interpreter (HardForkIndices blk))
compatGetInterpreter = CompatHardFork GetInterpreter

{-------------------------------------------------------------------------------
  Wrappers
-------------------------------------------------------------------------------}

-- | Wrapper used when connecting to a server that's running the HFC with
-- at least two eras
forwardCompatQuery ::
       forall m x xs. IsNonEmpty xs
    => (forall result. Query (HardForkBlock (x ': xs)) result -> m result)
    -- ^ Submit a query through the LocalStateQuery protocol.
    -> (forall result. HardForkCompatQuery (HardForkBlock (x ': xs)) result -> m result)
forwardCompatQuery f = go
  where
    go :: HardForkCompatQuery (HardForkBlock (x ': xs)) result -> m result
    go (CompatIfCurrent qry)    = f qry
    go (CompatAnytime   qry ix) = f (QueryAnytime qry ix)
    go (CompatHardFork  qry)    = f (QueryHardFork qry)

-- | Wrapper used when connecting to a server that's not using the HFC, or
-- is using the HFC but with a single era only.
singleEraCompatQuery ::
       forall m blk era. (Monad m, HardForkIndices blk ~ '[era])
    => EpochSize
    -> SlotLength
    -> (forall result. Query blk result -> m result)
    -- ^ Submit a query through the LocalStateQuery protocol.
    -> (forall result. HardForkCompatQuery blk result -> m result)
singleEraCompatQuery epochSize slotLen f = go
  where
    go :: HardForkCompatQuery blk result -> m result
    go (CompatIfCurrent qry)    = f qry
    go (CompatAnytime   qry ix) = const (goAnytime qry) (trivialIndex ix)
    go (CompatHardFork  qry)    = goHardFork qry

    goAnytime :: QueryAnytime result -> m result
    goAnytime GetEraStart = return $ Just initBound

    goHardFork :: QueryHardFork '[era] result -> m result
    goHardFork GetInterpreter = return $ Qry.mkInterpreter summary
    goHardFork GetCurrentEra  = return $ eraIndexZero

    summary :: Summary '[era]
    summary = neverForksSummary epochSize slotLen

    trivialIndex :: EraIndex '[era] -> ()
    trivialIndex (EraIndex (Z (K ()))) = ()
