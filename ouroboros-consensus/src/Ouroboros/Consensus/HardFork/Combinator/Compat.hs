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
import           Ouroboros.Consensus.Ledger.Basics (FootprintL (..))
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
data HardForkCompatQuery blk :: FootprintL -> Type -> Type where
  CompatIfCurrent ::
       BlockQuery          blk fp result
    -> HardForkCompatQuery blk fp result

  CompatAnytime ::
       QueryAnytime            SmallL result
    -> EraIndex (HardForkIndices blk)
    -> HardForkCompatQuery blk SmallL result

  CompatHardFork ::
       QueryHardFork (HardForkIndices blk) SmallL result
    -> HardForkCompatQuery blk             SmallL result

{-------------------------------------------------------------------------------
  Convenience constructors for 'HardForkCompatQuery'
-------------------------------------------------------------------------------}

-- | Submit query to underlying ledger
compatIfCurrent ::
     BlockQuery          blk fp result
  -> HardForkCompatQuery blk fp result
compatIfCurrent = CompatIfCurrent

-- | Get the start of the specified era, if known
compatGetEraStart ::
     EraIndex (HardForkIndices blk)
  -> HardForkCompatQuery blk SmallL (Maybe Bound)
compatGetEraStart = CompatAnytime GetEraStart

-- | Get an interpreter for history queries
--
-- I.e., this can be used for slot/epoch/time conversions.
compatGetInterpreter ::
    HardForkCompatQuery blk SmallL (Qry.Interpreter (HardForkIndices blk))
compatGetInterpreter = CompatHardFork GetInterpreter

{-------------------------------------------------------------------------------
  Wrappers
-------------------------------------------------------------------------------}

-- | Wrapper used when connecting to a server that's running the HFC with
-- at least two eras
forwardCompatQuery ::
       forall m x xs. IsNonEmpty xs
    => (forall fp result. BlockQuery (HardForkBlock (x ': xs)) fp result -> m result)
    -- ^ Submit a query through the LocalStateQuery protocol.
    -> (forall fp result. HardForkCompatQuery (HardForkBlock (x ': xs)) fp result -> m result)
forwardCompatQuery f = go
  where
    go :: HardForkCompatQuery (HardForkBlock (x ': xs)) fp result -> m result
    go (CompatIfCurrent qry)    = f qry
    go (CompatAnytime   qry ix) = f (QueryAnytime qry ix)
    go (CompatHardFork  qry)    = f (QueryHardFork qry)

-- | Wrapper used when connecting to a server that's not using the HFC, or
-- is using the HFC but with a single era only.
singleEraCompatQuery ::
       forall m blk era. (Monad m, HardForkIndices blk ~ '[era])
    => EpochSize
    -> SlotLength
    -> (forall fp result. BlockQuery blk fp result -> m result)
    -- ^ Submit a query through the LocalStateQuery protocol.
    -> (forall fp result. HardForkCompatQuery blk fp result -> m result)
singleEraCompatQuery epochSize slotLen f = go
  where
    go :: HardForkCompatQuery blk fp result -> m result
    go (CompatIfCurrent qry)    = f qry
    go (CompatAnytime   qry ix) = const (goAnytime qry) (trivialIndex ix)
    go (CompatHardFork  qry)    = goHardFork qry

    goAnytime :: QueryAnytime fp result -> m result
    goAnytime GetEraStart = return $ Just initBound

    goHardFork :: QueryHardFork '[era] fp result -> m result
    goHardFork GetInterpreter = return $ Qry.mkInterpreter summary
    goHardFork GetCurrentEra  = return $ eraIndexZero

    summary :: Summary '[era]
    summary = neverForksSummary epochSize slotLen

    trivialIndex :: EraIndex '[era] -> ()
    trivialIndex (EraIndex (Z (K ()))) = ()
