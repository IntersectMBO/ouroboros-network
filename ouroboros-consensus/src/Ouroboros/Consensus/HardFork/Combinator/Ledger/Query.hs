{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Ouroboros.Consensus.HardFork.Combinator.Ledger.Query (
    Query(..)
  , HardForkQuery(..)
  , HardForkQueryResult
  , getHardForkQuery
  , hardForkQueryInfo
  ) where

import           Data.Bifunctor
import           Data.Functor.Product
import           Data.Proxy
import           Data.SOP.Strict
import           Data.Type.Equality

import           Ouroboros.Consensus.Ledger.Abstract

import           Ouroboros.Consensus.HardFork.Combinator.Abstract
import           Ouroboros.Consensus.HardFork.Combinator.AcrossEras
import           Ouroboros.Consensus.HardFork.Combinator.Basics
import           Ouroboros.Consensus.HardFork.Combinator.Info
import           Ouroboros.Consensus.HardFork.Combinator.Ledger ()
import           Ouroboros.Consensus.HardFork.Combinator.PartialConfig
import qualified Ouroboros.Consensus.HardFork.Combinator.State as State
import           Ouroboros.Consensus.HardFork.Combinator.Util.Match
                     (Mismatch (..))

instance CanHardFork xs => ShowQuery (Query (HardForkBlock xs)) where
  showResult = \(HardForkQuery qry) mResult ->
      case mResult of
        Left  err    -> show err
        Right result -> go qry result
    where
      go :: All SingleEraBlock xs'
         => HardForkQuery xs' result -> result -> String
      go (QZ qry) = showResult qry
      go (QS qry) = go qry

type HardForkQueryResult xs = Either (MismatchEraInfo xs)

instance CanHardFork xs => QueryLedger (HardForkBlock xs) where
  data Query (HardForkBlock xs) :: * -> * where
    HardForkQuery :: HardForkQuery xs result
                  -> Query (HardForkBlock xs) (HardForkQueryResult xs result)

  answerQuery hardForkConfig@HardForkLedgerConfig{..}
              (HardForkQuery hardForkQuery)
              (HardForkLedgerState hardForkState) =
      go hardForkQuery (hzipWith Pair cfgs (State.tip hardForkState))
    where
      cfgs = getPerEraLedgerConfig hardForkLedgerConfigPerEra
      ei   = State.epochInfoLedger hardForkConfig hardForkState

      go :: All SingleEraBlock xs'
         => HardForkQuery xs' result
         -> NS (Product WrapPartialLedgerConfig LedgerState) xs'
         -> HardForkQueryResult xs' result
      go (QZ qry) (Z (Pair cfg st)) =
          Right $ answerQuery (completeLedgerConfig' ei cfg) qry st
      go (QS qry) (S st) =
          first shiftMismatch $ go qry st
      go (QZ qry) (S st) =
          Left $ MismatchEraInfo $ ML (queryInfo qry) (hcmap proxySingle ledgerInfo st)
      go (QS qry) (Z st) =
          Left $ MismatchEraInfo $ MR (hardForkQueryInfo qry) (ledgerInfo st)

  eqQuery = \(HardForkQuery qry) (HardForkQuery qry') ->
      -- Lift the type equality to the @Either@
      case go qry qry' of
        Nothing   -> Nothing
        Just Refl -> Just Refl
    where
      go :: All SingleEraBlock xs'
         => HardForkQuery xs' result
         -> HardForkQuery xs' result'
         -> Maybe (result :~: result')
      go (QZ qry) (QZ qry') = eqQuery qry qry'
      go (QS qry) (QS qry') = go qry qry'
      go _        _         = Nothing

deriving instance CanHardFork xs => Show (Query (HardForkBlock xs) result)

getHardForkQuery :: Query (HardForkBlock xs) result
                 -> (forall result'.
                          result :~: HardForkQueryResult xs result'
                       -> HardForkQuery xs result'
                       -> a)
                 -> a
getHardForkQuery (HardForkQuery qry) k = k Refl qry

{-------------------------------------------------------------------------------
  Queries
-------------------------------------------------------------------------------}

data HardForkQuery :: [*] -> * -> * where
  QZ :: Query x result          -> HardForkQuery (x ': xs) result
  QS :: HardForkQuery xs result -> HardForkQuery (x ': xs) result

deriving instance All SingleEraBlock xs => Show (HardForkQuery xs result)

{-------------------------------------------------------------------------------
  Auxiliary
-------------------------------------------------------------------------------}

ledgerInfo :: forall blk. SingleEraBlock blk
           => Product WrapPartialLedgerConfig LedgerState blk
           -> LedgerEraInfo blk
ledgerInfo _ = LedgerEraInfo $ singleEraInfo (Proxy @blk)

queryInfo :: forall blk result. SingleEraBlock blk
          => Query blk result -> SingleEraInfo blk
queryInfo _ = singleEraInfo (Proxy @blk)

hardForkQueryInfo :: All SingleEraBlock xs
                  => HardForkQuery xs result -> NS SingleEraInfo xs
hardForkQueryInfo = go
  where
    go :: All SingleEraBlock xs'
       => HardForkQuery xs' result -> NS SingleEraInfo xs'
    go (QZ qry) = Z (queryInfo qry)
    go (QS qry) = S (go qry)

shiftMismatch :: MismatchEraInfo xs -> MismatchEraInfo (x ': xs)
shiftMismatch = MismatchEraInfo . MS . getMismatchEraInfo
