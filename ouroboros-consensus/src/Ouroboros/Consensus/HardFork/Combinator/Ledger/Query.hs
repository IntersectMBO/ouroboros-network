{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE EmptyCase             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Ouroboros.Consensus.HardFork.Combinator.Ledger.Query (
    Query(..)
  , QueryIfCurrent(..)
  , HardForkQueryResult
  , QueryAnytime(..)
  , getHardForkQuery
  , hardForkQueryInfo
  , encodeQueryAnytimeResult
  , decodeQueryAnytimeResult
  ) where

import           Codec.CBOR.Decoding (Decoder)
import qualified Codec.CBOR.Decoding as Dec
import           Codec.CBOR.Encoding (Encoding)
import qualified Codec.CBOR.Encoding as Enc
import           Codec.Serialise (Serialise (..))
import           Data.Bifunctor
import           Data.Proxy
import           Data.SOP.Strict
import           Data.Type.Equality

import           Cardano.Binary (enforceSize)

import           Ouroboros.Consensus.HardFork.History (Bound (..), EraParams,
                     Shape (..))
import qualified Ouroboros.Consensus.HardFork.History as History
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Node.Serialisation (Some (..))

import           Ouroboros.Consensus.HardFork.Combinator.Abstract
import           Ouroboros.Consensus.HardFork.Combinator.AcrossEras
import           Ouroboros.Consensus.HardFork.Combinator.Basics
import           Ouroboros.Consensus.HardFork.Combinator.Info
import           Ouroboros.Consensus.HardFork.Combinator.Ledger ()
import           Ouroboros.Consensus.HardFork.Combinator.PartialConfig
import           Ouroboros.Consensus.HardFork.Combinator.State (Current (..),
                     Past (..), Situated (..))
import qualified Ouroboros.Consensus.HardFork.Combinator.State as State
import           Ouroboros.Consensus.HardFork.Combinator.Util.Match
                     (Mismatch (..))

instance All SingleEraBlock xs => ShowQuery (Query (HardForkBlock xs)) where
  showResult (QueryAnytime   qry _) result = showResult qry result
  showResult (QueryIfCurrent qry)  mResult =
      case mResult of
        Left  err    -> show err
        Right result -> showResult qry result

type HardForkQueryResult xs = Either (MismatchEraInfo xs)

instance All SingleEraBlock xs => QueryLedger (HardForkBlock xs) where
  data Query (HardForkBlock xs) :: * -> * where
    -- | Answer a query about an era if it is the current one.
    QueryIfCurrent ::
         QueryIfCurrent xs result
      -> Query (HardForkBlock xs) (HardForkQueryResult xs result)

    -- | Answer a query about an era from /any/ era.
    --
    -- NOTE: this is restricted to eras other than the first era so that the
    -- HFC applied to a single era is still isomorphic to the single era.
    QueryAnytime ::
         QueryAnytime result
      -> EraIndex xs
      -> Query (HardForkBlock (x ': xs)) result

  answerQuery hardForkConfig@HardForkLedgerConfig{..}
              query
              (HardForkLedgerState hardForkState) =
      case query of
        QueryIfCurrent queryIfCurrent ->
          interpretQueryIfCurrent
            ei
            cfgs
            queryIfCurrent
            (State.tip hardForkState)
        QueryAnytime queryAnytime (EraIndex era) ->
          interpretQueryAnytime
            hardForkConfig
            queryAnytime
            (EraIndex (S era))
            hardForkState
    where
      cfgs = getPerEraLedgerConfig hardForkLedgerConfigPerEra
      ei   = State.epochInfoLedger hardForkConfig hardForkState

  eqQuery (QueryAnytime qry era) (QueryAnytime qry' era')
    | era == era'
    = eqQueryAnytime qry qry'
    | otherwise
    = Nothing
  eqQuery (QueryIfCurrent qry) (QueryIfCurrent qry') =
      apply Refl <$> eqQueryIfCurrent qry qry'
  eqQuery (QueryIfCurrent {}) (QueryAnytime {}) =
      Nothing
  eqQuery (QueryAnytime {}) (QueryIfCurrent {}) =
      Nothing

deriving instance All SingleEraBlock xs => Show (Query (HardForkBlock xs) result)

getHardForkQuery :: Query (HardForkBlock xs) result
                 -> (forall result'.
                          result :~: HardForkQueryResult xs result'
                       -> QueryIfCurrent xs result'
                       -> r)
                 -> (forall x' xs'.
                          xs :~: x' ': xs'
                       -> QueryAnytime result
                       -> EraIndex xs'
                       -> r)
                 -> r
getHardForkQuery (QueryIfCurrent qry) k1 _   = k1 Refl qry
getHardForkQuery (QueryAnytime qry era) _ k2 = k2 Refl qry era

{-------------------------------------------------------------------------------
  Current era queries
-------------------------------------------------------------------------------}

data QueryIfCurrent :: [*] -> * -> * where
  QZ :: Query x result           -> QueryIfCurrent (x ': xs) result
  QS :: QueryIfCurrent xs result -> QueryIfCurrent (x ': xs) result

deriving instance All SingleEraBlock xs => Show (QueryIfCurrent xs result)

instance All SingleEraBlock xs => ShowQuery (QueryIfCurrent xs) where
  showResult (QZ qry) = showResult qry
  showResult (QS qry) = showResult qry

eqQueryIfCurrent ::
     All SingleEraBlock xs
  => QueryIfCurrent xs result
  -> QueryIfCurrent xs result'
  -> Maybe (result :~: result')
eqQueryIfCurrent (QZ qry) (QZ qry') = eqQuery qry qry'
eqQueryIfCurrent (QS qry) (QS qry') = eqQueryIfCurrent qry qry'
eqQueryIfCurrent _        _         = Nothing

interpretQueryIfCurrent ::
     forall result xs. All SingleEraBlock xs
  => EpochInfo Identity
  -> NP WrapPartialLedgerConfig xs
  -> QueryIfCurrent xs result
  -> NS LedgerState xs
  -> HardForkQueryResult xs result
interpretQueryIfCurrent ei = go
  where
    go :: All SingleEraBlock xs'
       => NP WrapPartialLedgerConfig xs'
       -> QueryIfCurrent xs' result
       -> NS LedgerState xs'
       -> HardForkQueryResult xs' result
    go (c :* _)  (QZ qry) (Z st) =
        Right $ answerQuery (completeLedgerConfig' ei c) qry st
    go (_ :* cs) (QS qry) (S st) =
        first shiftMismatch $ go cs qry st
    go _         (QZ qry) (S st) =
        Left $ MismatchEraInfo $ ML (queryInfo qry) (hcmap proxySingle ledgerInfo st)
    go _         (QS qry) (Z st) =
        Left $ MismatchEraInfo $ MR (hardForkQueryInfo qry) (ledgerInfo st)

{-------------------------------------------------------------------------------
  Any era queries
-------------------------------------------------------------------------------}

data QueryAnytime result where
  EraStart :: QueryAnytime (Maybe Bound)

deriving instance Show (QueryAnytime result)

instance ShowQuery QueryAnytime where
  showResult EraStart = show

eqQueryAnytime ::
     QueryAnytime result
  -> QueryAnytime result'
  -> Maybe (result :~: result')
eqQueryAnytime EraStart EraStart = Just Refl

interpretQueryAnytime ::
     forall result xs. All SingleEraBlock xs
  => HardForkLedgerConfig xs
  -> QueryAnytime result
  -> EraIndex xs
  -> State.HardForkState LedgerState xs
  -> result
interpretQueryAnytime cfg query (EraIndex era) st =
    answerQueryAnytime cfg query (State.situate era st)

answerQueryAnytime ::
     All SingleEraBlock xs
  => HardForkLedgerConfig xs
  -> QueryAnytime result
  -> Situated h LedgerState xs
  -> result
answerQueryAnytime HardForkLedgerConfig{..} =
    go cfgs (getShape hardForkLedgerConfigShape)
  where
    cfgs = getPerEraLedgerConfig hardForkLedgerConfigPerEra

    go :: All SingleEraBlock xs'
       => NP WrapPartialLedgerConfig xs'
       -> NP (K EraParams) xs'
       -> QueryAnytime result
       -> Situated h LedgerState xs'
       -> result
    go Nil       _             _        ctxt = case ctxt of {}
    go (c :* cs) (K ps :* pss) EraStart ctxt = case ctxt of
      SituatedShift ctxt'   -> go cs pss EraStart ctxt'
      SituatedFuture _ _    -> Nothing
      SituatedPast past _   -> Just $ pastStart past
      SituatedCurrent cur _ -> Just $ currentStart cur
      SituatedNext cur _    ->
        History.mkUpperBound ps (currentStart cur) <$>
          singleEraTransition
          (unwrapPartialLedgerConfig c)
          ps
          (currentStart cur)
          (currentState cur)

{-------------------------------------------------------------------------------
  Serialisation
-------------------------------------------------------------------------------}

instance Serialise (Some QueryAnytime) where
  encode (Some EraStart) = mconcat [
        Enc.encodeListLen 1
      , Enc.encodeWord8 0
      ]

  decode = do
    enforceSize "QueryAnytime" 1
    tag <- Dec.decodeWord8
    case tag of
      0 -> return $ Some EraStart
      _ -> fail $ "QueryAnytime: invalid tag " ++ show tag

encodeQueryAnytimeResult :: QueryAnytime result -> result -> Encoding
encodeQueryAnytimeResult EraStart = encode

decodeQueryAnytimeResult :: QueryAnytime result -> forall s. Decoder s result
decodeQueryAnytimeResult EraStart = decode

{-------------------------------------------------------------------------------
  Auxiliary
-------------------------------------------------------------------------------}

ledgerInfo :: forall blk. SingleEraBlock blk
           => LedgerState blk
           -> LedgerEraInfo blk
ledgerInfo _ = LedgerEraInfo $ singleEraInfo (Proxy @blk)

queryInfo :: forall blk query result. SingleEraBlock blk
          => query blk result -> SingleEraInfo blk
queryInfo _ = singleEraInfo (Proxy @blk)

hardForkQueryInfo :: All SingleEraBlock xs
                  => QueryIfCurrent xs result -> NS SingleEraInfo xs
hardForkQueryInfo = go
  where
    go :: All SingleEraBlock xs'
       => QueryIfCurrent xs' result -> NS SingleEraInfo xs'
    go (QZ qry) = Z (queryInfo qry)
    go (QS qry) = S (go qry)

shiftMismatch :: MismatchEraInfo xs -> MismatchEraInfo (x ': xs)
shiftMismatch = MismatchEraInfo . MS . getMismatchEraInfo
