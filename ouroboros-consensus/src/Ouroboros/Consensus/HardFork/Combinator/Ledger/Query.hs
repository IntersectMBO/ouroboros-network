{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE EmptyCase             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE LambdaCase            #-}
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
    BlockQuery (..)
  , HardForkQueryResult
  , QueryAnytime (..)
  , QueryHardFork (..)
  , QueryIfCurrent (..)
  , decodeQueryAnytimeResult
  , decodeQueryHardForkResult
  , encodeQueryAnytimeResult
  , encodeQueryHardForkResult
  , getHardForkQuery
  , hardForkQueryInfo
  ) where

import           Codec.CBOR.Decoding (Decoder)
import qualified Codec.CBOR.Decoding as Dec
import           Codec.CBOR.Encoding (Encoding)
import qualified Codec.CBOR.Encoding as Enc
import           Codec.Serialise (Serialise (..))
import           Control.Applicative (liftA2)
import           Data.Bifunctor
import           Data.Functor.Product
import           Data.Kind (Type)
import           Data.Proxy
import           Data.SOP.Strict
import           Data.Type.Equality
import           Data.Typeable (Typeable)

import           Cardano.Binary (enforceSize)

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Config
import           Ouroboros.Consensus.HardFork.Abstract (hardForkSummary)
import           Ouroboros.Consensus.HardFork.History (Bound (..), EraParams,
                     Shape (..))
import qualified Ouroboros.Consensus.HardFork.History as History
import           Ouroboros.Consensus.HeaderValidation
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Ledger.Extended
import           Ouroboros.Consensus.Ledger.Query
import           Ouroboros.Consensus.TypeFamilyWrappers (WrapChainDepState (..))
import           Ouroboros.Consensus.Util (ShowProxy)
import           Ouroboros.Consensus.Util.Counting (getExactly)

import           Ouroboros.Consensus.HardFork.Combinator.Abstract
import           Ouroboros.Consensus.HardFork.Combinator.AcrossEras
import           Ouroboros.Consensus.HardFork.Combinator.Basics
import           Ouroboros.Consensus.HardFork.Combinator.Block
import           Ouroboros.Consensus.HardFork.Combinator.Info
import           Ouroboros.Consensus.HardFork.Combinator.Ledger ()
import           Ouroboros.Consensus.HardFork.Combinator.PartialConfig
import           Ouroboros.Consensus.HardFork.Combinator.State (Current (..),
                     Past (..), Situated (..))
import qualified Ouroboros.Consensus.HardFork.Combinator.State as State
import           Ouroboros.Consensus.HardFork.Combinator.Util.Functors
                     (Flip (..))
import           Ouroboros.Consensus.HardFork.Combinator.Util.Match
                     (Mismatch (..), mustMatchNS)
import qualified Ouroboros.Consensus.HardFork.Combinator.Util.Telescope as Tele

instance Typeable xs => ShowProxy (BlockQuery (HardForkBlock xs)) where

instance All SingleEraBlock xs => ShowQuery (BlockQuery (HardForkBlock xs)) where
  showResult (QueryAnytime   qry _) result = showResult qry result
  showResult (QueryHardFork  qry)   result = showResult qry result
  showResult (QueryIfCurrent qry)  mResult =
      case mResult of
        Left  err    -> show err
        Right result -> showResult qry result

type HardForkQueryResult xs = Either (MismatchEraInfo xs)

data instance BlockQuery (HardForkBlock xs) :: FootprintL -> Type -> Type where
  -- | Answer a query about an era if it is the current one.
  QueryIfCurrent ::
       QueryIfCurrent xs             fp result
    -> BlockQuery (HardForkBlock xs) fp (HardForkQueryResult xs result)

  -- | Answer a query about an era from /any/ era.
  --
  -- NOTE: we don't allow this when there is only a single era, so that the
  -- HFC applied to a single era is still isomorphic to the single era.
  QueryAnytime ::
       IsNonEmpty xs
    => QueryAnytime                         fp result
    -> EraIndex (x ': xs)
    -> BlockQuery (HardForkBlock (x ': xs)) fp result

  -- | Answer a query about the hard fork combinator
  --
  -- NOTE: we don't allow this when there is only a single era, so that the
  -- HFC applied to a single era is still isomorphic to the single era.
  QueryHardFork ::
       IsNonEmpty xs
    => QueryHardFork (x ': xs)              fp result
    -> BlockQuery (HardForkBlock (x ': xs)) fp result

instance
     (All SingleEraBlock xs, LedgerTablesCanHardFork xs)
  => QueryLedger (HardForkBlock xs) where
  answerBlockQuery
    (ExtLedgerCfg cfg)
    query
    ext@(ExtLedgerState st@(HardForkLedgerState hardForkState) _) =
      case query of
        QueryIfCurrent queryIfCurrent ->
          interpretQueryIfCurrent
            cfgs
            queryIfCurrent
            (distribExtLedgerState ext)
        QueryAnytime queryAnytime (EraIndex era) ->
          interpretQueryAnytime
            lcfg
            queryAnytime
            (EraIndex era)
            hardForkState
        QueryHardFork queryHardFork ->
          interpretQueryHardFork
            lcfg
            queryHardFork
            st
    where
      cfgs = hmap ExtLedgerCfg $ distribTopLevelConfig ei cfg
      lcfg = configLedger cfg
      ei   = State.epochInfoLedger lcfg hardForkState

  prepareBlockQuery = \case
      QueryIfCurrent queryIfCurrent  -> prepareQueryIfCurrent queryIfCurrent
      QueryAnytime queryAnytime _era -> proveNotLargeQuery    queryAnytime
      QueryHardFork queryHardFork    -> proveNotLargeQuery    queryHardFork

  answerWholeBlockQuery = \case
      QueryIfCurrent queryIfCurrent  -> answerWholeBlockQueryIfCurrent queryIfCurrent
      QueryAnytime queryAnytime _era -> proveNotWholeQuery             queryAnytime
      QueryHardFork queryHardFork    -> proveNotWholeQuery             queryHardFork

instance All SingleEraBlock xs => IsQuery (BlockQuery (HardForkBlock xs)) where
  classifyQuery = \case
      QueryIfCurrent queryIfCurrent  -> classifyQuery queryIfCurrent
      QueryAnytime queryAnytime _era -> classifyQuery queryAnytime
      QueryHardFork queryHardFork    -> classifyQuery queryHardFork

-- | Precondition: the 'ledgerState' and 'headerState' should be from the same
-- era. In practice, this is _always_ the case, unless the 'ExtLedgerState' was
-- manually crafted.
distribExtLedgerState ::
     All SingleEraBlock xs
  => ExtLedgerState (HardForkBlock xs) mk -> NS (Flip ExtLedgerState mk) xs
distribExtLedgerState (ExtLedgerState ledgerState headerState) =
    hmap (\(Pair hst (Flip lst)) -> Flip (ExtLedgerState lst hst)) $
      mustMatchNS
        "HeaderState"
        (distribHeaderState headerState)
        (State.tip (hardForkLedgerStatePerEra ledgerState))

-- | Precondition: the 'headerStateTip' and 'headerStateChainDep' should be from
-- the same era. In practice, this is _always_ the case, unless the
-- 'HeaderState' was manually crafted.
distribHeaderState ::
     All SingleEraBlock xs
  => HeaderState (HardForkBlock xs) -> NS HeaderState xs
distribHeaderState (HeaderState tip chainDepState) =
    case tip of
      Origin ->
        hmap (HeaderState Origin . unwrapChainDepState) (State.tip chainDepState)
      NotOrigin annTip ->
        hmap
          (\(Pair t cds) -> HeaderState (NotOrigin t) (unwrapChainDepState cds))
          (mustMatchNS "AnnTip" (distribAnnTip annTip) (State.tip chainDepState))

instance All SingleEraBlock xs => EqQuery (BlockQuery (HardForkBlock xs)) where
  eqQuery (QueryIfCurrent qry) (QueryIfCurrent qry') =
      (\Refl -> Refl) <$> eqQuery qry qry'
  eqQuery (QueryIfCurrent {}) _ =
      Nothing
  eqQuery (QueryAnytime qry era) (QueryAnytime qry' era')
    | era == era'
    = eqQuery qry qry'
    | otherwise
    = Nothing
  eqQuery (QueryAnytime {}) _ =
      Nothing
  eqQuery (QueryHardFork qry) (QueryHardFork qry') =
      eqQuery qry qry'
  eqQuery (QueryHardFork {}) _ =
      Nothing

deriving instance All SingleEraBlock xs => Show (BlockQuery (HardForkBlock xs) fp result)

getHardForkQuery :: BlockQuery (HardForkBlock xs) fp result
                 -> (forall result'.
                          result :~: HardForkQueryResult xs result'
                       -> QueryIfCurrent xs fp result'
                       -> r)
                 -> (forall x' xs'.
                          xs :~: x' ': xs'
                       -> ProofNonEmpty xs'
                       -> QueryAnytime fp result
                       -> EraIndex xs
                       -> r)
                 -> (forall x' xs'.
                          xs :~: x' ': xs'
                       -> ProofNonEmpty xs'
                       -> QueryHardFork xs fp result
                       -> r)
                 -> r
getHardForkQuery q k1 k2 k3 = case q of
    QueryIfCurrent qry   -> k1 Refl qry
    QueryAnytime qry era -> k2 Refl (isNonEmpty Proxy) qry era
    QueryHardFork qry    -> k3 Refl (isNonEmpty Proxy) qry

{-------------------------------------------------------------------------------
  Current era queries
-------------------------------------------------------------------------------}

data QueryIfCurrent :: [Type] -> FootprintL -> Type -> Type where
  QZ :: BlockQuery     x  fp result -> QueryIfCurrent (x ': xs) fp result
  QS :: QueryIfCurrent xs fp result -> QueryIfCurrent (x ': xs) fp result

deriving instance All SingleEraBlock xs => Show (QueryIfCurrent xs fp result)

instance All SingleEraBlock xs => ShowQuery (QueryIfCurrent xs) where
  showResult (QZ qry) = showResult qry
  showResult (QS qry) = showResult qry

instance All SingleEraBlock xs => EqQuery (QueryIfCurrent xs) where
  eqQuery (QZ qry) (QZ qry') = eqQuery qry qry'
  eqQuery (QS qry) (QS qry') = eqQuery qry qry'
  eqQuery _        _         = Nothing

instance All SingleEraBlock xs => IsQuery (QueryIfCurrent xs) where
  classifyQuery = \case
    QZ qry -> classifyQuery qry
    QS qry -> classifyQuery qry

interpretQueryIfCurrent ::
     forall mk fp result xs. (All SingleEraBlock xs, QuerySat mk fp)
  => NP ExtLedgerCfg xs
  -> QueryIfCurrent xs fp result
  -> NS (Flip ExtLedgerState mk) xs
  -> HardForkQueryResult xs result
interpretQueryIfCurrent = go
  where
    go :: All SingleEraBlock xs'
       => NP ExtLedgerCfg xs'
       -> QueryIfCurrent xs' fp result
       -> NS (Flip ExtLedgerState mk) xs'
       -> HardForkQueryResult xs' result
    go (c :* _)  (QZ qry) (Z st) =
        Right $ answerBlockQuery c qry (unFlip st)
    go (_ :* cs) (QS qry) (S st) =
        first shiftMismatch $ go cs qry st
    go _         (QZ qry) (S st) =
        Left $ MismatchEraInfo $ ML (queryInfo qry) (hcmap proxySingle ledgerInfo st)
    go _         (QS qry) (Z st) =
        Left $ MismatchEraInfo $ MR (hardForkQueryInfo qry) (ledgerInfo st)

prepareQueryIfCurrent ::
  forall xs result.
     (All SingleEraBlock xs, LedgerTablesCanHardFork xs)
  => QueryIfCurrent xs LargeL result
  -> LedgerTables (LedgerState (HardForkBlock xs)) KeysMK
prepareQueryIfCurrent =
    \qic -> go qic hardForkInjectLedgerTablesKeysMK
  where
    go ::
         All SingleEraBlock ys
      => QueryIfCurrent ys LargeL result
      -> NP (InjectLedgerTables xs) ys
      -> LedgerTables (LedgerState (HardForkBlock xs)) KeysMK
    go (QS qic) (_   :* injs) = go qic injs
    go (QZ bq)  (inj :* _)    =
      applyInjectLedgerTables inj $ prepareBlockQuery bq

answerWholeBlockQueryIfCurrent ::
     forall result xs. All SingleEraBlock xs
  => QueryIfCurrent xs WholeL result
  -> IncrementalQueryHandler
       (HardForkBlock xs)
       (Either (MismatchEraInfo xs) result)
answerWholeBlockQueryIfCurrent =
    go (\f -> f)
  where
    go :: All SingleEraBlock xs'
       => (forall st.
              (LedgerState (HardForkBlock xs') ValuesMK -> Either (MismatchEraInfo xs') st)
           -> (LedgerState (HardForkBlock xs)  ValuesMK -> Either (MismatchEraInfo xs)  st)
          )
       -> QueryIfCurrent xs' WholeL result
       -> IncrementalQueryHandler
            (HardForkBlock xs)
            (Either (MismatchEraInfo xs) result)
    go acc = \case
      QZ qry  -> case answerWholeBlockQuery qry of
        IncrementalQueryHandler partial empty comb post ->
          let partial' st =
                case hfStateTail st of
                  Left  st'  -> Right $ partial st'
                  Right sts' ->
                      Left
                    $ MismatchEraInfo
                    $ ML (queryInfo qry)
                    $ hcmap proxySingle ledgerInfo
                    $ hmap currentState
                    $ Tele.tip
                    $ State.getHardForkState
                    $ hardForkLedgerStatePerEra sts'
          in IncrementalQueryHandler (acc partial') (Right empty) (liftA2 comb) (fmap post)
      QS qic ->
        go
          (\f -> acc $ \sts -> case hfStateTail sts of
              Left  st   -> Left $ MismatchEraInfo $ MR (hardForkQueryInfo qic) (ledgerInfo (Flip st))
              Right sts' -> first shiftMismatch $ f sts'
          )
          qic

hfStateTail ::
                                LedgerState (HardForkBlock (x ': xs)) mk
  -> Either (LedgerState x mk) (LedgerState (HardForkBlock       xs)  mk)
hfStateTail (HardForkLedgerState (State.HardForkState tele)) = case tele of
    Tele.TZ f        -> Left $ unFlip $ currentState f
    Tele.TS _g tele' -> Right $ HardForkLedgerState $ State.HardForkState tele'

{-------------------------------------------------------------------------------
  Any era queries
-------------------------------------------------------------------------------}

data QueryAnytime (fp :: FootprintL) result where
  GetEraStart :: QueryAnytime SmallL (Maybe Bound)

deriving instance Show (QueryAnytime fp result)

instance ShowQuery QueryAnytime where
  showResult GetEraStart = show

instance EqQuery QueryAnytime where
  eqQuery GetEraStart GetEraStart = Just Refl

instance SmallQuery QueryAnytime where
  proveSmallQuery k = \case
    GetEraStart -> k

instance IsQuery QueryAnytime where

interpretQueryAnytime ::
     forall xs mk fp result. All SingleEraBlock xs
  => HardForkLedgerConfig xs
  -> QueryAnytime fp result
  -> EraIndex xs
  -> State.HardForkState (Flip LedgerState mk) xs
  -> result
interpretQueryAnytime cfg query (EraIndex era) st =
    answerQueryAnytime cfg query (State.situate era st)

answerQueryAnytime ::
     All SingleEraBlock xs
  => HardForkLedgerConfig xs
  -> QueryAnytime fp result
  -> Situated h (Flip LedgerState mk) xs
  -> result
answerQueryAnytime HardForkLedgerConfig{..} =
    go cfgs (getExactly (getShape hardForkLedgerConfigShape))
  where
    cfgs = getPerEraLedgerConfig hardForkLedgerConfigPerEra

    go :: All SingleEraBlock xs'
       => NP WrapPartialLedgerConfig xs'
       -> NP (K EraParams) xs'
       -> QueryAnytime fp result
       -> Situated h (Flip LedgerState mk) xs'
       -> result
    go Nil       _             _           ctxt = case ctxt of {}
    go (c :* cs) (K ps :* pss) GetEraStart ctxt = case ctxt of
      SituatedShift ctxt'   -> go cs pss GetEraStart ctxt'
      SituatedFuture _ _    -> Nothing
      SituatedPast past _   -> Just $ pastStart $ unK past
      SituatedCurrent cur _ -> Just $ currentStart cur
      SituatedNext cur _    ->
        History.mkUpperBound ps (currentStart cur) <$>
          singleEraTransition
          (unwrapPartialLedgerConfig c)
          ps
          (currentStart cur)
          (unFlip $ currentState cur)

{-------------------------------------------------------------------------------
  Hard fork queries
-------------------------------------------------------------------------------}

data QueryHardFork xs (fp :: FootprintL) result where
  GetInterpreter :: QueryHardFork xs SmallL (History.Interpreter xs)
  GetCurrentEra  :: QueryHardFork xs SmallL (EraIndex xs)

deriving instance Show (QueryHardFork xs fp result)

instance All SingleEraBlock xs => ShowQuery (QueryHardFork xs) where
  showResult GetInterpreter = show
  showResult GetCurrentEra  = show

instance EqQuery (QueryHardFork xs) where
  eqQuery GetInterpreter GetInterpreter =
      Just Refl
  eqQuery GetInterpreter _ =
      Nothing
  eqQuery GetCurrentEra GetCurrentEra =
      Just Refl
  eqQuery GetCurrentEra _ =
      Nothing

instance SmallQuery (QueryHardFork xs) where
  proveSmallQuery k = \case
    GetInterpreter -> k
    GetCurrentEra  -> k

instance All SingleEraBlock xs => IsQuery (QueryHardFork xs) where

interpretQueryHardFork ::
     All SingleEraBlock xs
  => HardForkLedgerConfig xs
  -> QueryHardFork xs fp result
  -> LedgerState (HardForkBlock xs) mk
  -> result
interpretQueryHardFork cfg query st =
    case query of
      GetInterpreter ->
        History.mkInterpreter $ hardForkSummary cfg st
      GetCurrentEra  ->
        eraIndexFromNS $ State.tip $ hardForkLedgerStatePerEra st

{-------------------------------------------------------------------------------
  Serialisation
-------------------------------------------------------------------------------}

instance Serialise (SomeQuery QueryAnytime) where
  encode (SomeQuery GetEraStart) = mconcat [
        Enc.encodeListLen 1
      , Enc.encodeWord8 0
      ]

  decode = do
    enforceSize "QueryAnytime" 1
    tag <- Dec.decodeWord8
    case tag of
      0 -> return $ SomeQuery GetEraStart
      _ -> fail $ "QueryAnytime: invalid tag " ++ show tag

encodeQueryAnytimeResult :: QueryAnytime fp result -> result -> Encoding
encodeQueryAnytimeResult GetEraStart = encode

decodeQueryAnytimeResult :: QueryAnytime fp result -> forall s. Decoder s result
decodeQueryAnytimeResult GetEraStart = decode

encodeQueryHardForkResult ::
     SListI xs
  => QueryHardFork xs fp result -> result -> Encoding
encodeQueryHardForkResult = \case
    GetInterpreter -> encode
    GetCurrentEra  -> encode

decodeQueryHardForkResult ::
     SListI xs
  => QueryHardFork xs fp result -> forall s. Decoder s result
decodeQueryHardForkResult = \case
    GetInterpreter -> decode
    GetCurrentEra  -> decode

{-------------------------------------------------------------------------------
  Auxiliary
-------------------------------------------------------------------------------}

ledgerInfo :: forall blk f (mk :: MapKind). SingleEraBlock blk
           => Flip f mk blk
           -> LedgerEraInfo blk
ledgerInfo _ = LedgerEraInfo $ singleEraInfo (Proxy @blk)

queryInfo :: forall query blk (fp :: FootprintL) result. SingleEraBlock blk
          => query blk fp result -> SingleEraInfo blk
queryInfo _ = singleEraInfo (Proxy @blk)

hardForkQueryInfo :: All SingleEraBlock xs
                  => QueryIfCurrent xs fp result -> NS SingleEraInfo xs
hardForkQueryInfo = go
  where
    go :: All SingleEraBlock xs'
       => QueryIfCurrent xs' fp result -> NS SingleEraInfo xs'
    go (QZ qry) = Z (queryInfo qry)
    go (QS qry) = S (go qry)

shiftMismatch :: MismatchEraInfo xs -> MismatchEraInfo (x ': xs)
shiftMismatch = MismatchEraInfo . MS . getMismatchEraInfo
