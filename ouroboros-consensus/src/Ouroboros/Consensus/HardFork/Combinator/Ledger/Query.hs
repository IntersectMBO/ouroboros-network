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

import           Cardano.Binary (enforceSize)
import           Cardano.Slotting.Slot (WithOrigin (At))
import           Codec.CBOR.Decoding (Decoder)
import qualified Codec.CBOR.Decoding as Dec
import           Codec.CBOR.Encoding (Encoding)
import qualified Codec.CBOR.Encoding as Enc
import           Codec.Serialise (Serialise (..))
import           Data.Bifunctor
import           Data.Functor.Product
import           Data.Kind (Type)
import           Data.Proxy
import           Data.SOP.Counting (getExactly)
import           Data.SOP.Functors (Flip (..))
import           Data.SOP.Index (Index, hcimap, projectNP)
import           Data.SOP.Match (Mismatch (..), matchNS, mustMatchNS)
import           Data.SOP.Strict
import qualified Data.SOP.Telescope as T (Telescope (TS, TZ), tip)
import           Data.Type.Equality
import           Data.Typeable (Typeable)
import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Config
import           Ouroboros.Consensus.HardFork.Abstract (hardForkSummary)
import           Ouroboros.Consensus.HardFork.Combinator.Abstract
import           Ouroboros.Consensus.HardFork.Combinator.AcrossEras
import           Ouroboros.Consensus.HardFork.Combinator.Basics
import           Ouroboros.Consensus.HardFork.Combinator.Block
import           Ouroboros.Consensus.HardFork.Combinator.Info
import           Ouroboros.Consensus.HardFork.Combinator.Ledger ()
import           Ouroboros.Consensus.HardFork.Combinator.PartialConfig
import           Ouroboros.Consensus.HardFork.Combinator.State (Current (..),
                     HardForkState (..), Past (..), Situated (..))
import qualified Ouroboros.Consensus.HardFork.Combinator.State as State
import           Ouroboros.Consensus.HardFork.History (Bound (..), EraParams,
                     Shape (..))
import qualified Ouroboros.Consensus.HardFork.History as History
import           Ouroboros.Consensus.HeaderValidation
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Ledger.Extended
import           Ouroboros.Consensus.Ledger.Query
import           Ouroboros.Consensus.Ledger.Tables.Utils
import           Ouroboros.Consensus.Node.Serialisation (Some (..))
import           Ouroboros.Consensus.Storage.LedgerDB.BackingStore
import           Ouroboros.Consensus.TypeFamilyWrappers (WrapChainDepState (..))
import           Ouroboros.Consensus.Util (ShowProxy)

instance Typeable xs => ShowProxy (BlockQuery (HardForkBlock xs)) where

instance All SingleEraBlock xs => ShowQuery (BlockQuery (HardForkBlock xs)) where
  showResult (QueryAnytime   qry _) result = showResult qry result
  showResult (QueryHardFork  qry)   result = showResult qry result
  showResult (QueryIfCurrent qry)  mResult =
      case mResult of
        Left  err    -> show err
        Right result -> showResult qry result

type HardForkQueryResult xs = Either (MismatchEraInfo xs)

data instance BlockQuery (HardForkBlock xs) :: Type -> Type where
  -- | Answer a query about an era if it is the current one.
  QueryIfCurrent ::
       QueryIfCurrent xs result
    -> BlockQuery (HardForkBlock xs) (HardForkQueryResult xs result)

  -- | Answer a query about an era from /any/ era.
  --
  -- NOTE: we don't allow this when there is only a single era, so that the
  -- HFC applied to a single era is still isomorphic to the single era.
  QueryAnytime ::
       IsNonEmpty xs
    => QueryAnytime result
    -> EraIndex (x ': xs)
    -> BlockQuery (HardForkBlock (x ': xs)) result

  -- | Answer a query about the hard fork combinator
  --
  -- NOTE: we don't allow this when there is only a single era, so that the
  -- HFC applied to a single era is still isomorphic to the single era.
  QueryHardFork ::
       IsNonEmpty xs
    => QueryHardFork (x ': xs) result
    -> BlockQuery (HardForkBlock (x ': xs)) result

instance ( HasLedgerTables (LedgerState (HardForkBlock xs))
         , All SingleEraBlock xs
         , LedgerTablesCanHardFork xs
         )
      => QueryLedger (HardForkBlock xs) where
  answerBlockQuery
    (ExtLedgerCfg cfg)
    query
    dlv@(DiskLedgerView (ExtLedgerState st@(HardForkLedgerState hardForkState) _) _ _ _) =
      case query of
        QueryIfCurrent queryIfCurrent ->
          interpretQueryIfCurrent
            cfgs
            queryIfCurrent
            (distribDiskLedgerView dlv)
        QueryAnytime queryAnytime (EraIndex era) -> pure $
          interpretQueryAnytime
            lcfg
            queryAnytime
            (EraIndex era)
            hardForkState
        QueryHardFork queryHardFork -> pure $
          interpretQueryHardFork
            lcfg
            queryHardFork
            st
    where
      cfgs = hmap ExtLedgerCfg $ distribTopLevelConfig ei cfg
      lcfg = configLedger cfg
      ei   = State.epochInfoLedger lcfg hardForkState

  getQueryKeySets = \case
    QueryIfCurrent queryIfCurrent -> getQueryKeySetsIfCurrent queryIfCurrent
    QueryAnytime _ _              -> emptyLedgerTables
    QueryHardFork _               -> emptyLedgerTables

  tableTraversingQuery = \case
    QueryIfCurrent queryIfCurrent -> tableTraversingQueryIfCurrent queryIfCurrent
    QueryAnytime _ _ -> Nothing
    QueryHardFork _  -> Nothing

distribDiskLedgerView ::
     forall xs m.
     ( Monad m
     , All SingleEraBlock xs
     , LedgerTablesCanHardFork xs
     )
  => DiskLedgerView m (ExtLedgerState (HardForkBlock xs))
  -> NS (DiskLedgerView m :.: ExtLedgerState) xs
distribDiskLedgerView dlv =
   hcimap (Proxy @Top) f $
     mustMatchNS
     "HeaderState"
     (distribHeaderState headerState)
     (State.tip (hardForkLedgerStatePerEra ledgerState))
 where
   DiskLedgerView (ExtLedgerState ledgerState headerState) query rangeQuery closer = dlv

   f :: Index xs x
     -> Product HeaderState (Flip LedgerState EmptyMK) x
     -> (DiskLedgerView m :.: ExtLedgerState) x
   f idx (Pair hst lst) = Comp
                        $ DiskLedgerView
                            (ExtLedgerState (unFlip lst) hst)
                            (query' idx)
                            (rangeQuery' idx)
                            closer

   query' :: forall x.
        Index xs x
     -> LedgerTables (ExtLedgerState x) KeysMK
     -> m (LedgerTables (ExtLedgerState x) ValuesMK)
   query' i = fmap (distribLedgerTables i) . query . injectLedgerTables i

   rangeQuery' :: forall x.
        Index xs x
     -> RangeQuery (LedgerTables (ExtLedgerState x) KeysMK)
     -> m (LedgerTables (ExtLedgerState x) ValuesMK)
   rangeQuery' i = fmap (distribLedgerTables i) . rangeQuery . injectRangeQuery i

   injectLedgerTables :: forall x mk.
        IsMapKind mk
     => Index xs x
     -> LedgerTables (ExtLedgerState x) mk
     -> LedgerTables (ExtLedgerState (HardForkBlock xs)) mk
   injectLedgerTables i = ExtLedgerStateTables
                        . applyInjectLedgerTables (projectNP i hardForkInjectLedgerTables)
                        . unExtLedgerStateTables

   injectRangeQuery :: forall x mk.
        IsMapKind mk
     => Index xs x
     -> RangeQuery (LedgerTables (ExtLedgerState x) mk)
     -> RangeQuery (LedgerTables (ExtLedgerState (HardForkBlock xs)) mk)
   injectRangeQuery i (RangeQuery ks p) = RangeQuery (fmap (injectLedgerTables i) ks) p

   distribLedgerTables :: forall x mk.
        IsMapKind mk
     => Index xs x
     -> LedgerTables (ExtLedgerState (HardForkBlock xs)) mk
     -> LedgerTables (ExtLedgerState x) mk
   distribLedgerTables i = ExtLedgerStateTables
                         . applyDistribLedgerTables (projectNP i hardForkInjectLedgerTables)
                         . unExtLedgerStateTables

-- | Precondition: the 'ledgerState' and 'headerState' should be from the same
-- era. In practice, this is _always_ the case, unless the 'ExtLedgerState' was
-- manually crafted.
distribExtLedgerState ::
     All SingleEraBlock xs
  => ExtLedgerState (HardForkBlock xs) mk -> NS (Flip ExtLedgerState mk) xs
distribExtLedgerState (ExtLedgerState ledgerState headerState) =
    hmap (\(Pair hst lst) -> Flip $ ExtLedgerState (unFlip lst) hst) $
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

instance All SingleEraBlock xs => SameDepIndex (BlockQuery (HardForkBlock xs)) where
  sameDepIndex (QueryIfCurrent qry) (QueryIfCurrent qry') =
      apply Refl <$> sameDepIndex qry qry'
  sameDepIndex (QueryIfCurrent {}) _ =
      Nothing
  sameDepIndex (QueryAnytime qry era) (QueryAnytime qry' era')
    | era == era'
    = sameDepIndex qry qry'
    | otherwise
    = Nothing
  sameDepIndex (QueryAnytime {}) _ =
      Nothing
  sameDepIndex (QueryHardFork qry) (QueryHardFork qry') =
      sameDepIndex qry qry'
  sameDepIndex (QueryHardFork {}) _ =
      Nothing

deriving instance All SingleEraBlock xs => Show (BlockQuery (HardForkBlock xs) result)

getHardForkQuery :: BlockQuery (HardForkBlock xs) result
                 -> (forall result'.
                          result :~: HardForkQueryResult xs result'
                       -> QueryIfCurrent xs result'
                       -> r)
                 -> (forall x' xs'.
                          xs :~: x' ': xs'
                       -> ProofNonEmpty xs'
                       -> QueryAnytime result
                       -> EraIndex xs
                       -> r)
                 -> (forall x' xs'.
                          xs :~: x' ': xs'
                       -> ProofNonEmpty xs'
                       -> QueryHardFork xs result
                       -> r)
                 -> r
getHardForkQuery q k1 k2 k3 = case q of
    QueryIfCurrent qry   -> k1 Refl qry
    QueryAnytime qry era -> k2 Refl (isNonEmpty Proxy) qry era
    QueryHardFork qry    -> k3 Refl (isNonEmpty Proxy) qry

{-------------------------------------------------------------------------------
  Current era queries
-------------------------------------------------------------------------------}

data QueryIfCurrent :: [Type] -> Type -> Type where
  QZ :: BlockQuery x result      -> QueryIfCurrent (x ': xs) result
  QS :: QueryIfCurrent xs result -> QueryIfCurrent (x ': xs) result

deriving instance All SingleEraBlock xs => Show (QueryIfCurrent xs result)

instance All SingleEraBlock xs => ShowQuery (QueryIfCurrent xs) where
  showResult (QZ qry) = showResult qry
  showResult (QS qry) = showResult qry

instance All SingleEraBlock xs => SameDepIndex (QueryIfCurrent xs) where
  sameDepIndex (QZ qry) (QZ qry') = sameDepIndex qry qry'
  sameDepIndex (QS qry) (QS qry') = sameDepIndex qry qry'
  sameDepIndex _        _         = Nothing

queryIfCurrent2NS :: forall xs f b.
                     (All SingleEraBlock xs)
                  => (forall x . SingleEraBlock x => BlockQuery x b -> f x)
                  -> QueryIfCurrent xs b
                  -> NS f xs
queryIfCurrent2NS f (QZ q) = Z $ f q
queryIfCurrent2NS f (QS q) = S (queryIfCurrent2NS f q)

interpretQueryIfCurrent ::
     forall result xs m. (Monad m, All SingleEraBlock xs)
  => NP ExtLedgerCfg xs
  -> QueryIfCurrent xs result
  -> NS (DiskLedgerView m :.: ExtLedgerState) xs
  -> m (HardForkQueryResult xs result)
interpretQueryIfCurrent = go
  where
    go :: All SingleEraBlock xs'
       => NP ExtLedgerCfg xs'
       -> QueryIfCurrent xs' result
       -> NS (DiskLedgerView m :.: ExtLedgerState) xs'
       -> m (HardForkQueryResult xs' result)
    go (c :* _)  (QZ qry) (Z (Comp st)) =
        Right <$> answerBlockQuery c qry st
    go (_ :* cs) (QS qry) (S st) =
        first shiftMismatch <$> go cs qry st
    go _         (QZ qry) (S st) =
        pure $ Left $ MismatchEraInfo $ ML (queryInfo qry) (hcmap proxySingle f st)
      where
        f (Comp (DiskLedgerView s _ _ _)) = ledgerInfo s
    go _         (QS qry) (Z (Comp (DiskLedgerView st _ _ _))) =
        pure $ Left $ MismatchEraInfo $ MR (hardForkQueryInfo qry) (ledgerInfo st)

getQueryKeySetsIfCurrent ::
     forall result xs. (All SingleEraBlock xs, LedgerTablesCanHardFork xs)
  => QueryIfCurrent xs result
  -> LedgerTables (LedgerState (HardForkBlock xs)) KeysMK
getQueryKeySetsIfCurrent = go hardForkInjectLedgerTables
  where
    go :: All SingleEraBlock xs' => NP (InjectLedgerTables xs) xs'
       -> QueryIfCurrent xs' result
       -> LedgerTables (LedgerState (HardForkBlock xs)) KeysMK
    go (_ :* n) (QS query) = go n query
    go (f :* _) (QZ query) = applyInjectLedgerTables f $ getQueryKeySets query
    -- This case cannot happen because xs matches in both the query and the np
    -- of injections, so there is no way we would reach the end of the np
    -- without having matched before if we go unwrapping them at the same steps
    go Nil _               = error "Mismatch in the number of eras!"

-- TODO: #4399 This whole logic can use an audit
tableTraversingQueryIfCurrent ::
  forall result xs. (All SingleEraBlock xs) =>
    QueryIfCurrent xs result
    -> Maybe (TraversingQueryHandler (HardForkBlock xs) (Either (MismatchEraInfo xs) result))
tableTraversingQueryIfCurrent = go
  where
    go :: All SingleEraBlock xs'
       => QueryIfCurrent xs' result
       -> Maybe (TraversingQueryHandler
                   (HardForkBlock xs')
                   (Either (MismatchEraInfo xs') result)
                )
    go q@(QS query) = (shiftTQH q) <$> go query
    go q@(QZ query) = fmap (distrib q) $ tableTraversingQuery query

    -- Embed a traverser for era x into a traverser for the hardfork x:(...)
    distrib ::
         All SingleEraBlock (x ': xs')
      => QueryIfCurrent (x ': xs') result
      -> TraversingQueryHandler x result
      -> TraversingQueryHandler
             (HardForkBlock (x ': xs'))
             (Either (MismatchEraInfo (x ': xs')) result)
    distrib qry (TraversingQueryHandler partial mt c p) =
      TraversingQueryHandler
         (f . distribExtLedgerState)
         (Right mt)
         (\x y -> c <$> x <*> y)
         (fmap p)
      where
        f (Z (Flip v)) = Right $ partial v
        f ss           = Left
                       $ MismatchEraInfo
                       $ case matchNS
                                (queryIfCurrent2NS queryInfo qry)
                                (hcmap proxySingle (ledgerInfo . unFlip) ss) of
                           Left v  -> v
                           Right _ -> error "Impossible!"


    -- Having a traverser for xs, embed it into a traverser for x:xs
    shiftTQH ::
         All SingleEraBlock (x' ': xs')
      => QueryIfCurrent (x' ': xs') result
      -> TraversingQueryHandler
           (HardForkBlock xs')
           (Either (MismatchEraInfo xs') result)
      -> TraversingQueryHandler
           (HardForkBlock (x' ': xs'))
           (Either (MismatchEraInfo (x' ': xs')) result)
    shiftTQH qry (TraversingQueryHandler partial mt c ps) =
      TraversingQueryHandler
        (\ e -> f e . distribExtLedgerState $ e)
        (Right mt)
        (\x y -> c <$> x <*> y)
        (>>= first shiftMismatch . ps)
      where
        f e (S _) = fmap partial $ shiftExtLedgerState qry e
        f _ ss    = Left
                  $ MismatchEraInfo
                  $ case matchNS
                          (queryIfCurrent2NS queryInfo qry)
                          (hcmap proxySingle (ledgerInfo . unFlip) ss) of
                      Left v  -> v
                      Right _ -> error "Impossible!"

    -- Given a ledger state of x:xs which MUST BE in xs (because the query is if
    -- current and we just got this DiskLedgerView), then produce a state for
    -- one of xs. If not, produce a mismatch error.
    shiftExtLedgerState ::
         All SingleEraBlock (x ': xs')
      => QueryIfCurrent (x ': xs') result
      -> ExtLedgerState (HardForkBlock (x ': xs')) mk
      -> Either (MismatchEraInfo (x ': xs')) (ExtLedgerState (HardForkBlock xs') mk)
    shiftExtLedgerState qry (ExtLedgerState lstate (HeaderState tip chainDepState)) = do
        ls <- HardForkLedgerState . HardForkState <$> lstate'
        t <- tip'
        cd <- chainDepState'
        pure (ExtLedgerState ls (HeaderState t cd))
      where
        mismatchError = MismatchEraInfo
                      $ case matchNS
                             (queryIfCurrent2NS queryInfo qry)
                             (hcmap proxySingle g (T.tip . getHardForkState . hardForkLedgerStatePerEra $ lstate)) of
                          Left v  -> v
                          Right _ -> error "Impossible!"

        lstate' = case getHardForkState . hardForkLedgerStatePerEra $ lstate of
          T.TS _ ts -> Right ts
          T.TZ _    -> Left mismatchError

        chainDepState' = case getHardForkState chainDepState of
          T.TS _ cd -> Right $ HardForkState cd
          T.TZ _    -> Left mismatchError

        tip' = case tip of
          Origin -> Right Origin
          At (AnnTip s b (OneEraTipInfo i)) ->
            case i of
              S ti -> Right (At (AnnTip s b (OneEraTipInfo ti)))
              Z{}  -> Left mismatchError

        g :: forall f blk. SingleEraBlock blk => f blk -> LedgerEraInfo blk
        g _ = LedgerEraInfo $ singleEraInfo (Proxy @blk)

{-------------------------------------------------------------------------------
  Any era queries
-------------------------------------------------------------------------------}

data QueryAnytime result where
  GetEraStart :: QueryAnytime (Maybe Bound)

deriving instance Show (QueryAnytime result)

instance ShowQuery QueryAnytime where
  showResult GetEraStart = show

instance SameDepIndex QueryAnytime where
  sameDepIndex GetEraStart GetEraStart = Just Refl

interpretQueryAnytime ::
     forall result xs mk. All SingleEraBlock xs
  => HardForkLedgerConfig xs
  -> QueryAnytime result
  -> EraIndex xs
  -> State.HardForkState (Flip LedgerState mk) xs
  -> result
interpretQueryAnytime cfg query (EraIndex era) st =
    answerQueryAnytime cfg query (State.situate era st)

answerQueryAnytime ::
     All SingleEraBlock xs
  => HardForkLedgerConfig xs
  -> QueryAnytime result
  -> Situated h (Flip LedgerState mk) xs
  -> result
answerQueryAnytime HardForkLedgerConfig{..} =
    go cfgs (getExactly (getShape hardForkLedgerConfigShape))
  where
    cfgs = getPerEraLedgerConfig hardForkLedgerConfigPerEra

    go :: All SingleEraBlock xs'
       => NP WrapPartialLedgerConfig xs'
       -> NP (K EraParams) xs'
       -> QueryAnytime result
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

data QueryHardFork xs result where
  GetInterpreter :: QueryHardFork xs (History.Interpreter xs)
  GetCurrentEra  :: QueryHardFork xs (EraIndex xs)

deriving instance Show (QueryHardFork xs result)

instance All SingleEraBlock xs => ShowQuery (QueryHardFork xs) where
  showResult GetInterpreter = show
  showResult GetCurrentEra  = show

instance SameDepIndex (QueryHardFork xs) where
  sameDepIndex GetInterpreter GetInterpreter =
      Just Refl
  sameDepIndex GetInterpreter _ =
      Nothing
  sameDepIndex GetCurrentEra GetCurrentEra =
      Just Refl
  sameDepIndex GetCurrentEra _ =
      Nothing

interpretQueryHardFork ::
     All SingleEraBlock xs
  => HardForkLedgerConfig xs
  -> QueryHardFork xs result
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

instance Serialise (Some QueryAnytime) where
  encode (Some GetEraStart) = mconcat [
        Enc.encodeListLen 1
      , Enc.encodeWord8 0
      ]

  decode = do
    enforceSize "QueryAnytime" 1
    tag <- Dec.decodeWord8
    case tag of
      0 -> return $ Some GetEraStart
      _ -> fail $ "QueryAnytime: invalid tag " ++ show tag

encodeQueryAnytimeResult :: QueryAnytime result -> result -> Encoding
encodeQueryAnytimeResult GetEraStart = encode

decodeQueryAnytimeResult :: QueryAnytime result -> forall s. Decoder s result
decodeQueryAnytimeResult GetEraStart = decode

encodeQueryHardForkResult ::
     SListI xs
  => QueryHardFork xs result -> result -> Encoding
encodeQueryHardForkResult = \case
    GetInterpreter -> encode
    GetCurrentEra  -> encode

decodeQueryHardForkResult ::
     SListI xs
  => QueryHardFork xs result -> forall s. Decoder s result
decodeQueryHardForkResult = \case
    GetInterpreter -> decode
    GetCurrentEra  -> decode

{-------------------------------------------------------------------------------
  Auxiliary
-------------------------------------------------------------------------------}

ledgerInfo :: forall blk mk. SingleEraBlock blk
           => ExtLedgerState blk mk
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
